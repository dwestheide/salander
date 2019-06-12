import java.util.UUID
import sbt._, sbt.Keys._, sbt.io.IO, sbt.internal.util.ManagedLogger
import scala.meta._

object Salander {

  final case class Incantation(state: State,
                               scalaSourceDir: sbt.File,
                               salanderSourceDir: sbt.File,
                               salanderTargetDir: sbt.File)

  final case class SourceFile(source: Source, path: String)

  final case class Mutant(id: UUID,
                          sourceFile: SourceFile,
                          original: Tree,
                          mutated: Tree,
                          mutation: String)

  type Mutation = SourceFile => PartialFunction[Tree, List[Mutant]]

  sealed trait Result extends Product with Serializable
  final case class Detected(mutant: Mutant) extends Result
  final case class Undetected(mutant: Mutant) extends Result
  final case class Error(mutant: Mutant) extends Result

  final case class ResultStats(detectedCount: Int,
                               undetectedMutants: List[Mutant]) {
    val undectedCount: Int = undetectedMutants.size
    val total: Int = detectedCount + undectedCount
    val detectedRate: Int = if (total > 0) detectedCount * 100 / total else 0
  }
  object ResultStats {
    val empty: ResultStats = ResultStats(0, List.empty)
  }

  def salanderMutanderer(incantation: Incantation)(
      implicit logger: ManagedLogger): Unit = {

    val results = for {
      sourceFile <- scalaSourceFiles(incantation)
      mutant <- summonMutants(sourceFile)
    } yield runExperiment(incantation, mutant)

    val stats = analyseResults(results)
    logResults(stats)
  }

  private def runExperiment(incantation: Incantation, mutant: Mutant)(
      implicit logger: Logger): Result = {

    val mutantSourceDir = createSourceDir(incantation, mutant)

    val settings = mutationSettings(
      mutantSourceDir,
      incantation.salanderTargetDir / mutant.id.toString)

    val newState = Project
      .extract(incantation.state)
      .appendWithSession(settings, incantation.state)

    Project.runTask(test in Test, newState) match {
      case None                => Error(mutant)
      case Some((_, Value(_))) => Undetected(mutant)
      case Some((_, Inc(_)))   => Detected(mutant)
    }
  }

  private def analyseResults(results: Seq[Salander.Result]): ResultStats =
    results.foldLeft(ResultStats.empty) {
      case (stats @ ResultStats(detectedCount, undectedMutants), result) =>
        result match {
          case Detected(_) => ResultStats(detectedCount + 1, undectedMutants)
          case Undetected(mutant) =>
            ResultStats(detectedCount, mutant :: undectedMutants)
          case Error(_) => stats
        }
    }

  private def summonMutants(sourceFile: SourceFile): List[Mutant] =
    sourceFile.source.collect(Mutations.in(sourceFile)).flatten

  private object Mutations {

    def in(sourceFile: SourceFile): PartialFunction[Tree, List[Mutant]] =
      spellbook
        .map(_.apply(sourceFile))
        .foldLeft(PartialFunction.empty[Tree, List[Mutant]])(_.orElse(_))

    val `Change >= to > or ==` : Mutation = sourceFile => {
      case original @ Term.ApplyInfix(_, Term.Name(">="), Nil, List(_)) =>
        List(
          Mutant(UUID.randomUUID(),
            sourceFile,
            original,
            original.copy(op = Term.Name(">")),
            "changed >= to >") ,
          Mutant(UUID.randomUUID(),
            sourceFile,
            original,
            original.copy(op = Term.Name("==")),
            "changed >= to ==")
        )
    }

    val `Replace Int expression with 0`: Mutation = sourceFile => {
      case original @ Defn.Def(_, _, _, _, Some(Type.Name(declaredType)), _)
          if declaredType == "Int" =>
        List(Mutant(UUID.randomUUID(),
               sourceFile,
               original,
               original.copy(body = Lit.Int(0)),
               "replaced Int expression with 0"))
    }

    val spellbook: List[Mutation] = List(
      `Change >= to > or ==`,
      `Replace Int expression with 0`
    )
  }

  private def createSourceDir(incantation: Incantation, mutant: Mutant)(
      implicit logger: Logger): File = {
    val mutationDir = incantation.salanderSourceDir / mutant.id.toString
    IO.copyDirectory(incantation.scalaSourceDir, mutationDir)
    val mutatedPath = mutationDir / mutant.sourceFile.path
    IO.write(mutatedPath, mutatedSource(mutant).syntax, append = false)
    mutationDir
  }

  private def mutatedSource(mutant: Mutant): Tree =
    mutant.sourceFile.source.transform {
      case tree if tree.structure == mutant.original.structure =>
        mutant.mutated
    }

  private def scalaSources(base: File): Seq[File] = (base ** "*.scala").get()

  private def scalaSourceFiles(incantation: Incantation): Seq[SourceFile] =
    scalaSources(incantation.scalaSourceDir)
      .map(fileWithRelativePath(incantation.scalaSourceDir))
      .collect(defined)
      .map(Function.tupled(sourceFile))
      .collect(defined)

  private def sourceFile(file: sbt.File,
                         relativePath: String): Option[SourceFile] =
    file.parse[Source].toOption.map(SourceFile(_, relativePath))

  private def fileWithRelativePath(scalaSourceDir: sbt.File)(
      file: sbt.File): Option[(sbt.File, String)] =
    file.relativeTo(scalaSourceDir).map(rf => (file, rf.getPath))

  private def defined[A]: PartialFunction[Option[A], A] = { case Some(x) => x }

  private def mutationSettings(
      mutationSourceDir: sbt.File,
      mutationTargetDir: sbt.File): Seq[Def.Setting[_]] = Seq(
    scalaSource in Compile := mutationSourceDir,
    classDirectory in Compile := mutationTargetDir
  )

  private def logResults(stats: ResultStats)(
      implicit logger: ManagedLogger): Unit = {
    logger.info(
      s"Total mutants: ${stats.total}, " +
        s"detected mutants: ${stats.detectedCount} (${stats.detectedRate}%)")
    if (stats.undetectedMutants.nonEmpty) {
      logger.info("Undetected mutants:")
      val lines = stats.undetectedMutants.map(mutant =>
        (formatPosition(mutant.original.pos), mutant.mutation))
      val length = lines.map(_._1.length).max + 4
      lines.foreach {
        case (position, mutation) =>
          val format = s"%-${length}s%s%n"
          logger.info(String.format(format, position, mutation))
      }
    }
  }

  private def formatPosition(pos: Position): String = pos.input match {
    case Input.File(path, _) =>
      s"$path:${pos.startLine + 1}:${pos.startColumn + 1}:"
    case _ => "<unknown>"
  }

}
