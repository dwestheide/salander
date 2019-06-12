import sbt._
import Keys._
import Salander.Incantation

object SalanderPlugin extends AutoPlugin {

  object Keys {
    val salanderMutanderer = taskKey[Unit]("Salander Mutanderer")
    val salanderSource =
      settingKey[File]("Directory for mutated Scala source files")
    val salanderTarget =
      settingKey[File]("Directory for compiled classes of mutated source files")
  }

  val autoImport: Keys.type = Keys

  import autoImport._

  val defaultSalanderSource = salanderSource := (Compile / target).value / "salander"
  val defaultSalanderTarget = salanderTarget := (Compile / target).value / "salander-classes"

  val salanderMutandererTask = Keys.salanderMutanderer := {
    val incantation = Incantation(
      state.value,
      (Compile / scalaSource).value,
      Keys.salanderSource.value,
      Keys.salanderTarget.value
    )
    Salander.salanderMutanderer(incantation)(streams.value.log)
  }

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(defaultSalanderSource, defaultSalanderTarget, salanderMutandererTask)

  override def trigger = allRequirements

  override def requires = empty
}
