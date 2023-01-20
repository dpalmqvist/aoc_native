import scala.scalanative.build._

ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.12.17"

enablePlugins(ScalaNativePlugin)

lazy val root = (project in file("."))
  .settings(
    name := "aoc_native",
    idePackagePrefix := Some("org.norrehem.aoc")
  )

nativeConfig ~= {
  _.withLTO(LTO.full)
    .withMode(Mode.releaseFull)
    .withGC(GC.default)
}
