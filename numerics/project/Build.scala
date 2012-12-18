import sbt._

object NumericBuild extends Build {
  // library
  lazy val root = Project("root", file("."))

  // plugin
  lazy val plugin = Project("plugin", file("plugin")) dependsOn(root)

  // performance testing
  lazy val perf = Project("perf", file("perf")) dependsOn(root, plugin)
}
