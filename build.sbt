lazy val root = project
  .in(file("."))
  .settings(
    name := "algorithms",
    description := "Algorithms in scala3",
    version := "0.1.0",
    scalaVersion := "3.1.0",
    bloopExportJarClassifiers in Global := Some(Set("sources"))
  )
