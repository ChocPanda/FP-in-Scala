import Dependencies._
import Tasks._

lazy val root = (project in file("."))
  .configs(localConfig)
  .settings(
    inThisBuild(List(
      organization := "org.panda",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT",
      localTestOptions
    )),
    
    inConfig(localConfig)(Defaults.testTasks),
    
    name := "functional",

    libraryDependencies ++= Seq(scalaTest % Test, scalaCheck % Test)
  )
