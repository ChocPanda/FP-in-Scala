import sbt._
import Keys._

object Tasks {
    lazy val localConfig = config("local") extend(Test)
    lazy val localTestOptions = testOptions in localConfig := Seq(Tests.Argument("-l", "org.panda.tags.Slow"))    
}