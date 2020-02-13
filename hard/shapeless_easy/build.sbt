libraryDependencies ++= Seq(
  "com.chuusai" %% "shapeless" % "2.3.0",
  "org.scala-lang.modules" % "scala-jline" % "2.12.1",
  "org.scala-lang"  % "scala-compiler" % "2.11.8" % Compile


)

scalaVersion := "2.11.8"

initialCommands in (Compile, console) :=
    "import shapeless._"
