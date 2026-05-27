enablePlugins(ScalaJSPlugin)

name := "web-dsl"
scalaVersion := "3.6.3"

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "2.8.0",
  "com.github.j-mie6" %%% "parsley" % "4.6.2"
)

scalaJSUseMainModuleInitializer := true