enablePlugins(ScalaJSPlugin)

name := "scalajs_game_demo"
version := "0.1"
scalaVersion := "2.12.7"

scalaJSUseMainModuleInitializer := true
libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.6"