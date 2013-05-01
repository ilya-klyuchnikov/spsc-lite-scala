name := "hosc"

scalaVersion := "2.10.1"

scalaSource in Compile <<= baseDirectory(_ / "src")

scalaSource in Test <<= baseDirectory(_ / "test-src")

libraryDependencies += "junit" % "junit" % "4.10"

libraryDependencies += "com.novocode" % "junit-interface" % "0.10-M3"