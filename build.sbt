name := "spsc-lite"

scalaVersion := "2.10.1"

scalaSource in Compile <<= baseDirectory(_ / "src")
