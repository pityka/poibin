scalaVersion := "2.12.4"

libraryDependencies += "com.github.wendykierp" % "JTransforms" % "3.1"

name := "poibin"

organization := "io.github.pityka"

version := "0.0.1"

pomExtra in Global := {
  <url>https://pityka.github.io/poibin</url>
  <licenses>
    <license>
      <name>MIT</name>
      <url>https://opensource.org/licenses/GPL-2.0</url>
    </license>
  </licenses>
  <scm>
    <connection>scm:git:github.com/pityka/poibin</connection>
    <developerConnection>scm:git:git@github.com:pityka/poibin</developerConnection>
    <url>github.com/pityka/poibin</url>
  </scm>
  <developers>
    <developer>
      <id>pityka</id>
      <name>Istvan Bartha</name>
      <url>https://pityka.github.io/poibin</url>
    </developer>
  </developers>
}
