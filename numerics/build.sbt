// project name
name := "Numeric"

// shrug?
version := "0.1"

// test
libraryDependencies += "org.scalatest" % "scalatest_2.9.0" % "1.6.1"

// hide backup files
defaultExcludes ~= (filter => filter || "*~")

scalacOptions += "-optimise"

// any of these work, although 2.9.1 performs the best
//scalaVersion := "2.8.1
//scalaVersion := "2.9.0-1"
scalaVersion := "2.9.1"
