name := "CSE-4303-Computer-Graphics"

version := "1.0"

scalaVersion := "2.11.5"

scalacOptions in Test ++= Seq("-Yrangepos")

resolvers ++= Seq("snapshots", "releases").map(Resolver.sonatypeRepo)

val scalaFx       = "org.scalafx" %% "scalafx" % "8.0.31-R7"
val breeze        = "org.scalanlp" %% "breeze" % "0.10"
val breezeNatives = "org.scalanlp" %% "breeze-natives" % "0.10"

libraryDependencies ++= Seq() :+ scalaFx

libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-core" % "2.4.14" % "test",
  "org.specs2" %% "specs2-mock" % "2.4.14" % "test",
  "org.mockito" % "mockito-all" % "1.10.8"
)

//unmanagedJars in Compile += Attributed.blank(file("/usr/lib/jvm/java-7-jdk/jre/lib/jfxrt.jar"))