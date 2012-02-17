organization := "seance"

name := "sakismet"

version := "1.0.0"

resolvers ++= Seq(
  "scala-tools releases" at "http://scala-tools.org/repo-releases")
  
libraryDependencies ~= { seq =>
  val dispatchVersion = "0.8.7"
  seq ++ Seq(
    "net.databinder" %% "dispatch-core" % dispatchVersion,
    "net.databinder" %% "dispatch-oauth" % dispatchVersion,
    "net.databinder" %% "dispatch-nio" % dispatchVersion,
    "net.databinder" %% "dispatch-http" % dispatchVersion,
    "net.databinder" %% "dispatch-tagsoup" % dispatchVersion,
    "net.databinder" %% "dispatch-jsoup" % dispatchVersion)
}