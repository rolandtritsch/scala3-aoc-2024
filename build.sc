import mill._
import mill.scalalib._
import mill.scalalib.scalafmt._

import $ivy.`com.goyeau::mill-scalafix::0.4.2`
import com.goyeau.mill.scalafix.ScalafixModule

import $ivy.`com.lihaoyi::mill-contrib-scoverage:`
import mill.contrib.scoverage.ScoverageModule

object main extends ScalaModule with ScoverageModule with ScalafmtModule with ScalafixModule {
  def scalaVersion = "3.4.3"
  def scalacOptions = Seq(
    "-Wunused:imports", 
    "-deprecation", 
    "-Xfatal-warnings",
  )
  def scoverageVersion = "2.2.1"

  def ivyDeps = Agg(
    ivy"com.typesafe.scala-logging::scala-logging:3.9.5",
    ivy"ch.qos.logback:logback-classic:1.3.5",
    ivy"org.scala-lang.modules::scala-parallel-collections:1.0.4",
    ivy"org.scala-lang.modules::scala-collection-contrib:0.4.0",
    ivy"com.eed3si9n.eval:eval_3.4.0:0.3.0",
    ivy"org.scala-graph:graph-core_2.13:2.0.2",
    ivy"org.scalanlp::breeze:2.1.0",
    ivy"org.tritsch::scala-corner:1.0.4",
  )
  
  def scalafixIvyDeps = Agg(
    ivy"com.github.xuwei-k::scalafix-rules:0.5.1",
  )

  def scalafixConfig = T {
    Some(millSourcePath / ".." /".scalafix.conf")
  }
  
  object test extends ScoverageTests with TestModule.Munit with ScalafmtModule {
    def testCachedArgs = Seq("--exclude-tags=slow")
    //def testCachedArgs = Seq("--include-tags=only")
    //def testCachedArgs = Seq("--exclude-tags=ignore")
    def ivyDeps = Agg(
      ivy"org.scalameta::munit::1.0.0",
      ivy"org.scalameta::munit-scalacheck:1.0.0",
      ivy"org.typelevel::spire:0.18.0",
    )
    def forkArgs: T[Seq[String]] = Seq("-Xss1G", "-Xmx10G")
  }

  object migrate extends ScalaModule {
    def scalaVersion = main.scalaVersion
    def scalacOptions = Seq(
      "-rewrite",
      "-indent",
    )
    //def sources = main.sources
    def sources = T {
      main.sources() ++ test.sources()
    }
    def ivyDeps = T {
      main.ivyDeps() ++ test.ivyDeps()
    }
  }
}
