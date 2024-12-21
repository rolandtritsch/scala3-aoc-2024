import mill._
import mill.scalalib._
import mill.scalalib.scalafmt._

import $ivy.`com.goyeau::mill-scalafix::0.4.0`
import com.goyeau.mill.scalafix.ScalafixModule

import $ivy.`com.lihaoyi::mill-contrib-scoverage:`
import mill.contrib.scoverage.ScoverageModule

object main extends ScalaModule with ScoverageModule with ScalafmtModule with ScalafixModule {
  def scalaVersion = "3.6.2"
  def scalacOptions = Seq("-Wunused:imports", "-deprecation")
  def scoverageVersion = "2.2.1"
  //def scalaVersion = "3.3.4"
  //override def ammoniteVersion = "3.0.0-2-6342755f"

  def ivyDeps = Agg(
    ivy"com.typesafe.scala-logging::scala-logging:3.9.5",
    ivy"ch.qos.logback:logback-classic:1.3.5",
    ivy"org.scala-lang.modules::scala-parallel-collections:1.0.4",
    ivy"org.scala-lang.modules::scala-collection-contrib:0.4.0",
  )
  
  def scalafixIvyDeps = Agg(
    ivy"com.github.xuwei-k::scalafix-rules:0.4.3",
  )

  object test extends ScoverageTests with TestModule.Munit {
    def testCachedArgs = Seq("--include-tags=only")
    //def testCachedArgs = Seq("--exclude-tags=ignore")
    def ivyDeps = Agg(
      ivy"org.scalameta::munit::1.0.0",
      ivy"org.scalameta::munit-scalacheck:1.0.0",
      ivy"org.typelevel::spire:0.18.0",
    )
  }
}
