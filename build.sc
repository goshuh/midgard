import mill._
import scalalib._


object midgard extends ScalaModule {
  def sv = "2.12.12"

  override def millSourcePath = os.pwd
  override def scalaVersion   = sv
  override def scalacOptions  = Seq(
    "-Xsource:2.11",
    "-deprecation",
    "-feature"
  )

  // local published
  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::rocketchip:1.2-SNAPSHOT"
  )
}
