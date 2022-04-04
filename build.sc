import mill._
import scalalib._


object midgard extends ScalaModule {
  def sv = "2.12.12"

  override def scalaVersion  = sv
  override def scalacOptions = Seq(
    "-Xsource:2.11"
  )

  override def ivyDeps = Agg(
    ivy"edu.berkeley.cs::chisel3:3.5.0"
  )

  override def millSourcePath = os.pwd
}
