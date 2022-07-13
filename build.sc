import mill._
import scalalib._


object midgard extends ScalaModule {
  def sv = "2.12.12"

  def chisel        = ivy"edu.berkeley.cs::chisel3:3.5.0"
  def chisel_plugin = ivy"edu.berkeley.cs:::chisel3-plugin:3.5.0"
  def paradise      = ivy"org.scalamacros:::paradise:2.1.1"

  override def scalaVersion  = sv
  override def scalacOptions = Seq(
    "-Xsource:2.11"
  )

  override def ivyDeps             = Agg(chisel)
  override def scalacPluginIvyDeps = Agg(chisel_plugin, paradise)

  override def millSourcePath = os.pwd
}
