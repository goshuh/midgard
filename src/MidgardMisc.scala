package midgard

import chisel3._
import chisel3.util._


package object misc {

  // log2
  def log2(i: Int): Int = {
    BigInt(i - 1).bitLength
  }


  def Pad(d: UInt, n: Int): UInt = {
    val w = d.getWidth

    if (w >= n)
      d(n - 1, 0)
    else
      Cat(0.U((n - w).W),
          d)
  }

  def OrM(s: Seq[UInt], a: Seq[UInt]): UInt = {
    val x = s.size
    val y = a.size

    require(x == y, s"OrM: unmatched widths: ${x} vs. ${y}")

    val m = a.map(_.getWidth).max

    def mux(n: Int): UInt = {
      if (n >= x)
        0.U(m.W)
      else
        Mux(s(n), Pad(a(n), m), 0.U(m.W)) | mux(n + 1)
    }

    mux(0)
  }

  def ShL(d: UInt, n: Int): UInt = {
    val w = d.getWidth

    if (n == 0)
      d
    else if (n >= w)
      0.U(w.W)
    else
      Cat(d(w - n - 1, 0),
          0.U(n.W))
  }

  def ShR(d: UInt, n: Int): UInt = {
    val w = d.getWidth

    if (n == 0)
      d
    else if (n >= w)
      0.U(w.W)
    else
      Cat(0.U(n.W),
          d(w - 1, n))
  }

  def RoL(d: UInt, n: Int): UInt = {
    val w = d.getWidth
    val m = n % w

    if (m == 0)
      d
    else
      Cat(d(w - m - 1, 0),
          d(w - 1, w - m))
  }

  def RoR(d: UInt, n: Int): UInt = {
    val w = d.getWidth
    val m = n % w

    if (m == 0)
      d
    else
      Cat(d(m - 1, 0),
          d(w - 1, m))
  }

  private def shf(f: (UInt, Int) => UInt, d: UInt, s: UInt, n: Int): UInt = {
    if (n >= s.getWidth)
      d
    else
      shf(f, Mux(s(n),
               f(d, 1 << n),
                 d),
          s, n + 1)
  }

  def BSL(d: UInt, s: UInt): UInt = {
    shf(ShL, d, s, 0)
  }

  def BSR(d: UInt, s: UInt): UInt = {
    shf(ShR, d, s, 0)
  }

  def BRL(d: UInt, s: UInt): UInt = {
    shf(RoL, d, s, 0)
  }

  def BRR(d: UInt, s: UInt): UInt = {
    shf(RoR, d, s, 0)
  }

  def Ext(d: UInt, n: Int): UInt = {
    Seq.fill(n)({
      d.asBools
    }).reduce(_ ++ _)
  }

  private def orx(f: (UInt, Int) => UInt, d: UInt, n: Int): UInt = {
    if (n >= d.getWidth)
      d
    else
      orx(f, d | f(d, n), n << 1)
  }

  def OrL(d: UInt): UInt = {
    orx(ShL, d, 1)
  }

  def OrR(d: UInt): UInt = {
    orx(ShR, d, 1)
  }

  private def tap(n: Int): Seq[Int] = {
    n match {
      case  4 => Seq( 2,  3)
      case  8 => Seq( 3,  4,  5,  7)
      case 16 => Seq(10, 12, 13, 15)
      case 32 => Seq(27, 28, 29, 30)
      case 48 => Seq(38, 40, 43, 47)
      case 64 => Seq(59, 60, 62, 63)
      case  _ => {
        require(false, s"tap: invalid width: ${n}")
        Seq()
      }
    }
  }

  def Ran(d: UInt): UInt = {
    val w = d.getWidth

    Cat(d(w - 2, 0),
        tap(w).map(d(_)).xorR)
  }

  def Enc(d: UInt, b: Int = 0): UInt = {
    val w = d.getWidth
    val x = log2(w)

    OrM(d.asBools,
        Seq.tabulate(w)(n => {
          (n + b).U(x.W)
        }))
  }

  def Dec(d: UInt): UInt = {
    val w = 1 << d.getWidth

    BSL(1.U(w.W), d)
  }

  def Pri(d: UInt): UInt = {
    val o = OrR(d)

    o ^ ShR(o, 1)
  }


  import scala.language.implicitConversions

  implicit def UIntToBool(i: UInt): Bool = {
    require(i.getWidth == 1)

    i(0).asBool
  }

  implicit def ISeqToUInt(i: Seq[UInt]): UInt = {
    VecInit(i).asUInt
  }
}