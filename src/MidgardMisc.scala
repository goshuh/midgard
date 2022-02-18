package midgard

import  scala.annotation.tailrec

import  chisel3._
import  chisel3.util._
import  chisel3.internal.firrtl._


package object util {

  // see: https://github.com/chipsalliance/chisel3/issues/1743

  def Any(d: UInt): Bool = {
    d.orR()
  }
  def All(d: UInt): Bool = {
    d.andR()
  }
  def Non(d: UInt): Bool = {
    d === 0.U
  }

  def Any(ds: Bool*): Bool = {
    Any(Cat(ds))
  }
  def All(ds: Bool*): Bool = {
    All(Cat(ds))
  }
  def Non(ds: Bool*): Bool = {
    Non(Cat(ds))
  }

  def ShL(d: UInt, n: Int): UInt = {
    val w = d.getWidth

    if (n == 0)
      d
    else if (n >= w)
      0.U(w.W)
    else
      d(w - n - 1, 0) ## 0.U(n.W)
  }
  def ShR(d: UInt, n: Int): UInt = {
    val w = d.getWidth

    if (n == 0)
      d
    else if (n >= w)
      0.U(w.W)
    else
      0.U(n.W) ## d(w - 1, n)
  }

  def RoL(d: UInt, n: Int): UInt = {
    val w = d.getWidth
    val m = n % w

    if (m == 0)
      d
    else
      d(w - m - 1, 0) ## d(w - 1, w - m)
  }
  def RoR(d: UInt, n: Int): UInt = {
    val w = d.getWidth
    val m = n % w

    if (m == 0)
      d
    else
      d(m - 1, 0) ## d(w - 1, m)
  }

  @tailrec
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

  def Rep(d: UInt, n: Int ): UInt = {
    Cat(Seq.fill(n)(d))
  }
  def Div(d: UInt, n: Int ): Seq[UInt] = {
    val s = (d.getWidth + n - 1) / n
    val t =  d.pad(s * n)

    Seq.tabulate(s) { i =>
      t(i * n + n - 1, i * n)
    }
  }

  def EnQ[T <: Data](e: Bool, d: T): T = {
    Mux(e, d, 0.U.asTypeOf(d))
  }
  def NeQ[T <: Data](e: Bool, d: T): T = {
    Mux(e, 0.U.asTypeOf(d), d)
  }

  @tailrec
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

  private def pri(f: UInt => UInt, g: (UInt, Int) => UInt, d: UInt): UInt = {
    val o = f(d)

    o ^ g(o, 1)
  }
  def PrL(d: UInt): UInt = {
    pri(OrR, ShR, d)
  }
  def PrR(d: UInt): UInt = {
    pri(OrL, ShL, d)
  }

  def OHp(d: UInt, z: Bool): Bool = {
    z && Non(d) || d === PrL(d)
  }

  def Dec = UIntToOH
  def Enc = OHToUInt
  def OrM = Mux1H


  case class pair[T1, +T2](a: T1, b: T2)

  implicit class withIInt(d: Int) {
    def :+(a: Int): pair[Int, Int] = {
      pair(d + a, d)
    }
    def :-(a: Int): pair[Int, Int] = {
      pair(d, d - a)
    }
    def :=(a: Int): pair[Int, Int] = {
      pair(d, a)
    }
  }

  implicit class withBits[T <: Bits](d: T) {
    def apply(p: pair[Int, Int]): UInt = {
      d(p.a   - 1, p.b)
    }
    def apply(p: Width): UInt = {
      d(p.get - 1, 0)
    }
  }

  implicit class withBool(d: Bool) {
    def ??[T <: Data](t: T): pair[Bool, T] = {
      pair(d, t)
    }
    def ->(c: Bool): Bool = {
      !(d && !c)
    }
  }

  implicit class withData[T <: Data](d: T) {
    def ::[S <: Data](p: pair[Bool, S]): T = {
      Mux(p.a, p.b.asInstanceOf[T], d)
    }
  }

  implicit class withDecoupled[T <: Data](d: DecoupledIO[T]) {
    def <=(s: DecoupledIO[T]): Unit = {
      d.valid := RegEnable(s.valid || d.valid && !d.ready,
                           false.B,
                           s.valid || d.valid)
      d.bits  := RegEnable(s.bits,
                           s.valid)
    }
  }

  // any explicit calls are bugs
  implicit def VecToUInt[T <: Data](v: Vec[T]): UInt = {
    v.asUInt()
  }

  implicit def SeqToUInt[T <: Data](v: Seq[T]): UInt = {
    Cat(v.map(_.asUInt()).reverse)
  }
}