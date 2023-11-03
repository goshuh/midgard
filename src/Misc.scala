package midgard

import  scala.annotation.tailrec

import  chisel3._
import  chisel3.experimental._
import  chisel3.util._
import  chisel3.util.random._
import  chisel3.internal.firrtl._


package object util {

  // see: https://github.com/chipsalliance/chisel3/issues/1743

  def Any[T <: Data](d: T): Bool = {
    d.asUInt.orR()
  }
  def All[T <: Data](d: T): Bool = {
    d.asUInt.andR()
  }
  def Non[T <: Data](d: T): Bool = {
    d.asUInt === 0.U
  }
  def Neg[T <: Data](d: T): UInt = {
   ~d.asUInt
  }

  def ShL(d: UInt, n: Int): UInt = {
    val w = d.getWidth
    val x = w - n

    if (n == 0)
      d
    else if (x <= 0)
      0.U(w.W)
    else
      d(x.W) ## 0.U(n.W)
  }
  def ShR(d: UInt, n: Int): UInt = {
    val w = d.getWidth

    if (n == 0)
      d
    else if (n >= w)
      0.U(w.W)
    else
      0.U(n.W) ## d(w := n)
  }
  def SSL(d: UInt, n: Int): UInt = {
    val w = d.getWidth
    val x = w - n

    if (n == 0)
      d
    else if (x <= 0)
     ~0.U(w.W)
    else
      d(x.W) ## ~0.U(n.W)
  }
  def SSR(d: UInt, n: Int): UInt = {
    val w = d.getWidth

    if (n == 0)
      d
    else if (n >= w)
     ~0.U(w.W)
    else
     ~0.U(n.W) ## d(w := n)
  }

  def RoL(d: UInt, n: Int): UInt = {
    val w = d.getWidth
    val m = n % w
    val x = w - m

    if (m == 0)
      d
    else
      d(x.W) ## d(w := x)
  }
  def RoR(d: UInt, n: Int): UInt = {
    val w = d.getWidth
    val m = n % w

    if (m == 0)
      d
    else
      d(m.W) ## d(w := m)
  }

  @tailrec
  private def shf(f: (UInt, Int) => UInt, d: UInt, s: UInt, n: Int): UInt = {
    if (n >= s.getWidth)
      d
    else
      shf(f, s(n) ?? f(d, 1 << n) :: d, s, n + 1)
  }
  def BSL(d: UInt, s: UInt): UInt = {
    shf(ShL, d, s, 0)
  }
  def BSR(d: UInt, s: UInt): UInt = {
    shf(ShR, d, s, 0)
  }
  def BFL(d: UInt, s: UInt): UInt = {
    shf(SSL, d, s, 0)
  }
  def BFR(d: UInt, s: UInt): UInt = {
    shf(SSR, d, s, 0)
  }
  def BRL(d: UInt, s: UInt): UInt = {
    shf(RoL, d, s, 0)
  }
  def BRR(d: UInt, s: UInt): UInt = {
    shf(RoR, d, s, 0)
  }

  def Rev(d: UInt): UInt = {
    Cat(d.asBools).asUInt
  }

  def Rep(d: UInt, n: Int): UInt = {
    Cat(Seq.fill(n)(d))
  }
  def Div(d: UInt, n: Int): Seq[UInt] = {
    val s = (d.getWidth + n - 1) / n
    val t =  Ext(d, s * n)

    Seq.tabulate(s) { i =>
      t(i * n :+ n)
    }
  }

  def Ext(d: UInt, n: Int): UInt = {
    require(n != 0)

    val w = d.getWidth
    val m = n.abs
    val e = if (n > 0) 0.U(1.W) else d(w - 1);

    if (w >= m)
      d(m - 1, 0)
    else
      Rep(e, m - w) ## d
  }

  def EnQ[T <: Data](e: Bool, d: T): T = {
    e ?? d :: 0.U.asTypeOf(d)
  }
  def NeQ[T <: Data](e: Bool, d: T): T = {
    e ?? 0.U.asTypeOf(d) :: d
  }

  @tailrec
  private def rex(f: (UInt, Int) => UInt, o: (UInt, UInt) => UInt, d: UInt, n: Int): UInt = {
    if (n >= d.getWidth)
      d
    else
      rex(f, o, o(d, f(d, n)), n << 1)
  }
  private def or (a: UInt, b: UInt): UInt = {
    a | b
  }
  private def ar (a: UInt, b: UInt): UInt = {
    a & b
  }
  def OrL(d: UInt): UInt = {
    rex(ShL, or, d, 1)
  }
  def OrR(d: UInt): UInt = {
    rex(ShR, or, d, 1)
  }
  def ArL(d: UInt): UInt = {
    rex(SSL, ar, d, 1)
  }
  def ArR(d: UInt): UInt = {
    rex(SSR, ar, d, 1)
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

  def RRA(d: UInt, e: Bool): UInt = {
    val w = d.getWidth

    if (w > 1) {
      val arb_q = Pin(UInt(w.W))
      val fwd   = d &  arb_q
      val bwd   = d & ~arb_q
      val sel   = PrR(Any(fwd) ?? fwd :: bwd)

      arb_q := RegEnable(OrL(RoL(sel, 1)),
                        ~0.U(w.W),
                         e)
      sel

    } else
     ~0.U(w.W)
  }

  def PRA(w: Int,  e: Bool): UInt = {
    Dec(LFSR(log2Ceil(w).max(2), e))(w.W)
  }

  def OHp(d: UInt, z: Bool): Bool = {
    Non(d & ShL(OrL(d), 1)) && (z || Any(d))
  }

  def Dec(d: UInt): UInt = {
    BSL(1.U(scala.math.pow(2, d.getWidth).intValue.W), d)
  }

  def Enc = OHToUInt
  def OrM = Mux1H
  def Pop = PopCount

  private def exp(v: Seq[Bool], n: Int): Seq[Bool] = {
    if (n == 0)
      v
    else
      exp(RegNext(v.head) +: v, n - 1)
  }

  def Exp(b: Bool, n: Int): UInt = {
    Cat(exp(Seq(b), n))
  }

  def Src(i: Bool, o: Bool, s: Bool, n: Int = 2): Bool = {
    require(n >= 2)

    val w = BigInt(n).bitLength

    val src_q = Pin(UInt(n.W))
    val ptr_q = Pin(UInt(w.W))

    src_q := RegEnable(src_q(n - 2, 0) ## s,              0.U, i)
    ptr_q := RegEnable(ptr_q + (Rep(o, w - 1) ## true.B), 0.U, i ^ o)

    Chk((ptr_q === n.U) -> !i)
    Chk((ptr_q === 0.U) -> !o)

    Any(src_q & Dec(ptr_q)(n, 1))
  }

  def Opt(n: Int): Option[UInt] = {
    require(n >= 0)

    if (n > 0)
      Some(UInt(n.W))
    else
      None
  }
  def Pin[T <: Data](d: T): T = {
    dontTouch(Wire(d))
  }
  def Chk(c: Bool): Unit = {
    assert(c)
  }


  case class pair[T1, +T2](a: T1, b: T2)

  implicit class withWInt(d: Int) {
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
      !d || c
    }
  }

  implicit class withData[T <: Data](d: T) {
    def ::[S <: Data](p: pair[Bool, S]): T = {
      Mux(p.a, p.b.asInstanceOf[T], d)
    }
    def V: Vec[Bool] = {
      VecInit(d.asUInt.asBools)
    }
  }

  implicit class withVec[T <: Data](v: Vec[T]) {
    def U: UInt = {
      v.asUInt
    }
  }

  implicit class withSeq[T <: Data](s: Seq[T]) {
    def U: UInt = {
      Cat(s.map(_.asUInt).reverse)
    }
  }

  implicit class withValid[T <: Data](d: ValidIO[T]) {
    def tie: Unit = {
      d.valid := false.B
      d.bits  := DontCare
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

    def tie: Unit = {
      DataMirror.directionOf(d) match {
        case ActualDirection.Bidirectional(ActualDirection.Flipped) =>
          d.ready := false.B
        case _ =>
          d.valid := false.B
          d.bits  := DontCare
      }
    }
  }

  class SPXPM(a: Int, d: Int, f: String) extends ExtModule(Map("A" -> a,
                                                               "D" -> d,
                                                               "F" -> f)) {
    override def desiredName = s"SPXPM___${a}_${d}_${f}"

    val clk   = IO(Input (Clock()))
    val rst   = IO(Input (Reset()))

    val en    = IO(Input (Bool()))
    val wnr   = IO(Input (Bool()))
    val addr  = IO(Input (UInt(a.W)))

    val rdata = IO(Output(UInt(d.W)))
    val wdata = IO(Input (UInt(d.W)))
  }

  class SPRAM(a: Int, d: Int, s: Int) extends ExtModule(Map("A" -> a,
                                                            "D" -> d,
                                                            "S" -> s)) {
    override def desiredName = s"SPRAM___${a}_${d}_${s}"

    val clk   = IO(Input (Clock()))

    val en    = IO(Input (Bool()))
    val wnr   = IO(Input (Bool()))
    val addr  = IO(Input (UInt(a.W)))

    val rdata = IO(Output(UInt(d.W)))
    val wdata = IO(Input (UInt(d.W)))
    val wstrb = IO(Input (UInt(s.W)))
  }

  class DPRAM(a: Int, d: Int, s: Int) extends ExtModule(Map("A" -> a,
                                                            "D" -> d,
                                                            "S" -> s)) {
    override def desiredName = s"DPRAM___${a}_${d}_${s}"

    val clk   = IO(Input (Clock()))

    val ren   = IO(Input (Bool()))
    val raddr = IO(Input (UInt(a.W)))
    val rdata = IO(Output(UInt(d.W)))

    val wen   = IO(Input (Bool()))
    val waddr = IO(Input (UInt(a.W)))
    val wdata = IO(Input (UInt(d.W)))
    val wstrb = IO(Input (UInt(s.W)))
  }
}