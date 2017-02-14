package utils.algebra

import cats.kernel.CommutativeGroup

import scala.{specialized => sp}

/**
  * LAW: getIndex(order) = empty
  * LAW: closed - for any positive n, combineN(generator, n) must be in A
  */
trait CyclicGroup[@sp(Int, Long, Float, Double) A] extends Any with CommutativeGroup[A] { fa =>

  def order: Int

  /**
    * Return the generator element for this cyclic group.
    */
  def generator: A

  def overflows(x: A, y: A): Boolean = x != empty && y != empty && combine(x, y) == empty

  def getIndex(i: Int): A = combineN(generator, i)

  def combineWithOverflow(x: A, y: A): (A, Boolean) = (combine(x, y), overflows(x, y))

  /** Right inclined cyclic composed group. (Rightmost most significant position) */
  def compose[B](fb: CyclicGroup[B]): CyclicGroup[(A, B)] = CyclicGroupOps.compose(fa, fb)

  override def toString: String = s"CyclicGroup(order = $order, generator = $generator)"
}

object CyclicGroupOps {

  /** Right inclined cyclic composed group. (Rightmost most significant position) */
  def compose[A, B](fa: CyclicGroup[A], fb: CyclicGroup[B]): CyclicGroup[(A, B)] = new CyclicGroup[(A, B)] {

    override def order: Int = fa.order * fb.order

    override def generator: (A, B) = (fa.generator, fb.empty)

    override def inverse(a: (A, B)): (A, B) = (fa.inverse(a._1), fb.inverse(a._2))

    override def empty: (A, B) = (fa.empty, fb.empty)

    override def combine(x: (A, B), y: (A, B)): (A, B) = {
      val (xa, xb)                         = x
      val (ya, yb)                         = y
      val (resA, lessSignificantOverflows) = fa.combineWithOverflow(xa, ya)
      val resB                             = fb.combine(xb, yb)
      if (lessSignificantOverflows) (resA, fb.combine(resB, fb.generator))
      else (resA, resB)
    }
  }
}

object CyclicGroup {

  /**
    * Access an implicit `CyclicGroup[A]`.
    */
  @inline final def apply[A](implicit ev: CyclicGroup[A]): CyclicGroup[A] = ev
}
