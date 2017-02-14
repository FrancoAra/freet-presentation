package utils.algebra

/** IMPORTANT: n must be > 1 */
case class ModuloN(n: Int) extends CyclicGroup[Int] {

  override def order: Int = n

  override def generator: Int = 1

  override def inverse(a: Int): Int =
    if (a == empty) empty
    else if (a < 0 || a >= n) throw new IllegalArgumentException(s"$a is not in $toString")
    else n - a

  override def empty: Int = 0

  override def combine(x: Int, y: Int): Int =
    if (x < 0 || x >= n) throw new IllegalArgumentException(s"$x is not in ModuloN with n = $n")
    else if (y < 0 || y >= n) throw new IllegalArgumentException(s"$y is not in ModuloN with n = $n")
    else (x + y) % n
}

case class ModuloNOf[T](value: ModuloN)
