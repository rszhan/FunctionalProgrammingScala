package week3 {

  /**
    * Created by rickzhan on 2016-07-09.
    */
  class Rational(x: Int, y: Int) {
    // there is also another check method (assert)
    // require is used to enforce a precondition on the caller of a function
    // assert is used as to check the code of the function itself
    require(y != 0, "denominator must be nonzero")

    //secondary constructor
    def this(x: Int) = this(x, 1)

    private def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)

    private val g = gcd(x, y)
    val numer = x / g
    // could have used def as well
    val denom = y / g

    def <(that: Rational) = this.numer * that.denom < that.numer * this.denom

    def max(that: Rational) = if (this < that) that else this

    def +(that: Rational) =
      new Rational(
        numer * that.denom + that.numer * denom,
        denom * that.denom
      )

    def *(that: Rational) = new Rational(this.numer * that.numer, this.denom * that.denom)

    def -(that: Rational) = this + -that

    def unary_- : Rational = new Rational(-numer, denom)

    override def toString = numer + "/" + denom
  }

}
