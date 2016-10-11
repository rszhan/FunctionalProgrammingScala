val x = new Rational(1,3)
val y = new Rational(5,7)
val z = new Rational(3,2)

x - y - z
y + y
x < y
x max y
x * x + y * y
new Rational(2)

class Rational(x: Int, y: Int){
  // there is also another check method (assert)
  // require is used to enforce a precondition on the caller of a function
  // assert is used as to check the code of the function itself
  require(y != 0, "denominator must be nonzero")

  //secondary constructor
  def this(x: Int) = this(x,1)

  private def gcd(a: Int, b: Int): Int = if(b==0) a else gcd(b,a % b)
  private val g = gcd(x,y)
  val numer = x / g // could have used def as well
  val denom = y / g

  def < (that: Rational) = this.numer * that.denom < that.numer * this.denom

  def max(that: Rational) = if (this < that) that else this

  def + (that: Rational) =
    new Rational(
      numer*that.denom + that.numer * denom,
      denom * that.denom
    )

  def * (that: Rational) = new Rational(this.numer * that.numer, this.denom * that.denom)

  def - (that: Rational)= this + -that

  def unary_- : Rational = new Rational(-numer,denom)

  override def toString = numer + "/" + denom
}

