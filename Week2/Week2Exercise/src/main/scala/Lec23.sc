import math.abs

object exercise{
  val tolerance = 0.0001

  def averageDamp(f: Double => Double)(x: Double) = (x + f(x))/2

  def isCloseEnough(x: Double, y: Double) =
    abs((x-y)/x)/x < tolerance

  def fixedPoint(f: Double => Double)(firstGuess: Double)={
    def iterate(guess: Double): Double = {
      //println("Guess = " + guess)
      val next = f(guess) //or averageDamp can be placed here as = averageDamp(f)(guess)
      if (isCloseEnough(guess, next)) next
      else iterate(next)
    }
    iterate(firstGuess)
  }
  fixedPoint(x => 1 + x/2)(1)

  def sqrt(x: Double) = fixedPoint(y => (x+x/y)/2)(1.0)

  sqrt(2)
  sqrt(4)

  def sqrt2(x: Double) =
    fixedPoint(averageDamp(y => x/y))(1)

  sqrt2(2)
  sqrt2(4)
}