import week3._
// many ways
// import week3.Rational
// import week3.{Rational, Hello}
// import week3._

new Rational(1,2)

//exception handling returns type Nothing
def error(msg: String)= throw new Error(msg)

//null is incompatible with AnyVal type
val x = null
val y:String = x
//val z: Int = null <-- type mismatch

//what is the type?
if (true) 1 else false










