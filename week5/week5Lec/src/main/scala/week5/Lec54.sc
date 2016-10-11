
object listfun{
  val nums = List(2,-4,5,7,1)
  val fruits = List("apple", "pineapple", "orange", "banana")

  nums filter (x => x>0)
  nums filterNot (x=> x>0)
  nums partition (x => x>0)

  nums takeWhile (x => x>0)
  nums dropWhile (x => x> 0)
  nums span (x=> x>0)

  //Exercise pack
  // write a function pack that packs consecutive duplicates of list elements
  // into sublists, For instancr,
  // pack(List("a","a","a", "b","c","c","a"))
  // should give
  // List*List("a","a","a"), List("b"), List("c","c"), List("a"))

  val data = List("a","a","a", "b","c","c","a")
  def pack[T](xs: List[T]): List[List[T]] = xs match{
    case Nil => Nil
    case x :: xs1 =>
      val (first, rest) = xs span (y=> y ==x)
      first :: pack(rest)
  }

  pack(data)

  //Exercise 2
  // Using pack, write a function encode that produces the run-length encoding of a list
  // The idea is to encode n consecutive duplicates of an element x as a pair (x,n).
  // For instance,
  //encode(data)
  // should give
  // List(("a", 3), ("b", 1),("c",2), ("a",1))

  def encode[T](xs: List[T]): List[(T,Int)] =
    pack(xs) map (ys => (ys.head, ys.length))

  encode(data)

  def concat[T](xs: List[T], ys:List[T]): List[T]=
    (xs foldRight ys)(_ :: _)


}