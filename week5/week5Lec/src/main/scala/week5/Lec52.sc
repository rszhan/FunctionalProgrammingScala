object mergesort{
  def msort[T](xs: List[T])(lt:(T,T) => Boolean): List[T] = {
    val n = xs.length / 2
    if(n==0) xs
    else {
      def merge(xs: List[T], ys:List[T]): List[T]= (xs , ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if(lt(x,y)) x :: merge(xs1,ys)
          else y :: merge(xs,ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort(fst)(lt),msort(snd)(lt))
    }
  }

  val nums = List(2, -4, 5, 7, 1)
  msort(nums)((x: Int, y: Int) => x<y)
  msort(nums)((x, y) => x<y) //scala auto type inference

  val fruits = List("apple", "pineapple", "orange", "banana")
  msort(fruits)((x:String, y:String) => x.compareTo(y)<0)

  //using scala library Ordering
  import math.Ordering
  def msort2[T](xs: List[T])(ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if(n==0) xs
    else {
      def merge(xs: List[T], ys:List[T]): List[T]= (xs , ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if(ord.lt(x,y)) x :: merge(xs1,ys)
          else y :: merge(xs,ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msort2(fst)(ord),msort2(snd)(ord))
    }
  }

  val nums2 = List(2, -4, 5, 7, 1)
  msort2(nums2)(Ordering.Int)

  val fruits2 = List("apple", "pineapple", "orange", "banana")
  msort2(fruits2)(Ordering.String)

  //implicit parameters
  def msortImp[T](xs: List[T])(implicit ord: Ordering[T]): List[T] = {
    val n = xs.length / 2
    if(n==0) xs
    else {
      def merge(xs: List[T], ys:List[T]): List[T]= (xs , ys) match {
        case (Nil, ys) => ys
        case (xs, Nil) => xs
        case (x :: xs1, y :: ys1) =>
          if(ord.lt(x,y)) x :: merge(xs1,ys)
          else y :: merge(xs,ys1)
      }

      val (fst, snd) = xs splitAt n
      merge(msortImp(fst),msortImp(snd))
    }
  }

  val nums3 = List(2, -4, 5, 7, 1)
  msortImp(nums3)

  val fruits3 = List("apple", "pineapple", "orange", "banana")
  msortImp(fruits3)

}