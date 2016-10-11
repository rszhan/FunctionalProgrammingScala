package recfun
//package tailrecfunc

//import scala.annotation.tailrec

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    //@tailrec
    def pascal(c: Int, r: Int): Int = {
      if(c==0 || c==r) {
        1;
      }
      else{
        pascal(c-1,r-1)+pascal(c,r-1);
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      //chars.isEmpty: Boolean returns whether a list is empty
      //chars.head: Char returns the first element of the list
      //chars.tail: List[Char] returns the list without the first element
      def count(chars: List[Char], num_open: Int): Boolean = {
        if (chars.isEmpty) num_open == 0
        else{
          val first_c = chars.head
          var new_num_open: Int = {
            if (first_c =='(') num_open + 1
            else if(first_c == ')') num_open - 1
            else num_open
          }
          if (new_num_open >= 0) count(chars.tail, new_num_open)
          else false
        }
      }
      count(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int =
    if (coins.isEmpty || money < 0) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
