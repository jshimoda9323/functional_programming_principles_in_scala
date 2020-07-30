package recfun
import scala.annotation.tailrec
object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
    val inputStrings = List("(if(zero? x) max(/ 1 x))","I told him (that it's not (yet) done). (But he wasn't listening)",":-)","())(","abc(((def")
    inputStrings.foreach { s =>
      println("Checking balance of '"+s+"' which has length "+s.toList.length)
      println(s"${balance(s.toList)}")
    }
    println(s"${countChange(100,List(1,50))}")

  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int = {
    if (c == 0) 1
    else if (r == c) 1
    else pascal(c-1,r-1) + pascal(c,r-1)
  }

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = {
    @tailrec def balanceIter(chars: List[Char], depth: Int): Int = {
      if (depth < 0) -1
      else if (chars.isEmpty) depth
      else {
        if (chars.head == ')') balanceIter(chars.tail, depth - 1)
        else if (chars.head != '(') balanceIter(chars.tail, depth)
        else balanceIter(chars.tail, depth + 1)
      }
    }
    (balanceIter(chars,0) == 0)
  }

  /**
   * Exercise 3
   */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money == 0) 1
    else if (money < 0) 0
    else if (coins.isEmpty) 0
    else countChange(money,coins.tail) + countChange(money-coins.head,coins)
  }
}
