import scala.annotation.tailrec

object RecursionWeek1 {

  def pascal(c: Int, r: Int): Int = {
    if (c > r || c < 0) 0
    else if (c == 0 || c == r) 1
    else pascal(c-1, r-1) + pascal(c, r-1)
  }

  def isBalanced(chars: String): Boolean = {
    
    @tailrec
    def balanceRec(charsRec: List[Char], balance: Int): Int = {
      charsRec.headOption match {
        case Some(c) => balanceRec(charsRec.tail, balance + incVal(c, balance))
        case None => balance
      }
    }

    def incVal(c: Char, balance: Int) = {
      if (c == '(' && balance >= 0) 1
      else if (c == ')') -1
      else 0
    }

    balanceRec(chars.toList, 0) == 0
  }

  def main(args: Array[String]) = {
    println(pascal(-1, 0))
    println(pascal(0, 2))
    println(pascal(1, 2))
    println(pascal(1, 3))
    println(pascal(2, 4))
    println(pascal(4, 8))

    println(isBalanced("(if (zero? x) max (/ 1 x))"))
    println(isBalanced("I told him (that it’s not (yet) done). (But he wasn’t listening)"))
    println(isBalanced(":-)"))
    println(isBalanced("())("))
    println(isBalanced(")()()("))
  }


}