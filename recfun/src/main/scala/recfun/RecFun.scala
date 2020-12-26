package recfun


object RecFun extends RecFunInterface {

  def main(args: Array[String]): Unit = {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(s"${pascal(col, row)} ")
      println()
    }
  }

  /**
   * Exercise 1
   */
  def pascal(c: Int, r: Int): Int =
    if (c == 0 || r == 0 || c == r) 1 else pascal(c - 1, r - 1) + pascal(c, r - 1)

  /**
   * Exercise 2
   */
  def balance(chars: List[Char]): Boolean = _balance(chars, List[Boolean]())

  def _balance(chars: List[Char], parensStack: List[Boolean]): Boolean =
    if (chars.isEmpty) parensStack.isEmpty
    else if (chars.head == '(') _balance(chars.tail, List(true).concat(parensStack))
    else if (chars.head == ')')
      !parensStack.isEmpty &&
        _balance(chars.tail, parensStack.tail)
    else _balance(chars.tail, parensStack)

  /**
   * Exercise 3
   * Help:
   * https://www.youtube.com/watch?v=DJ4a7cmjZY0&ab_channel=BackToBackSWE
   * https://www.geeksforgeeks.org/coin-change-dp-7/
   */
  def countChange(money: Int, coins: List[Int]): Int = if (money == 0) 1
  else if (coins.isEmpty) 0
  else if (money < 0) 0
  else countChange(money, coins.tail) + countChange(money - coins.head, coins)

}
