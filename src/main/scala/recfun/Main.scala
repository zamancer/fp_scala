package recfun

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
  def pascal(c: Int, r: Int): Int = c + r - 1

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    def isSymbol(chain: List[Char], symbol: Char): Boolean = {
      if (chain.isEmpty) false
      else if (chain.head == symbol) true
      else isSymbol(chain.tail, symbol)
    }

    def compareEnclosing(chars: List[Char], left: Char, right: Char): Boolean = {
      if (chars.isEmpty) true
      else if (chars.head == left) isSymbol(chars.tail, right) && compareEnclosing(chars.tail, left, right)
      else compareEnclosing(chars.tail, left, right)
    }

    compareEnclosing(chars, '(', ')') && compareEnclosing(chars.reverse, ')', '(')
  }

  /**
    * Exercise 3
    */
  def countChange(money: Int, coins: List[Int]): Int = {
    if (money < 0 || coins.isEmpty) 0
    else if (money == 0) 1
    else countChange(money - coins.head, coins) + countChange(money, coins.tail)
  }
}
