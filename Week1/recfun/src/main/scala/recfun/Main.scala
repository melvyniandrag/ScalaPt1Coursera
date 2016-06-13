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
    def pascal(c: Int, r: Int): Int = {
      assert(c >= 0)
      assert(c <= r)
      if(c == 0 || c == r){
        return 1
      }
      else{
        return pascal(c - 1, r - 1) + pascal(c, r - 1)
      }
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def count_parens(open : Int, close : Int, tail_chars : List[Char]) : Int = {
        if (close > open){
          return 1
        }
        else if (tail_chars.isEmpty){
          return open - close
        }
        else if(tail_chars.head == '('){
          return count_parens(open + 1, close, tail_chars.tail)
        }
        else if(tail_chars.head == ')'){
          return count_parens(open, close + 1 , tail_chars.tail)
        }
        else{
          return count_parens(open, close, tail_chars.tail)
        }
      }
      val balanced = count_parens(0, 0, chars)
      if (balanced == 0){
        return true
      }
      else{
        return false
      }
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      val total = 0
      if (money < 0 || coins.isEmpty){
        return 0
      }
      if (money == 0){
        return 1
      }
      return total + countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  
}
