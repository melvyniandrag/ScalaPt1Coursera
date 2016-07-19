

object nqueens {
  def queens(n: Int): Set[List[Int]] = {
    def placeQueens(k:Int): Set[List[Int]] = {
      if(k==0) Set(List())
      else
        for{
          queens <- placeQueens(k-1)
          col <- 0 until n
          if isSafe(col, queens)
        } yield col::queens
    }
    placeQueens(n)
  }
  
  def isSafe(col: Int, queens: List[Int]): Boolean = {
    val row = queens.length
    val queensWithRow = (row - 1 to 0 by -1) zip queens
    queensWithRow  forall {
      case (r,c) => col != c && math.abs(col - c) != row -r
    }
  }
  
  def show(queens: List[Int]) = {
    val lines = 
        for (col <- queens.reverse)
        yield Vector.fill(queens.length)("*").updated(col, "X").mkString
    "\n" + (lines mkString "\n")
  }
  
  (queens(4) map show) mkString "\n"
} 


object polynomials {
  class Poly(val terms: Map[Int, Double]){
    def + (other: Poly) = {
      new Poly(terms ++ other.terms)
    }
    override def toString = 
      (for((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString "+"
  }
  
  class BetterPoly(val terms: Map[Int, Double]){
    // So far maps are partial functions.
    // Applying a map to a key value in map(key) could lead to an exception if the key was not stored in the map
    // There is an operation called withDefaultValue that turns a map into a total function
    def + (other: BetterPoly) = {
      new BetterPoly(terms ++ (other.terms map adjust))
    }
    def adjust(term: (Int, Double)):(Int, Double) = {
      val (exp, coeff) = term
      terms get exp match{
        case Some(coeff1) => exp -> (coeff  + coeff1)
        case None => exp -> coeff
      }
    }
    override def toString = 
      (for((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString "+"
  }  
  
  class BestPoly(terms0: Map[Int, Double]){
    // This class is an improvement over the BetterPoly class as described in the 
    // BetterPoly comments.
    // Also, we have added a secondary constructor below that allows a. the user
    // to pass a list of mappings and not a map, 
    // and b. and arbitrary number of these mappings.
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
    def + (other: BestPoly) = {
      new BestPoly(terms ++ (other.terms map adjust))
    }
    def adjust(term: (Int, Double)):(Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))     
    }
    override def toString = 
      (for((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString "+"
  }  
  
  class BestPoly2(terms0: Map[Int, Double]){
    // This class is an improvement over the BetterPoly class as described in the 
    // BetterPoly comments.
    // Also, we have added a secondary constructor below that allows a. the user
    // to pass a list of mappings and not a map, 
    // and b. and arbitrary number of these mappings.
    def this(bindings: (Int, Double)*) = this(bindings.toMap)
    val terms = terms0 withDefaultValue 0.0
    
    def + (other: BestPoly2) = {
      new BestPoly2((other.terms foldLeft terms)(addTerm))
    }
    def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] = {
      terms + (term._1 -> (terms(term._1) + term._2))
    }
    
    def adjust(term: (Int, Double)):(Int, Double) = {
      val (exp, coeff) = term
      exp -> (coeff + terms(exp))     
    }
    override def toString = 
      (for((exp, coeff) <- terms.toList.sorted.reverse) yield coeff + "x^" + exp) mkString "+"
  }  
    
  val p1 = new Poly(Map(1 -> 2.0, 3 -> 5.0))
  val p2 = new Poly(Map(0 -> 8.0, 2 -> 3.0, 3 -> 4.0))
  println(p1 + p2)
  
  val p3 = new BestPoly2(Map(1 -> 2.0, 3 -> 5.0))
  val p4 = new BestPoly2(Map(0 -> 8.0, 2 -> 3.0, 3 -> 4.0))
  val p5 = new BestPoly2(0 -> 2.0)
  val p6 = p3 + p4
  p6+p5
}