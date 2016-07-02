object rationals{
  val x = new Rational(1, 2)
  val y = new Rational(2, 3)
  x.sub(y)
  x.printX()
}

class Rational(x : Int, y: Int) {
  def numer = x
  def denom = y
  override def toString = numer + "/" + denom
  def neg = {
    new Rational(-numer, denom)
  }
  def add(that: Rational) = {
    new Rational(
        numer * that.denom + that.numer * denom
        denom * that.denom)
  }
  def sub(that: Rational) = {
    add(that.neg)
  }
  def printX() = {
    println(x)
  }
}