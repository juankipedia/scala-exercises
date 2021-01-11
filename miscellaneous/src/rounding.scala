import  scala.io.StdIn.readLine
object exercise2 {
  // rounds 'decimal' to 'digits' number of digits after decimal point
  def rounding(decimal: Double, digits: Int): Double = {
    // declares c needed to round number
    val c = Math.pow(10.0, digits)
    // round off the number
    return Math.round(decimal * c) / c
  }
  def main(args: Array[String]): Unit = {
    // read decimal value to be rounded off
    val decimal = readLine("What is the value to be rounded off > ").toFloat
    // read number of digits
    val digits = readLine("What is the number of digits after the decimal point for rounding off > ").toInt
    // call rounding method and print result
    val result = rounding(decimal, digits)
    println(result)
  }
}