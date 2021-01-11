import scala.io.StdIn.readLine
object exercise1 {
    def main(args: Array[String]): Unit = {
        val tableau = readLine("Enter a list of numbers separated by commas with no spaces > ").split(',').map(_.toInt)
        //val cumultableau
        println(tableau.mkString(","))
        tableau
    }
}