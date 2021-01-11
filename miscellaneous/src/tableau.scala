import scala.io.StdIn.readLine
object exercise1 {
    def main(args: Array[String]): Unit = {
        val tableau = readLine("Enter a list of numbers separated by commas with no spaces > ").split(',').map(_.toInt)
        // complete declaration of cumutableau bellow :
        // Initializing cumutableau Array initially with 0
        val cumultableau = Array.fill(tableau.size){0}
        // fill cumutableau from the values of tableau :
        // Fist element in cumutableau equals first element in tableau
        cumultableau(0) = tableau(0)
        // cumutableau(i) = cumutableau(i - 1) + tableau(i)
        for(i <- 1 to tableau.size - 1)
            cumultableau(i) = cumultableau(i - 1) + tableau(i)
        println(cumultableau.mkString(","))
    }
}