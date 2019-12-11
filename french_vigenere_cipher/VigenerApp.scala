object VigenerApp {
      val alphabet = Map[Char, Int](
    'A' -> 0, 'B' -> 1, 'C' -> 2, 'D' -> 3, 'E' -> 4, 'F' -> 5,  'G' -> 6, 
    'H' -> 7, 'I' -> 8, 'J' -> 9, 'K' -> 10, 'L' -> 11, 'M' -> 12, 'N' -> 13, 'O' -> 14, 
    'P' -> 15, 'Q' -> 16, 'R' -> 17, 'S' -> 18, 'T' -> 19, 'U' -> 20, 'V' -> 21, 'W' -> 22, 
    'X' -> 23, 'Y' -> 24, 'Z' -> 25, 'a' -> 26, 'b' -> 27, 'c' -> 28, 'd' -> 29, 'e' -> 30, 'f' -> 31,
    'g' -> 32, 'h' -> 33, 'i' -> 34, 'j' -> 35, 'k' -> 36, 'l' -> 37, 'm' -> 38, 'n' -> 39, 'o' -> 40,
    'p' -> 41, 'q' -> 42, 'r' -> 43, 's' -> 44, 't' -> 45, 'u' -> 46, 'v' -> 47, 'w' -> 48, 'x' -> 49,
    'y' -> 50, 'z' -> 51, 'é' -> 52, 'è' -> 53, 'ê' -> 54, 'à' -> 55, ' ' -> 56, ',' -> 57, ';' -> 58,
    '0' -> 59, '1' -> 60, '2' -> 61, '3' -> 62, '4' -> 63, '5' -> 64, '6' -> 65, '7' -> 66, '8' -> 67,
    '9' -> 68)

    val alphabetPos : Map[Int, Char] = alphabet.map {case (c, p) => (p -> c)}
  object Vigenere {
    def encrypt(msg: String, key: String) : String = {
      var result: String = ""
      var j = 0
      val invalid = key.filter(c => !(alphabet contains c))
      for (i <- 0 to msg.length - 1) {
        val c = msg.charAt(i)
        if (alphabet contains c) {
          result += alphabetPos((alphabet(c) + alphabet(key.charAt(j))) % alphabet.size)
          j = (j + 1) % key.length
        }
        else{
          println("ERROR  text cannot be encrypted because contains an invalid character [" + c + "]")
          return ""
        }
      }
      return result
    }
  
    def decrypt(msg: String, key: String) : String = {
      var result: String = ""
      var j = 0
      for (i <- 0 to msg.length - 1) {
        val c = msg.charAt(i)
        if (alphabet contains c) {
          result += alphabetPos((alphabet(c) - alphabet(key.charAt(j)) + alphabet.size) % alphabet.size)
          j = (j + 1) % key.length
        }
        else{
          println("ERROR  text cannot be decrypted because contains an invalid character [" + c + "]")
          return ""
        }
      }
  
      return result
    }
  }

  def main(args: Array[String]): Unit = {
    println("\n" * 40)
    println("Please Enter a Key")
    val key =  scala.io.StdIn.readLine()
    var option = 2
    val invalid = key.filter(c => !(alphabet contains c))
    if(invalid.size > 0){
      println("ERROR  text cannot be encrypted because key contains invalid characters [" + invalid + "]")
      return
    }  

    while(option != 1 && option != 0){
      println("[0] .....................  ENCRYPT ")
      println("[1] .....................  DECRYPT ")
      option = Console.readInt()
    }

    if(option == 0){
      println("Please Enter Text to encrypt")
      val txt =  scala.io.StdIn.readLine()
      println("Encrypted => \n" + Vigenere.encrypt(txt, key))
    }
    else{
      println("Please Enter Encrypted text")
      val txt =  scala.io.StdIn.readLine()
      println("Decrypted text => \n" + Vigenere.decrypt(txt, key))
    }

  }



}