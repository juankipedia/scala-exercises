import io.StdIn;
import java.io.PrintWriter;
import scala.io.Source;
import scala.collection.mutable._;
import scala.io.StdIn.{readInt, readDouble, readLine};

object EWalletApp {
  def fileName = "e-WalletData.txt"
  def separator = ","
  
  class EWallet(val clientReference: String){
    var pin = 100 + (new scala.util.Random).nextInt((99999 - 100) + 1)
    var value: Double = 0.0
    val overDraft: Double = 10.0
    val fee: Double = 0.02
    
    def this(clientReference: String, pin: Int, value: Double){
      this(clientReference)
      this.pin = pin
      this.value = value
    }

    def this(clientReference: String, value: Double){
      this(clientReference)
      this.value = value
    }

    def getFormat: String = {
      return clientReference + separator + pin + separator + value
    }

    def print = {
      println(" client reference | pin | value")
      println(clientReference + " | " + pin + " | " + value)
    }

    def verifyClient(cReference: String, p: Int) : Boolean = {
      if(cReference == this.clientReference && p == this.pin){
        return true
      }
      else{
        return false
      }
    }

    def operationDebit(debitValue: Double): Boolean = {
      if(value == 0) return false;
      if((value + overDraft) >= (debitValue + debitValue * fee)){
        if((debitValue + debitValue * fee) > value){
          value = 0
          return true;
        }
        else{
          value = value - (debitValue + debitValue * fee)
          return true;
        }
      }
      else return false;
    }

    def operationAdd(valueToAdd: Double) = {
      value = value + valueToAdd
    }
  }
  var eWalletsbuffer: ArrayBuffer[EWallet] = ArrayBuffer()
  

  def saveToFile(){
    val o = new PrintWriter(fileName)
    o.println("Client Reference | PIN | Amount available |")
    for(ew <- eWalletsbuffer){
      o.println(ew.getFormat)
    } 
    o.close
  }

  def getFromFile(){
    eWalletsbuffer = ArrayBuffer[EWallet]()
    val in = Source.fromFile(fileName)
    val lines = in.reset.getLines
    val line = lines.next
    while(! lines.isEmpty){
      val line = lines.next
      val eWalletList = line.split(separator)
      eWalletsbuffer += new EWallet(eWalletList(0),
                                   eWalletList(1).toInt,
                                   eWalletList(2).toDouble)
    }
    return
  }

  def getEWallet(reference: String): Option[EWallet] ={
    for(ew <- eWalletsbuffer){
      if(ew.clientReference == reference){
        return Some(ew)
      }
    }
    return None
  }

  def main(args: Array[String]): Unit = {
    getFromFile()
    var option: Int = -1
    while(option != 0){
      println("\n" * 30)
      println("MENU\n")
      println("[1]         create e-Wallet")
      println("[2]         access e-Wallet")
      println("[0]         exit")
      option = readInt()
      if(option == 1){
        println("Please Insert Client Reference")
        val reference = readLine()
        getEWallet(reference) match {
          case None =>{
            println("Please Insert Initial Amount")
            val value = readDouble()
            val nEw = new EWallet(reference, value)
            eWalletsbuffer += nEw
            println("e-wallet created! info: ")
            nEw.print 
          }
          case Some(ew) => println("ERROR e-wallet found for that reference, user can only have one e-wallet")
        }
        println("press [ENTER] to continue")
        readLine()

      }
      else if(option == 2){
        println("Please Insert Client Reference")
        val reference = readLine()
        println("Please Insert PIN for e-wallet")
        val pin = readInt()
        getEWallet(reference) match {
          case None => println("ERROR e-wallet NOT found for that reference.")
          case Some(ew) => {
            if(!ew.verifyClient(reference, pin)){
              println("ERROR invalid PIN")
            }
            else{
              println("e-wallet found! info:")
              ew.print
              println("want to transfer amount (y/n)")
              val yOn = readLine()
              if(yOn == "y"){
                println("Please Insert Amount to Transfer")
                val value = readDouble()
                println("Please Insert Destination Client Reference")
                val dReference = readLine()
                getEWallet(dReference) match {
                  case None => println("ERROR Destination Reference not Found")
                  case Some(dEw) => {
                    if(ew.operationDebit(value)){
                      println("Money SENT")
                      dEw.operationAdd(value)
                    }
                    else{
                      println("ERROR Not enough money on e-wallet")
                    }
                  }
                }
              }
            }
          }
        }
        println("press [ENTER] to continue")
        readLine()
      }
    }
    saveToFile()
  }



}