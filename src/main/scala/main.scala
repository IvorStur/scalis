import scala.io.Source
object prax extends App{
  // // 1
  //  trait Cofmgs{
  //    val coffeinMgs : Int
  //  }
  //  class Coffein() extends Cofmgs{
  //    val coffeinMgs: Int = 50
  //}
  //  class NoCoffein() extends Cofmgs{
  //    val coffeinMgs: Int = 0
  //  }
  //  var late = new Coffein()
  //
  //  println(late.coffeinMgs)
  // 2
  val cardsFile = Source.fromFile("src/main/resources/cards.csv")
  //  println(cardsFile.nonEmpty)
  trait Card{
    val limit : Int
    val bank : String
    val cnum : Long
    val cvc : Int
    val name : String
    val balance : Long

    def processInternal(amount: Int): Card

  }

  case class blue(val bank : String, val cnum: Long, val cvc: Int, val name: String, val balance: Long) extends Card{
    val limit: Int = 500

    def processInternal(amount: Int): Card = this.copy(balance = balance - amount)

  }
  case class gold(val bank : String, val cnum: Long, val cvc: Int, val name: String, val balance: Long) extends Card{
    val limit: Int = 1000

    def processInternal(amount: Int): Card = this.copy(balance = balance - amount)
  }
  //val map: Map[long, Array[]]
  val lines = cardsFile.getLines()
  //println(lines)
  val processdLines = lines.foldLeft(Map[Long, Card]()){(map, line) => line.split(",")
  match {
    case Array(bank, cnum, cvc, name, balance) => bank match {
  case "b" => map + (cnum.toLong -> new blue(bank, cnum.toLong, cvc.toInt, name, balance.toInt))
    case "g" => map + (cnum.toLong -> new gold(bank, cnum.toLong, cvc.toInt, name, balance.toInt))
  }
    case _ => map
  }
  }

  val transactions = Source.fromFile("src/main/resources/transactions.csv").getLines()

  val result = transactions.foldLeft(processdLines){(map, line) =>
    line.split(",") match {
      case Array(c_num, amount, cvc) => processdLines.get(c_num.toLong) match {
        case Some(card2) => if (cvc.toInt == card2.cvc) {
          if (amount.toLong <= card2.balance) {
            if (amount.toInt <= card2.limit) {
              map + (c_num.toLong -> card2.processInternal(amount.toInt))
            } else {map}
          } else {map}
        }
        else {map}
        case None => map
      }
      case _ => map
    }
  }
println(processdLines)
  println(result)

//  for (line <- transactions) {
//    val cur = line.split(',') match {
//      case Array(c_num, amount, cvc) => processdLines.get(c_num.toLong) match {
//        case Some(card_2) =>
//        case None =>
//      }
//    }
//  }


 // println(processdLines)
  //for (line <- cardsFile.getLines()) {
  //  //val map: Map(long, )
  // val map: Map[cnum, Array[bank, cnum, cvc, name, balance]] = line.split(",").map(_.trim)

  //  if (bank=="g"){
  //      new gold(bank, cnum.toLong, cvc.toInt, name, balance.toLong)
  //    }else {
  //    new blue(bank, cnum.toLong, cvc.toInt, name, balance.toLong)
  //  }
  //
  //  }

  // val transactionsFile = Source.fromFile("src/main/resources/transactions.csv")
  //val transactions = transactionsFile.getLines()
  // for (line <- transactionsFile.getLines()) {
  //   val array1 = line.split(",").map(_.trim)
  //   }


}