import scala.annotation.tailrec

object Day3 extends App {

  val in: List[String] = scala.io.Source.fromResource("Day3_1.txt").getLines().toList

  private val binaryStrings:List[List[String]] = in.map(_.map(_.toString).toList)
  private val listOfInts: List[List[Int]] = binaryStrings.map(_.map(_.toInt))


  private val startValues = List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  val countOfOnes = listOfInts.foldLeft(startValues)((acc: List[Int], x: List[Int]) => {
    acc.zip(x).map(y => y._1 + y._2)
  })

  val halfLength = listOfInts.size/2

  val gammaList = countOfOnes.map {
    case x if x < halfLength => 0
    case _ => 1
  }

  val epsilonList = countOfOnes.map {
    case x if x > halfLength => 0
    case _ => 1
  }

  val gammaBinaryString = gammaList.map(_.toString).mkString

  val gammaRate = Integer.parseInt(gammaBinaryString, 2)
  val epsilonBinaryString = epsilonList.map(_.toString).mkString

  val epsilonRate = Integer.parseInt(epsilonBinaryString, 2)
  println(gammaRate*epsilonRate)


  def findRating(input: List[List[Int]], index: Int, filterFunction: (Int, Int) => Int): String = {
    if (input.size == 1) {
      return input.head.map(_.toString).mkString
    }

    val onesAtIndex = input.foldLeft(0)((acc, x) => acc + x(index))

    val toFilter = filterFunction(onesAtIndex, (input.size/2) + (input.size % 2))

    val filtered = input.filter(_(index) == toFilter)
    findRating(filtered, index +1, filterFunction)
  }

  private def filterFunctionOxy(onesAtIndex: Int, size: Int) = {
    onesAtIndex match {
      case x if x >= size => 1
      case _ => 0
    }
  }

  private def filterFunctionCo2(onesAtIndex: Int, size: Int) = {
    onesAtIndex match {
      case x if x < size => 1
      case _ => 0
    }
  }

  val oxyBinaryString = findRating(listOfInts, 0, filterFunctionOxy)
  val co2BinaryString = findRating(listOfInts, 0, filterFunctionCo2)
  val oxyRate = Integer.parseInt(oxyBinaryString, 2)
  val co2Rate = Integer.parseInt(co2BinaryString, 2)

  println(oxyRate*co2Rate)
}
