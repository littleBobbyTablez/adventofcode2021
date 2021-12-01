import scala.::

object Day1 extends App {

  val in: List[String] = scala.io.Source.fromResource("Day1_1.txt").getLines().toList

  val ints = in.map(_.toInt)

  val zipped: List[(Int, Int)] = ints.zip(ints.tail :+ 0)

  val result = zipped.map(x => x._2 > x._1).count(_ == true)

  println(result)

  private val third: List[Int] = ints.tail.tail :+ 0 :+ 0
  val zippedAgain: Seq[((Int, Int), Int)] = zipped.zip(third)

  val sums = zippedAgain.map(x => x._1._1 + x._1._2 + x._2)

  val result2 = sums.zip(sums.tail:+0).map(x => x._2 > x._1).count(_ == true)

  println(result2)
}
