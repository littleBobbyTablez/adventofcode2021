object Day2 extends App {

  case class aimPosition(x: Int, y: Int, aim: Int)

  val in: List[String] = scala.io.Source.fromResource("Day2_1.txt").getLines().toList

  private val split: List[(String, Int)] = in.map(_.split(" ")).map(y => (y(0), y(1).toInt))

  val move = (command: (String, Int), pos: (Int, Int)) => {
    command._1 match {
      case "forward" => (pos._1 + command._2, pos._2)
      case "down" => (pos._1, pos._2 + command._2)
      case "up" => (pos._1, pos._2 - command._2)
    }
  }

  private val result: (Int, Int) = split.foldLeft((0, 0)) ((acc: (Int, Int), x: (String, Int)) => move(x, acc))

  println(result)
  println(result._1 * result._2)


  val moveWithAim = (command: (String, Int), pos: aimPosition) => {
    command._1 match {
      case "forward" => aimPosition(pos.x + command._2, pos.y + (pos.aim * command._2), pos.aim)
      case "down" => aimPosition(pos.x, pos.y, pos.aim + command._2)
      case "up" => aimPosition(pos.x, pos.y, pos.aim - command._2)
    }
  }

  private val result2: aimPosition = split.foldLeft(aimPosition(0, 0, 0)) ((acc: aimPosition, x: (String, Int)) =>
      moveWithAim(x, acc))

  println(result2)
  println(result2.x * result2.y)
}
