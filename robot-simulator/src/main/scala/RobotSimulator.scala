object Bearing extends Enumeration {
  type Bearing = Value
  val North, South, East, West = Value
}
import Bearing._

class Robot {
  var d: Bearing = _
  var c: (Int, Int) = _

  override def equals(that: Any): Boolean = that match {
    case that: Robot => that.d == this.d && that.c == this.c
    case _ => false
  }

  def coordinates: (Int, Int) = {
    return c
  }

  def bearing: Bearing = {
    return d
  }

  def simulate(s: String): Robot = {
    for (c <- s) {
      c match {
        case 'A' => advance
        case 'R' => turnRight
        case 'L' => turnLeft
      }
    }
    this
  }

  def advance: Robot = {
    d match {
      case Bearing.North => c = (c._1, c._2 + 1)
      case Bearing.East => c = (c._1 + 1, c._2)
      case Bearing.South => c = (c._1, c._2 - 1)
      case Bearing.West => c = (c._1 - 1, c._2)
    }
    this
  }

  def turnRight: Robot = {
    d match {
      case Bearing.North => d = Bearing.East
      case Bearing.East => d = Bearing.South
      case Bearing.South => d = Bearing.West
      case Bearing.West => d = Bearing.North
    }
    this
  }

  def turnLeft: Robot = {
    d match {
      case Bearing.North => d = Bearing.West
      case Bearing.East => d = Bearing.North
      case Bearing.South => d = Bearing.East
      case Bearing.West => d = Bearing.South
    }
    this
  }
}

object Robot {
  def apply(d: Bearing, c: (Int, Int)): Robot = {
    var r = new Robot
    r.d = d
    r.c = c
    r
  }
}
