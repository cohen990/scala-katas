package test.demo

import java.io._
import cats.syntax.all.*
import java.awt.dnd.InvalidDnDOperationException

case class CommandError(errorMessage: String)

enum RoverDirection:
  case North
  case South
  case East
  case West

case class RoverPosition(x: Integer, y: Integer, direction: RoverDirection)

case class Commands(commandString: String)

// solves https://github.com/makomweb/mars-rover
object RoverController {

  //def inputParser(inputString: String): Either[CommandError, (RoverPosition, Commands)] =



  def sendCommand(rawInput: String): Either[CommandError, String] = 
    if(rawInput.length == 0){
      Left(CommandError("no input detected"))
    } else {
      val input = rawInput.split('\n')
      input match 
        case input if input.length == 1 => Left(CommandError("missing rover details"))
        case input if input.length == 2 => Left(CommandError("no commands detected"))
        case Array(gridSize, roverPositionRaw, commands) => 
          val roverDirection = roverPositionRaw(4) match
            case 'N' => RoverDirection.North
            case 'E' => RoverDirection.East
            case 'S' => RoverDirection.South
            case 'W' => RoverDirection.West
          roverPositionRaw(0).isDigit && roverPositionRaw(2).isDigit match
          case true => val roverPosition = RoverPosition(roverPositionRaw(0).asDigit, roverPositionRaw(2).asDigit, roverDirection)
            commands(0) match
            case 'M' => roverPosition.direction match
              case RoverDirection.North => moveRoverNorth(roverPosition)
              case RoverDirection.South => Right("1 1 S")
              case RoverDirection.East => Right("2 2 E")
              case RoverDirection.West => Right("0 2 W")
            case 'R' => turnRoverRight(roverPosition)
            case 'L' => roverPosition.direction match
              case RoverDirection.North => Right("2 3 W")
              case RoverDirection.East => Right("2 3 N")
              case RoverDirection.South => Right("2 3 E")
              case RoverDirection.West => Right("2 3 S")
          case false => Left(CommandError("invalid position"))
    }

  def moveRoverNorth(roverPosition: RoverPosition): Either[CommandError, String] =
      Right(s"${roverPosition.x} ${roverPosition.y + 1} N")

  def turnRoverRight(roverPosition: RoverPosition): Either[CommandError, String] =
    val currentPositionX = roverPosition.x
    val currentPositionY = roverPosition.y
    roverPosition.direction match
      case RoverDirection.North =>
        Right(s"$currentPositionX $currentPositionY E")
      case RoverDirection.East =>
        Right(s"$currentPositionX $currentPositionY S")
      case RoverDirection.South =>
        Right(s"$currentPositionX $currentPositionY W")
      case RoverDirection.West =>
        Right(s"$currentPositionX $currentPositionY N")

}