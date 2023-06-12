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

object RoverDirection:
  def fromChar(value: Char): RoverDirection =
    value match
      case 'N' => RoverDirection.North
      case 'E' => RoverDirection.East
      case 'S' => RoverDirection.South
      case 'W' => RoverDirection.West

  def toChar(value: RoverDirection): Char =
    value match
      case RoverDirection.North => 'N'
      case RoverDirection.East => 'E'
      case RoverDirection.South => 'S'
      case RoverDirection.West => 'W'

case class Rover(x: Integer, y: Integer, direction: RoverDirection)

object Rover:
  def toString(rover: Rover): String =
    s"${rover.x} ${rover.y} ${RoverDirection.toChar(rover.direction)}"



enum RoverCommand:
  case Move
  case Left
  case Right

object RoverCommand:
  def fromChar(value: Char): RoverCommand =
    value match
      case 'M' => RoverCommand.Move
      case 'R' => RoverCommand.Right
      case 'L' => RoverCommand.Left

// solves https://github.com/makomweb/mars-rover
object RoverController {
  def sendCommand(rawInput: String): Either[CommandError, String] = 

    if(rawInput.length == 0){
      Left(CommandError("no input detected"))
    } else {
      val input = rawInput.split('\n').toList

      input match

        case input if input.length == 1 => Left(CommandError("missing rover details"))
        case input if input.length == 2 => Left(CommandError("no commands detected"))
        case (gridSize: String) :: (roversAndCommands: List[String]) =>
          extractRawRoversAndCommands(roversAndCommands)
          .map { (rawRover, rawCommands) => 
            val rover = extractRover(rawRover) 
            rover match 
              case Left(error) => Left(error)
              case Right(rover) =>

                val result = rawCommands.foldLeft(rover) { 
                  (accumulator: Rover, command: Char) => executeCommand(accumulator, RoverCommand.fromChar(command))
                }

                Right(Rover.toString(result))
    
          }
          .sequence
          .map(
            _.fold("") { 
              (accumulator: String, item: String) => s"${accumulator}${item}\n" }.stripSuffix("\n")
          )
        case nil => Left(CommandError("missing gridSize"))

    }

  def extractRawRoversAndCommands(roversAndCommandsRaw: List[String]) : List[(String, String)] =
    roversAndCommandsRaw
    .grouped(2) 
    .toList // List[List[String]] with each sub-list containing up to two items (a string representing a rover and another with its commands)
    .filter( (pair: List[String]) => pair.length == 2) // Filter any pairs without two items
    .map(group => (group(0), group(1))) // Create Tuple

  def executeCommand(rover: Rover, command: RoverCommand) : Rover =
    command match
      case RoverCommand.Move => moveRover(rover)
      case RoverCommand.Right => turnRoverRight(rover)
      case RoverCommand.Left => turnRoverLeft(rover)

  def extractRover(roverPositionRaw: String): Either[CommandError, Rover] =
    roverPositionRaw(0).isDigit && roverPositionRaw(2).isDigit match
      case true => 
        val direction = RoverDirection.fromChar(roverPositionRaw(4))
        Right(Rover(roverPositionRaw(0).asDigit, roverPositionRaw(2).asDigit, direction))
      case false => Left(CommandError("invalid position"))

  def moveRover(rover: Rover): Rover =
    rover.direction match
      case RoverDirection.North => rover.copy(y = rover.y + 1)
      case RoverDirection.South => rover.copy(y = rover.y - 1)
      case RoverDirection.East => rover.copy(x = rover.x + 1)
      case RoverDirection.West => rover.copy(x = rover.x - 1)

  def turnRoverRight(rover: Rover): Rover =
    rover.direction match
      case RoverDirection.North => rover.copy(direction = RoverDirection.East)
      case RoverDirection.East => rover.copy(direction = RoverDirection.South)
      case RoverDirection.South => rover.copy(direction = RoverDirection.West)
      case RoverDirection.West => rover.copy(direction = RoverDirection.North)

  def turnRoverLeft(rover: Rover): Rover =
    rover.direction match
      case RoverDirection.North => rover.copy(direction = RoverDirection.West)
      case RoverDirection.East => rover.copy(direction = RoverDirection.North)
      case RoverDirection.South => rover.copy(direction = RoverDirection.East)
      case RoverDirection.West => rover.copy(direction = RoverDirection.South)
}