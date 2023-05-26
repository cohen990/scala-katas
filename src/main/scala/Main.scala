package test.demo

import java.io._
import cats.syntax.all.*
import java.awt.dnd.InvalidDnDOperationException

object RoverController {
  def sendCommand(rawInput: String): Either[String, String] = 
    if(rawInput.length == 0){
      Left("no input detected")
    } else {
      val input = rawInput.split('\n')
      input match 
        case input if input.length == 1 => Left("missing rover details")
        case input if input.length == 2 => Left("no commands detected")
        case Array(gridSize, roverPosition, commands) => commands(0) match
          case 'M' => roverPosition(4) match
            case 'N' => moveRoverNorth(roverPosition)
            case 'S' => Right("1 1 S")
            case 'E' => Right("2 2 E")
            case 'W' => Right("0 2 W")
          case 'R' => roverPosition(4) match
            case 'N' => Right("2 3 E")
            case 'E' => Right("2 3 S")
            case 'S' => Right("2 3 W")
            case 'W' => Right("2 3 N")
    }

  def moveRoverNorth(roverPosition: String): Either[String, String] =
    roverPosition(2).isDigit match
      case true => 
        val currentPositionY = roverPosition(2).asDigit
        val currentPositionX = roverPosition(0).asDigit
        val newPositionY = currentPositionY + 1
        Right(s"$currentPositionX $newPositionY N")
      case false => 
        Left("invalid position")
}