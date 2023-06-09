import test.demo.RoverController.sendCommand
import test.demo.CommandError

class AcceptanceTests extends munit.FunSuite {
  test("example test that succeeds") {
    val input = """
|5 5
|1 2 N
|LMLMLMLMM
|3 3 E
|MMRMMRMRRM
""".stripMargin
    val output = """
|1 3 N
|5 1 E
""".stripMargin

    assertEquals(sendCommand(input), Right(output))
  }
}

class UnitTests extends munit.FunSuite {

  test("should be able to handle multiple commands for a single rover") {
    val input = "5 5\n1 2 N\nLMLMLMLMM"
    val finalPosition = "1 3 N"

    assertEquals(sendCommand(input), Right(finalPosition))
  }

  test("should fail when there is no input") {
    val input = ""
    val error = CommandError("no input detected")

    assertEquals(sendCommand(input), Left(error))
  }

  test("should fail when only a grid size is given") {
    val input = "5 5"
    val error = CommandError("missing rover details")

    assertEquals(sendCommand(input), Left(error))
  }

  test("should fail when no commands are included") {
    val input = "5 5\n1 2 E"
    val error = CommandError("no commands detected")

    assertEquals(sendCommand(input), Left(error))
  }

  test("should fail when no commands are included for a large plateau") {
    val input = "50 50\n10 20 E"
    val error = CommandError("no commands detected")

    assertEquals(sendCommand(input), Left(error))
  }

  test("should error if the x position is not numeric") {
    val input = "5 5\n1 V N\nM"
    val error = CommandError("invalid position")
    
    assertEquals(sendCommand(input), Left(error))
  }

  test("should error if the y position is not numeric") {
    val input = "5 5\nD 2 N\nM"
    val error = CommandError("invalid position")
    
    assertEquals(sendCommand(input), Left(error))
  }



  test("should move north from 1 2 N") {
    val input = "5 5\n1 2 N\nM"
    val finalPosition = "1 3 N"

    assertEquals(sendCommand(input), Right(finalPosition))
  }
  test("should move north from 1 3 N") {
    val input = "5 5\n1 3 N\nM"
    val finalPosition = "1 4 N"

    assertEquals(sendCommand(input), Right(finalPosition))
  }
  test("should move north from 2 3 N") {
    val input = "5 5\n2 3 N\nM"
    val finalPosition = "2 4 N"

    assertEquals(sendCommand(input), Right(finalPosition))
  }


  test("should move east from 1 2 E") {
    val input = "5 5\n1 2 E\nM"
    val finalPosition = "2 2 E"

    assertEquals(sendCommand(input), Right(finalPosition))
  }
  test("should move east from 1 3 E") {
    val input = "5 5\n1 3 E\nM"
    val finalPosition = "2 3 E"

    assertEquals(sendCommand(input), Right(finalPosition))
  }


  test("should move south from 1 2 S") {
    val input = "5 5\n1 2 S\nM"
    val finalPosition = "1 1 S"

    assertEquals(sendCommand(input), Right(finalPosition))
  }
  test("should move south from 1 3 S") {
    val input = "5 5\n1 3 S\nM"
    val finalPosition = "1 2 S"

    assertEquals(sendCommand(input), Right(finalPosition))
  }


  test("should move west from 1 2 W") {
    val input = "5 5\n1 2 W\nM"
    val finalPosition = "0 2 W"

    assertEquals(sendCommand(input), Right(finalPosition))
  }
  test("should move west from 1 3 W") {
    val input = "5 5\n1 3 W\nM"
    val finalPosition = "0 3 W"

    assertEquals(sendCommand(input), Right(finalPosition))
  }





  



  test("should turn right from north") {
    val input = "5 5\n2 3 N\nR"
    val finalPosition = "2 3 E"

    assertEquals(sendCommand(input), Right(finalPosition))
  }

  test("should turn right from east") {
    val input = "5 5\n2 3 E\nR"
    val finalPosition = "2 3 S"

    assertEquals(sendCommand(input), Right(finalPosition))
  }

  test("should turn right from south") {
    val input = "5 5\n2 3 S\nR"
    val finalPosition = "2 3 W"

    assertEquals(sendCommand(input), Right(finalPosition))
  }

  test("should turn right from west") {
    val input = "5 5\n2 3 W\nR"
    val finalPosition = "2 3 N"

    assertEquals(sendCommand(input), Right(finalPosition))
  }




  test("should turn right from north when at a different starting position") {
    val input = "5 5\n4 5 N\nR"
    val finalPosition = "4 5 E"

    assertEquals(sendCommand(input), Right(finalPosition))
  }

  test("should turn right from east when at a different starting position") {
    val input = "5 5\n4 5 E\nR"
    val finalPosition = "4 5 S"

    assertEquals(sendCommand(input), Right(finalPosition))
  }

  test("should turn right from south when at a different starting position") {
    val input = "5 5\n4 5 S\nR"
    val finalPosition = "4 5 W"

    assertEquals(sendCommand(input), Right(finalPosition))
  }

  test("should turn right from west when at a different starting position") {
    val input = "5 5\n4 5 W\nR"
    val finalPosition = "4 5 N"

    assertEquals(sendCommand(input), Right(finalPosition))
  }




  test("should turn left from north") {
    val input = "5 5\n2 3 N\nL"
    val finalPosition = "2 3 W"

    assertEquals(sendCommand(input), Right(finalPosition))
  }

  test("should turn left from east") {
    val input = "5 5\n2 3 E\nL"
    val finalPosition = "2 3 N"

    assertEquals(sendCommand(input), Right(finalPosition))
  }

  test("should turn left from south") {
    val input = "5 5\n2 3 S\nL"
    val finalPosition = "2 3 E"

    assertEquals(sendCommand(input), Right(finalPosition))
  }

  test("should turn left from west") {
    val input = "5 5\n2 3 W\nL"
    val finalPosition = "2 3 S"

    assertEquals(sendCommand(input), Right(finalPosition))
  }
  test("should turn left from north when starting at a different position") {
    val input = "5 5\n1 3 N\nL"
    val finalPosition = "1 3 W"

    assertEquals(sendCommand(input), Right(finalPosition))
  }

  test("should turn left from east when starting at a different position") {
    val input = "5 5\n1 3 E\nL"
    val finalPosition = "1 3 N"

    assertEquals(sendCommand(input), Right(finalPosition))
  }

  test("should turn left from south when starting at a different position") {
    val input = "5 5\n1 3 S\nL"
    val finalPosition = "1 3 E"

    assertEquals(sendCommand(input), Right(finalPosition))
  }

  test("should turn left from west when starting at a different position") {
    val input = "5 5\n1 3 W\nL"
    val finalPosition = "1 3 S"

    assertEquals(sendCommand(input), Right(finalPosition))
  }
}