package snake.logic

import engine.random.{RandomGenerator, ScalaRandomGen}
import snake.game._
import snake.logic.SnakeLogic._

import scala.collection.mutable.ArrayBuffer

//class Coordinates(var x: Int, var y: Int)

class SnakeLogic(val randomGen: RandomGenerator,
                 val nrColumns: Int,
                 val nrRows: Int) {

  def this() = this(new ScalaRandomGen(), DefaultRows, DefaultColumns)

  //TODO: 2 games at the same time fails, find global state

  object PlayerSnake {
    var x : ArrayBuffer[Int] = ArrayBuffer[Int]()
    var y : ArrayBuffer[Int] = ArrayBuffer[Int]()
    var dir : Direction = East()
    /* Due to the fact that changeDir and step are not called at the same time,
     * This var is needed to keep track of the quickly changing direction and update the "real" dir
     * only once per step
     */
    var quickSwitched : Boolean = false
    var dirX : Int = 1
    var dirY : Int = 0
    setup()

    //Add head and first 2 elements of body as starting setup
    def setup(): Unit = {
      x += (2,1,0)
      y += (0,0,0)
      dir = East()
      directionShift()
    }

    //Updates the value by which head will move depending on dir
    def directionShift(): Unit = {
      if (dir == North()) { dirX = 0; dirY = -1 }
      else if (dir == South()) { dirX = 0; dirY = 1 }
      else if (dir == West()) { dirX = -1; dirY = 0 }
      else if (dir == East()) { dirX = 1; dirY = 0 }
    }

    //Run function on each step to move in dirX, dirY
    def moveSnake(): Unit = {
      var newX : Int = x(0) + dirX
      var newY : Int = y(0) + dirY
      var limitX : Int = nrColumns
      var limitY : Int = nrRows
      //Simple newX % limitX will not work for negative numbers as % isn't actually a math mod statement
      x +:= (newX % limitX + limitX) % limitX
      y +:= (newY % limitY + limitY) % limitY
    }

    //Run function on each step except for when apple is eaten
    def cutSnakeTail(): Unit = {
      x.remove(x.length - 1)
      y.remove(y.length - 1)
    }
  } //End of Body Obj

  object AppleLogic {
    var x : Int = 0
    var y : Int = 0
    var numEatingSteps : Int = 0
    newLoc()

    def newLoc(): Unit = {
      var nrFreeSpots : Int = 0
      for (j <- 0 until nrRows; i <- 0 until nrColumns) {
        var foundSnake : Boolean = false
        for (k <- PlayerSnake.x.indices) {
          if (PlayerSnake.x(k) == i && PlayerSnake.y(k) == j) foundSnake = true
        }
        if (!foundSnake) nrFreeSpots += 1
      }
      if (nrFreeSpots == 0) {
        x = -1
        y = -1
        return
      }
      val r: Int = randomGen.randomInt(nrFreeSpots)
      var count : Int = 0
      for (j <- 0 until nrRows; i <- 0 until nrColumns) {
        var foundSnake : Boolean = false
        for (k <- PlayerSnake.x.indices) {
          if (PlayerSnake.x(k) == i && PlayerSnake.y(k) == j) foundSnake = true
        }
        if (!foundSnake) {
          if (count == r) {
            x = i
            y = j
            return
          }
          count += 1
        }
      }
    }

    def eatingLogic(): Unit = {
      if (numEatingSteps == 0) PlayerSnake.cutSnakeTail()
      else if (numEatingSteps > 0) numEatingSteps -= 1
      if (PlayerSnake.x(0) == x && PlayerSnake.y(0) == y) {
        numEatingSteps += 3
        newLoc()
      }
    }
  } //End of Apple Obj

  def isGameOver: Boolean = {
    //Checks for collision of head against any other part of snake
    for (i <- 1 until PlayerSnake.x.length) {
      if (PlayerSnake.x(0) == PlayerSnake.x(i) && PlayerSnake.y(0) == PlayerSnake.y(i)) {
        return true
      }
    }
    false
  }

  def step(): Unit = {
    if (!isGameOver) {
      PlayerSnake.quickSwitched = false
      PlayerSnake.directionShift()
      PlayerSnake.moveSnake()
      AppleLogic.eatingLogic()
    }
  }

  // TODO implement me
  def setReverseTime(reverse: Boolean): Unit = ()

  def changeDir(d: Direction): Unit = {
    if (!PlayerSnake.quickSwitched) {
      if (PlayerSnake.dir == North() && d == South()) {
      } else if (PlayerSnake.dir == South() && d == North()) {
      } else if (PlayerSnake.dir == West() && d == East()) {
      } else if (PlayerSnake.dir == East() && d == West()) {
      } else PlayerSnake.dir = d

      if (PlayerSnake.dir == d) PlayerSnake.quickSwitched = true
    }
  }

  def getGridTypeAt(x: Int, y: Int): GridType = {
    for(i <- PlayerSnake.x.indices) {
      if (x==PlayerSnake.x(i) && y==PlayerSnake.y(i)) {
        if (i==0) return SnakeHead(PlayerSnake.dir)
        else return SnakeBody(1)
      }
    }
    if (x==AppleLogic.x && y==AppleLogic.y) return Apple()

    Empty()
  }
} //end of SnakeLogic class

/** SnakeLogic companion object */
object SnakeLogic {

  val DefaultColumns = 25
  val DefaultRows = 25

}