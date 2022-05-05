package domain

import cats.data.NonEmptyList



case class Board(maxX: Int, maxY: Int)
/*       1   2   3   ... maxX
      1    |   |   |   |
      2    |   |   |   |
      3    |   |   |   |
      .    |   |   |   |
      .    |   |   |   |
      maxY |   |   |   |
     */


case class Position(x: Int, y: Int)

case class Snake(positions: NonEmptyList[Position])
case class Food(position: Position)

sealed trait Axis
object X extends Axis
object Y extends Axis

sealed abstract class Direction(v: Axis){
  val axis = v
}

object Up extends Direction(Y)
object Down extends Direction(Y)
object Left extends Direction(X)
object Right extends Direction(X)

case class BoardState(board: Board, snake: Snake, food: Food, direction: Direction)

object SnakeGame{

  def getNewSnake(currentState: BoardState): Snake = {
    val newHead = getNewHead(currentState)
    val newSnake = if(newHead == currentState.food.position) {
      val newSnakePos = NonEmptyList(newHead, currentState.snake.positions.toList)
      Snake(newSnakePos)
    } else {
      val newSnakePos = NonEmptyList(newHead, currentState.snake.positions.init)
      Snake(newSnakePos)
    }
    newSnake
  }

  def snakeOverlaps(snake: Snake): Boolean = {
    val head = snake.positions.head
    val tail = snake.positions.tail
    tail.contains(head)
  }

  private def getNewHead(state: BoardState): Position = {
    val currentHead = state.snake.positions.head
    state.direction match{
      case Up =>
        val newPosY = if(currentHead.y == 1) state.board.maxY else currentHead.y - 1
        Position(currentHead.x, newPosY)
      case Down =>
        val newPosY = if(currentHead.y == state.board.maxY ) 1 else currentHead.y + 1
        Position(currentHead.x, newPosY)
      case Left =>
        val newPosX = if(currentHead.x == 1 ) state.board.maxX else currentHead.x - 1
        Position(newPosX, currentHead.y)
      case Right =>
        val newPosX = if(currentHead.x == state.board.maxX ) 1 else currentHead.x + 1
        Position(newPosX, currentHead.y)
    }
  }


}


