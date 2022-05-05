package domain

import cats.data.{NonEmptyList, State}
import domain.SnakeGame.getNewHead


object stateMonad{
  def updateState(direction: Direction): State[BoardState, Unit] = {
    for{
      _ <- updateDirection(direction)
      _ <- updateStake()
      _ <- updateFood()
    }yield ()
  }

  private def updateDirection(d: Direction): State[BoardState, Unit] = State{ state =>
    val newState = BoardState(state.board, state.snake, state.food, d)
    (newState, ())
  }
  private def updateFood(): State[BoardState, Unit]  = State{ state =>
    if(state.snake.positions.head == state.food.position){
      val newFood = Food(Position(state.food.position.x + 1, state.food.position.y + 1)
      ) //TODO: create new food with random position
      val newState = BoardState(state.board, state.snake, newFood, state.direction)
      (newState, ())
    }else{
      (state,())
    }
  }
  private def updateStake(): State[BoardState, Unit] = State{ state =>
    val newSnake = domain.SnakeGame.getNewSnake(state)
    val newState = BoardState(state.board, newSnake, state.food, state.direction)
    (newState, ())
  }
}