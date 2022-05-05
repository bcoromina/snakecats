package domain

import cats.Monad
import cats.effect.Ref
import cats.effect.std.Random
import cats.implicits._
import domain.SnakeGame.{getNewSnake, snakeOverlaps}

object finalTagless{

  def updateState[F[_]: Monad: Random](direction: Direction, stateRef: Ref[F, BoardState], initialState: BoardState) = {
    for{
      _ <- updateDirection[F](direction, stateRef)
      _ <- updateStake[F](stateRef, initialState)
      _ <- updateFood[F](stateRef)
    }yield ()
  }

  private def changeDirections(a: Direction, b: Direction) = a.axis != b.axis

  private def updateDirection[F[_]: Monad](d: Direction, stateRef: Ref[F, BoardState]): F[Unit] = {
    for{
      state <- stateRef.get
      _ <- {
        if( changeDirections(state.direction, d)){
          stateRef.update(state => BoardState(state.board, state.snake, state.food, d))
        }else{
          Monad[F].pure(())
        }
      }
    }yield ()


  }

  private def randomPosCoordinate[F[_]: Random](maxPos: Int): F[Int] = {
    Random[F].betweenInt(1, maxPos)
  }

  private def randomPos[F[_]: Monad: Random ](board: Board): F[Position]={
    for{
      x <- randomPosCoordinate(board.maxX)
      y <- randomPosCoordinate(board.maxY)
    }yield{
      Position(x,y)
    }
  }

  private def updateFood[F[_]: Monad: Random](stateRef: Ref[F, BoardState]): F[Unit] = {
    for{
      currentState <- stateRef.get
      rPos <- randomPos[F](currentState.board)
      _ <- {
        if(currentState.snake.positions.head == currentState.food.position) {
          stateRef.update(state => BoardState(state.board, state.snake, Food(rPos), state.direction))
        }else {
          Monad[F].pure(())
        }
      }
    }yield ()
  }



  private def updateStake[F[_]: Monad](stateRef: Ref[F, BoardState], initialState: BoardState) = {
    for{
      state <- stateRef.get
      newSnake <- Monad[F].pure( getNewSnake(state) )
      _ <- {
        if(snakeOverlaps(newSnake)){
          stateRef.update( _ => initialState)
        }else{
          stateRef.update( state => BoardState(state.board, getNewSnake(state), state.food, state.direction))
        }
      }
    }yield ()

  }


}