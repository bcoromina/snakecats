import cats.Monad
import cats.data.{NonEmptyList, State, StateT}
import cats.effect.std.Random
import cats.effect.unsafe.implicits.global
import cats.effect.{IO, Ref, Sync}
import domain._
import ui._

import scala.concurrent.duration._

object SnakeCats {

  def main(args: Array[String]): Unit = {
    val panel = Panel(600,600)
    val board = Board(24, 24)
    val initialGameState = BoardState(
      board,
      Snake(NonEmptyList(Position(20,20), List(Position(20,20), Position(21,20), Position(22,20), Position(23,20)))),
      Food(Position(3, 4)),
      domain.Left
    )

    /*
    def gameLoop(ui: SnakeUI): StateT[IO, BoardState, Unit] = {
     for{
        d <- StateT.liftF(ui.currentDirection())
        _ <- stateMonad.updateState(d).liftState
        _ <- ui.paintState()
        _ <- StateT.liftF(IO.sleep(100.millis))
        _ <- gameLoop(ui)
      }yield ()
    }

    def mainProgram(): StateT[IO, BoardState, Unit] = {
      for{
        ui <- StateT.liftF(getUI(panel, board, initialGameState))
        _ <- gameLoop(ui)
      }yield ()
    }

    mainProgram.run(initialGameState).unsafeRunSync()
    */

    import cats.implicits._

    def taglessGameLoop[F[_]: Sync: Random](ui: SnakeUITagless, stateRef:  Ref[F, BoardState], initialState: BoardState): F[Unit] = {
      for{
        direction <- ui.currentDirection[F]()
        _ <- finalTagless.updateState(direction, stateRef, initialState)
        _ <- ui.paintState(stateRef)
        _ <- sleepTagless[F](100.millis)
        _ <- taglessGameLoop(ui, stateRef, initialState)
      }yield ()
    }

    def main[F[_] : Sync]: F[Unit] = for{
      r <- Random.scalaUtilRandom[F]
      ui <- SnakeUITagless.createUI[F](panel, initialGameState)
      stateRef <- Ref[F].of(initialGameState)
      _<- {
        implicit val random: Random[F] = r
        taglessGameLoop[F](ui, stateRef, initialGameState)
      }
    }yield ()

    main[IO].unsafeRunSync()

  }





  def sleepTagless[F[_]: Sync](duration: FiniteDuration)() = Sync[F].delay(Thread.sleep(duration.toMillis))

  def sleep(duration: FiniteDuration):IO[Unit]  = IO.sleep(duration)

  def getUI(panel: Panel, board: Board, boardState: BoardState): IO[SnakeUI] = IO(new SnakeUI(panel, board, boardState))

  implicit class StateOps[S,B](state: State[S, B]){
    def liftState: StateT[IO, S, B] =
        StateT[IO, S, B] { s => IO.eval(state.run(s)) }
  }

  def liftState[S, B](state: State[S, B]): StateT[IO, S, B] = StateT[IO, S, B] { s => IO.eval(state.run(s)) }

}




