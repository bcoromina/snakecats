package ui
import cats.data.{NonEmptyList, StateT}
import cats.effect.{IO, Ref, Sync}
import cats.implicits._
import domain._

import java.awt._
import java.awt.event.{KeyAdapter, KeyEvent}
import javax.swing.{JFrame, JPanel, WindowConstants}

case class Panel(width: Int, height: Int)


object SnakeUITagless{
  def createUI[F[_]: Sync](panel: Panel, boardState: BoardState): F[SnakeUITagless] = Sync[F].delay(new SnakeUITagless(panel, boardState))
}

class SnakeUITagless(panel: Panel, initialGameState: BoardState){
  val gameFrame = new GameFrame()
  val gamePanel = new GamePanel( panel, initialGameState.board, initialGameState.direction)

  gameFrame.add(gamePanel)
  gameFrame.setTitle("snakeCats")
  gameFrame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  gameFrame.setResizable(false)
  gameFrame.pack()
  gameFrame.setVisible(true)
  gameFrame.setLocationRelativeTo(null)
  gamePanel.setSize(new Dimension(600,600))

  def currentDirection[F[_]: Sync](): F[Direction] = Sync[F].delay(gamePanel.currentDirection())

  def paintState[F[_]: Sync](stateRef: Ref[F, BoardState]): F[Unit]  = {
    for{
      currentState <- stateRef.get
      _ <- Sync[F].delay(gamePanel.paintRects(gamePanel.getGraphics,UI.calculateRectangles(panel, currentState)))
    }yield ()
  }




}

class SnakeUI(panel: Panel, board: Board, initialGameState: BoardState){
  val gameFrame = new GameFrame()
  val gamePanel = new GamePanel( panel, board, initialGameState.direction)

  gameFrame.add(gamePanel)
  gameFrame.setTitle("snakeCats")
  gameFrame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  gameFrame.setResizable(false)
  gameFrame.pack()
  gameFrame.setVisible(true)
  gameFrame.setLocationRelativeTo(null)
  gamePanel.setSize(new Dimension(600,600))

  def currentDirection(): IO[Direction] = IO(gamePanel.currentDirection())
  def paintState():StateT[IO, BoardState, Unit]  = StateT[IO, BoardState, Unit] { s =>
    for{
      _ <- IO(gamePanel.paintRects(gamePanel.getGraphics,UI.calculateRectangles(panel, s)))
    }yield{
      (s,())
    }
  }
}


class GameFrame() extends JFrame

class GamePanel(panel: Panel, board: Board, initialDirection: Direction) extends JPanel {


  this.setPreferredSize(new Dimension(panel.width, panel.height))
  this.setBackground(Color.black)
  this.setFocusable(true)
  val itemSize= panel.height / board.maxX

  private var direction = initialDirection
  def  currentDirection(): Direction = {
    direction
  }

  addKeyListener(new KeyAdapter() {
    override def keyPressed(e: KeyEvent): Unit = {
      val keyCode = e.getKeyCode
      direction = keyCode match {
        case KeyEvent.VK_UP => Up
        case KeyEvent.VK_DOWN => Down
        case KeyEvent.VK_RIGHT => Right
        case KeyEvent.VK_LEFT => Left
        case _ => direction
      }
    }
  })


  def paintRects(g: Graphics, r: NonEmptyList[GraphicPosition]) = {
    super.paint(g)
    r.toList.foreach{ e=>
      g.setColor(e.color)
      g.drawRect(e.rect.x, e.rect.y, e.rect.width, e.rect.height)
      g.asInstanceOf[Graphics2D].fillRect(e.rect.x, e.rect.y, e.rect.width, e.rect.height)
    }
  }

  override def paintComponent(g: Graphics): Unit = {
    super.paintComponent(g)
    draw(g)
  }

  def draw(g:Graphics) = {
    g.setColor(Color.GRAY)
    (1 to board.maxX).toList.foreach{ i =>
      g.drawLine(i * itemSize, 0, i * itemSize, panel.height)
    }
    (1 to board.maxY).toList.foreach{ i =>
      g.drawLine(0, i * itemSize, panel.width, i * itemSize)
    }
  }


}

case class GraphicPosition(rect: Rectangle, color: Color)

object UI{

  private def rectangleFromPos(panel: Panel, board: Board, position: Position) = {
    val pointsPerX = panel.height / board.maxX
    val pointsPerY = panel.width / board.maxY

    new Rectangle(
      pointsPerX * (position.x - 1),
      pointsPerY * (position.y - 1),
      pointsPerX,
      pointsPerY
    )
  }

  def calculateRectangles(panel: Panel, boardState: BoardState): NonEmptyList[GraphicPosition] = {

    val snake: NonEmptyList[GraphicPosition] = boardState.snake.positions.map{ pos =>
      GraphicPosition(rectangleFromPos(panel, boardState.board, pos), Color.RED)
    }

    val food = GraphicPosition(rectangleFromPos(panel, boardState.board, boardState.food.position), Color.GREEN)

    NonEmptyList(
      food,
      snake.toList
    )
  }


}

