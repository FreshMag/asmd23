package u04.mvc.engineer

import java.awt.{Color, Graphics, Graphics2D}
import javax.swing.JPanel

object SwingCustomComponents:
  class BallPanel extends JPanel:
    private var ballX: Int = 0
    private var ballY: Int = 0
    private var ballRadius: Int = 0

    def drawBall(x: Int, y: Int, radius: Int): Unit =
      ballX = x
      ballY = y
      ballRadius = radius
      this.repaint()

    override def paint(g: Graphics): Unit =
      super.paint(g)
      g.setColor(Color.blue)
      g.fillOval(ballX, ballY, ballRadius * 2, ballRadius * 2)
