package u07.simulation;

import u04.mvc.engineer.SwingCustomComponents

import javax.swing.*
import javax.swing.WindowConstants.EXIT_ON_CLOSE
import java.awt.*
import java.util.concurrent.LinkedBlockingQueue
import java.util.function.Supplier
import scala.collection.mutable
import scala.util.Try

object SwingFunctionalFacade:

  def createFrame(): Frame = new FrameImpl();

  trait Frame:
    def setSize(width: Int, height: Int): Frame
    def addPanel(panel: JPanel, name: String): Frame
    def schedule(millis: Int, eventName: String): Frame
    def show(): Frame
    def events(): () => String


  class FrameImpl extends Frame:
    private val jFrame: JFrame = new JFrame()
    private val jPanels: mutable.Map[String, JPanel] = mutable.Map()
    private val eventQueue: LinkedBlockingQueue[String] = new LinkedBlockingQueue()
    override def events(): () => String = () =>
      Try:
        eventQueue.take()
      .getOrElse("")

    this.jFrame.setLayout(new GridLayout(1, 1))
    this.jFrame.setDefaultCloseOperation(EXIT_ON_CLOSE)

    override def setSize(width: Int, height: Int): Frame =
      this.jFrame.setSize(width, height)
      this

    override def addPanel(panel: JPanel, name: String): Frame =
      panel.setPreferredSize(this.jFrame.getSize());
      panel.setBackground(Color.BLACK);
      this.jFrame.add(panel);
      this.jPanels += (name -> panel);
      this

    def schedule(millis: Int, eventName: String): Frame = 
      val timer = new Timer(millis, _ => Try(eventQueue.put(eventName)))
      timer.setRepeats(false)
      timer.start()
      this
      
    def show(): Frame = 
      jFrame.setVisible(true)
      this



