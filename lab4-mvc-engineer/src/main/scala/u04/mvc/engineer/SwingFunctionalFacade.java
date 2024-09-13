package scala.u04.mvc.engineer;

import javax.swing.*;
import java.awt.*;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.function.Supplier;

class SwingFunctionalFacade {

    public interface Frame {
        Frame setSize(int width, int height);
        Frame addPanel(JPanel panel, String name);
        Frame drawEllipse(String panelName, int x, int y, int radius);
        Frame schedule(int millis, String eventName);
        Frame show();
        Supplier<String> events();        
    }

    public static Frame createFrame(){
        return new FrameImpl();
    }

    private static class FrameImpl implements Frame {
        private final JFrame jframe = new JFrame();
        private final Map<String, JPanel> jPanels = new HashMap<>();
        private final LinkedBlockingQueue<String> eventQueue = new LinkedBlockingQueue<>();
        private final Supplier<String> events = () -> {
            try{
                return eventQueue.take();
            } catch (InterruptedException e){
                return "";
            }
        };
        public FrameImpl() {
            this.jframe.setLayout(new GridLayout(1, 1));
            this.jframe.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        }

        @Override
        public Frame setSize(int width, int height) {
            this.jframe.setSize(width, height);
            return this;
        }

        @Override
        public Frame addPanel(JPanel panel, String name) {
            panel.setPreferredSize(this.jframe.getSize());
            panel.setBackground(Color.BLACK);
            this.jframe.add(panel);
            this.jPanels.put(name, panel);
            return this;
        }

        @Override
        public Frame drawEllipse(String panelName, int x, int y, int radius) {
            if (this.jPanels.containsKey(panelName)) {
                var panel = this.jPanels.get(panelName);
                if (panel instanceof SwingCustomComponents.BallPanel drawablePanel) {
                    drawablePanel.drawBall(x, y, radius);
                    drawablePanel.repaint();
                }
            }
            return this;
        }

        @Override
        public Frame schedule(int millis, String eventName) {
            Timer timer = new Timer(millis, (ev) -> {
                try {
                    eventQueue.put(eventName);
                } catch (InterruptedException ex){}
            });
            timer.setRepeats(false); 
            timer.start();
            return this;
        }


        @Override
        public Supplier<String> events() {
            return events;
        }

        @Override
        public Frame show() {
            this.jframe.setVisible(true);
            return this;
        }

    }
}
