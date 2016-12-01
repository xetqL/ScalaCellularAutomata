/**
  * Created by xetql on 29.11.16.
  */
package cellular_automata

import cellular_automata.population._
import cellular_automata.state.State
import java.awt._
import javax.swing._
import java.awt.event.WindowAdapter
import java.awt.event.WindowEvent
import javax.swing.JComponent
import javax.swing.JFrame
import scalaz.Functor;


object GUIHelper {

    def showOnFrame(component:JComponent , frameName:String) {
        val frame = new JFrame(frameName);
        val wa = new WindowAdapter() {
            override def windowClosing(e:WindowEvent) {
                System.exit(0);
            }
        };
        component.setBackground(Color.WHITE)
        component.setPreferredSize(new Dimension(800, 800))
        frame.addWindowListener(wa);
        frame.getContentPane().add(component);
        frame.pack();
        frame.setVisible(true);
    }

    class JCanvas[U](var matrix: Population[State[U]], f: U => Color) extends JPanel {
        override def paint(g: Graphics) = {
            super.paint(g)
            for(i <- 0 to matrix.size-1) for(j <- 0 to matrix(0).size-1){
                g.setColor(Functor[State].map(matrix(i)(j))(f).v)
                g.fillRect(
                    java.lang.Math.round(j * this.getSize().getWidth() / matrix.size).toInt,
                    java.lang.Math.round(i * this.getSize().getHeight()/ matrix(0).size).toInt,
                    java.lang.Math.round(    this.getSize().getWidth() / matrix(0).size).toInt,
                    java.lang.Math.round(    this.getSize().getHeight()/ matrix.size).toInt
                )
            }
        }
        def setMatrix(matrix: Population[State[U]]): Unit = {
            this.matrix = matrix
            this.repaint()
        }
    }
}


