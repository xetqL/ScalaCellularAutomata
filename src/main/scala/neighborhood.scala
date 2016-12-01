/**
  * Created by xetql on 01.12.16.
  */
package cellular_automata
object neighborhood {
    sealed trait Neighborhood{
        def getNeighbors: (Int, Int) => List[(Int,Int)]
    }
    case class MooreNeighborhood() extends Neighborhood{
        override def getNeighbors: (Int, Int) => List[(Int,Int)] =
            (x, y) => {
                List(
                    (x-1, y-1), (x, y-1), (x+1, y-1),
                    (x-1, y)  ,   /*P*/   (x+1, y)  ,
                    (x-1, y+1), (x, y+1), (x+1, y+1)
                )
            }
    }
    case class VonNeumannNeighborhood() extends Neighborhood{
        override def getNeighbors: (Int, Int) => List[(Int,Int)] =
            (x, y) => {
                List(
                    (x, y-1),
                    (x-1, y),   /*P*/   (x+1, y),
                    (x, y+1)
                )
            }
    }
}