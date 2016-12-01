/**
  * Created by xetql on 01.12.16.
  */
package cellular_automata
import cellular_automata.state.State

object population{

    type Population[T] = Seq[Seq[T]]
    def isValidMatrixPosition(x:Int, y:Int, xSize:Int, ySize:Int) : Boolean = if (x < xSize && y < ySize && x >= 0 && y >= 0) true else false
    def disposeOnSphere      (x:Int, y:Int, xSize:Int, ySize:Int) : (Int, Int) = (x,y) match {
        case (i,j) if i < 0 && j < 0            => (xSize-1, ySize-1)
        case (i,j) if i >= xSize && j >= ySize  => (0, 0)
        case (i,j) if i < 0 && j >= ySize       => (xSize-1, 0)
        case (i,j) if i >= xSize && j < 0       => (0, ySize-1)
        case (i,j) if i >= xSize                => (0, j)
        case (i,j) if j >= ySize                => (i, 0)
        case (i,j) if i < 0                     => (xSize-1, j)
        case (i,j) if j < 0                     => (i, ySize-1)
        case (i, j)                             => (i,j)
    }
    def printPopulation[T](pop:Population[T]) : Unit = {
        pop.foreach( a => println(a.mkString(",")) )
    }
    def createRandomPopulation[T](sz:Int)(lp: List[Double], v:List[T])(f : (List[Double], List[T], Double) => T): Population[State[T]] = Seq.fill(sz){
            val r = scala.util.Random
            Seq.fill(sz)( State(f(lp.scan(0.0)(_+_).tail, v, r.nextDouble())) )
        }

}
