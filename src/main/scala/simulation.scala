package cellular_automata

import cellular_automata.GUIHelper._
import cellular_automata.neighborhood.Neighborhood
import cellular_automata.population._
import cellular_automata.rule.CellularAutomatonRule
import cellular_automata.state.State

import scala.annotation.tailrec
import scala.collection.parallel.ParSeq

/**
  * Created by xetql on 01.12.16.
  */
object simulation {
    def start2[T](pop: Population[State[T]], jc:JCanvas[T], max:Int = 100)
                 (rule: (List[State[T]], State[T]) => State[T], onSphere: Boolean)
                 (fn: (Int, Int) => List[(Int,Int)]): Population[State[T]] = {
        def simulateCellularAutomaton(pop: Population[State[T]], neighborhood: ParSeq[ParSeq[List[(Int,Int)]]]): Population[State[T]] = {
            val populationNeighbors = neighborhood.map{
                _.map(n => n.map(i => {
                    pop(i._1)(i._2)
                }))
            }
            val p = for (i <- populationNeighbors.seq.indices) yield populationNeighbors(i) zip pop(i)
            p.map(
                _.map(
                    np => rule(np._1, np._2)
                ).seq
            ).seq
        }
        val populationNeighbors = if(onSphere) ParSeq.tabulate(pop.size, pop.head.size)((x, y) => fn(x,y).map(xy => {
            disposeOnSphere(xy._1, xy._2, pop.size, pop.head.size)
        })) else ParSeq.tabulate(pop.size, pop(0).size)((x, y) =>
            fn(x,y).filter(i => isValidMatrixPosition(i._1, i._2, pop.size, pop(0).size))
        )
        @tailrec
        def rec(i:Int, currentPope: Population[State[T]]) : Population[State[T]] = {
            //jc.setMatrix(currentPope)
            if (i == max) currentPope
            else rec(i+1, simulateCellularAutomaton(currentPope, populationNeighbors))
        }
        rec(0, pop)
    }

    def start[T] (pop: Population[State[T]], r: CellularAutomatonRule[T], neighborhood: Neighborhood)(max:Int = 1000)(jc:JCanvas[T]): Population[State[T]] = {
        def simulateCellularAutomaton(pop: Population[State[T]], neighborhoodMatrix: ParSeq[ParSeq[List[(Int,Int)]]]): Population[State[T]] = {
            val populationNeighbors = neighborhoodMatrix.map{
                _.map(n => n.map(i => {
                    pop(i._1)(i._2)
                }))
            }
            val p = for (i <- populationNeighbors.seq.indices) yield populationNeighbors(i) zip pop(i)
            p.map(
                _.map(
                    np => r.rule(np._1, np._2)
                ).seq
            )
        }
        val populationNeighbors = if(r.onSphere) ParSeq.tabulate(pop.size, pop.head.size)((x, y) => neighborhood.getNeighbors(x,y).map(xy => {
            disposeOnSphere(xy._1, xy._2, pop.size, pop.head.size)
        })) else ParSeq.tabulate(pop.size, pop(0).size)((x, y) =>
            neighborhood.getNeighbors(x,y).filter(i => isValidMatrixPosition(i._1, i._2, pop.size, pop(0).size))
        )
        GUIHelper.showOnFrame(jc, "Cellular Automaton") //border effect
        @tailrec
        def rec(i:Int, currentPope: Population[State[T]]) : Population[State[T]] = {
            jc.setMatrix(currentPope) //border effect
            i match {
                case it if it == max => currentPope
                case _ => rec(i+1, simulateCellularAutomaton(currentPope, populationNeighbors))
            }
        }
        rec(0, pop)
    }
}
