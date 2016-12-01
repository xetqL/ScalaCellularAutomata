package cellular_automata

import cellular_automata.state._
import cellular_automata.state.Tree
import cellular_automata.neighborhood._
import cellular_automata.rule._
import cellular_automata.population._
import cellular_automata.simulation._
import cellular_automata.GUIHelper._

/**
  * Created by xetql on 25.11.16.
  */

object CellularAutomaton extends App {
    lazy val arrGOL = createRandomPopulation[DeadOrAlive](300)(List(0.45, 0.55), List(Dead(), Alive()))((lp, v, x) => v(lp.count(_ < x)))
    lazy val arrFF  = createRandomPopulation[Forest](300)(List(0.00003, 0.60, 0.60), List(OnFire(), Empty(), Tree()))((lp, v, x) => v(lp.count(_ < x)))
    lazy val jcGOL = new JCanvas[DeadOrAlive](arrGOL, GOLtoColor)
    lazy val jcFF  = new JCanvas[Forest](arrFF, FFtoColor)

    def time[R](block: => R): R = {
        val t0 = System.currentTimeMillis()
        val result = block    // call-by-name
        val t1 = System.currentTimeMillis()
        println("Elapsed time: " + (t1 - t0) + "ms")
        result
    }

    time{  start[Forest](arrFF, ForestFireRule(), MooreNeighborhood())(1000)(jcFF) }
    time{  start2[DeadOrAlive](arrGOL,jcGOL, 1000)( GameOfLifeRule().rule, true)(MooreNeighborhood().getNeighbors) }
}
