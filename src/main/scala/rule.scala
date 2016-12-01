package cellular_automata
import cellular_automata.state._

/**
  * Created by xetql on 01.12.16.
  */
object rule{

    sealed trait CellularAutomatonRule[T] {
        def rule: (List[State[T]], State[T]) => State[T]
        val onSphere: Boolean
    }
    case class GameOfLifeRule() extends CellularAutomatonRule[DeadOrAlive] {
        val onSphere = true
        override def rule: (List[State[DeadOrAlive]], State[DeadOrAlive]) => State[DeadOrAlive] =
            (neighbors, target) => (neighbors.count(_.v.isInstanceOf[Alive]), target) match {
                case (n, t) if n  < 2               => State(Dead())
                case (n, t) if n  > 3               => State(Dead())
                case (n, t) if n == 3               => State(Alive())
                case (n, t@State(Alive())) if n == 2=> State(Alive())
                case _                              => State(Dead())
            }
    }
    case class DayAndNightRule() extends CellularAutomatonRule[DeadOrAlive] {
        val onSphere = true
        override def rule: (List[State[DeadOrAlive]], State[DeadOrAlive]) => State[DeadOrAlive] =
            (neighbors, target) => (neighbors.count(_.v.isInstanceOf[Alive]), target) match {
                case (3|6|7|8,   t@State(Dead()))   => State(Alive())
                case (3|4|6|7|8, t@State(Alive()))  => State(Alive())
                case (_,t)                          => State(Dead())
            }
    }
    case class ForestFireRule() extends CellularAutomatonRule[Forest] {
        val onSphere = false
        override def rule: (List[State[Forest]], State[Forest]) => State[Forest] =
            (neighbors, target) => (neighbors.exists(_.v.isInstanceOf[OnFire]), target) match {
                case (_, t@State(Empty()))          => t
                case (_, t@State(Cinder()))         => t
                case (_, t@State(OnFire()))         => State(Cinder())
                case (n, t@State(Tree())) if n      => State(OnFire())
                case (n, t@State(Tree()))           => t
            }
    }
}