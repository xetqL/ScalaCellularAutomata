package cellular_automata
import java.awt.Color
import cellular_automata.state.State
import scalaz.Functor

/**
  * Created by xetql on 01.12.16.
  */
object state{

    import java.awt.Color

    import scalaz.Functor

    sealed trait DeadOrAlive
    case class Alive() extends DeadOrAlive
    case class Dead() extends DeadOrAlive

    sealed trait Forest
    case class Empty()  extends Forest
    case class Cinder() extends Forest
    case class OnFire() extends Forest
    case class Tree()   extends Forest

    case class State[T](v : T)

    implicit val stateFunctor = new Functor[State] {
        def map[A, B](fa: State[A])(f: A => B): State[B] = State(f(fa.v))
    }
    def GOLtoColor (s:DeadOrAlive) : Color = s match {
        case s:Dead => Color.BLACK
        case _      => Color.WHITE
    }
    def FFtoColor  (s:Forest) : Color = s match {
        case s:Cinder => Color.GRAY
        case s:OnFire => Color.RED
        case s:Tree   => Color.GREEN
        case s:Empty  => Color.BLACK
    }
}