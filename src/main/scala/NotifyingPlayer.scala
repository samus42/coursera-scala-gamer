import org.ggp.base.player.gamer.event.GamerSelectedMoveEvent
import org.ggp.base.player.gamer.statemachine.sample.SampleGamer
import org.ggp.base.util.statemachine.{MachineState, Move, Role}
import scala.collection.JavaConversions._

trait NotifyingPlayer extends SampleGamer {
    override def stateMachineSelectMove(timeout: Long) = {
        val start = System.currentTimeMillis()
        println("New move starting....")

        val selection = bestmove(getRole, getCurrentState)
        val stop = System.currentTimeMillis()

        notifyObservers(new GamerSelectedMoveEvent(getStateMachine.getLegalMoves(getCurrentState, getRole), selection, stop - start))
        println(s"Selected move: $selection , Total Time: ${stop - start}")
        try {
            val nextState = getStateMachine.getNextState(getCurrentState, List(selection))
            println("Goal: " + getStateMachine.getGoal(nextState, getRole))
        } catch {
            case ex: Exception => println(ex.getMessage)
        }
        selection

    }

    def bestmove(role: Role, state: MachineState): Move
}
