import org.ggp.base.player.gamer.event.GamerSelectedMoveEvent
import org.ggp.base.player.gamer.statemachine.sample.SampleGamer
import org.ggp.base.util.statemachine.{MachineState, Move, Role}

trait NotifyingPlayer extends SampleGamer {
    override def stateMachineSelectMove(timeout: Long) = {
        val start = System.currentTimeMillis()

        val selection = bestmove(getRole, getCurrentState)
        val stop = System.currentTimeMillis()

        notifyObservers(new GamerSelectedMoveEvent(getStateMachine.getLegalMoves(getCurrentState, getRole), selection, stop - start))

        selection
    }

    def bestmove(role: Role, state: MachineState): Move
}
