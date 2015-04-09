import org.ggp.base.player.gamer.event.GamerSelectedMoveEvent
import org.ggp.base.player.gamer.statemachine.sample.SampleGamer

import scala.collection.JavaConversions._

class LegalGamer extends SampleGamer {
    override def stateMachineSelectMove(timeout: Long) = {
        val start = System.currentTimeMillis()

        val moves = getStateMachine.getLegalMoves(getCurrentState, getRole)

        val selection = moves.head

        val stop = System.currentTimeMillis()

        notifyObservers(new GamerSelectedMoveEvent(moves, selection, stop - start))

        selection
    }
}
