import org.ggp.base.player.gamer.event.GamerSelectedMoveEvent
import org.ggp.base.player.gamer.statemachine.sample.SampleGamer

import scala.collection.JavaConversions._
import scala.util.Random

class RandomPlayer extends SampleGamer {
    override def stateMachineSelectMove(timeout: Long) = {
        val start = System.currentTimeMillis()

        val moves = getStateMachine.getLegalMoves(getCurrentState, getRole)

        val index = Random.nextInt(moves.length)
        val selection = moves(index)

        val stop = System.currentTimeMillis()

        notifyObservers(new GamerSelectedMoveEvent(moves, selection, stop - start))

        selection
    }
}