import org.ggp.base.player.gamer.event.GamerSelectedMoveEvent
import org.ggp.base.player.gamer.statemachine.sample.SampleGamer
import org.ggp.base.util.statemachine.{MachineState, Move, Role}

import scala.collection.JavaConversions._

class DeliberativePlayer extends SampleGamer {
    val maxDepth = 10

    override def stateMachineSelectMove(timeout: Long) = {
        val start = System.currentTimeMillis()
        val selection = bestmove(getRole, getCurrentState)
        val stop = System.currentTimeMillis()

        notifyObservers(new GamerSelectedMoveEvent(getStateMachine.getLegalMoves(getCurrentState, getRole), selection, stop - start))

        selection
    }

    private def bestmove(role: Role, state: MachineState): Move = {
        val actions = getStateMachine.getLegalMoves(state, role)
        actions.maxBy(a => maxscore(role, getStateMachine.getNextState(state, List(a)), 0))
    }

    private def maxscore(role: Role, state: MachineState, level: Int): Int = {
        if (getStateMachine.isTerminal(state) || getStateMachine.getGoal(state, role) == 100) {
            getStateMachine.getGoal(state, role)
        } else if (level >= maxDepth) {
            simpleGoalProximity(role, state)
        } else {
            val actions = getStateMachine.getLegalMoves(state, role)
            actions.map(m => maxscore(role, getStateMachine.getNextState(state, List(m)), level + 1)).max
        }
    }

    private def simpleGoalProximity(role: Role, state: MachineState) = {
        getStateMachine.getGoal(state, role)
    }
}
