import org.ggp.base.util.statemachine.{Move, MachineState, Role}
import scala.collection.JavaConversions._

class SinglePlayerLimitedDepth extends NotifyingPlayer {
    val maxDepth = 10
    override def bestmove(role: Role, state: MachineState) = {
        val moves = getStateMachine.getLegalMoves(getCurrentState, getRole)
        moves.maxBy(findMax(_, state, 0))
    }

    def findMax(action: Move, state: MachineState, level: Int) : Int = {
        if (getStateMachine.isTerminal(state)) {
            getStateMachine.getGoal(state, getRole)
        } else if (level >= maxDepth) {
            0
        }
        else {
            val nextState = getStateMachine.getNextState(state, List(action))
            val moves = getStateMachine.getLegalMoves(nextState, getRole)
            moves.map(findMax(_, nextState, level + 1)).max
        }
    }
}
