import org.ggp.base.player.gamer.event.GamerSelectedMoveEvent
import org.ggp.base.player.gamer.statemachine.sample.SampleGamer
import org.ggp.base.util.statemachine.{MachineState, Move, Role}

import scala.collection.JavaConversions._
import scala.util.Random

class DeliberativePlayer extends SampleGamer {
    val maxDepth = 8

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
        if (isTerminal(role, state)) {
            getStateMachine.getGoal(state, role)
        } else if (level >= maxDepth) {
//            simpleGoalProximity(role, state)
            monteCarlo(role, state, 4)
        } else {
            val actions = getStateMachine.getLegalMoves(state, role)
            actions.map(m => maxscore(role, getStateMachine.getNextState(state, List(m)), level + 1)).max
        }
    }

    private def isTerminal(role: Role, state: MachineState) = {
        getStateMachine.isTerminal(state) || getStateMachine.getGoal(state, role) == 100
    }
    private def simpleGoalProximity(role: Role, state: MachineState) = {
        getStateMachine.getGoal(state, role)
    }

    private def monteCarlo(role: Role, state: MachineState, numProbes: Int) = {
        def depthcharge(role: Role, state: MachineState) : Int = {
            if (isTerminal(role, state)) {
                getStateMachine.getGoal(state, role)
            } else {
                val moves = getStateMachine.getRoles.map(r => {
                    val roleMoves = getStateMachine.getLegalMoves(state, r)
                    roleMoves(Random.nextInt(roleMoves.length))
                })
                val newState = getStateMachine.getNextState(state, moves)
                depthcharge(role, newState)
            }
        }
        val results = for {
            i <- 1 to numProbes
        } yield depthcharge(role, state)

        (results.sum.toFloat / numProbes.toFloat).toInt
    }
}
