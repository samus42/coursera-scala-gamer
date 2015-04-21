import org.ggp.base.util.statemachine.{MachineState, Move, Role}

import scala.collection.JavaConversions._

class MinMaxHeuristic extends NotifyingPlayer {
    val maxDepth = 3

    override def bestmove(role: Role, state: MachineState) = {
        println("New move starting: " + state.toString)
        val startTime = System.currentTimeMillis()
        def showScore(action: Move) = {
            val result = minscore(role, action, state, 0)
//            println(s"Move: $action result = $result - Time Taken: ${System.currentTimeMillis() - startTime}")
            result
        }
        val actions = getStateMachine.getLegalMoves(state, role)
        println(s"#actions = ${actions.size()}")
        actions.maxBy(showScore)
    }

    private def minscore(role: Role, action: Move, state: MachineState, level: Int): Int = {

        def movesByRole(nextAction: Move) = {
            if (role == getStateMachine.getRoles.get(0)) {
                List(action, nextAction)
            } else {
                List(nextAction, action)
            }
        }
        val opponent = findOpponent(role)
        val opponentActions = getStateMachine.getLegalMoves(state, opponent)
        val scores = opponentActions.map(a => {
            val move = movesByRole(a)
            val newState = getStateMachine.getNextState(state, move)
            maxscore(role, newState, level + 1)
        })
        scores.min
    }

    private def findOpponent(role: Role): Role = {
        getStateMachine.getRoles.find(_.getName != role.getName).getOrElse(role)
    }

    private def maxscore(role: Role, state: MachineState, level: Int): Int = {
        if (getStateMachine.isTerminal(state)) {
            getStateMachine.getGoal(state, role)
        } else if (level >= maxDepth) {
            evaluateState(role, state)
        }
        else {
            val actions = getStateMachine.getLegalMoves(state, role)
            actions.map(a => minscore(role, a, state, level)).max
        }
    }

    private def evaluateState(role: Role, state: MachineState) = {
        val result = simpleGoalProximity(role, state)
        if (result > 100) {
            println("over 100")
            100
        } else {
            result.toInt
        }
    }

    private def mobility(role: Role, state: MachineState) = {
        val actions = getStateMachine.getLegalMoves(state, role).size().toFloat
        (actions / findFeasibles(role, state)) * 100
    }

    private def focus(role: Role, state: MachineState) = {
        val actions = getStateMachine.getLegalMoves(state, role).size().toFloat
        100.0 - ((actions / findFeasibles(role, state)) * 100)
    }

    private def simpleGoalProximity(role: Role, state: MachineState) = {
        getStateMachine.getGoal(state, role)
    }

    private def advancedGoalProximity(role: Role, state: MachineState) = {

    }
    private def findFeasibles(role: Role, state: MachineState) = {
        //according to TA, GGP base doesn't have the required method, so just give it an average
        20.0f
    }
}
