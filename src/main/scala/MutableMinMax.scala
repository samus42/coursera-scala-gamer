import org.ggp.base.util.statemachine.{MachineState, Move, Role}

import scala.collection.JavaConversions._

/**
 * based on : https://github.com/ch-ms/ggp-players/blob/master/ChmsMinmaxPlayer.java
 */
class MutableMinMax extends NotifyingPlayer {
    var me: Role = _
    var opponent: Role = _

    override def bestmove(role: Role, state: MachineState) = {
        me = role
        for (r: Role <- getStateMachine().getRoles()) {
            if (!r.equals(me)) {
                opponent = r
            }
        }

        val moves = getStateMachine().getLegalMoves(getCurrentState(), me)
        var bestMove = moves(0)
        var score = 0
        moves.foreach(move => {
            val result = getMinScore(move, getCurrentState())
            outMoveScore(move, result)
            if (result > score) {
                score = result
                bestMove = move
            }
        }
        )
        bestMove
    }

    private def getMinScore(action: Move, state: MachineState): Int = {
        val moves = getStateMachine().getLegalMoves(state, opponent)
        var score = 100
        moves.foreach(move => {
            var tryMove: List[Move] = List()
            // The action sequence is important in tryMove list
            getStateMachine.getRoleIndices.entrySet().foreach(entry => {
                if (entry.getKey.equals(me)) {
                    tryMove = tryMove ++ List(action)
                } else {
                    tryMove = tryMove ++ List(move)
                }
            })

            val newState = getStateMachine().getNextState(state, tryMove)
            val result = getMaxScore(newState)
            if (result < score) {
                score = result;
            }
        })
        score
    }

    private def getMaxScore(state: MachineState): Int = {
        if (getStateMachine().isTerminal(state)) {
            getStateMachine().getGoal(state, me)
        } else {
            val moves = getStateMachine().getLegalMoves(state, me)
            var score = 0
            moves.foreach(move => {
                val result = getMinScore(move, state)
                if (result > score) {
                    score = result;
                }
            })
            score
        }
    }

    private def outMoveScore(move: Move, result: Integer) {
        println(s"move is $move result is $result")
    }
}
