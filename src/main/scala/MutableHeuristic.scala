import org.ggp.base.util.statemachine.{MachineState, Move, Role}

import scala.collection.JavaConversions._
import scala.util.Random

/**
 * based on : https://github.com/ch-ms/ggp-players/blob/master/ChmsMinmaxPlayer.java
 */
class MutableHeuristic extends NotifyingPlayer {
    var me: Role = _
    var opponent: Role = _
    val numMonteCarloCharges = 2

    val maxDepth = 1

    override def bestmove(role: Role, state: MachineState) = {
        me = role
        for (r: Role <- getStateMachine.getRoles) {
            if (!r.equals(me)) {
                opponent = r
            }
        }

        val moves = getStateMachine.getLegalMoves(getCurrentState, me)
        var bestMove = moves(0)
        var score = 0
        moves.foreach(move => {
            val result = getMinScore(move, getCurrentState, 1)
            outMoveScore(move, result)
            if (result > score) {
                score = result
                bestMove = move
            }
        }
        )
        bestMove
    }

    private def getMinScore(action: Move, state: MachineState, level: Int): Int = {
        val moves = getStateMachine.getLegalMoves(state, opponent)
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

            val newState = getStateMachine.getNextState(state, tryMove)
            val result = getMaxScore(newState, level + 1)
            if (result == 0) {
                return 0
            }
            else if (result < score) {
                score = result
            }
        })
        score
    }

    private def getMaxScore(state: MachineState, level: Int): Int = {
        if (getStateMachine.isTerminal(state)) {
            val goalScore = getStateMachine.getGoal(state, me)
            goalScore
        } else if (level >= maxDepth) {
            monteCarlo(me, state, numMonteCarloCharges)
        }
        else {
            val moves = getStateMachine.getLegalMoves(state, me)
            var score = 0
            moves.foreach(move => {
                val result = getMinScore(move, state, level)
                if (result == 100) {
                    println("max returning 100!")
                    return 100
                }
                else if (result > score) {
                    score = result
                }
            })
            score
        }
    }

    private def outMoveScore(move: Move, result: Integer) {
        println(s"move is $move result is $result")
    }


    private def isTerminal(role: Role, state: MachineState) = {
        getStateMachine.isTerminal(state) || getStateMachine.getGoal(state, role) == 100
    }

    private def monteCarlo(role: Role, state: MachineState, numProbes: Int) = {
        def depthcharge(role: Role, state: MachineState): Int = {
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
