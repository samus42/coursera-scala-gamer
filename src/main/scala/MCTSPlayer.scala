import org.ggp.base.util.statemachine.{MachineState, Move, Role}

import scala.collection.JavaConversions._
import scala.util.Random

class MCTSPlayer extends NotifyingPlayer {

    case class GameNode(currentState: MachineState, currentPlayer: Role, move: Move = null, var visits: Int = 0, parent: GameNode = null) {
        private var cachedChildren: Option[Seq[GameNode]] = None

        def expand = {
            cachedChildren match {
                case Some(c) => c
                case None =>
                    cachedChildren = Some(getStateMachine.getLegalMoves(currentState, currentPlayer).map {
                        move =>
                            val nextState = getStateMachine.getNextState(currentState, List(move))
                            GameNode(nextState, currentPlayer, move, 0, this)
                    })
                    cachedChildren.get
            }
        }

        def children = cachedChildren.getOrElse(Nil)

        def beenVisited = visits > 0

        var utility: Int = 0

        def addVisit() = visits = visits + 1

        def addUtility(score: Int) = utility += score
    }

    override def bestmove(role: Role, state: MachineState) = {
        val root = GameNode(state, role)

        val playClock = getMatch.getPlayClock * 1000
        val endTime = System.currentTimeMillis() + playClock
        val softEnd = System.currentTimeMillis() + (playClock * .8f).toInt
        println(s"Soft End: " + softEnd)
        var cycles = 1
        while (System.currentTimeMillis() < softEnd) {
//            println(s"Cycle $cycles starting.  Time Left: ${softEnd - System.currentTimeMillis()}")
            val selected = select(root)
            selected.expand
            val score = simulate(selected)
            backpropagate(selected, score)
            cycles += 1
        }

        root.children.maxBy(_.utility).move
    }

    private def select(node: GameNode): GameNode = {
        if (!node.beenVisited) node
        else {
            node.children.find(!_.beenVisited).headOption match {
                case Some(n) => n
                case None =>
                    select(node.children.maxBy(selectfn))
            }
        }
    }

    private def selectfn(node: GameNode) = {
        node.utility + Math.sqrt(2 * Math.log(node.parent.visits.toFloat / node.visits.toFloat))
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

    private def isTerminal(role: Role, state: MachineState) = {
        getStateMachine.isTerminal(state) || getStateMachine.getGoal(state, role) == 100
    }

    private def simulate(node: GameNode): Int = {
        monteCarlo(node.currentPlayer, node.currentState, 4)
    }

    private def backpropagate(node: GameNode, score: Int): Unit = {
        node.addVisit()
        node.addUtility(score)
        if (node.parent != null) {
            backpropagate(node.parent, score)
        }
    }
}
