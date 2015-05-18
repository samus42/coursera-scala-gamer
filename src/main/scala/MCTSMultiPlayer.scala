import java.util

import org.ggp.base.util.statemachine.{MachineState, Move, Role}

import scala.collection.JavaConversions._
import scala.util.Random
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
class MCTSMultiPlayer extends NotifyingPlayer {
    var me: Role = _
    var roles: List[Role] = _

    case class GameNode(currentState: MachineState, currentPlayer: Role, move: Move = null, var visits: Int = 0, parent: GameNode = null) {
        def findNextRole = roles.find(_ != currentPlayer).headOption.getOrElse(currentPlayer)

        private var cachedChildren: Option[Seq[GameNode]] = None

        def expand = {
            cachedChildren match {
                case Some(c) => c
                case None =>
                    val nextRole = findNextRole
                    if (getStateMachine.isTerminal(currentState)) {
                        //                        println("TERMINAL STATE EXPANSION")
                        cachedChildren = Some(Nil)
                    } else {
                        //                    println(s"currentPlayer: $currentPlayer nextPlayer: $nextRole")
                        cachedChildren = Some(getStateMachine.getLegalJointMoves(currentState).map {
                            moves =>
                                val nextState = getStateMachine.getNextState(currentState, moves)
                                GameNode(nextState, nextRole, moves.head, 0, this)
                        })
                    }
                    cachedChildren.get

            }
        }

        def children = cachedChildren.getOrElse(throw new RuntimeException("Coding issue!"))

        def beenVisited = visits > 0

        ///TODO: N players
        var utilities: Array[Float] = Array(0.0f, 0.0f)


        def getUtility(role: Role) = {
            utilities(roles.indexOf(role))
        }

        def addVisit(scores: Array[Float]) = {
            visits += 1

            //            utility = utility + (score - utility) / visits
            utilities = utilities.zip(scores) map {
                case (u, s) => u + (s - u) / visits
            }
        }

        def isMaxNode = currentPlayer != me

    }

    override def bestmove(role: Role, state: MachineState) = {
        me = role
        roles = getStateMachine.getRoles.toList
        val root = GameNode(state, role)

        val availableMoves: List[Move] = getStateMachine.getLegalMoves(state, role).toList
        println("Available Moves: " + availableMoves)
        val quickDecision = checkForQuickDecision(availableMoves, role, state)
        if (quickDecision.isDefined) {
            quickDecision.get
        } else {
            val playClock = getMatch.getPlayClock * 1000
            val endTime = System.currentTimeMillis() + playClock
            val softEnd = System.currentTimeMillis() + (playClock * .9f).toInt
            //        println(s"Soft End: " + softEnd)
            var cycles = 1
            while (System.currentTimeMillis() < softEnd) {
                //            println(s"Cycle $cycles starting.  Time Left: ${softEnd - System.currentTimeMillis()}")
                val selected = select(root)
                selected.expand
                val scores = simulate(selected)
                backpropagate(selected, scores)
                cycles += 1
            }

            println("Utilities: " + root.children.map(_.getUtility(role)).mkString(","))
            root.children.maxBy(_.getUtility(role)).move
        }
    }

    private def checkForQuickDecision(availableMoves: List[Move], role: Role, state: MachineState): Option[Move] = {
        if (availableMoves.size == 1) availableMoves.headOption
        else {
            val jointMoves = getStateMachine.getLegalJointMoves(state)
            val win = jointMoves.find(m => getStateMachine.getGoal(getStateMachine.getNextState(state, m), role) == 100)
            if (win.isDefined) println("Quick win found!")
            win.map(_.head)
        }
    }

    private def select(node: GameNode): GameNode = {
        if (!node.beenVisited) node
        else if (node.children.isEmpty) node
        else {
            node.children.find(!_.beenVisited).headOption match {
                case Some(n) => n
                case None =>
                    select(node.children.maxBy(n => selectfn(n, node.currentPlayer)))
            }
        }
    }

    private def selectfn(node: GameNode, role: Role) = {
        //        node.avgUtility + 2 * Math.sqrt(2 * Math.log(node.parent.visits.toFloat / node.visits.toFloat))
        node.getUtility(role) + Math.sqrt(2 * Math.log(node.parent.visits.toFloat) / node.visits.toFloat)

    }

    private def monteCarlo(role: Role, state: MachineState, numProbes: Int) = {
        def depthcharge(role: Role, state: MachineState): List[Float] = {
            if (isTerminal(role, state)) {
                roles.map(getStateMachine.getGoal(state, _).toFloat / 100.0f)
            } else {
                val moves = getStateMachine.getRoles.map(r => {
                    val roleMoves = getStateMachine.getLegalMoves(state, r)
                    roleMoves(Random.nextInt(roleMoves.length))
                })
                val newState = getStateMachine.getNextState(state, moves)
                depthcharge(role, newState)
            }
        }


        val resultFutures = for {
            i <- 1 to numProbes
        } yield future { depthcharge(role, state) }

        val results = Await.result(Future.sequence(resultFutures), 10.seconds)

        val combinedResults = results.transpose.map(_.sum)
        combinedResults.map(_ / numProbes.toFloat).toArray
    }

    private def isTerminal(role: Role, state: MachineState) = {
        getStateMachine.isTerminal(state) //|| getStateMachine.getGoal(state, role) == 100
    }

    private def simulate(node: GameNode): Array[Float] = {
        monteCarlo(node.currentPlayer, node.currentState, 4)
    }

    private def backpropagate(node: GameNode, scores: Array[Float]): Unit = {
        node.addVisit(scores)
        if (node.parent != null) {
            backpropagate(node.parent, scores)
        }
    }
}
