import org.ggp.base.util.statemachine.{MachineState, Move, Role}

import scala.collection.JavaConversions._

class AlphaBetaPlayer extends NotifyingPlayer {

    override def bestmove(role: Role, state: MachineState) = {
        println(s"Starting bestmove, user: ${role.getName}")
        val actions = getStateMachine.getLegalMoves(state, role)
        actions.maxBy(a => {
            val result = minscore(role, a, state, 0, 100)
            println(s"Move: $a result = $result")
            result
        })
    }

    /**
    function maxscore (role,state,alpha,beta)
     {if (findterminalp(state,game)) {return findreward(role,state,game)};
      var actions = findlegals(role,state,game);
      for (var i=0; i<actions.length; i++)
          {var result = minscore(role,actions[i],state,alpha,beta);
           alpha = max(alpha,result);
           if (alpha>=beta) then {return beta}};
      return alpha}
      */

    def maxscore(role: Role, state: MachineState, alpha: Int, beta: Int): Int = {
        def helper(actions: List[Move], curAlpha: Int): Int = {
            actions match {
                case Nil => curAlpha
                case head :: tail =>
                    val result = minscore(role, head, state, curAlpha, beta)
                    val newAlpha = Math.max(curAlpha, result)
                    if (newAlpha >= beta) {
                        beta
                    } else {
                        helper(tail, newAlpha)
                    }
            }
        }
        if (getStateMachine.isTerminal(state)) {
            getStateMachine.getGoal(state, role)
        } else {
            helper(getStateMachine.getLegalMoves(state, role).toList, alpha)
        }
    }

    /**
    function minscore (role,action,state,alpha,beta)
     {var opponent = findopponent(role,game);
      var actions = findlegals(opponent,state,game);
      for (var i=0; i<actions.length; i++)
          {var move;
           if (role==roles[0]) {move = [action,actions[i]]}
              else {move = [actions[i],action]}
           var newstate = findnext(move,state,game);
           var result = maxscore(role,newstate,alpha,beta);
           beta = min(beta,result);
           if (beta<=alpha) then {return alpha}};
       return beta}
      */

    def minscore(role: Role, action: Move, state: MachineState, alpha: Int, beta: Int): Int = {
        def movesByRole(nextAction: Move) = {
            //            println(s"role: ${role.getName} roles[0]: ${getStateMachine.getRoles.get(0).getName}")
            if (role == getStateMachine.getRoles.get(0)) {
                List(action, nextAction)
            } else {
                List(nextAction, action)
            }
        }
        val opponent = findOpponent(role)
        //        println(s"Opponent for ${role.toString} is ${opponent.toString}")
        val opponentActions = getStateMachine.getLegalMoves(state, opponent)

        def helper(actions: List[Move], curBeta: Int): Int = {
            actions match {
                case Nil => beta
                case head :: tail =>
                    val newState = getStateMachine.getNextState(state, movesByRole(head))
                    val result = maxscore(role, newState, alpha, curBeta)
                    val newBeta = Math.min(curBeta, result)
                    if (newBeta <= alpha) {
                        alpha
                    } else {
                        helper(tail, newBeta)
                    }
            }
        }

        helper(opponentActions.toList, beta)
    }

    private def findOpponent(role: Role): Role = getStateMachine.getRoles.filter(_.getName != role.getName).head

}
