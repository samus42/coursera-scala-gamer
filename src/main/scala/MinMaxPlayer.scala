import org.ggp.base.util.statemachine.{MachineState, Move, Role}

import scala.collection.JavaConversions._

class MinMaxPlayer extends NotifyingPlayer {
    /**
     * function bestmove (role,state)
          {var actions = findlegals(role,state,game);
           var action = actions[0];
           var score = 0;
           for (var i=0; i<actions.length; i++)
               {var result = minscore(role,actions[i],state);
                if (result>score) {score = result; action = actions[i]}};
           return action}
     */


    override def bestmove(role: Role, state: MachineState) = {
        def showScore(action: Move) = {
            val result = minscore(role, action, state)
            println(s"Move: $action result = $result")
            result
        }
        val actions = getStateMachine.getLegalMoves(state, role)
        actions.maxBy(showScore)
    }

    /**
     * function minscore (role,action,state)
      {var opponent = findopponent(role,game);
       var actions = findlegals(opponent,state,game);
       var score = 100;
       for (var i=0; i<actions.length; i++)
           {var move;
            if (role==roles[0]) {move = [action,actions[i]]}
               else {move = [actions[i],action]}
            var newstate = findnext(move,state,game);
            var result = maxscore(role,newstate);
            if (result<score) {score = result}};
       return score}
     */
    private def minscore(role: Role, action: Move, state: MachineState) : Int = {

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
            maxscore(role, newState)
        })
        scores.min
    }

    private def findOpponent(role: Role): Role = getStateMachine.getRoles.filter(_.getName != role.getName).head

    /**
     * function maxscore (role,state)
      {if (findterminalp(state,game)) {return findreward(role,state,game)};
       var actions = findlegals(role,state,game);
       var score = 0;
       for (var i=0; i<actions.length; i++)
           {var result = minscore(role,actions[i],state);
            if (result>score) {score = result}};
       return score}
     */
    private def maxscore(role: Role, state: MachineState): Int = {
        if(getStateMachine.isTerminal(state)) {
            getStateMachine.getGoal(state, role)
        } else {
            val actions = getStateMachine.getLegalMoves(state, role)
            actions.map(a => minscore(role, a, state)).max
        }
    }
}
