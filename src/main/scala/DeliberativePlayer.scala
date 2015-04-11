import org.ggp.base.player.gamer.event.GamerSelectedMoveEvent
import org.ggp.base.player.gamer.statemachine.sample.SampleGamer
import org.ggp.base.util.statemachine.{Move, Role, MachineState}

import scala.collection.JavaConversions._

class DeliberativePlayer extends SampleGamer {
    override def stateMachineSelectMove(timeout: Long) = {
        val start = System.currentTimeMillis()



        val selection = bestmove(getRole, getCurrentState)
        val stop = System.currentTimeMillis()

        notifyObservers(new GamerSelectedMoveEvent(getStateMachine.getLegalMoves(getCurrentState, getRole), selection, stop - start))

        selection
    }

    /**
     * function bestmove (role,state)
      {var actions = findlegals(role,state,game);
       var action = actions[0];
       var score = 0;
       for (var i=0; i<actions.length; i++)
           {var result = maxscore(role,simulate([actions[i]],state));
            if (result==100) {return actions[i]};
            if (result>score) {score = result; action = actions[i]}};
       return action}
     */
    private def bestmove(role: Role, state: MachineState) : Move = {
        val actions = getStateMachine.getLegalMoves(state, role)
        actions.maxBy(a => maxscore(role, getStateMachine.getNextState(state, List(a))))
    }

    /**
     *
     function maxscore (role,state)
      {if (findterminalp(state,game))
          {return findreward(role,state,game)};
       var actions = findlegals(role,state,game);
       var score = 0;
       for (var i=0; i<actions.length; i++)
           {var result = maxscore(role,simulate([actions[i]],state));
            if (result>score) {score = result}};
       return score}
     */
    private def maxscore(role: Role, state: MachineState): Int = {
        if(getStateMachine.isTerminal(state) || getStateMachine.getGoal(state, role) == 100) {
            getStateMachine.getGoal(state, role)
        } else {
            val actions = getStateMachine.getLegalMoves(state, role)
            actions.map(m => maxscore(role, getStateMachine.getNextState(state, List(m)))).max
        }
    }
}
