import java.util.*;

/// Sample AI module that picks random moves at each point.
/**
 * This AI performs random actions.  It is meant to illustrate the basics of how to
 * use the AIModule and GameStateModule classes.
 *
 * Since this terminates in under a millisecond, there is no reason to check for
 * the terminate flag.  However, your AI needs to check for the terminate flag.
 *
 * @author Leonid Shamis
 */
public class RandomAI extends AIModule
{
	public void getNextMove(final GameStateModule game)
	{
		final Random r = new Random();
		// set chosenMove to a random column
		int move = r.nextInt(game.getWidth());
		while(!game.canMakeMove(move))
			move = r.nextInt(game.getWidth());
		chosenMove = move;
	}
}