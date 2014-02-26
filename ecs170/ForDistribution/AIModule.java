/// Base class for all AI players.
/**
 * AIModule is the base class for all AI players.  To create a new AI class,
 * simply extend AIModule and override the getNextMove method.
 * 
 * getNextMove will be invoked by a thread that will run for a specified period
 * of time before having the terminate flag set by the GameController.  Once the
 * terminate flag is set, the getNextMove function will be given a short amount
 * of time to exit.  If it does not, a Exception will be invoked.
 * 
 * @author Leonid Shamis
 * @see RandomAI, StupidAI, MonteCarloAI
 */
public abstract class AIModule
{
	/// Flag that is set when getNextMove should exit.
	/**
	 * The GameController will set this flag to false before running the getNextMove
	 * function, then to true when the alloted time has come up.  Once this flag is
	 * set, the getNextMove function will have a short amount of time to set chosenMove
	 * and return.
	 *
	 * @see getNextMove
	 */
	public volatile boolean terminate = true;
	/// Move to be played when getNextMove finishes or times out.
	/**
	 * The GameController will read the value of this field after running the
	 * getNextMove function for a period of time.  getNextMove should set this value
	 * before it returns.
	 *
	 * @see getNextMove
	 */
	public int chosenMove = -1;

	/// Determines the best move to be played for the active player.
	/**
	 * This function is invoked by the GameController and is responsible for filling
	 * in the chosenMove field with the column the computer should play as its next
	 * move.  The GameController will run this function for a set period of time
	 * (customizable through the command line) and will then set the terminate flag.
	 * At that point, the function should set chosenMove and return.
	 *
	 * @param game Current state of the game.
	 * @see terminate
	 * @see chosenMove
	 */
	public abstract void getNextMove(final GameStateModule game);
}