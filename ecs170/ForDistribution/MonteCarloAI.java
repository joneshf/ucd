import java.util.Random;
/// A sample AI that uses a Monte Carlo approach to play Connect Four.

/**
 * A sampleAI that uses a Monte Carlo approach to play Connect Four.  Unlike the heuristic
 * searches and minimax approaches we've covered in class, the Monte Carlo player plays
 * Connect Four by simulating purely random games and choosing the move that has the highest
 * expected outcome.  Since the Monte Carlo player plays moves randomly,
 * it does not always play the optimal move (see if you can convince yourself about why this is),
 * but is good at strategic play and likes to make threats.
 * 
 * Unlike StupidAI and RandomAI, this AI player's getNextMove function will continues to play
 * random games indefinitely until the terminate flag is set.
 * 
 * @author Leonid Shamis
 */
public class MonteCarloAI extends AIModule
{
	/// Random number generator to play random games.
	private final Random r = new Random(System.currentTimeMillis());
	/// Used as a helper when picking random moves.
	private int[] moves;

	/// Simulates random games and chooses the move that leads to the highest expected value.
	@Override
	public void getNextMove(final GameStateModule state)
	{
		// Set up the legal moves buffer.  We use only one buffer repeatedly to
		// avoid needless memory allocations.
		moves = new int[state.getWidth()];

		// Default to choosing the first column (should be fixed after a few rounds)
		chosenMove = 0;

		// Cache our index.
		final int ourPlayer = state.getActivePlayer();

		// Create value array and set all illegal moves to minimum value.
		// This will be filled in using results from random games:
		// +1 point for each win
		// +0 point for each draw
		// -1 point for each loss.
		// We also initialize all illegal moves to -Integer.MAX_VALUE.  We could also
		// have used Integer.MIN_VALUE, but this is a "weird number" because
		// -Integer.MIN_VALUE == Integer.MIN_VALUE.
		int[] values = new int[state.getWidth()];
		for(int i = 0; i < values.length; ++i)
			if(!state.canMakeMove(i))
				values[i] = -Integer.MAX_VALUE;

		// Start simulating games! Continue until told to stop.
		while(!terminate)
		{
			final int move = getMove(state);
			state.makeMove(move);
			updateGuess(ourPlayer, playRandomGame(state), values, move);
			state.unMakeMove();
		}
	}

	/// Returns a random legal move in a given state.
	/**
	 * Given a game state, returns the index of a column that is a legal move.
	 *
	 * @param state The state in which to get a legal move.
	 * @return A random legal column to drop a coin in.
	 */
	private int getMove(final GameStateModule state)
	{
		// Fill in what moves are legal.
		int numLegalMoves = 0;
		for(int i = 0; i < state.getWidth(); ++i)
			if(state.canMakeMove(i))
				moves[numLegalMoves++] = i;

		// Pick one randomly.
		final int n = r.nextInt(numLegalMoves);
		return moves[n];
	}

	// Given the result of the last game, update our chosen move.
	/**
	 * After simulating a game, updates the array containing all of the expected values
	 * and updates the chosen move to reflect the move with the highest positive expectation
	 * value.
	 *
	 * @param ourPlayer The index of the player representing us.
	 * @param result The result of the last game (0 for draw, 1 for player 1 win, etc.)
	 * @param values The array of expected values.
	 * @param move The move played that led to this outcome.
	 */
	private void updateGuess(final int ourPlayer, final int result, int[] values, int move)
	{
		// On a draw, we can skip making changes.
		if(result == 0)
			return;

		// Update the expected value of this move depending on whether we win or lose.
		values[move] += (result == ourPlayer ? 1 : -1);

		// Update the move to be the best known move.  This is necessary since we need
		// to have the best move available at all times because we run forever.
		for(int i = 0; i < values.length; ++i)
			if(values[i] > values[chosenMove])
				chosenMove = i;
	}

	/// Given a game, plays it through to the end using random moves.
	/**
	 * Given a game state, chooses a sequence of random moves until the end of the game
	 * and returns the result of the game.  The input state is not modified.
	 *
	 * @param state The state from which to play.
	 * @return The result of the game as dictated by GameStateModule.getWinner
	 * @see GameStateModule.getWinner
	 */
	private int playRandomGame(final GameStateModule state)
	{
		// Duplicate the state to prevent changes from propagating.
		final GameStateModule game = state.copy();
		while(!game.isGameOver())
			game.makeMove(getMove(game));

		// It's over!  Return who won.
		return game.getWinner();
	}
}
