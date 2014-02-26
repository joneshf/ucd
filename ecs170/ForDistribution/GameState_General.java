import java.awt.Point;
import java.util.*;

/// Representation of a game of Connect Four to be used if an optimized version cannot be found.
/**
 * @see GameStateModule
 * @author Leonid Shamis
 */
public final class GameState_General implements GameStateModule
{
	/// Keeps track of the current player.
	private int ActivePlayer;
	/// Keeps track of the contents of all tiles. 0 represents empty.
	private int[][] Board;
	/// Keeps track of the number of filled items in each colomn.
	private int[] Heights;
	/// The player that was declared the victor of the game.
	private int Winner;
	/// Used by the graphics module for drawing victory states.  You will not need to use this.
	private Point startPt;
	/// Used by the graphics module for drawing victory states.  You will not need to use this.
	private Point endPt;
	/// Width of the board
	private int WIDTH;
	/// Height of the board
	private int HEIGHT;
	/// Game history for makeMove / unMakeMove
	/**
	 * This stack is used to keep track of the move history for makeMove and unMakeMove.
	 */
	private final Stack<Integer> history = new Stack<Integer>();
	/// The number of coins on the board.
	private int Coins;

	/// Primary Constructor.
	/**
	 * Creates a new game board of the specified width and height.  The starting player
	 * is player 1 and the board is initially empty and with no undo/redo history.
	 *
	 * @param w The width of the board.
	 * @param h The height of the board.
	 */
	public GameState_General(final int w, final int h)
	{
		ActivePlayer = 1;
		Winner = -1;
		WIDTH = w;
		HEIGHT = h;
		Coins = 0;
		Board = new int[WIDTH][HEIGHT];
		Heights = new int[WIDTH];
		for(int i = 0; i < WIDTH; i++)
			Arrays.fill(Board[i], 0);
		Arrays.fill(Heights, 0);
	}

	/// Creates a deep copy of this.
	public GameState_General copy()
	{
		final GameState_General game = new GameState_General(WIDTH, HEIGHT);
		game.ActivePlayer = ActivePlayer;
		game.Winner = Winner;
		game.Coins = Coins;
		for(int i = 0; i < WIDTH; i++)
			System.arraycopy(Board[i], 0, game.Board[i], 0, Board[i].length);
		System.arraycopy(Heights, 0, game.Heights, 0, Heights.length);
		return game;
	}

	/// Returns whether a move is legal.
	/**
	 * Returns whether it is legal to drop a coin in the specified column.
	 *
	 * @param x The column to test.
	 * @return Whether it is legal to drop a coin in this column.
	 */
	public boolean canMakeMove(final int x)
	{
		return x >= 0 && x < WIDTH && Heights[x] < HEIGHT && Winner == -1;
	}

	/// Makes the specified move for the active player.
	/**
	 * Makes the specified move for the active player.  This updates the game
	 * history and can be undone with unMakeMove.  If the move is illegal,
	 * throws a RuntimeException exception.
	 *
	 * @param x The move to be made.
	 * @throws RuntimeException If the move is illegal.
	 * @see unMakeMove
	 */
	public void makeMove(final int x) throws RuntimeException
	{
		// If the move is out of range, then the move is invalid
		if(!canMakeMove(x))
			throw new RuntimeException("Illegal Move: " + x);

		// Update the Board
		Board[x][Heights[x]] = getActivePlayer();
		// Update the Heights
		Heights[x]++;
		// Update the Player
		ActivePlayer = (ActivePlayer == 1) ? 2 : 1;

		Coins++;

		// Keep track of the history of this move.
		history.push(x);

		computeVictory();
	}

	/// Undoes the most recent action.
	/**
	 * Undoes the last action performed by makeMove.  This function can be called
	 * multiple successive times, but no more than the number of times that makeMove
	 * has been called.
	 *
	 * @see makeMove
	 */
	public void unMakeMove()
	{
		final int x = history.pop();
		Coins--;
		// Switch ActivePlayer
		ActivePlayer = (ActivePlayer == 1) ? 2 : 1;

		// Update Board
		Heights[x]--;
		Board[x][Heights[x]] = 0;
		Winner = -1;
	}

	/// Check if there exists a victory condition.
	/**
	 * @return Whether or not the game is over.
	 */
	public boolean isGameOver()
	{
		return Winner != -1;
	}

	/// Return the winner of the current game.
	/**
	 * Returns the index of the winning player.  If player 1 wins, the return
	 * value is 1.  If player 2 wins, the return value is 2.  On a draw, the
	 * return value is 0.  If this function is called and the game is not over,
	 * throws a RuntimeException.
	 *
	 * @return The index of the winning player.
	 * @throws RuntimeException If the game is not over.
	 * @see isGameOver
	 */
	public int getWinner() throws RuntimeException
	{
		if(!isGameOver())
			throw new RuntimeException("Cannot get winner; game isn't over.");
		return Winner;
	}

	/// Used internally to check for victory.
	/**
	 * Determines if a player has won the game and updates the state accordingly.
	 */
	private void computeVictory()
	{
		// for each column
		for(int i = 0; i < WIDTH; i++)
			// for each row
			for(int j = 0; j < HEIGHT; j++)
				// if not empty
				if(Board[i][j] != 0)
					for(int dx = -1; dx <= 1; dx++)
						for(int dy = -1; dy <= 1; dy++)
						{
							if(dx == 0 && dy == 0)
								continue;

							// If extent is out of bounds, abort.
							if(i + 3 * dx < 0 || i + 3 * dx >= WIDTH || j + 3 * dy < 0 || j + 3 * dy >= HEIGHT)
								continue;

							boolean victory = true;
							for(int step = 1; step <= 3; step++)
								if(Board[i][j] != Board[i + step * dx][j + step * dy])
								{
									victory = false;
									break;
								}

							if(victory)
							{
								startPt = new Point(i, j);
								endPt = new Point(i + 3 * dx, j + 3 * dy);
								Winner = Board[i][j];
								return;
							}
						}

		if(Coins == WIDTH * HEIGHT)
		{
			Winner = 0;
			return;
		}
	}

	/// Returns the index of the active player.
	/**
	 * @return The 1-based index of the active player.
	 * @throws RuntimeException If the game is over.
	 */
	public int getActivePlayer()
	{
		if(isGameOver())
			throw new RuntimeException("Game is over; there is no active player.");
		return ActivePlayer;
	}

	/// Returns what coin is at the given location.
	/**
	 * Returns the value of the coin at position (x, y).  If there is no coin, the
	 * return value is 0; otherwise it is the 1-based index of the player who placed the
	 * coin there.
	 *
	 * @param x The x coordinate to look up.
	 * @param y The y coordinate to look up.
	 * @return The value of the coin at the given position, or 0 if not present.
	 */
	public int getAt(final int x, final int y)
	{
		return Board[x][y];
	}

	/// Returns the height of the given column.
	/**
	 * @param x The x-index of the column to look up.
	 * @return The height of that column.
	 */
	public int getHeightAt(final int x)
	{
		return Heights[x];
	}

	/// Returns the width of the board.
	/**
	 * @return The width of the board.
	 */
	public int getWidth()
	{
		return WIDTH;
	}

	/// Returns the height of the board.
	/**
	 * @return The height of the board.
	 */
	public int getHeight()
	{
		return HEIGHT;
	}

	/// Returns the number of filled slots
	/**
	 * @return The number of filled slots
	 */
	public int getCoins()
	{
		return Coins;
	}

	/// Used by the graphics module.
	/**
	 * This function is used by the graphics module.  You will not need to use this
	 * function.
	 *
	 * @return For internal use only.
	 */
	public Point getStartPt()
	{
		return startPt;
	}

	/// Used by the graphics module.
	/**
	 * This function is used by the graphics module.  You will not need to use this
	 * function.
	 *
	 * @return For internal use only.
	 */
	public Point getEndPt()
	{
		return endPt;
	}
}
