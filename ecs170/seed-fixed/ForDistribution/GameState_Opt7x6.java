import java.awt.Point;

/// Representation of a game of Connect Four optimized for the traditional 7x6 board size.
/**
 * This class uses two longs as bitboards for the respective players such that the
 * bits are represented in the following manner:
 *
 * ................... -= TOP		\n
 * 5.12.19.26.33.40.47				\n
 * 4.11.18.25.32.39.46				\n
 * 3.10.17.24.31.38.45				\n
 * 2..9.16.23.30.37.44				\n
 * 1..8.15.22.29.36.43				\n
 * 0..7.14.21.28.35.42 -= BOTTOM	\n
 *
 * A bit is set to one it the player has a coin in that position.
 *
 * @see GameStateModule
 * @author Leonid Shamis
 */
public final class GameState_Opt7x6 implements GameStateModule
{
	private final static int WIDTH = 7;
	private final static int HEIGHT = 6;
	/// Amount to shift the bitboard to move over one column.
	private final static int H1 = HEIGHT + 1;
	/// Amount to shift the bitboard to move over one column and up one row.
	private final static int H2 = HEIGHT + 2;
	private final static int SIZE = HEIGHT * WIDTH;
	/// Bitboard with just the above-top row filled in.
	private final static long TOP = 0x1020408102040L;
	/// Player bitboards.
	private final long color[] = new long[2];
	/// History of plays.
	private final int moves[] = new int[SIZE];
	/// Number of moves executed.
	private int nplies = 0;
	private int coins = 0;
	/// Holds the bit index of lowest free tile in a given column.
	private final byte height[] = new byte[WIDTH];
	private boolean gameOver = false;
	private int Winner;
	private boolean pointsComputed = false;
	private Point startPt;
	private Point endPt;

	/// Primary Constructor.
	/**
	 * Creates a new game board of the specified width and height.  The starting player
	 * is player 1 and the board is initially empty and with no undo/redo history.
	 * Width and Height are preset.
	 */
	public GameState_Opt7x6()
	{
		color[0] = color[1] = 0L;
		for(int i = 0; i < WIDTH; i++)
			height[i] = (byte) (H1 * i);
	}

	/// Creates a deep copy of this.
	public GameState_Opt7x6 copy()
	{
		final GameState_Opt7x6 game = new GameState_Opt7x6();
		System.arraycopy(color, 0, game.color, 0, 2);
		System.arraycopy(moves, 0, game.moves, 0, SIZE);
		System.arraycopy(height, 0, game.height, 0, WIDTH);
		game.nplies = nplies;
		game.coins = coins;
		game.gameOver = gameOver;
		game.Winner = Winner;
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
		if(x < 0 || x >= WIDTH || gameOver)
			return false;
		final long newBoard = color[nplies & 1] | (1L << height[x]);
		return (newBoard & TOP) == 0;
	}

	/// Used internally to check for victory.
	/**
	 * @param board Bitboard from one of the players.
	 * @return Determines if a player has won the game.
	 */
	private boolean computeVictory(final long board)
	{
		long temp = board & (board >> HEIGHT);
		if((temp & (temp >> 2 * HEIGHT)) != 0) // check diagonal \
			return true;
		temp = board & (board >> H1);
		if((temp & (temp >> 2 * H1)) != 0) // check horizontal -
			return true;
		temp = board & (board >> H2); // check diagonal /
		if((temp & (temp >> 2 * H2)) != 0)
			return true;
		temp = board & (board >> 1); // check vertical |
		return (temp & (temp >> 2)) != 0;
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
		final int n = moves[--nplies];
		color[nplies & 1] ^= 1L << --height[n];
		coins--;
		gameOver = false;
		pointsComputed = false;
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
	public void makeMove(final int x)
	{
		if(!canMakeMove(x))
			throw new RuntimeException("Illegal Move: " + x);
		color[nplies & 1] |= 1L << height[x]++;
		++coins;
		if(computeVictory(color[nplies & 1]))
		{
			gameOver = true;
			Winner = (nplies & 1) + 1;
		}
		else if(coins == SIZE)
		{
			gameOver = true;
			Winner = 0;
		}
		moves[nplies++] = x;
	}

	/// Check if there exists a victory condition.
	/**
	 * @return Whether or not the game is over.
	 */
	public boolean isGameOver()
	{
		return gameOver;
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
	public int getWinner()
	{
		if(!gameOver)
			throw new RuntimeException("Cannot get winner; game isn't over.");
		return Winner;
	}

	/// Returns the index of the active player.
	/**
	 * @return The 1-based index of the active player.
	 * @throws RuntimeException If the game is over.
	 */
	public int getActivePlayer()
	{
		return (nplies & 1) + 1;
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
		final long bit = 1L << (x * H1 + y);
		if((color[0] & bit) != 0)
			return 1;
		if((color[1] & bit) != 0)
			return 2;
		return 0;
	}

	/// Returns the height of the given column.
	/**
	 * @param x The x-index of the column to look up.
	 * @return The height of that column.
	 */
	public int getHeightAt(final int x)
	{
		return height[x] - (x * H1);
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
		return coins;
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
		if(!gameOver)
			throw new RuntimeException("Cannot get start/end points until the game is over");
		if(!pointsComputed || startPt == null)
			computePoints();
		if(startPt == null)
			throw new RuntimeException("Could not compute points");
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
		if(!gameOver)
			throw new RuntimeException("Cannot get start/end points until the game is over");
		if(!pointsComputed || endPt == null)
			computePoints();
		if(endPt == null)
			throw new RuntimeException("Could not compute points");
		return endPt;
	}

	/// Used internally to find the start and end coordinates of the victory.
	private void computePoints()
	{
		if(Winner == 0)
		{
			startPt = new Point(-1, -1);
			endPt = new Point(-1, -1);
		}

		for(int x = 0; x < WIDTH; x++)
			for(int y = 0; y < HEIGHT; y++)
			{
				if(getAt(x, y) != Winner)
					continue;
				for(int dx = -1; dx <= 1; dx++)
					next:
					for(int dy = -1; dy <= 1; dy++)
					{
						if(dx == dy && dy == 0)
							continue;
						if(x + (3 * dx) >= WIDTH)
							continue;
						if(y + (3 * dy) >= HEIGHT)
							continue;
						if(x + (3 * dx) < 0)
							continue;
						if(y + (3 * dy) < 0)
							continue;
						for(int i = 0; i < 4; i++)
							if(getAt(x + (i * dx), y + (i * dy)) != Winner)
								continue next;
						startPt = new Point(x, y);
						endPt = new Point(x + (3 * dx), y + (3 * dy));
						return;
					}
			}
	}
}
