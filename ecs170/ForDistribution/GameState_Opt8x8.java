import java.awt.Point;
import java.util.Arrays;

/// Representation of a game of Connect Four optimized for the 8x8 board size on which you will be competing.
/**
 * This class uses two longs as bitboards for the respective players such that the
 * bits are represented in the following manner:
 *
 * 7..15..23..31..39..47..55..63 -= TOP		\n
 * 6..14..22..30..38..46..54..62			\n
 * 5..13..21..29..37..45..53..61			\n
 * 4..12..20..28..36..44..52..60			\n
 * 3..11..19..27..35..43..51..59			\n
 * 2..10..18..26..34..42..50..58			\n
 * 1...9..17..25..33..41..49..57			\n
 * 0...8..16..24..32..40..48..56 -= BOTTOM	\n
 *
 * A bit is set to one it the player has a coin in that position.
 *
 * @see GameStateModule
 * @author Leonid Shamis
 */
public class GameState_Opt8x8 implements GameStateModule
{
	private final static int WIDTH = 8;
	private final static int HEIGHT = 8;
	/// Amount to shift the bitboard to move over one column and up one row.
	private final static int FD = HEIGHT + 1;
	/// Amount to shift the bitboard to move back one column and up one row.
	private final static int BD = HEIGHT - 1;
	private final static int SIZE = HEIGHT * WIDTH;
	/// Bitboard completely filled minus the top row.
	private final static long BUT_TOP = 0x7F7F7F7F7F7F7F7FL;
	/// Player bitboards.
	private final long color[] = new long[2];
	/// History of plays.
	private final byte moves[] = new byte[SIZE];
	/// Number of moves executed.
	private int nplies = 0;
	private int coins = 0;
	/// The height at any column.
	private final byte Heights[] = new byte[WIDTH];
	private boolean gameOver = false;
	private int Winner = -1;
	private boolean pointsComputed = false;
	private Point startPoint;
	private Point endPoint;

	/// Primary Constructor.
	/**
	 * Creates a new game board of the specified width and height.  The starting player
	 * is player 1 and the board is initially empty and with no undo/redo history.
	 * Width and Height are preset.
	 */
	public GameState_Opt8x8()
	{
		Arrays.fill(color, 0L);
		Arrays.fill(moves, (byte)0);
		Arrays.fill(Heights, (byte)0);
	}

	/// Creates a deep copy of this.
	public GameState_Opt8x8 copy()
	{
		final GameState_Opt8x8 game = new GameState_Opt8x8();
		System.arraycopy(color, 0, game.color, 0, 2);
		System.arraycopy(moves, 0, game.moves, 0, SIZE);
		System.arraycopy(Heights, 0, game.Heights, 0, WIDTH);
		game.nplies = nplies;
		game.coins = coins;
		game.gameOver = gameOver;
		game.Winner = Winner;
		return game;
	}

	/// Used internally to check for victory.
	/**
	 * @param board Bitboard from one of the players.
	 * @return Determines if a player has won the game.
	 */
	private boolean computeVictory(final long board)
	{
		// VERTICAL |
		long temp = board;
		temp &= (temp & BUT_TOP) << 1;
		temp &= (temp & BUT_TOP) << 1;
		temp &= (temp & BUT_TOP) << 1;
		if(temp != 0)
			return true;

		// HORIZONTAL --
		temp = board;
		temp &= temp << HEIGHT;
		temp &= temp << HEIGHT;
		temp &= temp << HEIGHT;
		if(temp != 0)
			return true;

		// DIAGONAL /
		temp = board;
		temp &= (temp & BUT_TOP) << FD;
		temp &= (temp & BUT_TOP) << FD;
		temp &= (temp & BUT_TOP) << FD;
		if(temp != 0)
			return true;

		// DIAGONAL \
		temp = board;
		temp &= (temp & BUT_TOP) >> BD;
		temp &= (temp & BUT_TOP) >> BD;
		temp &= (temp & BUT_TOP) >> BD;
		return (temp != 0);
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
		return x >= 0 && x < WIDTH && Heights[x] < HEIGHT && !gameOver;
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
		color[nplies & 1] |= 1L << (x * HEIGHT + Heights[x]);
		++Heights[x];
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
        moves[nplies++] = (byte)x;
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
		final int x = moves[--nplies];
		--Heights[x];
        color[nplies & 1] ^= 1L << (x * HEIGHT + Heights[x]);
        --coins;
        gameOver = false;
        pointsComputed = false;
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
		final long bit = 1L << (x * HEIGHT + y);
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
		if(!pointsComputed || startPoint == null)
			computePoints();
		if(startPoint == null)
			throw new RuntimeException("Could not compute points");
		return startPoint;
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
		if(!pointsComputed || endPoint == null)
			computePoints();
		if(endPoint == null)
			throw new RuntimeException("Could not compute points");
		return endPoint;
	}

	/// Used internally to find the start and end coordinates of the victory.
	private void computePoints()
	{
		if(Winner == 0)
		{
			startPoint = new Point(-1, -1);
			endPoint = new Point(-1, -1);
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
						startPoint = new Point(x, y);
						endPoint = new Point(x + (3 * dx), y + (3 * dy));
						return;
					}
			}
	}
}
