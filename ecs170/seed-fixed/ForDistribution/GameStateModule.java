import java.awt.Point;

/// Base class for all AI players.
/**
 * This interface defines the functionality for any class which tries to define
 * the state and legal actions of the Connect Four game.
 *
 * Derived classes are responsible for maintaining the state of the game: keeping track
 * of the positions of the coins, the current player, and who has won or lost the
 * game.  It also allows you to check whether given moves are legal and to make
 * and unmake moves.
 *
 * The board is set up such that (0, 0) is the bottom-left corner of the board and
 * (width - 1, height - 1) is the upper-right corner of the board.
 *
 * @author Leonid Shamis
 */
public interface GameStateModule
{
	/// Creates a deep copy of the GameState.
    public GameStateModule copy();
	/// Verifies that a move in column x is legal.
	/**
	 * @param x Column to check.
	 * @return Whether or not the move is legal.
	 */
    public boolean canMakeMove(final int x);
	/// Drops a coin into column x for the active player.
	/**
	 * @param x Column to drop the coin.
	 * @throws java.lang.RuntimeException On illegal move.
	 */
    public void makeMove(final int x) throws RuntimeException;
	/// Undo the last played move.
    public void unMakeMove();
	/// Checks if the game has come to an end.
    public boolean isGameOver();
	/// Return the winner of the current game.
	/**
	 * @return The winner of the current game.
	 * @throws java.lang.RuntimeException If game has not ended.
	 */
    public int getWinner() throws RuntimeException;
	/// Gets the ID of the Active Player {1, 2}
    public int getActivePlayer();
	/// Retrieves the occupancy of the selected tile.
	/**
	 * @param x The selected column.
	 * @param y The selected row.
	 * @return The ID of the player whose coin occupies the selected tile, or 0
	 * if the selected tile is empty.
	 */
    public int getAt(final int x, final int y);
	/// Determines the row at which a coin would fall to in column x.
	/**
	 * @param x The selected column.
	 * @return The row that a coin would land in if dropped in the provided column.
	 */
    public int getHeightAt(final int x);
	/// The width of the board.
    public int getWidth();
	/// The height of the board.
    public int getHeight();
	/// The number of coins that have been dropped so far.
    public int getCoins();
	/// The start point of the winning four tiles.
	/**
	 * Note: This is for graphical purposes only. Do not call this function.
	 */
    public Point getStartPt();
	/// The end point of the winning four tiles.
	/**
	 * Note: This is for graphical purposes only. Do not call this function.
	 */
    public Point getEndPt();
}