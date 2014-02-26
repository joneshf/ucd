/// Base class for all I/O modules.
/**
 * This interface acts as the root interface for any possible devices that can be used
 * for game input and output.  You will not need to implement this interface in your solution.
 * 
 * @author Leonid Shamis
 */
public interface IOModule
{
    /// Request a move from a human player.
    public int getHumanMove();
    
    /// Call to update the graphics.
    /**
     * @param game State of the game to draw.
     */
    public void drawBoard(GameStateModule game);
}
