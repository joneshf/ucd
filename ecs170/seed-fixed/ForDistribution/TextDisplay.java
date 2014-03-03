import java.io.*;

/// Simplified IO system that uses the console.

/**
 * An IO module that uses the console for input and output.  You will not need to
 * modify the contents of this class.  You can use this IO module by running the
 * Connect Four program with the -t command-line switch.
 * 
 * @author Leonid Shamis
 */
public class TextDisplay implements IOModule
{
    /// Request a move from a human player.
    public int getHumanMove()
    {
        try
        {
            System.out.print("Enter Column #: ");
            BufferedReader br = new BufferedReader(new InputStreamReader(System.in));
            return Integer.parseInt(br.readLine());
        }
        catch(Exception e)
        {
            return -1;
        }
    }

    /// Call to update the graphics.
    /**
     * @param game State of the game to draw.
     */
    public void drawBoard(final GameStateModule game)
    {
        System.out.println();
        for(int y = game.getHeight() - 1; y >= 0; y--)
        {
            for(int x = 0; x < game.getWidth(); x++)
            {
                switch(game.getAt(x, y))
                {
                    case 0: System.out.print("."); break;
                    case 1: System.out.print("x"); break;
                    case 2: System.out.print("o"); break;
                }
            }
            System.out.println();
        }
    }
}
