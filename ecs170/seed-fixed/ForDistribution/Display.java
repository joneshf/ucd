import java.awt.*;
import java.awt.event.*;
import java.util.concurrent.*;

/// Window class responsible for human IO and graphics display

/**
 * A window class that displays the game and is responsible for performing human
 * IO if necessary.  You should not need to worry about the contents of this
 * class, as none of the methods contained here will be necessary for developing
 * an AI player.
 * 
 * @author Leonid Shamis
 */
public final class Display extends javax.swing.JPanel implements IOModule, MouseListener, MouseMotionListener
{
    /// Subclass to synchronize human player actions.
    /**
     * This class is used to implement the human moves.  It relies on a bit of
     * threading hacking and should not be modified as minor changes can break
     * the human input functions.
     */
    private static final class HumanBlocker
    {
        /// Semaphore used for thread blocking.
        private final Semaphore readySignal = new Semaphore(0);
        
        /// Move chosen by human player.
        private volatile int result;
        
        /// Flag indicating whether or not the human player has chosen a move.
        private volatile boolean moveRetrieved = false;

        /// Sets what the player's move was, then unblocks the thread so it it will complete.
        /**
         * Sets what the player's move was, then unblocks the thread so it it will complete.
         * @param move The move to record.
         */
        public void setMove(int move)
        {
            result = move;
            readySignal.release();
        }

        /// Request a move selection from active human player.
        /**
         * Blocks until the human has chosen a move, then returns that move.  Calling this
         * function more than once per HumanBlocker will cause an IllegalStateException.
         * 
         * @return The move chosen by the human.
         */
        public int getMove()
        {
            // Set that we've retrieved the move.
            synchronized(this)
            {
                if(moveRetrieved)
                {
                    throw new IllegalStateException("Already retrieved stored move!");
                }
                moveRetrieved = true;
            }

            // Wait for the move to be ready
            try
            {
                readySignal.acquire();
            }
            catch(InterruptedException iex)
            {
                throw new RuntimeException("Interrupted a thread... this shouldn't be possible");
            }

            return result;
        }
    }
    
    /// Colors used in the display.
    private static final Color [] coinColors = new Color[]{Color.WHITE, Color.RED, Color.YELLOW};    
    /// Human interaction blocker, to be set on human action request.
    private volatile HumanBlocker currentBlocker = null;
    /// Active game being displayed.
    private GameStateModule game;
    /// Column over which the mouse is located.
    private int MouseX;

    /// Default constructor.
    public Display()
    {
        setBackground(Color.GRAY);
        addMouseListener(this);
        addMouseMotionListener(this);
        setPreferredSize(new Dimension(500, 450));
    }

    /// Set up request for human player chosen move.
    public int getHumanMove()
    {
        final HumanBlocker ourBlocker = new HumanBlocker();
        currentBlocker = ourBlocker;
        this.repaint();
        return ourBlocker.getMove();
    }

    /// Set up pretty anti-aliased graphics.
    private Graphics2D setup2DGraphics(final Graphics g)
    {
        final Graphics2D g2d = (Graphics2D) g;
        g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
        return g2d;
    }

    /// Draw the active game board.
    private void drawBoard(final Graphics2D g2d)
    {
        // Get panel dimentions
        final int width = this.getWidth();
        final int height = this.getHeight();

        // Dimentions of the circles for a game.getWidth()xgame.getWidth() grid
        final double circleWidth = ((double) width) / game.getWidth();
        final double circleHeight = ((double) height) / (1 + game.getHeight());

        // Reduce the dimentions to prevent overlaps
        final int cW = (int) (circleWidth) - 2;
        final int cH = (int) (circleHeight) - 2;

        // for each column
        for(int x = 0; x < game.getWidth(); x++)
        {
            //for each row
            for(int y = 1; y <= game.getHeight(); y++)
            {
                // Location of the current circle
                final int cX = (int) (x * circleWidth) + 1;
                final int cY = (int) (y * circleHeight) + 1;
                
                // Choose the appropriate color
                g2d.setColor(coinColors[game.getAt(x, game.getHeight() - y)]);
                
                // Draw circle
                g2d.fillOval(cX, cY, cW, cH);
            }
        }
    }

    /// Outlines all pieces to make it look nicer.
    private void drawOutlines(final Graphics2D g2d)
    {
        // Setup the brush
        g2d.setStroke(new BasicStroke(2));
        g2d.setColor(Color.BLACK);

        // Get panel dimentions
        final int width = this.getWidth();
        final int height = this.getHeight();

        // Dimentions of the circles for a game.getWidth()xgame.getWidth() grid
        final double circleWidth = ((double) width) / game.getWidth();
        final double circleHeight = ((double) height) / (1 + game.getHeight());

        // Reduce the dimentions to prevent overlaps
        final int cW = (int) (circleWidth) - 2;
        final int cH = (int) (circleHeight) - 2;

        // for each column
        for(int x = 0; x < game.getWidth(); x++)
        {
            // for each row
            for(int y = 1; y <= game.getHeight(); y++)
            {
                // Location of the current circle
                final int cX = (int) (x * circleWidth) + 1;
                final int cY = (int) (y * circleHeight) + 1;

                // Outline circle
                g2d.drawOval(cX, cY, cW, cH);
            }
        }

        // Draw a line between the move selector and the board
        g2d.setStroke(new BasicStroke(3));
        g2d.drawLine(-1, cH - 2, width + 1, cH - 2);
    }

    /// Draw the human player move selector.
    private void drawMove(final Graphics2D g2d)
    {
        // Get dimentions
        final int width = this.getWidth();
        final int height = this.getHeight();

        final double circleWidth = ((double) width) / game.getWidth();
        final double circleHeight = ((double) height) / (1 + game.getHeight());

        final int cX = (int) (MouseX * circleWidth) + game.getHeight();
        final int cY = 1;
        final int cW = (int) (circleWidth) - 12;
        final int cH = (int) (circleHeight) - 12;

        // Get color
        g2d.setColor(coinColors[game.getActivePlayer()]);

        // Draw and outline move
        g2d.fillOval(cX, cY, cW, cH);

        g2d.setStroke(new BasicStroke(2));
        g2d.setColor(Color.BLACK);
        g2d.drawOval(cX, cY, cW, cH);
    }

    /// If a player has won draw a line connecting the four checkers.
    private void drawVictory(final Graphics2D g2d, final Point start, final Point end)
    {
        // Get panel dimentions
        final int width = this.getWidth();
        final int height = this.getHeight();

        // Dimentions of the circles for a game.getWidth()xgame.getWidth() grid
        final double circleWidth = ((double) width) / game.getWidth();
        final double circleHeight = ((double) height) / (1 + game.getHeight());

        // Get the first victory tile
        final int startX = (int) (start.x * circleWidth) + 1 + (int) (circleWidth / 2);
        final int startY = (int) ((game.getHeight() - start.y) * circleHeight) + 1 + (int) (circleHeight / 2);

        // Get the last victory tile
        final int endX = (int) (end.x * circleWidth) + 1 + (int) (circleWidth / 2);
        final int endY = (int) ((game.getHeight() - end.y) * circleHeight) + 1 + (int) (circleHeight / 2);

        // Draw a line through the victory tiles
        g2d.setColor(Color.BLACK);
        g2d.setStroke(new BasicStroke(10, BasicStroke.CAP_ROUND, 0));
        g2d.drawLine(startX, startY, endX, endY);

        g2d.setColor(Color.WHITE);
        g2d.setStroke(new BasicStroke(5, BasicStroke.CAP_ROUND, 0));
        g2d.drawLine(startX, startY, endX, endY);
    }

    /// Instructions for repainting the panel.
    @Override
    public void paintComponent(final Graphics g)
    {
        super.paintComponent(g);
        
        if(game == null)
            return;
        
        final Graphics2D g2d = setup2DGraphics(g);
        drawBoard(g2d);
        drawOutlines(g2d);
        // Draw move selector if there is a request for it
        if(currentBlocker != null)
        {
            drawMove(g2d);
        }
        if(game.isGameOver() && game.getWinner() != 0)
        {
            drawVictory(g2d, game.getStartPt(), game.getEndPt());
        }
    }
    
    /// Calls for repaint
    public void drawBoard(final GameStateModule game)
    {
        this.game = game;
        repaint();
    }

    /// Empty.
    public void mousePressed(MouseEvent e)
    {
    }

    /// Gets selected move from the active human player if it was requested.
    public void mouseReleased(MouseEvent e)
    {
        // Update mouse position
        MouseX = game.getWidth() * e.getX() / this.getWidth();
        
        // If this isn't a legal move, abort.
        if(!game.canMakeMove(MouseX)) return;
        
        // Ignore if there is no request
        if(currentBlocker == null)
        {
            return;
        }

        // Releases the blocker.  DO NOT REORDER THESE LINES!
        final HumanBlocker current = currentBlocker;
        currentBlocker = null;
        
        // Report Selection
        current.setMove(MouseX);
        // Update graphics
        this.repaint();
    }

    /// Empty.
    public void mouseEntered(MouseEvent e)
    {
    }

    /// Empty.
    public void mouseExited(MouseEvent e)
    {
    }

    /// Empty.
    public void mouseClicked(MouseEvent e)
    {
    }

    /// Updates mouse position if the mouse has moved.
    public void mouseMoved(MouseEvent e)
    {
        MouseX = game.getWidth() * e.getX() / this.getWidth();
        this.repaint();
    }

    /// Updates mouse position if the mouse has moved.
    public void mouseDragged(MouseEvent e)
    {
        MouseX = game.getWidth() * e.getX() / this.getWidth();
        this.repaint();
    }
}
