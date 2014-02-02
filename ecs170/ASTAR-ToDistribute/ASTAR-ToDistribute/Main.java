
import java.awt.*;
import java.awt.image.BufferedImage;
import java.util.*;
import javax.swing.*;

/// Main program entry point.
/**
 * The Main class glues together all of the pieces of the program.  It is responsible
 * for parsing command-line input and creating the displays.  Use the --help switch
 * to get more information about the command line arguments.
 *
 * @author Leonid Shamis
 */
public class Main
{
    /// Prints the commandline instructions.
    public static void helpPrinter()
    {
        System.out.println("  Command Line Parameters are as follows:");
        System.out.println("    \"--help\" : You're looking at it");
        System.out.println("    \"-w [int]\" : Set the width of the terrain map");
        System.out.println("      Example: -w 500");
        System.out.println("    \"-h [int]\" : Set the height of the terrain map");
        System.out.println("      Example: -h 500");
        System.out.println("    \"-seed [int]\" : Set the seed value for generating the terrain map");
        System.out.println("      Example: -seed 3");
        System.out.println("    \"-roughness [0-7]\" : Set the roughness of the terrain map");
        System.out.println("      Example: -roughness 4");
        System.out.println("    \"-movement [cem]\" : Set the type of movement [c: Chess, e: Euclidean, m: Manhattan]");
        System.out.println("      Example: -movement c");
        System.out.println("    \"-contour\" : Displays the order of revealed nodes");
        System.out.println("    \"-chaotic\" : Slightly corrupts the terrain every one second");
        System.out.println("    \"-load [filename]\" : Loads up the given file to use as the terrain map");
        System.out.println("      Example: -load MTAFT.XYZ");
        System.out.println("    All other arguments are algorithms to be run on the generated map");
        System.out.println("Note: Later command-line options override earlier ones if they are incompatable\n");
        System.out.println("Example: java Main -w 500 -h 500 DijkstraAI AstarAI");
    }

    /// Displays an image in a new frame.
    /**
     * @param im The image to display.
     * @param title The title of the new window.
     */
    public static void createDisplayWindow(final BufferedImage im, final String title)
    {
        // Set up a frame and a panel that paints this image.
        final JFrame frame = new JFrame("Path Finder: " + title);
        final JPanel panel = new JPanel()
        {
            @Override
            public void paintComponent(final Graphics g)
            {
                super.paintComponent(g);
                g.drawImage(im, 0, 0, getWidth(), getHeight(), null);
            }
        };

        panel.setPreferredSize(new Dimension(im.getWidth(), im.getHeight()));
        frame.setDefaultCloseOperation(WindowConstants.DISPOSE_ON_CLOSE);
        frame.add(panel);
        frame.pack();
        frame.setVisible(true);
    }

    /// Main program entry point.
    public static void main(String[] args)
    {
        ArrayList<AIModule> aiList = new ArrayList<AIModule>();
        TerrainGenerator terraGen = null;
        boolean flexible = true;
        boolean showContour = false;
        boolean chaotic = false;
        int width = 500;
        int height = 500;
        int roughness = 3;
        long seed = System.currentTimeMillis();
        TerrainMap.MovementType moveType = TerrainMap.MovementType.Chess;

        // Parse through the command line arguements
        int i = 0;
        try
        {
            while(i < args.length)
            {
                // Width switch.
                if(args[i].equalsIgnoreCase("-w"))
                {
                    if(!flexible)
                        continue;

                    width = Integer.parseInt(args[i + 1]);
                    if(width <= 0)
                    {
                        throw new IllegalArgumentException("Widths must be nonnegative.");
                    }
                }
                // Height switch
                else if(args[i].equalsIgnoreCase("-h"))
                {
                    if(!flexible)
                        continue;

                    height = Integer.parseInt(args[i + 1]);
                    if(width <= 0)
                    {
                        throw new IllegalArgumentException("Heights must be nonnegative.");
                    }
                }
                // Random seed switch
                else if(args[i].equalsIgnoreCase("-seed"))
                {
                    seed = Integer.parseInt(args[i + 1]);
                }
                // Roughness switch.
                else if(args[i].equalsIgnoreCase("-roughness"))
                {
                    roughness = Integer.parseInt(args[i + 1]);
                    if(roughness < 0 || roughness >= 8)
                        throw new IllegalArgumentException("Roughness must be between 0 and 7.");
                }
                // Movement switch.
                else if(args[i].equalsIgnoreCase("-movement"))
                {
                    switch(args[i + 1].charAt(0))
                    {
                        case 'c': moveType = TerrainMap.MovementType.Chess; break;
                        case 'e': moveType = TerrainMap.MovementType.Euclidean; break;
                        case 'm': moveType = TerrainMap.MovementType.Manhattan; break;
                        default: throw new IllegalArgumentException("Unrecognized movement type.");
                    }
                }
                // Contour switch.
                else if(args[i].equalsIgnoreCase("-contour"))
                {
                    showContour = true;
                    i--;
                }
                // Chaos switch.
                else if(args[i].equalsIgnoreCase("-chaotic"))
                {
                    chaotic = true;
                    i--;
                }
                // Load a file
                else if(args[i].equalsIgnoreCase("-load"))
                {
                    String filename = args[i + 1];
                    terraGen = new TerrainFileLoader(filename);
                    byte[][] terrain = terraGen.getTerrain();
                    width = terrain.length;
                    height = terrain[0].length;
                    flexible = false;
                }
                // Help switch
                else if(args[i].equalsIgnoreCase("--help"))
                {
                    helpPrinter();
                    System.exit(0);
                }
                // If none of these, then we should treat it as a class to load.
                else
                {
                    aiList.add((AIModule) Class.forName(args[i]).newInstance());
                    i--;
                }
                i += 2;
            }
        }
        catch(ClassNotFoundException cnf)
        {
            System.err.println("AI Not Found: " + args[i]);
            System.exit(1);
        }
        catch(IndexOutOfBoundsException ioob)
        {
            System.err.println("Invalid Arguements: " + args[i]);
            System.exit(2);
        }
        catch(NumberFormatException e)
        {
            System.err.println("Invalid Integer: " + args[i]);
            System.exit(3);
        }
        catch(Exception e)
        {
            System.err.println("Unknown Error");
            System.exit(4);
        }

        if(terraGen == null)
        {
            terraGen = new PerlinTerrainGenerator(width, height, roughness, seed);
        }

        // For each module we've loaded, create a new map and run the appropriate
        // AI module on that map.
        for(AIModule ai : aiList)
        {
            final TerrainMap map = new TerrainMap(width, height, terraGen, moveType, chaotic);
            final long startTime = System.currentTimeMillis();
            final double cost = map.findPath(ai);
            final long endTime = System.currentTimeMillis();

            System.out.println(ai.getClass().getName());
            // System.out.println("Path Cost:  " + cost);
            // System.out.println("Uncovered:  " + map.getNumVisited());
            // System.out.println("Time taken: " + (endTime - startTime) + "\n");
            System.out.println("PathCost, " + cost + ", Uncovered, " + map.getNumVisited() + ", TimeTaken, " + (endTime - startTime));

            createDisplayWindow(map.createImage(), ai.getClass().getName());
            if(showContour)
                createDisplayWindow(map.createContourImage(), ai.getClass().getName());
        }
    }
}

/**
 * @mainpage Project 1: A* On Terrain Maps
 *
 * @section intro_sec Introduction
 *
 * You are lost but fortunately you have access to a map, knowledge of the A* algorithm and
 * Leonid Shamis's framework which you can use to find your way home.  But what path should
 * you take?  Should you just take the straight-line path "as the crow flies," or try to find
 * a more circuitous route that avoids major mountains and valleys?
 *
 * In this assignment, you will devise an algorithm for plotting a course home, minimizing the
 * total cost of the path and the amount of time spent searching.  You'll get a chance to explore
 * the informed searching techniques we've covered in class, and by the time you're done you'll
 * have a solid understanding of the benefits and drawbacks of the A* search algorithm.
 *
 * @section world_sec The World
 *
 * In this assignment, you'll work with the TerrainMap class, a class encapsulating
 * a two-dimensional world layered on top of a rectangular grid.  Each point in the
 * world has a height, represented by an integer value between 0 and 255.  Depending
 * on the selected movement type, you can either move to any of the eight squares
 * adjacent to your own location (e.g. the four cardinal directions and the diagonals)
 * or just the cardinal directions.  As you would expect, the cost to traverse between
 * tiles is dependent on the differences in height between the tiles.  Below are
 * different const functions for you to experiment with.
 *
 * The TerrainMap class also keeps track of which tiles your algorithm visits as it looks for an optimal path
 * home.  Part of your grade on this assignment will be determined by how many tiles your algorithm visited.
 * For example, if two different algorithms each yield the optimal path, but one accomplishes this
 * task and only considers 10% of the number of squares as the other, the algorithm that visited fewer squares
 * would be considered superior to the other.  Interestingly, many of the better search algorithms that visit
 * fewer squares also run in considerably less time, so you'll have a dual incentive to keep your search space
 * small.  Please note that this is still secondary to finding the shortest path.
 *
 * @section instruct_sec Coding Instructions
 *
 * Your assignment is to create an implementation of the AIModule interface that computes a path from the start
 * location to the end location while minimizing the total search space.  Once you've written this function,
 * you can plug it in to the existing starter code by compiling your module and specifying it as a command-line
 * parameter to the main program.  For example, if you've written an AI module called PerfectAI, you can see
 * the result by running "java Main PerfectAI".  This will run your AI, print its score and number of visited
 * squares to stdout, and will create a display window showing the terrain, the path you've taken, and the
 * squares your AI module visited.
 *
 * To help test your implementation, we've provided a working Dijkstra's algorithm AI class called DijkstraAI.
 * As you've learned in class, Dijkstra's algorithm always yields the optimal path, so you can compare your own
 * AI against the DijkstraAI module to see if your path is indeed optimal.  The starter code is designed such that
 * you can have several different AI modules each plot a path over the terrain.  You can do this by specifying
 * multiple parameters to the main program, as in "java Main PerfectAI DijkstraAI".  Because you can interface
 * with the starter code directly from the command line, you do not need to make any changes to the provided starter code.
 *
 * Your submission for this assignment should consist of a single .java file containing your AIModule implementation,
 * and it will be run using a clean copy of the starter code.  If you have any extensions to the project that require
 * changes to the starter code, please let us know before you begin making changes.
 *
 * @section grading_sec Grading
 *
 * @subsection cost_functions Cost Functions
 * \code
 * // Exponential of the height difference
 * public double getCost(final Point p1, final Point p2)
 * {
 *      return Math.exp(getTile(p2) - getTile(p1));
 * }
 * \endcode\code
 * // Exponential of the absolute value in height difference
 * public double getCost(final Point p1, final Point p2)
 * {
 *      return Math.exp(Math.abs(getTile(p2) - getTile(p1)));
 * }
 * \endcode\code
 * // Difference in heights with a min step cost of 1
 * public double getCost(final Point p1, final Point p2)
 * {
 *      return Math.max(getTile(p2) - getTile(p1), 1);
 * }
 * \endcode\code
 * // New height divided by old height
 * public double getCost(final Point p1, final Point p2)
 * {
 *      return getTile(p2) / (getTile(p1) + 1);
 * }
 * \endcode\code
 * // Old height divided by new height
 * public double getCost(final Point p1, final Point p2)
 * {
 *      return getTile(p1) / (getTile(p2) + 1);
 * }
 * \endcode
 * You may cut and paste them into TerrainMap.java over the current getCost function.
 *
 * @subsection part_1 Part 1: Creating Heuristics
 * For each cost function above X For each movement type, do the following: \n\n
 * a) Create an admissible heuristic, document the exact form of the heuristic and explain why
 * it is admissable. (6 points per cost function) \n\n
 * b) Show that the heuristic is consistent (3 points per cost function) \n\n
 * <i>Submission Requirements: For questions a) and b) submit a detailed writeup.</i>
 *
 * @subsection part_2 Part 2: Implementing the Heuristics and A* Algorithm
 * <i>For implementation purposes only consider chess movement with the built in cost function</i> \n\n
 * a) You will now implement your own version of A*. Note, depending on if your heuristic is consistent or
 * not will effect what queues need to be maintained. Look at the DirectAI and StupidAI classes to get an idea
 * of how to search the state space. (20 points) \n\n
 * b)  To implement the heuristic, write valid java code in the form of: (5 points)
 * \code
 * private double getHeuristic(final TerrainMap map, final Point pt1, final Point pt2)
 * {
 *  ...
 * }
 * \endcode\n
 * <i>Submission Requirements: For questions a) and b) submit an java module labeled as AStar_<your-student-id>.java</i>
 *
 * @subsection part_3 Part 3: Trying out your Code on a Small Problem
 * Try out your heuristic functions with the appropriate cost function on 500x500 maps with
 * random seeds 1, 2, 3, 4 and 5. \n\n
 * <i>Submission Requirements: For each execution record the cost of the shortest path and the
 * number of nodes expanded.</i> \n\n
 * 5 points per cost function for getting the shortest path. Non-optimal paths will get \n
 * 5 * (shortest path cost) / (your path cost) \n\n
 * For all students who get the shortest path: we will rank your performance and you shall receive
 * bonus marks of: \n
 * 5*(Number of Qualified Students + 1 - Your Rank)  / (Number of Qualified Students) for each cost function.
 *
 * @subsection part_4 Part 4: Climbing Mount Saint Helens During The Eruption
 * Go to http://tahoe.usgs.gov/viewers.html and download the Dem3D viewer. This will allow you to view the file
 * in MtAftDem.zip which is in the old USGS DEM format of the Mount Saint Helens after the eruption. \n\n
 * There are better DEM viewers you could use that allows fly throughts etc, but this one works with
 * a standards graphics card and will help them understand the terrain you are navigating. \n\n
 * This is a much larger grid that will change every second and hence your aim will be to find the optimal path
 * quickly.  Modify both your A* algorithm and admissible heuristic so as to find the optimal path in this new environment.\n\n
 * Use the following command to run this part of the assignment: "java Main YourAIModule -chaotic -load MTAFT.XYZ".\n\n
 * <i>Submission Requirements:</i> \n\n
 * a) A clear and concise description of your modified A* and admissible heuristic.(10 points) \n\n
 * b) The implementation of your modified A* and admissible heuristic in the file
 * MtStHelens_<your-student-id>.java (10 points) \n\n
 * c) The cost of your shortest path graded simmilarly to Part 3.\n\n
 * <b>Note: Your submitted module must work on any map that uses chess movement and is erupting, not just the Mount Saint Helens file</b>
 */
