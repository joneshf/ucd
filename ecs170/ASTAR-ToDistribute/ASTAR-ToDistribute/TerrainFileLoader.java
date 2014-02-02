
import java.io.*;
import java.util.*;

/// A class that loads a terrain from a file.
/**
 * This class is responsible for retrieving the terrain in the world from a specified file.
 * You do not need to understand how this code works in order to write a pathfinding AI.
 * 
 * @author Leonid Shamis
 */
public final class TerrainFileLoader implements TerrainGenerator
{
    /// A class used to hold individual coordinate information durring file reading
    private final class Node
    {
        public final int x;
        public final int y;
        public final double z;

        public Node(final int x, final int y, final double z)
        {
            this.x = x;
            this.y = y;
            this.z = z;
        }
    }
    
    private byte[][] Terrain;
    private String filename;
    private int min_x = Integer.MAX_VALUE;
    private int max_x = Integer.MIN_VALUE;
    private int min_y = Integer.MAX_VALUE;
    private int max_y = Integer.MIN_VALUE;
    private double min_z = Double.POSITIVE_INFINITY;
    private double max_z = Double.NEGATIVE_INFINITY;
    private double sum_z = 0;

    public TerrainFileLoader(String filename)
    {
        this.filename = filename;
        
        final ArrayList<Node> nodes = parse();
        final int x_range = max_x - min_x + 1;
        final int y_range = max_y - min_y + 1;
        final double scaled_z = 255 / (max_z - min_z);
        Terrain = new byte[x_range][y_range];

        // Set default values
        byte averageValue = (byte) (scaled_z * sum_z / nodes.size());
        for(int x = 0; x < x_range; x++)
        {
            Arrays.fill(Terrain[x], averageValue);
        }

        // Set known values
        for(Iterator<TerrainFileLoader.Node> it = nodes.iterator(); it.hasNext();)
        {
            TerrainFileLoader.Node node = it.next();
            Terrain[node.x - min_x][node.y - min_y] = (byte) (scaled_z * (node.z - min_z));
        }
    }

    public byte[][] getTerrain()
    {
        return Terrain;
    }

    private ArrayList<Node> parse()
    {
        try
        {
            final ArrayList<Node> nodes = new ArrayList<TerrainFileLoader.Node>();
            BufferedReader in = new BufferedReader(new FileReader(filename));
            String str;
            while((str = in.readLine()) != null)
            {
                final StringTokenizer parser = new StringTokenizer(str);
                final int x = Integer.parseInt(parser.nextToken()) / 10;
                final int y = Integer.parseInt(parser.nextToken()) / 10;
                final double z = Double.parseDouble(parser.nextToken());
                nodes.add(new Node(x, y, z));
                updateLimits(x, y, z);
            }
            in.close();
            return nodes;
        }
        catch(Exception ex)
        {
            throw new RuntimeException("Unable to parse file");
        }
    }

    private void updateLimits(final int x, final int y, final double z)
    {
        min_x = Math.min(min_x, x);
        max_x = Math.max(max_x, x);
        min_y = Math.min(min_y, y);
        max_y = Math.max(max_y, y);
        min_z = Math.min(min_z, z);
        max_z = Math.max(max_z, z);
        sum_z += z;
    }
}
