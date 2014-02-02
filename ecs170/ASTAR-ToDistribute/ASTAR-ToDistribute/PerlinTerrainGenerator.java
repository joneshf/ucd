import java.util.*;

/// A class that generates random terrains using Perlin noise functions.
/**
 * This class is responsible for generating the terrain in the world.  It uses Perlin
 * noise functions to create a somewhat realistic and smooth terrain.  You do not
 * need to understand how this code works in order to write a pathfinding AI.
 * 
 * This code is based on information on Perlin noise retrieved from www.siafoo.net/snippet/144
 * which was created by many authors.  If you are interested in Perlin noise, consider looking
 * into Ken Perlin's personal web page, which has all sorts of goodies.
 * 
 * @author Leonid Shamis
 */
public final class PerlinTerrainGenerator implements TerrainGenerator
{
    private final int width;
    private final int height;
    private final int roughness;
    private final long seed;
    
    public PerlinTerrainGenerator(final int width, final int height, final int roughness)
    {
        this(width, height, roughness, System.currentTimeMillis());
    }
    
    public PerlinTerrainGenerator(final int width, final int height, final int roughness, final long seed)
    {
        this.width = width;
        this.height = height;
        this.roughness = roughness;
        this.seed = seed;
    }

    public byte[][] getTerrain()
    {
        double [][] tempResult = new double[width][height];
        for(int scale = roughness; scale < 8; ++scale)
        {
            int [][] intermediate = generateOneOctave(width, height, seed + scale, 256 >> scale);
            for(int x = 0; x < width; ++x)
                for(int y = 0; y < height; ++y)
                    tempResult[x][y] += ((double)intermediate[x][y]) / (2 << (scale - roughness));
        }
        
        byte [][] result = new byte[width][height];
        for(int x = 0; x < width; ++x)
            for(int y = 0; y < height; ++y)
                result[x][y] = (byte)((int)tempResult[x][y]);
        return result;
    }

    private int[][] generateOneOctave(final int width, final int height, final long seed, final double scale)
    {
        final ArrayList<ArrayList<Double>> G = new ArrayList<ArrayList<Double>>();
        final Random r = new Random(seed);

        while(G.size() < 256)
        {
            while(true)
            {
                final double first = r.nextDouble() * 2 - 1;
                final double second = r.nextDouble() * 2 - 1;

                final double length = Math.sqrt(first * first + second * second);
                if(length < 1.0)
                {
                    final ArrayList<Double> newElem = new ArrayList<Double>();
                    newElem.add(first / length);
                    newElem.add(second / length);
                    G.add(newElem);
                    break;
                }
            }
        }
        
        final int[] P = new int[256];
        for(int i = 0; i < P.length; i++)
        {
            P[i] = i;
        }

        for(int i = P.length - 1; i > 0; i--)
        {
            final int index = r.nextInt(i);
            final int temp = P[index];
            P[index] = P[i];
            P[i] = temp;
        }

        final int[][] result = new int[width][height];
        for(int x = 0; x < width; ++x)
        {
            for(int y = 0; y < height; ++y)
            {
                result[x][y] = (int) ((noise(x / scale, y / scale, P, G) + 1) * 128);
            }
        }

        return result;
    }

    private double drop(final double a)
    {
        final double b = Math.abs(a);
        return 1.0 - b * b * b * (b * (b * 6 - 15) + 10);
    }

    private double Q(final double u, final double v)
    {
        return drop(u) * drop(v);
    }

    private double dotProduct(final ArrayList<Double> b, final double[] a)
    {
        return a[0] * b.get(0) + a[1] * b.get(1);
    }

    private double noise(final double x, final double y, final int[] P, final ArrayList<ArrayList<Double>> G)
    {
        final double[] cell = new double[] {Math.floor(x), Math.floor(y)};

        double sum = 0.0;
        for(int r = 0; r <= 1; ++r)
        {
            for(int s = 0; s <= 1; ++s)
            {
                final double i = cell[0] + r;
                final double j = cell[1] + s;

                final double[] uv = new double[] {x - i, y - j};

                int index = P[(int) i];
                index = P[(index + (int) j) % P.length];
                final ArrayList<Double> grad = G.get(index % G.size());
                sum += Q(uv[0], uv[1]) * dotProduct(grad, uv);
            }
        }

        return Math.max(Math.min(sum, 1.0), -1.0);
    }
}
