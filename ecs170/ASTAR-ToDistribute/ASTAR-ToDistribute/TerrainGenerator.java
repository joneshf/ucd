/// Base class of all Terrain Generators implementations
/**
 * An interface representing any terrain generator.
 * You do not need to understand how this code works in order to write a pathfinding AI.
 * 
 * @author Leonid Shamis
 */
public interface TerrainGenerator
{
    public byte[][] getTerrain();
}
