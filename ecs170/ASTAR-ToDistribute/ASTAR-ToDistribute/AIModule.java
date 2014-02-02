
import java.awt.Point;
import java.util.List;

/// Base class of all AI implementations
/**
 * An interface representing a pathfinder AI interface.  The interface exports
 * a single function (createPath) which accepts as input a TerrainMap and computes
 * a path from the start location to the end location.
 * 
 * @author Leonid Shamis
 * @see TerrainMap
 */
public interface AIModule
{
    /// Computes a path from the start to end points.
    /**
     * Given a terrain map containing a start and end point, computes a path from
     * the start point to the end point.  The returned list should be a sequence of
     * points from the start location to the end location (containing these points)
     * such that each point in the sequence is reachable from the previous point.
     * 
     * @param map The terrain map to compute a path across.
     * @return The path from the start point to the end point.
     */
    public List<Point> createPath(final TerrainMap map);
}