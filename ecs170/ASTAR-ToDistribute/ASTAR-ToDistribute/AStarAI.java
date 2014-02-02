import java.awt.Point;
import java.util.Stack;
import java.util.List;

public class AStarAI implements AIModule {

    public List<Point> createPath(final TerrainMap map) {
        Stack<Point> path = new Stack<Point>();

        path.add(map.getStartPoint());
        path.add(map.getEndPoint());

        return path;
    }
}
