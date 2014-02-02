import java.awt.Point;
import java.util.*;

public class DFSAI implements AIModule {

    protected HashSet<Point> discovered = new HashSet<Point>();
    protected HashMap<Point, Point> parentMap = new HashMap<Point, Point>();
    protected ArrayDeque<Point> path = new ArrayDeque<Point>();

    protected void dfs(final TerrainMap map, Point start, Point end) {
        ArrayDeque<Point> stack = new ArrayDeque<Point>();

        stack.push(start);

        while (!stack.isEmpty()) {
            Point next = stack.pop();
            if (next.equals(end)) {
                return;
            }
            if (!discovered.contains(next)) {
                discovered.add(next);

                for (Point neighbor : map.getNeighbors(next)) {
                    if (!discovered.contains(neighbor)) {
                        parentMap.put(neighbor, next);
                        stack.push(neighbor);
                    }
                }
            }
        }
    }

    public List<Point> createPath(final TerrainMap map) {

        Point start = map.getStartPoint();
        Point end = map.getEndPoint();

        dfs(map, start, end);

        ArrayList<Point> validPath = new ArrayList<Point>();

        Point child = end;
        Point parent;
        do {
            parent = parentMap.get(child);
            validPath.add(child);
            child = parent;
        } while (!parent.equals(start));
        validPath.add(parent);

        Collections.reverse(validPath);

        return validPath;
    }
}
