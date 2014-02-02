import java.awt.Point;
import java.util.*;

abstract public class UninformedAI implements AIModule {

    abstract protected void append(Collection<Point> deque, Point a);
    abstract protected Point detach(Collection<Point> deque);
    abstract protected Collection<Point> getCollection(final TerrainMap map);

    protected HashMap<Point, Point> search(Collection<Point> stack, final TerrainMap map, Point start, Point end) {
        HashSet<Point> discovered = new HashSet<Point>();
        HashMap<Point, Point> parentMap = new HashMap<Point, Point>();

        append(stack, start);

        while (!stack.isEmpty()) {
            Point next = detach(stack);
            if (next.equals(end)) {
                break;
            }
            if (!discovered.contains(next)) {
                discovered.add(next);

                for (Point neighbor : map.getNeighbors(next)) {
                    if (!discovered.contains(neighbor)) {
                        if (!parentMap.containsKey(neighbor)) {
                            parentMap.put(neighbor, next);
                        }
                        append(stack, neighbor);
                        map.getCost(next, neighbor);
                    }
                }
            }
        }

        return parentMap;
    }

    public List<Point> createPath(final TerrainMap map) {

        Point start = map.getStartPoint();
        Point end = map.getEndPoint();
        Collection<Point> col = getCollection(map);

        HashMap<Point, Point> parentMap = search(col, map, start, end);

        ArrayList<Point> path = new ArrayList<Point>();

        Point child = end;
        Point parent;
        do {
            parent = parentMap.get(child);
            path.add(child);
            child = parent;
        } while (!parent.equals(start));
        path.add(parent);

        Collections.reverse(path);

        return path;
    }
}
