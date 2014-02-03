import java.awt.Point;
import java.util.*;

public class AStarDiv_999397426 implements AIModule {

    protected TerrainMap map;
    protected Point startPoint;
    protected Point endPoint;
    protected HashMap<Point, Point> parentMap;

    protected class NodeComparator implements Comparator<Node> {

        @Override
        public int compare(Node x, Node y) {
            double xCost = x.pathCost;
            double yCost = y.pathCost;
            if (xCost < yCost) {
                return -1;
            } else if (xCost > yCost) {
                return 1;
            } else {
                return 0;
            }
        }
    }

    protected class Node {
        public final Point point;
        public final double pathCost;

        public Node(final Point point, double pathCost) {
            this.point = point;
            this.pathCost = pathCost;
        }

        @Override
        public String toString() {
            return "Node(point: ("+this.point.x+","+this.point.y+"), pathCost: "+this.pathCost+")";
        }
    }

    /**
     * Constant Distance. It should make the algorithm behave like Dijkstra's.
     *
     * @param p The point to determine distance from.
     * @return A constant distance to the end state.
     */
    // private double getHeuristic(final TerrainMap map, final Point p1, final Point p2) {
    //     return 0;
    // }

    /**
     * The Manhattan Distance.
     *
     * @param p The point to determine distance from.
     * @return The Manhattan distance to the end state.
     */
    // private double getHeuristic(final TerrainMap map, final Point p1, final Point p2) {
    //     return Math.abs(p1.x - p2.x) +
    //            Math.abs(p1.y - p2.y);
    // }

    /**
     * The Euclidean Distance.
     *
     * @param p The point to determine distance from.
     * @return The Euclidean distance to the end state.
     */
    // private double getHeuristic(final TerrainMap map, final Point p1, final Point p2) {
    //     return p1.distance(p2);
    // }

    /**
     * The Chebyshev Distance.
     *
     * @param p The point to determine distance from.
     * @return The Chebyshev distance to the end state.
     */
    // private double getHeuristic(final TerrainMap map, final Point p1, final Point p2) {
    //     return Math.max(Math.abs(p1.x - p2.x),
    //                     Math.abs(p1.y - p2.y));
    // }

    /**
     * The vertical Distance.
     *
     * @param p The point to determine distance from.
     * @return The vertical distance to the end state.
     */
    // private double getHeuristic(final TerrainMap map, final Point p1, final Point p2) {
    //     return map.getTile(p2) - map.getTile(p1);
    // }

    /**
     * The diagonal shortcut Distance.
     *
     * @param p The point to determine distance from.
     * @return The diagonal shortcut distance to the end state.
     */
    // private double getHeuristic(final TerrainMap map, final Point p1, final Point p2) {
    //     double dx = Math.abs(p1.x - p2.x);
    //     double dy = Math.abs(p1.y - p2.y);
    //     int big = 14;
    //     int small = 10;
    //     if (dx > dy) {
    //         return big*dy + small*(dx-dy);
    //     } else {
    //         return big*dx + small*(dy-dx);
    //     }
    // }

    /**
     * The Weighted Chebyshev Distance.
     *
     * @param p The point to determine distance from.
     * @return The Weighted Chebyshev distance to the end state.
     */
    private double getHeuristic(final TerrainMap map, final Point p1, final Point p2) {
        return 0.913 * Math.max(Math.abs(p1.x - p2.x), Math.abs(p1.y - p2.y));
    }

    protected List<Point> aStar() {
        HashMap<Point, Point> cameFrom = new HashMap<Point, Point>();
        HashMap<Point, Double> gScore = new HashMap<Point, Double>();
        HashMap<Point, Double> fScore = new HashMap<Point, Double>();

        PriorityQueue<Node> openPQ = new PriorityQueue<Node>(100, new NodeComparator());
        HashSet<Point> closedSet = new HashSet<Point>();
        HashSet<Point> openSet = new HashSet<Point>();

        Point start = this.startPoint;
        Point goal = this.endPoint;

        openPQ.add(new Node(start, 0.0));
        openSet.add(start);
        gScore.put(start, 0.0);

        fScore.put(start, gScore.get(start) + getHeuristic(map, start, goal));

        while (!openSet.isEmpty()) {
            Node curNode = openPQ.poll();
            Point curPoint = getNext(openSet, fScore);
            if (curPoint.equals(goal)) {
                break;
            }
            openSet.remove(curPoint);
            closedSet.add(curPoint);

            for (Point neighbor : map.getNeighbors(curPoint)) {
                if (closedSet.contains(neighbor)) {
                    continue;
                }

                double tentativeGScore = gScore.get(curPoint) + map.getCost(curPoint, neighbor);

                if (!openSet.contains(neighbor) || tentativeGScore < gScore.get(neighbor)) {
                    cameFrom.put(neighbor, curPoint);
                    gScore.put(neighbor, tentativeGScore);
                    fScore.put(neighbor, gScore.get(neighbor) + getHeuristic(map, neighbor, goal));

                    if (!openSet.contains(neighbor)) {
                        openPQ.add(new Node(neighbor, fScore.get(neighbor)));
                        openSet.add(neighbor);
                    }
                }
            }
        }
        return reconstructPath(cameFrom, goal);
    }

    protected List<Point> reconstructPath(HashMap<Point, Point> cameFrom, Point current) {
        List<Point> path = new ArrayList<Point>();

        while (cameFrom.containsKey(current)) {
            path.add(current);
            current = cameFrom.get(current);
        }
        path.add(this.startPoint);
        Collections.reverse(path);
        return path;
    }

    protected Point getNext(HashSet<Point> set, HashMap<Point, Double> score) {
        double min = Double.MAX_VALUE;
        Point minPoint = null;
        for (Point next : set) {
            if (score.get(next) < min) {
                min = score.get(next);
                minPoint = next;
            }
        }
        return minPoint;
    }

    public List<Point> createPath(final TerrainMap map) {

        this.map = map;
        this.startPoint = map.getStartPoint();
        this.endPoint = map.getEndPoint();
        this.parentMap = new HashMap<Point, Point>();

        return aStar();
    }
}
