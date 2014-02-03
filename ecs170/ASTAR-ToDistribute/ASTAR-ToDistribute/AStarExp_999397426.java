import java.awt.Point;
import java.util.*;

public class AStarExp_999397426 implements AIModule {

    protected TerrainMap map;
    protected Point startPoint;
    protected Point endPoint;
    protected HashMap<Point, Point> parentMap;

    protected class NodeComparator implements Comparator<Node> {

        @Override
        public int compare(Node x, Node y) {
            double xCost = x.pathCost + heuristic(x.point);
            double yCost = y.pathCost + heuristic(y.point);
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

    protected double heuristic(Point p) {
        return Math.max(Math.abs(this.endPoint.x - p.x),
                        Math.abs(this.endPoint.y - p.y));
    }

    protected void search(
            PriorityQueue<Node> pQueue,
            final TerrainMap map,
            Point start,
            Point end) {
        HashSet<Point> discovered = new HashSet<Point>();

        pQueue.add(new Node(start, 0));

        while (!pQueue.isEmpty()) {
            Node nextNode = pQueue.poll();
            Point nextPoint = nextNode.point;
            // System.out.println("Working with: "+nextNode);

            if (nextPoint.equals(end)) {
                break;
            }
            if (!discovered.contains(nextPoint)) {
                discovered.add(nextPoint);

                for (Point neighbor : map.getNeighbors(nextPoint)) {
                    if (!discovered.contains(neighbor)) {
                        if (!this.parentMap.containsKey(neighbor)) {
                            this.parentMap.put(neighbor, nextPoint);
                        }
                        double pathCost = map.getCost(nextPoint, neighbor) +
                            nextNode.pathCost;
                        Node neighborNode = new Node(neighbor, pathCost);
                        // System.out.println("Adding: "+neighborNode);
                        pQueue.add(neighborNode);
                    }
                }
            }
        }
    }

    public List<Point> createPath(final TerrainMap map) {

        this.map = map;
        this.startPoint = map.getStartPoint();
        this.endPoint = map.getEndPoint();
        this.parentMap = new HashMap<Point, Point>();

        PriorityQueue<Node> pQueue = new PriorityQueue<Node>(
                100, new NodeComparator()
            );

        ArrayList<Point> path = new ArrayList<Point>();

        search(pQueue, map, this.startPoint, this.endPoint);
        Point child = this.endPoint;
        Point parent;
        do {
            parent = this.parentMap.get(child);
            path.add(child);
            child = parent;
        } while (!parent.equals(this.startPoint));
        path.add(parent);

        Collections.reverse(path);

        return path;
    }
}
