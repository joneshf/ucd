import java.awt.Point;
import java.util.*;

abstract public class UCSAI extends UninformedAI {

    abstract protected double distance(Point p1, Point p2);

    protected class G implements Comparator<Point> {

        final TerrainMap map;
        final Point startPoint;
        final Point endPoint;

        public G(final TerrainMap map) {
            this.map = map;
            this.startPoint = map.getStartPoint();
            this.endPoint = map.getEndPoint();
        }

        @Override
        public int compare(Point x, Point y) {
            double xCost = distance(startPoint, x);
            double yCost = distance(startPoint, y);
            if (xCost < yCost) {
                return -1;
            } else if (xCost > yCost) {
                return 1;
            } else {
                return 0;
            }
        }
    }

    protected void append(Collection<Point> pQueue, Point a) {
        ((PriorityQueue<Point>) pQueue).add(a);
    }

    protected Point detach(Collection<Point> pQueue) {
        return ((PriorityQueue<Point>) pQueue).poll();
    }

    protected Collection<Point> getCollection(final TerrainMap map) {
        return new PriorityQueue<Point>(10, new G(map));
    }
}
