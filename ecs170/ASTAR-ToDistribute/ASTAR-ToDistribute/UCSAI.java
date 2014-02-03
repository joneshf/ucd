import java.awt.Point;
import java.util.*;

abstract public class UCSAI extends UninformedAI {

    abstract protected double distance(Point p);

    TerrainMap map;
    Point startPoint;
    Point endPoint;

    protected class G implements Comparator<Point> {

        @Override
        public int compare(Point x, Point y) {
            double xCost = distance(x);
            double yCost = distance(y);
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
        this.map = map;
        this.startPoint = map.getStartPoint();
        this.endPoint = map.getEndPoint();
        return new PriorityQueue<Point>(100, new G());
    }
}
