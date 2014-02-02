import java.awt.Point;
import java.util.*;

public class BFSAI extends UninformedAI {

    protected void append(Collection<Point> deque, Point a) {
        ((ArrayDeque<Point>) deque).addLast(a);
    }

    protected Point detach(Collection<Point> deque) {
        return ((ArrayDeque<Point>) deque).removeFirst();
    }

    protected Collection<Point> getCollection(final TerrainMap map) {
        return new ArrayDeque<Point>();
    }
}
