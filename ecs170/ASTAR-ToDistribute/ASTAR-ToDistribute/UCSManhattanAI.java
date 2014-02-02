import java.awt.Point;

public class UCSManhattanAI extends UCSAI {

    protected double distance(Point p1, Point p2) {
        return Math.abs(p1.x - p2.x) + Math.abs(p1.y - p2.y);
    }
}
