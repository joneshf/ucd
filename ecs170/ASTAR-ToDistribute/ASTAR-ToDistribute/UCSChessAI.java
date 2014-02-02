import java.awt.Point;

public class UCSChessAI extends UCSAI {
    protected double distance(Point p1, Point p2) {
        return Math.max(Math.abs(p1.x - p2.x), Math.abs(p1.y - p2.y));
    }
}
