import java.awt.Point;

public class UCSManhattanAI extends UCSAI {

    protected double distance(Point p) {
        return Math.abs(this.startPoint.x - p.x) +
               Math.abs(this.startPoint.y - p.y);
    }
}
