import java.awt.Point;

public class UCSChessAI extends UCSAI {

    protected double distance(Point p) {
        return Math.max(Math.abs(this.startPoint.x - p.x),
                        Math.abs(this.startPoint.y - p.y));
    }
}
