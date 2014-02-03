import java.awt.Point;

public class AStarChessAI extends UCSAI {

    protected double distance(Point p) {
        return pathCost(p) + goalCost(p);
    }

    protected double pathCost(Point p) {
        return cost(this.startPoint, p);
    }

    protected double goalCost(Point p) {
        return cost(this.endPoint, p);
    }

    protected double cost(Point p1, Point p2) {
        return Math.max(Math.abs(p1.x - p2.x),
                        Math.abs(p1.y - p2.y));
    }
}
