import java.awt.Point;

public class UCSEuclideanAI extends UCSAI {

    protected double distance(Point p1, Point p2) {
        return p1.distance(p2);
    }
}
