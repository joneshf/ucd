import java.awt.Point;

public class UCSEuclideanAI extends UCSAI {

    protected double distance(Point p) {
        return this.startPoint.distance(p);
    }
}
