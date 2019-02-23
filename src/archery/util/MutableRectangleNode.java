package archery.util;
import java.util.List;
import java.util.ArrayList;

public class MutableRectangleNode {
    private boolean isLeaf;
    private double xmin, ymin, xmax, ymax;
    private List<Object> children = new ArrayList<>();

    public MutableRectangleNode(boolean isLeaf, double xmin, double ymin, double xmax, double ymax) {
        this.xmin = xmin;
        this.ymin = ymin;
        this.xmax = xmax;
        this.ymax = ymax;
        this.isLeaf = isLeaf;
    }

    public MutableRectangleNode(boolean isLeaf, List<Object> children, double xmin, double ymin, double xmax, double ymax) {
        this.xmin = xmin;
        this.ymin = ymin;
        this.xmax = xmax;
        this.ymax = ymax;
        this.children = children;
        this.isLeaf = isLeaf;
    }

    public double getXMin() {
        return this.xmin;
    }

    public double getYMin() {
        return this.ymin;
    }

    public double getXMax() {
        return this.xmax;
    }

    public double getYMax() {
        return this.ymax;
    }

    public boolean isLeaf() {
        return this.isLeaf;
    }

    public boolean isBranch() {
        return true;
    }

    public List<Object> getChildren() {
        return this.children;
    }

    private void setChildren(List<Object> children) {
        this.children = children;
    }

    public static MutableRectangleNode setChildren(MutableRectangleNode node, List<Object> children) {
        node.setChildren(children);
        return node;
    }

    public List<Object> getChildrenNodes() {
        if (!this.isLeaf()) {
            return this.getChildren();
        } else {
            return null;
        }
    }

    public static MutableRectangleNode addChild(MutableRectangleNode node, Object child) {
        List<Object> children = node.getChildren();
        children.add(child);
        node.setChildren(children);
        return node;
    }

    private void setXMin(double x) {
        this.xmin = x;
    }

    private void setYMin(double y) {
        this.ymin = y;
    }

    private void setXMax(double x) {
        this.xmax = x;
    }

    private void setYMax(double y) {
        this.ymax = y;
    }

    public static MutableRectangleNode reshape(MutableRectangleNode node, double dxmin, double dymin, double dxmax, double dymax) {
        node.setXMin(dxmin);
        node.setYMin(dymin);
        node.setXMax(dxmax);
        node.setYMax(dymax);
        return node;
    }
}
