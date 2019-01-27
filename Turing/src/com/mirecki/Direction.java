package com.mirecki;

public enum Direction {
    LEFT, RIGHT;

    public static Direction fromName(String name) {
        if ("L".equals(name)) {
            return LEFT;
        } else if ("R".equals(name)) {
            return RIGHT;
        }

        System.out.printf("Invalid direction name: %s\n", name);
        System.exit(1);
        return LEFT;
    }

    public String toString () {
        return (this == LEFT) ? "L" : "R";
    }
}
