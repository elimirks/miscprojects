package com.mirecki;

public enum Direction {
    LEFT, RIGHT;

    public static Direction fromName(String name) {
        if ("L".equals(name)) {
            return LEFT;
        } else if ("R".equals(name)) {
            return RIGHT;
        }

        throw new TMException(String.format("Invalid direction name: %s", name));
    }

    public String toString () {
        return (this == LEFT) ? "L" : "R";
    }
}
