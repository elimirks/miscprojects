package com.mirecki;

import java.util.LinkedList;
import java.util.List;

/**
 * A doubly infinite tape for the TM to work with.
 */
public class Tape {
    private LinkedList<Letter> tape = new LinkedList<Letter>();
    private int currentPosition = 0;

    public Tape(List<Letter> initialTape) {
        tape.addAll(initialTape);
    }

    /**
     * Move the read/write head left.
     */
    public void moveLeft() {
        if (currentPosition == 0) {
            // Create new empty letters if we move left at the beginning, giving the illusion of an infinite tape.
            tape.addFirst(Letter.EMPTY);
        } else {
            currentPosition--;
        }
    }

    /**
     * Move the read/write head right.
     */
    public void moveRight() {
        currentPosition++;

        if (currentPosition == tape.size()) {
            tape.add(Letter.EMPTY);
        }
    }

    /**
     * Get the letter on the read/write head.
     * @return The letter, of course!
     */
    public Letter getCurrentLetter() {
        return tape.get(currentPosition);
    }

    /**
     * Replace the letter at the current position.
     * @param newLetter
     */
    public void replaceCurrentLetter(Letter newLetter) {
        tape.set(currentPosition, newLetter);
    }

    @Override
    public String toString() {
        final int PRINT_CONTEXT = 40;

        StringBuilder sb = new StringBuilder();

        // The tape representation
        for (int i = currentPosition - PRINT_CONTEXT; i < currentPosition + PRINT_CONTEXT; i++) {
            if (i < 0 || i >= tape.size()) {
                sb.append(' ');
            } else {
                Letter letter = tape.get(i);
                if (letter == Letter.EMPTY) {
                    sb.append(' ');
                } else {
                    sb.append(letter.toString());
                }
            }
        }

        sb.append('\n');

        // Display pointer to the current position
        for (int i = currentPosition - PRINT_CONTEXT; i < currentPosition + PRINT_CONTEXT; i++) {
            if (i == currentPosition) {
                sb.append('^');
            } else {
                sb.append('-');
            }
        }

        return sb.toString();
    }
}
