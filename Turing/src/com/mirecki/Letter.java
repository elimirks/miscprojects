package com.mirecki;

public abstract class Letter {
    /**
     * The terminal character for the tape.
     */
    public static final Letter EMPTY = new EmptyLetter();

    /**
     * Never actually written on the tape, but used as a wildcard for convenience.
     */
    public static final Letter WILDCARD = new WildcardLetter();

    /**
     * Creates a new letter from the given string.
     * @param letter The letter string to parse. Could be "empty", "*", or a string of length 1.
     * @return The appropriate letter.
     */
    public static Letter fromString(String letter) {
        if ("empty".equals(letter)) {
            return EMPTY;
        }
        if ("*".equals(letter)) {
            return WILDCARD;
        }

        if (letter.length() != 1) {
            throw new TMException(String.format("Letters should only be 1 character. '%s' given", letter));
        }

        return new CharLetter(letter.charAt(0));
    }

    /**
     * Determines if this letter matches another.
     * @param other The other letter to compare with.
     * @return The truth value.
     */
    public abstract boolean matches(Letter other);

    private static class EmptyLetter extends Letter {
        @Override
        public boolean matches(Letter other) {
            // EMPTY is a singleton object!
            return other == this;
        }

        @Override
        public String toString() {
            return "empty";
        }
    }

    private static class WildcardLetter extends Letter {
        @Override
        public boolean matches(Letter other) {
            // The wildcard should match any letter.
            return true;
        }

        @Override
        public String toString() {
            return "*";
        }
    }

    private static class CharLetter extends Letter {
        private char character;

        public CharLetter(char character) {
            this.character = character;
        }

        @Override
        public boolean matches(Letter other) {
            if (other == null) {
                return false;
            }

            if (other instanceof WildcardLetter) {
                return true;
            }

            CharLetter otherCharLetter = (CharLetter)other;
            return this.character == otherCharLetter.character;
        }

        @Override
        public int hashCode() {
            // FIXME: do this properly!
            return (int)character;
        }

        @Override
        public boolean equals(Object other) {
            if (other == null || ! (other instanceof  CharLetter)) {
                return false;
            }
            return this.character == ((CharLetter)other).character;
        }

        @Override
        public String toString() {
            return Character.toString(character);
        }
    }
}
