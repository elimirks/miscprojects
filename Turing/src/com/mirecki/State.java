package com.mirecki;

public abstract class State {
    public static final State ACCEPT = new AcceptState();
    public static final State REJECT = new RejectState();

    public boolean isRejectState() {
        return false;
    }

    public boolean isAcceptState() {
        return false;
    }

    public boolean isTerminalState() {
        return isAcceptState() || isRejectState();
    }

    public abstract String getName();

    @Override
    public String toString () {
        return getName();
    }

    public static State createState(String name) {
        if ("accept".equals(name)) {
            return ACCEPT;
        } else if ("reject".equals(name)) {
            return REJECT;
        }

        return new NamedState(name);
    }

    static class NamedState extends State {
        private String name;

        public NamedState(String name) {
            this.name = name;
        }

        @Override
        public String getName() {
            return name;
        }

        @Override
        public int hashCode() {
            return name.hashCode();
        }

        @Override
        public boolean equals(Object other) {
            if (other == null || ! (other instanceof NamedState)) {
                return false;
            }
            NamedState otherNamedState = (NamedState)other;
            return name.equals(otherNamedState.name);
        }
    }

    static class RejectState extends State {
        @Override
        public boolean isRejectState() {
            return true;
        }

        @Override
        public String getName() {
            return "reject";
        }
    }

    static class AcceptState extends State {
        @Override
        public boolean isAcceptState() {
            return true;
        }

        @Override
        public String getName() {
            return "accept";
        }
    }
}
