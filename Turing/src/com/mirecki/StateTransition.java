package com.mirecki;

public class StateTransition {
    private State state, newState;
    private Letter input, replacement;
    private Direction direction;

    public StateTransition(State state, Letter input, Letter replacement, State newState, Direction direction) {
        this.state = state;
        this.input = input;
        this.replacement = replacement;
        this.newState = newState;
        this.direction = direction;
    }

    public Letter getInput() {
        return input;
    }

    public Letter getReplacement() {
        return replacement;
    }

    public State getState() {
        return state;
    }

    public State getNewState() {
        return newState;
    }

    public Direction getDirection() {
        return direction;
    }

    @Override
    public String toString() {
        StringBuilder sb = new StringBuilder();
        sb.append(state.toString());
        sb.append(": ");
        sb.append(input.toString());
        sb.append('/');
        sb.append(replacement.toString());
        sb.append(" -> ");
        sb.append(newState.toString());
        sb.append('/');
        sb.append(direction.toString());
        return sb.toString();
    }
}
