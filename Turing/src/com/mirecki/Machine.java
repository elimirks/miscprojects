package com.mirecki;

import java.util.HashMap;
import java.util.LinkedList;
import java.util.List;

public class Machine {
    private State currentState;
    private List<Letter> inputAlphabet;
    private List<Letter> workingAlphabet;

    private HashMap<State, List<StateTransition>> transitionMap = new HashMap<>();

    /**
     * Add a state transition to the machine.
     * @param transition
     */
    public void addStateTransition(StateTransition transition) {
        if ( ! workingAlphabet.contains(transition.getInput())) {
            System.out.printf("Input %s isn't in machine alphabet\n", transition.getInput());
            System.exit(1);
        }

        if ( ! workingAlphabet.contains(transition.getReplacement())) {
            System.out.printf("Replacement %s isn't in machine alphabet\n", transition.getReplacement());
            System.exit(1);
        }

        State state = transition.getState();

        if ( ! transitionMap.containsKey(state)) {
            transitionMap.put(state, new LinkedList<>());
        }

        transitionMap.get(state).add(transition);
    }

    public void setInputAlphabet(List<Letter> inputAlphabet) {
        this.inputAlphabet = inputAlphabet;
    }

    public void setWorkingAlphabet(List<Letter> workingAlphabet) {
        this.workingAlphabet = workingAlphabet;
    }

    /**
     * Sets the initial machine state.
     * @param state
     */
    public void setInitialState(State state) {
        this.currentState = state;
    }

    /**
     * Prints out all state transitionMap in the machine, for debugging.
     */
    public void printStateTransitions() {
        for (List<StateTransition> l : transitionMap.values()) {
            for (StateTransition st : l) {
                System.out.println(st.toString());
            }
        }
    }

    private StateTransition findTransitionForLetter(Letter letter) {
        List<StateTransition> transitions = transitionMap.get(currentState);

        for (StateTransition transition : transitions) {
            if (letter.matches(transition.getInput())) {
                return transition;
            }
        }

        System.out.println("Couldn't find appropriate state transition for " + currentState.toString());
        System.exit(1);
        return null;
    }

    private void tick(Tape tape) {
        StateTransition transition = findTransitionForLetter(tape.getCurrentLetter());

        Letter replacementLetter = transition.getReplacement();
        if (replacementLetter != Letter.WILDCARD) {
            tape.replaceCurrentLetter(replacementLetter);
        }

        if (transition.getDirection() == Direction.LEFT) {
            tape.moveLeft();
        } else {
            tape.moveRight();
        }

        currentState = transition.getNewState();
    }

    /**
     * Run the machine on the given tape.
     * @param tape The tape to run the machine over.
     * @return true on accept, false on reject.
     */
    public boolean run(Tape tape) {
        while ( ! currentState.isTerminalState()) {
            System.out.println(tape.toString());
            tick(tape);

            try {
                Thread.sleep(100);
            } catch (InterruptedException e) {
                // Swallow it!
            }
        }

        return currentState.isAcceptState();
    }
}
