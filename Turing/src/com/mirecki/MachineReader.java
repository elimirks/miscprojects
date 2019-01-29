package com.mirecki;

import java.io.BufferedReader;
import java.io.File;
import java.io.FileReader;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

public class MachineReader {
    private Machine machine;
    private static final String inputAlphabetPrefix = "InputAlphabet:";
    private static final String workingAlphabetPrefix = "WorkingAlphabet:";
    private static final String initialStatePrefix = "InitialState:";

    private List<Letter> parseAlphabetLine(String line) {
        String trimmedLine = line.substring(line.indexOf(':') + 1).trim();

        List<Letter> alphabet = new LinkedList<>();

        for (String letterString : trimmedLine.split(",")) {
            alphabet.add(Letter.fromString(letterString));
        }

        return alphabet;
    }

    private List<Letter> parseInputAlphabetLine(String line) {
        return parseAlphabetLine(line);
    }

    private List<Letter> parseWorkingAlphabetLine(String line) {
        List<Letter> alphabet = parseAlphabetLine(line);
        alphabet.add(Letter.EMPTY);
        alphabet.add(Letter.WILDCARD); // Implicitly included in the working alphabet
        return alphabet;
    }

    private State parseInitialStateLine(String line) {
        String stateName = line.substring(line.indexOf(':') + 1).trim();
        return State.createState(stateName);
    }

    private StateTransition parseStateTransitionLine(String line) {
        String components[] = line.split(",");

        if (components.length != 5) {
            throw new TMException(String.format("Invalid state transition: %s", line));
        }

        String startStateName = components[0].trim();
        String inputString = components[1].trim();
        String replacementString = components[2].trim();
        String endStateName = components[3].trim();
        String directionName = components[4].trim();

        State startState = State.createState(startStateName);

        if (startState.isAcceptState() || startState.isRejectState()) {
            throw new TMException(String.format("Invalid state transition: %s", line));
        }

        Letter inputLetter = Letter.fromString(inputString);
        Letter replacementLetter = Letter.fromString(replacementString);

        State endState = State.createState(endStateName);

        Direction direction = Direction.fromName(directionName);

        return new StateTransition(startState, inputLetter, replacementLetter, endState, direction);
    }

    /**
     * Parses a line from the turing machine file.
     * @param line The line to parse.
     */
    public void parseLine(Machine machine, String line) {
        // Remove comments and extra whitespace
        String trimmedLine = line.split("#")[0].trim();

        // Ignore blank lines
        if (trimmedLine.length() == 0) {
            return;
        }

        if (trimmedLine.startsWith(inputAlphabetPrefix)) {
            machine.setInputAlphabet(parseInputAlphabetLine(trimmedLine));
        } else if (trimmedLine.startsWith(workingAlphabetPrefix)) {
            machine.setWorkingAlphabet(parseWorkingAlphabetLine(trimmedLine));
        } else if (trimmedLine.startsWith(initialStatePrefix)) {
            machine.setInitialState(parseInitialStateLine(trimmedLine));
        } else {
            machine.addStateTransition(parseStateTransitionLine(trimmedLine));
        }
    }

    public Machine loadTMFile(String fileName) throws IOException {
        machine = new Machine();
        BufferedReader reader = null;

        try {
            reader = new BufferedReader(new FileReader(new File(fileName)));

            String line;
            while ((line = reader.readLine()) != null) {
                parseLine(machine, line);
            }
        } finally {
            if (reader != null) {
                reader.close();
            }
        }

        return machine;
    }

    /**
     * Creates a new tape based on the given input tape string.
     * @param inputTapeString The string of character to initialize the tape.
     * @return The initialized Tape object.
     */
    public Tape parseInputTapeString(String inputTapeString) {
        List<Letter> inputTape = new LinkedList<>();
        List<Letter> inputAlphabet = machine.getInputAlphabet();

        for (char c : inputTapeString.toCharArray()) {
            Letter l = Letter.fromString(Character.toString(c));
            if ( ! inputAlphabet.contains(l)) {
                throw new TMException(
                        String.format("Input character %s is not in input alphabet", l.toString())
                );
            }
            inputTape.add(l);
        }

        return new Tape(inputTape);
    }
}
