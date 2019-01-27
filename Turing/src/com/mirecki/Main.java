package com.mirecki;

import java.io.IOException;

public class Main {
    public static void main(String[] args) {
        String fileName = "machines/palindrome.tm";
        // FIXME: Input this manually somehow.
        String inputTapeString = "aabbaa";

        try {
            MachineReader reader = new MachineReader();
            Machine machine = reader.loadTMFile(fileName);
            machine.printStateTransitions();

            Tape tape = reader.parseInputTapeString(inputTapeString);

            if (machine.run(tape)) {
                System.out.println("Accept!");
            } else {
                System.out.println("Reject!")
            }
        } catch (IOException e) {
            System.out.println("Error reading TM: " + e.getMessage());
            System.exit(1);
        }
    }
}
