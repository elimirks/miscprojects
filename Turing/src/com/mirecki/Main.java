package com.mirecki;

import java.io.IOException;
import java.util.Scanner;

public class Main {
    public static void main(String[] args) {
        String fileName = "machines/palindrome.tm";

        System.out.println("Write an input string: ");
        Scanner scanner = new Scanner(System.in);
        String inputTapeString = scanner.nextLine();

        try {
            MachineReader reader = new MachineReader();
            Machine machine = reader.loadTMFile(fileName);
            machine.printStateTransitions();

            Tape tape = reader.parseInputTapeString(inputTapeString);

            if (machine.run(tape)) {
                System.out.println("Accept!");
            } else {
                System.out.println("Reject!");
            }
        } catch (IOException e) {
            System.out.println("Error reading TM: " + e.getMessage());
            System.exit(1);
        }
    }
}
