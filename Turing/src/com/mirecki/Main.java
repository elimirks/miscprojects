package com.mirecki;

import java.io.File;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;
import java.util.Scanner;

/**
 * TODO:
 * - Verify input tape
 * - Add good ol' Java exceptions instead of using System.exit()
 */

public class Main {
    /**
     * Finds TM files in a given directory.
     * @param dirPath The directory to search.
     * @return A list of TM files.
     */
    private static List<File> findTMFiles(String dirPath) {
        List<File> tmFiles = new LinkedList<File>();
        File directory = new File(dirPath);

        // TODO: Check if the directory really exists

        // Add *.tm in dirPath to the list of TM files
        for (File f : directory.listFiles()) {
            if ( ! f.isFile()) {
                continue;
            }

            if (f.getName().endsWith(".tm")) {
                tmFiles.add(f);
            }
        }

        return tmFiles;
    }

    /**
     * Prompt the user which TM to run.
     * @return The TM File object.
     */
    private static File promptForTMFile() {
        List<File> tmFiles = findTMFiles("machines");

        System.out.println("Select a TM to run:");
        for (int i = 0; i < tmFiles.size(); i++) {
            File tmFile = tmFiles.get(i);
            System.out.printf("%d: %s\n", i + 1, tmFile.getName());
        }

        Scanner scanner = new Scanner(System.in);
        Integer inputIndex = 0;

        while (inputIndex <= 0 || inputIndex > tmFiles.size()) {
            System.out.printf("Which TM would you like to run (1-%d)? ", tmFiles.size());
            try {
                inputIndex = Integer.valueOf(scanner.nextLine());
            } catch (NumberFormatException e) {
                System.out.println("Input an integer!");
            }
        }

        return tmFiles.get(inputIndex.intValue() - 1);
    }

    public static void main(String[] args) {
        File tmFile = promptForTMFile();

        System.out.println("Write an input string: ");
        Scanner scanner = new Scanner(System.in);
        String inputTapeString = scanner.nextLine();

        try {
            MachineReader reader = new MachineReader();
            Machine machine = reader.loadTMFile(tmFile.getAbsolutePath());
            //machine.printStateTransitions();

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
