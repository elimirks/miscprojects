#!/bin/sh

gcc -std=gnu11 -g main.c graph.c -o maze && ./maze small.txt

