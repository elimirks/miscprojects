#!/bin/sh

gcc -std=gnu11 -O3 main.c -o maze && ./maze small.txt

