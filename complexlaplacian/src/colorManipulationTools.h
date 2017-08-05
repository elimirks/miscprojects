#pragma once

#include <stdio.h>
#include <algorithm>
#include <cmath>

typedef struct {
	double r; // percent
	double g; // percent
	double b; // percent
} RGB;

typedef struct {
	double h; // angle in degrees
	double s; // percent
	double v; // percent
} HSV;

// Taken from http://stackoverflow.com/questions/3018313
HSV rgb2hsv(RGB in);
RGB hsv2rgb(HSV in);

// Intensity in percentage
void blendIr(RGB *rgb, double intensity);
void compressHsv(HSV *hsv, double degrees);

