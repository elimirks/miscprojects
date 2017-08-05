#include <stdio.h>
#include <algorithm>
#include <cmath>

#include "colorManipulationTools.h"

/*
int main() {
	HSV huePixel = rgb2hsv({
		r: 0.0,
		g: 1.0,
		b: 0.5,
	});
	printf("%f, %f, %f\n", huePixel.h, huePixel.s, huePixel.v);
	compressHsv(&huePixel, 10.f);
	printf("%f, %f, %f\n", huePixel.h, huePixel.s, huePixel.v);

	RGB rgbPixel = hsv2rgb(huePixel);
	blendIr(&rgbPixel, 0.3);
	printf("%f, %f, %f\n", rgbPixel.r, rgbPixel.g, rgbPixel.b);

	return 0;
}
*/

void blendIr(RGB *rgb, double intensity) {
	// The IR dominanetes more depending on the intensity
	rgb->r *= (1.f - intensity);
	rgb->g *= (1.f - intensity);
	rgb->b *= (1.f - intensity);

	// Blend it up
	rgb->r += intensity;
}

void compressHsv(HSV *hsv, double degrees) {
	const double huePercent = hsv->h / 360.f;
	const double newAmplitude = 360.f - degrees;
	hsv->h = (huePercent * newAmplitude) + degrees;
}

HSV rgb2hsv(RGB in) {
	HSV         out;
	double      min, max, delta;

	min = in.r < in.g ? in.r : in.g;
	min = min  < in.b ? min  : in.b;

	max = in.r > in.g ? in.r : in.g;
	max = max  > in.b ? max  : in.b;

	out.v = max;                                // v
	delta = max - min;
	if (delta < 0.00001)
	{
		out.s = 0;
		out.h = 0; // undefined, maybe nan?
		return out;
	}
	if( max > 0.0 ) { // NOTE: if Max is == 0, this divide would cause a crash
		out.s = (delta / max);                  // s
	} else {
		// if max is 0, then r = g = b = 0              
		// s = 0, v is undefined
		out.s = 0.0;
		out.h = NAN;                            // its now undefined
		return out;
	}
	if( in.r >= max )                           // > is bogus, just keeps compilor happy
		out.h = ( in.g - in.b ) / delta;        // between yellow & magenta
	else
		if( in.g >= max )
			out.h = 2.0 + ( in.b - in.r ) / delta;  // between cyan & yellow
		else
			out.h = 4.0 + ( in.r - in.g ) / delta;  // between magenta & cyan

	out.h *= 60.0;                              // degrees

	if( out.h < 0.0 )
		out.h += 360.0;

	return out;
}

RGB hsv2rgb(HSV in) {
	double      hh, p, q, t, ff;
	long        i;
	RGB         out;

	if(in.s <= 0.0) {       // < is bogus, just shuts up warnings
		out.r = in.v;
		out.g = in.v;
		out.b = in.v;
		return out;
	}
	hh = in.h;
	if(hh >= 360.0) hh = 0.0;
	hh /= 60.0;
	i = (long)hh;
	ff = hh - i;
	p = in.v * (1.0 - in.s);
	q = in.v * (1.0 - (in.s * ff));
	t = in.v * (1.0 - (in.s * (1.0 - ff)));

	switch(i) {
		case 0:
			out.r = in.v;
			out.g = t;
			out.b = p;
			break;
		case 1:
			out.r = q;
			out.g = in.v;
			out.b = p;
			break;
		case 2:
			out.r = p;
			out.g = in.v;
			out.b = t;
			break;

		case 3:
			out.r = p;
			out.g = q;
			out.b = in.v;
			break;
		case 4:
			out.r = t;
			out.g = p;
			out.b = in.v;
			break;
		case 5:
		default:
			out.r = in.v;
			out.g = p;
			out.b = q;
			break;
	}
	return out;     
}

