#include <immintrin.h>
#include <stdio.h>

// https://gcc.gnu.org/onlinedocs/gcc/Vector-Extensions.html
typedef double v4sd __attribute__ ((vector_size (32)));

typedef union {
    v4sd vec;
    double elem[4];
} dvec4;

void dvec4_print(dvec4 vec) {
    printf("%f, %f, %f, %f\n",
           vec.elem[0],
           vec.elem[1],
           vec.elem[2],
           vec.elem[3]);
}

int main() {
    dvec4 a = {
        1.5,
        2.5,
        3.0,
        8.0
    };

    dvec4 b = {
        1.5,
        -2.5,
        3.0,
        7.0
    };

 d :e ec4 c = ((v4sd)a.vec) > ((v4sd)b.vec);

    dvec4_print(a);
    dvec4_print(b);
    dvec4_print(c);
}
