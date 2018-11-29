// Naive implementation for finding primes

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

uint64_t *prime_nums;
size_t prime_number_count = 0;
size_t prime_number_size  = 1024;

int is_prime(uint64_t candidate) {
    for (size_t i = 0; i < prime_number_count; i++) {
        uint64_t p = prime_nums[i];

        if (p * p > candidate) {
            break;
        }

        if (candidate % p == 0) {
            return 0;
        }
    }

    if (prime_number_count == prime_number_size) {
        prime_number_size <<= 1;
        prime_nums = realloc(prime_nums, sizeof(uint64_t) * prime_number_size);
    }

    prime_nums[prime_number_count++] = candidate;

    return 1;
}

int main() {
    prime_nums = malloc(sizeof(uint64_t) * prime_number_size);
    prime_number_count = 4;
    prime_nums[0] = 2;
    prime_nums[1] = 3;
    prime_nums[2] = 5;
    prime_nums[3] = 7;

    printf("2\n");
    printf("3\n");
    printf("5\n");
    printf("7\n");

    uint64_t base = 10;

    while (1) {
        if (is_prime(base + 1)) {
            printf("%llu\n", base + 1);
        }
        if (is_prime(base + 3)) {
            printf("%llu\n", base + 3);
        }
        if (is_prime(base + 7)) {
            printf("%llu\n", base + 7);
        }
        if (is_prime(base + 9)) {
            printf("%llu\n", base + 9);
        }

        base += 10;
    }
}
