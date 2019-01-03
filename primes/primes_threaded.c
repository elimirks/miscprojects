#include <pthread.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define BATCH_SIZE (1 << 14)
#define THREAD_COUNT (8)

uint64_t *prime_nums;
size_t prime_number_count = 0;
size_t prime_number_size = 1 << 20; // Nice, hefty initial size

struct thread_data {
    uint64_t begin;
    uint64_t out_primes[BATCH_SIZE];
    uint64_t out_count;
};

struct prime_thread_data {
    uint64_t *prime_nums;
    size_t count;
};

void add_prime(uint64_t prime) {
    if (prime_number_count == prime_number_size) {
        prime_number_size <<= 1;
        prime_nums = realloc(prime_nums, sizeof(uint64_t) * prime_number_size);
    }

    prime_nums[prime_number_count++] = prime;
}

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

    return 1;
}

// Thread function to find primes in a given range
void *find_primes(void *input) {
    struct thread_data *data = input;
    uint64_t batch_begin = data->begin;
    uint64_t batch_end = batch_begin + BATCH_SIZE;

    data->out_count = 0;

    for (uint64_t base = batch_begin; base < batch_end; base += 10) {
        if (is_prime(base + 1)) {
            data->out_primes[(data->out_count)++] = base + 1;
        }
        if (is_prime(base + 3)) {
            data->out_primes[(data->out_count)++] = base + 3;
        }
        if (is_prime(base + 7)) {
            data->out_primes[(data->out_count)++] = base + 7;
        }
        if (is_prime(base + 9)) {
            data->out_primes[(data->out_count)++] = base + 9;
        }
    }

    return (void*)data;
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

    // Compute the first 1000 prime numbers on the main thread.
    uint64_t base = 10;
    while (base < 1000) {
        if (is_prime(base + 1)) {
            add_prime(base + 1);
            printf("%llu\n", base + 1);
        }
        if (is_prime(base + 3)) {
            add_prime(base + 3);
            printf("%llu\n", base + 3);
        }
        if (is_prime(base + 7)) {
            add_prime(base + 7);
            printf("%llu\n", base + 7);
        }
        if (is_prime(base + 9)) {
            add_prime(base + 9);
            printf("%llu\n", base + 9);
        }

        base += 10;
    }

    pthread_t threads[THREAD_COUNT];
    struct thread_data results[THREAD_COUNT];

    while (1) {
        // Create threads
        for (int i = 0; i < THREAD_COUNT; i++) {
            pthread_t *t = &threads[i];
            struct thread_data *res = &results[i];
            res->begin = base + i * BATCH_SIZE;

            if (pthread_create(t, 0, find_primes, res)) {
                fprintf(stderr, "Error creating thread.");
                return 1;
            }
        }

        // Join them, and accumulate
        for (int i = 0; i < THREAD_COUNT; i++) {
            pthread_t *t = &threads[i];
            struct thread_data *res = &results[i];

            if (pthread_join(*t, 0)) {
                fprintf(stderr, "Error joining thread.");
            }

            for (size_t j = 0; j < res->out_count; j++) {
                uint64_t p = res->out_primes[j];
                add_prime(p);
								printf("%llu\n", p);
            }
        }

        base += THREAD_COUNT * BATCH_SIZE;
    }
}
