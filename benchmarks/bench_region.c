/* Region allocator benchmarks */

#include "../runtime/habu.h"
#include <stdio.h>
#include <stdlib.h>
#include <time.h>
#include <math.h>

#define ITERATIONS 1000000

static double time_diff_ms(uint64_t start, uint64_t end) {
    return (double)(end - start) / 1000000.0;
}

static void bench_region_alloc_small(void) {
    habu_region_t *region = habu_region_create(16 * 1024 * 1024);
    uint64_t start = habu_time_ns();

    for (int i = 0; i < ITERATIONS; i++) {
        void *p = habu_region_alloc(region, 16);
        (void)p;
    }

    uint64_t end = habu_time_ns();
    double elapsed = time_diff_ms(start, end);
    double per_alloc_ns = (double)(end - start) / ITERATIONS;

    printf("Small allocation (16 bytes):\n");
    printf("  Total: %.2f ms\n", elapsed);
    printf("  Per allocation: %.2f ns\n", per_alloc_ns);
    printf("  Throughput: %.2f M allocs/sec\n", ITERATIONS / (elapsed * 1000.0));

    habu_region_destroy(region);
}

static void bench_region_alloc_medium(void) {
    habu_region_t *region = habu_region_create(256 * 1024 * 1024);
    uint64_t start = habu_time_ns();

    for (int i = 0; i < ITERATIONS; i++) {
        void *p = habu_region_alloc(region, 256);
        (void)p;
    }

    uint64_t end = habu_time_ns();
    double elapsed = time_diff_ms(start, end);
    double per_alloc_ns = (double)(end - start) / ITERATIONS;

    printf("Medium allocation (256 bytes):\n");
    printf("  Total: %.2f ms\n", elapsed);
    printf("  Per allocation: %.2f ns\n", per_alloc_ns);
    printf("  Throughput: %.2f M allocs/sec\n", ITERATIONS / (elapsed * 1000.0));

    habu_region_destroy(region);
}

static void bench_region_reset(void) {
    habu_region_t *region = habu_region_create(1024 * 1024);
    uint64_t start = habu_time_ns();

    for (int i = 0; i < ITERATIONS; i++) {
        habu_region_alloc(region, 100);
        habu_region_reset(region);
    }

    uint64_t end = habu_time_ns();
    double elapsed = time_diff_ms(start, end);
    double per_op_ns = (double)(end - start) / ITERATIONS;

    printf("Allocation + reset:\n");
    printf("  Total: %.2f ms\n", elapsed);
    printf("  Per operation: %.2f ns\n", per_op_ns);
    printf("  Throughput: %.2f M ops/sec\n", ITERATIONS / (elapsed * 1000.0));

    habu_region_destroy(region);
}

static void bench_region_cons(void) {
    habu_region_t *region = habu_region_create(64 * 1024 * 1024);
    habu_value_t car = fixnum_to_value(1);
    habu_value_t cdr = fixnum_to_value(2);

    uint64_t start = habu_time_ns();

    for (int i = 0; i < ITERATIONS; i++) {
        habu_value_t cons = habu_region_cons(region, car, cdr);
        (void)cons;
    }

    uint64_t end = habu_time_ns();
    double elapsed = time_diff_ms(start, end);
    double per_cons_ns = (double)(end - start) / ITERATIONS;

    printf("Cons allocation:\n");
    printf("  Total: %.2f ms\n", elapsed);
    printf("  Per cons: %.2f ns\n", per_cons_ns);
    printf("  Throughput: %.2f M cons/sec\n", ITERATIONS / (elapsed * 1000.0));

    habu_region_destroy(region);
}

static void bench_control_loop_simulation(void) {
    habu_region_t *region = habu_region_create(256 * 1024);
    int loop_iterations = 10000;

    uint64_t *iteration_times = malloc(loop_iterations * sizeof(uint64_t));
    uint64_t total_start = habu_time_ns();

    for (int i = 0; i < loop_iterations; i++) {
        uint64_t iter_start = habu_time_ns();

        habu_value_t sensor1 = habu_region_cons(region, fixnum_to_value(i), NIL);
        habu_value_t sensor2 = habu_region_cons(region, fixnum_to_value(i * 2), NIL);
        habu_value_t state = habu_region_cons(region, sensor1, sensor2);
        habu_value_t control = habu_region_make_vector(region, 4);

        (void)state;
        (void)control;

        habu_region_reset(region);

        uint64_t iter_end = habu_time_ns();
        iteration_times[i] = iter_end - iter_start;
    }

    uint64_t total_end = habu_time_ns();

    uint64_t min = iteration_times[0];
    uint64_t max = iteration_times[0];
    uint64_t sum = 0;

    for (int i = 0; i < loop_iterations; i++) {
        uint64_t t = iteration_times[i];
        if (t < min) min = t;
        if (t > max) max = t;
        sum += t;
    }

    double avg = (double)sum / loop_iterations;
    double variance = 0;
    for (int i = 0; i < loop_iterations; i++) {
        double diff = (double)iteration_times[i] - avg;
        variance += diff * diff;
    }
    double stddev = sqrt(variance / loop_iterations);

    printf("Control loop simulation (10k iterations):\n");
    printf("  Average: %.2f ns\n", avg);
    printf("  Min: %lu ns\n", min);
    printf("  Max: %lu ns\n", max);
    printf("  Stddev: %.2f ns\n", stddev);
    printf("  Total: %.2f ms\n", time_diff_ms(total_start, total_end));

    free(iteration_times);
    habu_region_destroy(region);
}

int main(void) {
    printf("Region allocator benchmarks:\n\n");

    bench_region_alloc_small();
    printf("\n");

    bench_region_alloc_medium();
    printf("\n");

    bench_region_reset();
    printf("\n");

    bench_region_cons();
    printf("\n");

    bench_control_loop_simulation();
    printf("\n");

    return 0;
}
