/* Garbage collector benchmarks */

#include "../runtime/habu.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

#define ITERATIONS 100000

static double time_diff_us(uint64_t start, uint64_t end) {
    return (double)(end - start) / 1000.0;
}

static void bench_cons_allocation(void) {
    habu_init(4 * 1024 * 1024);
    habu_disable_gc();

    uint64_t start = habu_time_ns();

    for (int i = 0; i < ITERATIONS; i++) {
        habu_value_t cons = habu_cons(fixnum_to_value(i), NIL);
        (void)cons;
    }

    uint64_t end = habu_time_ns();
    double elapsed = time_diff_us(start, end);
    double per_cons_ns = (double)(end - start) / ITERATIONS;

    printf("Cons allocation (GC disabled):\n");
    printf("  Total: %.2f us\n", elapsed);
    printf("  Per cons: %.2f ns\n", per_cons_ns);
    printf("  Throughput: %.2f M cons/sec\n", ITERATIONS / elapsed);

    habu_shutdown();
}

static void bench_vector_allocation(void) {
    habu_init(4 * 1024 * 1024);
    habu_disable_gc();

    uint64_t start = habu_time_ns();

    for (int i = 0; i < ITERATIONS / 10; i++) {
        habu_value_t vec = habu_make_vector(10);
        (void)vec;
    }

    uint64_t end = habu_time_ns();
    double elapsed = time_diff_us(start, end);
    double per_vec_ns = (double)(end - start) / (ITERATIONS / 10);

    printf("Vector allocation (10 elements, GC disabled):\n");
    printf("  Total: %.2f us\n", elapsed);
    printf("  Per vector: %.2f ns\n", per_vec_ns);
    printf("  Throughput: %.2f M vectors/sec\n", (ITERATIONS / 10) / elapsed);

    habu_shutdown();
}

static void bench_gc_collection(void) {
    habu_init(4 * 1024 * 1024);

    for (int i = 0; i < 1000; i++) {
        habu_cons(fixnum_to_value(i), NIL);
    }

    uint64_t start = habu_time_ns();
    habu_gc_collect();
    uint64_t end = habu_time_ns();

    double pause_us = time_diff_us(start, end);

    printf("GC collection (1000 objects):\n");
    printf("  Pause time: %.2f us\n", pause_us);

    habu_shutdown();
}

static void bench_gc_with_allocation(void) {
    habu_init(4 * 1024 * 1024);
    habu_enable_gc();

    int num_collections = 0;
    uint64_t total_pause = 0;
    uint64_t max_pause = 0;

    uint64_t start = habu_time_ns();

    for (int i = 0; i < ITERATIONS; i++) {
        habu_gc_stats_t stats_before, stats_after;
        habu_gc_get_stats(&stats_before);

        habu_cons(fixnum_to_value(i), NIL);

        habu_gc_get_stats(&stats_after);

        if (stats_after.young_collections > stats_before.young_collections) {
            num_collections++;
            uint64_t pause = stats_after.last_pause_ns;
            total_pause += pause;
            if (pause > max_pause) {
                max_pause = pause;
            }
        }
    }

    uint64_t end = habu_time_ns();
    double total_time = time_diff_us(start, end);

    printf("Allocation with GC enabled (100k allocations):\n");
    printf("  Total time: %.2f us\n", total_time);
    printf("  Collections: %d\n", num_collections);
    if (num_collections > 0) {
        printf("  Average pause: %.2f us\n", time_diff_us(0, total_pause / num_collections));
        printf("  Max pause: %.2f us\n", time_diff_us(0, max_pause));
    }

    habu_shutdown();
}

static void bench_control_loop_with_gc(void) {
    habu_init(4 * 1024 * 1024);
    habu_disable_gc();

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

        habu_vector_set(control, 0, fixnum_to_value(i));

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

    printf("Control loop simulation (regions, GC disabled, 10k iterations):\n");
    printf("  Average: %.2f ns\n", avg);
    printf("  Min: %.2f ns\n", (double)min);
    printf("  Max: %.2f ns\n", (double)max);
    printf("  Stddev: %.2f ns\n", stddev);
    printf("  Total: %.2f us\n", time_diff_us(total_start, total_end));
    printf("  Target: <1000 ns per iteration\n");
    if (avg < 1000.0) {
        printf("  Result: PASS\n");
    } else {
        printf("  Result: FAIL\n");
    }

    free(iteration_times);
    habu_region_destroy(region);
    habu_shutdown();
}

static void bench_mixed_workload(void) {
    habu_init(4 * 1024 * 1024);
    habu_enable_gc();

    uint64_t start = habu_time_ns();

    for (int i = 0; i < 1000; i++) {
        habu_value_t cons = habu_cons(fixnum_to_value(i), NIL);
        habu_value_t vec = habu_make_vector(5);
        habu_value_t str = habu_make_string("test", 4);

        habu_vector_set(vec, 0, cons);
        habu_vector_set(vec, 1, str);

        (void)vec;
    }

    uint64_t end = habu_time_ns();

    habu_gc_stats_t stats;
    habu_gc_get_stats(&stats);

    printf("Mixed workload (1000 iterations):\n");
    printf("  Total time: %.2f us\n", time_diff_us(start, end));
    printf("  Collections: %lu\n", stats.young_collections);
    printf("  Total allocated: %lu bytes\n", stats.total_allocated);
    printf("  Max pause: %.2f us\n", time_diff_us(0, stats.max_pause_ns));

    habu_shutdown();
}

int main(void) {
    printf("Garbage collector benchmarks:\n\n");

    bench_cons_allocation();
    printf("\n");

    bench_vector_allocation();
    printf("\n");

    bench_gc_collection();
    printf("\n");

    bench_gc_with_allocation();
    printf("\n");

    bench_control_loop_with_gc();
    printf("\n");

    bench_mixed_workload();
    printf("\n");

    return 0;
}
