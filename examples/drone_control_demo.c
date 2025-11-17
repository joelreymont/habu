/* Drone control loop demonstration
 *
 * Shows how to use region allocator for hard real-time control loop
 * with zero GC pauses, while background tasks can use normal GC.
 */

#include "../runtime/habu.h"
#include <stdio.h>
#include <stdlib.h>
#include <math.h>

typedef struct {
    habu_value_t gyro;
    habu_value_t accel;
    habu_value_t gps;
} sensor_data_t;

typedef struct {
    habu_value_t attitude;
    habu_value_t position;
    habu_value_t velocity;
} state_t;

typedef struct {
    habu_value_t motor_outputs;
} control_t;

static sensor_data_t read_sensors(habu_region_t *region, int iteration) {
    sensor_data_t data;
    data.gyro = habu_region_make_vector(region, 3);
    data.accel = habu_region_make_vector(region, 3);
    data.gps = habu_region_make_vector(region, 3);

    habu_vector_set(data.gyro, 0, fixnum_to_value(iteration % 100));
    habu_vector_set(data.accel, 0, fixnum_to_value(iteration % 50));
    habu_vector_set(data.gps, 0, fixnum_to_value(iteration % 200));

    return data;
}

static state_t estimate_state(habu_region_t *region, sensor_data_t *sensors) {
    state_t state;
    state.attitude = habu_region_make_vector(region, 3);
    state.position = habu_region_make_vector(region, 3);
    state.velocity = habu_region_make_vector(region, 3);

    habu_vector_set(state.attitude, 0, habu_vector_ref(sensors->gyro, 0));
    habu_vector_set(state.position, 0, habu_vector_ref(sensors->gps, 0));

    return state;
}

static control_t compute_control(habu_region_t *region, state_t *state) {
    control_t control;
    control.motor_outputs = habu_region_make_vector(region, 4);

    habu_value_t pos = habu_vector_ref(state->position, 0);
    habu_vector_set(control.motor_outputs, 0, pos);
    habu_vector_set(control.motor_outputs, 1, pos);
    habu_vector_set(control.motor_outputs, 2, pos);
    habu_vector_set(control.motor_outputs, 3, pos);

    return control;
}

static void apply_control(control_t *control) {
    (void)control;
}

static void control_loop_iteration(habu_region_t *region, int iteration) {
    sensor_data_t sensors = read_sensors(region, iteration);
    state_t state = estimate_state(region, &sensors);
    control_t control = compute_control(region, &state);
    apply_control(&control);
}

static void run_control_loop(void) {
    habu_region_t *region = habu_region_create(256 * 1024);
    const int iterations = 1000;

    uint64_t *times = malloc(iterations * sizeof(uint64_t));
    uint64_t total_start = habu_time_ns();

    for (int i = 0; i < iterations; i++) {
        uint64_t start = habu_time_ns();

        control_loop_iteration(region, i);

        habu_region_reset(region);

        uint64_t end = habu_time_ns();
        times[i] = end - start;
    }

    uint64_t total_end = habu_time_ns();

    uint64_t min = times[0], max = times[0], sum = 0;
    for (int i = 0; i < iterations; i++) {
        if (times[i] < min) min = times[i];
        if (times[i] > max) max = times[i];
        sum += times[i];
    }

    double avg = (double)sum / iterations;
    double variance = 0;
    for (int i = 0; i < iterations; i++) {
        double diff = (double)times[i] - avg;
        variance += diff * diff;
    }
    double stddev = sqrt(variance / iterations);

    printf("Control Loop Performance:\n");
    printf("  Iterations: %d\n", iterations);
    printf("  Average: %.2f ns\n", avg);
    printf("  Min: %lu ns\n", min);
    printf("  Max: %lu ns\n", max);
    printf("  Stddev: %.2f ns\n", stddev);
    printf("  Total time: %.2f us\n", (double)(total_end - total_start) / 1000.0);
    printf("  Frequency: %.2f kHz\n", (iterations * 1000000.0) / (total_end - total_start));
    printf("\n");
    printf("Real-time Compliance:\n");
    printf("  Max latency: %lu ns = %.2f us\n", max, (double)max / 1000.0);
    printf("  Target: <1000 us (1 ms)\n");
    printf("  Status: %s\n", (max < 1000000) ? "PASS" : "FAIL");

    free(times);
    habu_region_destroy(region);
}

static void background_telemetry_task(void) {
    printf("\nBackground Task (with GC):\n");

    habu_value_t log_entries = NIL;

    for (int i = 0; i < 100; i++) {
        habu_value_t timestamp = fixnum_to_value(i);
        habu_value_t data = habu_make_vector(5);
        habu_value_t entry = habu_cons(timestamp, data);

        log_entries = habu_cons(entry, log_entries);
    }

    int count = 0;
    while (!is_nil(log_entries)) {
        count++;
        log_entries = habu_cdr(log_entries);
    }

    printf("  Log entries created: %d\n", count);

    habu_gc_stats_t stats;
    habu_gc_get_stats(&stats);
    printf("  GC collections: %lu\n", stats.young_collections);
    printf("  Bytes allocated: %lu\n", stats.total_allocated);
    printf("  Max GC pause: %.2f us\n", (double)stats.max_pause_ns / 1000.0);
}

int main(void) {
    printf("Habu Drone Control Demonstration\n");
    printf("=================================\n\n");

    habu_init(4 * 1024 * 1024);

    printf("Phase 1: Hard Real-Time Control Loop\n");
    printf("     (using regions, GC disabled)\n\n");

    habu_disable_gc();
    run_control_loop();

    printf("\nPhase 2: Background Telemetry Processing\n");
    printf("     (using GC, soft real-time)\n");

    habu_enable_gc();
    background_telemetry_task();

    habu_shutdown();

    printf("\nDemonstration complete.\n");
    return 0;
}
