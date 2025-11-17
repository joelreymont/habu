.POSIX:
.SUFFIXES:

CC = gcc
CFLAGS = -std=c11 -Wall -Wextra -Werror -O2 -g
LDFLAGS =
LDLIBS = -lm

# Cross-compilation support
CROSS_CC = aarch64-linux-gnu-gcc
CROSS_CFLAGS = $(CFLAGS)
CROSS_LDFLAGS = $(LDFLAGS)

# Source files
RUNTIME_SRCS = runtime/runtime.c runtime/region.c
RUNTIME_OBJS = $(RUNTIME_SRCS:.c=.o)

# Test files
TEST_SRCS = tests/test_region.c
TEST_PROGS = $(TEST_SRCS:.c=)

# Benchmark files
BENCH_SRCS = benchmarks/bench_region.c
BENCH_PROGS = $(BENCH_SRCS:.c=)

.PHONY: all clean test benchmark

all: $(TEST_PROGS)

# Build runtime object files
runtime/%.o: runtime/%.c runtime/*.h
	$(CC) $(CFLAGS) -c -o $@ $<

# Build tests
tests/test_region: tests/test_region.c $(RUNTIME_OBJS)
	$(CC) $(CFLAGS) -o $@ $< $(RUNTIME_OBJS) $(LDLIBS)

# Build benchmarks
benchmarks/bench_region: benchmarks/bench_region.c $(RUNTIME_OBJS)
	$(CC) $(CFLAGS) -o $@ $< $(RUNTIME_OBJS) $(LDLIBS)

# Run tests
test: $(TEST_PROGS)
	@echo "Running tests..."
	@for test in $(TEST_PROGS); do \
		echo ""; \
		./$$test || exit 1; \
	done
	@echo ""
	@echo "All tests passed"

# Run benchmarks
benchmark: $(BENCH_PROGS)
	@echo "Running benchmarks..."
	@for bench in $(BENCH_PROGS); do \
		echo ""; \
		./$$bench; \
	done

# Clean build artifacts
clean:
	rm -f $(RUNTIME_OBJS)
	rm -f $(TEST_PROGS)
	rm -f $(BENCH_PROGS)

# Cross-compile for ARM64
cross: CC = $(CROSS_CC)
cross: CFLAGS = $(CROSS_CFLAGS)
cross: LDFLAGS = $(CROSS_LDFLAGS)
cross: all
