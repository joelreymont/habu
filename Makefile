.POSIX:
.SUFFIXES:

CC = gcc
CFLAGS = -std=c11 -Wall -Wextra -O2 -g
LDFLAGS =
LDLIBS = -lm

# Cross-compilation support
CROSS_CC = aarch64-linux-gnu-gcc
CROSS_CFLAGS = $(CFLAGS)
CROSS_LDFLAGS = $(LDFLAGS)

# Source files
RUNTIME_SRCS = runtime/runtime.c runtime/region.c runtime/gc.c runtime/lineedit.c runtime/io.c
RUNTIME_OBJS = $(RUNTIME_SRCS:.c=.o)

# Test files
TEST_SRCS = tests/test_platform.c tests/test_region.c tests/test_gc.c tests/test_roots.c tests/test_compiler_simple.c tests/test_compiled_execution.c
TEST_PROGS = $(TEST_SRCS:.c=)

# Benchmark files
BENCH_SRCS = benchmarks/bench_region.c benchmarks/bench_gc.c
BENCH_PROGS = $(BENCH_SRCS:.c=)

# Example programs
EXAMPLE_SRCS = examples/drone_control_demo.c examples/root_examples.c
EXAMPLE_PROGS = $(EXAMPLE_SRCS:.c=)

# JIT helper library
JIT_LIB_DYLIB = libhabu-jit.dylib
JIT_LIB_SO = libhabu-jit.so
PRINT_RUNTIME = bin/print-runtime-addrs

# Main executables
HABU = habu
RUN_BYTECODE = run-bytecode
RUN_FASL = run-fasl

.PHONY: all clean test benchmark examples habu-tools

all: $(TEST_PROGS) habu-tools

habu-tools: $(HABU) $(RUN_BYTECODE) $(RUN_FASL)

examples: $(EXAMPLE_PROGS)
jit: $(JIT_LIB_DYLIB) $(JIT_LIB_SO)
runtime-addrs: $(PRINT_RUNTIME)

# Build runtime object files
runtime/%.o: runtime/%.c runtime/*.h
	$(CC) $(CFLAGS) -c -o $@ $<

# Build tests
tests/test_%: tests/test_%.c $(RUNTIME_OBJS)
	$(CC) $(CFLAGS) -o $@ $< $(RUNTIME_OBJS) $(LDLIBS)

# Build benchmarks
benchmarks/bench_%: benchmarks/bench_%.c $(RUNTIME_OBJS)
	$(CC) $(CFLAGS) -o $@ $< $(RUNTIME_OBJS) $(LDLIBS)

# Build examples
examples/%: examples/%.c $(RUNTIME_OBJS)
	$(CC) $(CFLAGS) -o $@ $< $(RUNTIME_OBJS) $(LDLIBS)

# Build tiny JIT helper (macOS .dylib and generic .so)
$(JIT_LIB_DYLIB): habu-jit.c runtime/habu.h
	$(CC) $(CFLAGS) -shared -fPIC -o $@ $< $(LDLIBS)

$(JIT_LIB_SO): habu-jit.c runtime/habu.h
	$(CC) $(CFLAGS) -shared -fPIC -o $@ $< $(LDLIBS)

$(PRINT_RUNTIME): bin/print-runtime-addrs.c runtime/habu.h
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
	rm -f $(EXAMPLE_PROGS)
	rm -f $(JIT_LIB_DYLIB) $(JIT_LIB_SO)
	rm -f $(PRINT_RUNTIME)

# Cross-compile for ARM64
cross: CC = $(CROSS_CC)
cross: CFLAGS = $(CROSS_CFLAGS)
cross: LDFLAGS = $(CROSS_LDFLAGS)
cross: all

# Build habu main executable (REPL and runner)
$(HABU): habu-main.c $(RUNTIME_OBJS) runtime/habu.h
	$(CC) $(CFLAGS) -o $@ habu-main.c $(RUNTIME_OBJS) $(LDLIBS)

# Build run-bytecode (legacy executor)
$(RUN_BYTECODE): run-bytecode.c $(RUNTIME_OBJS) runtime/habu.h
	$(CC) $(CFLAGS) -o $@ run-bytecode.c $(RUNTIME_OBJS) $(LDLIBS)

# Build run-fasl (FASL loader and executor)
$(RUN_FASL): run-fasl.c $(RUNTIME_OBJS) runtime/habu.h
	$(CC) $(CFLAGS) -o $@ run-fasl.c $(RUNTIME_OBJS) $(LDLIBS)

# Install to /usr/local/bin
install: $(HABU) $(RUN_BYTECODE) $(RUN_FASL)
	install -m 755 $(HABU) /usr/local/bin/
	install -m 755 $(RUN_BYTECODE) /usr/local/bin/
	install -m 755 $(RUN_FASL) /usr/local/bin/

# Clean build artifacts (runtime objs, tests, benches, examples)
clean:
	rm -f $(TEST_PROGS) $(BENCH_PROGS) $(EXAMPLE_PROGS) $(RUNTIME_OBJS)
	rm -f $(HABU) $(RUN_BYTECODE) $(RUN_FASL)
	rm -f $(JIT_LIB_DYLIB) $(JIT_LIB_SO) $(PRINT_RUNTIME)
