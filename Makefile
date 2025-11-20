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

.PHONY: all clean test benchmark examples habu clean-habu

all: $(TEST_PROGS)

examples: $(EXAMPLE_PROGS)

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

# Cross-compile for ARM64
cross: CC = $(CROSS_CC)
cross: CFLAGS = $(CROSS_CFLAGS)
cross: LDFLAGS = $(CROSS_LDFLAGS)
cross: all

# ============================================
# REPL Targets
# ============================================

# Build Habu Lisp (requires SBCL)
# Like SBCL, the binary is just 'habu' and launches REPL by default
habu: habu-repl.lisp bootstrap/compiler.lisp bootstrap/c-backend.lisp $(RUNTIME_OBJS)
	@echo "Building Habu Lisp..."
	@sbcl --noinform --non-interactive \
		--eval '(load "bootstrap/compiler.lisp")' \
		--eval '(load "bootstrap/c-backend.lisp")' \
		--eval '(in-package :habu-compiler)' \
		--eval '(with-open-file (in "habu-repl.lisp" :direction :input) \
		          (let ((file-contents (make-string (file-length in)))) \
		            (read-sequence file-contents in) \
		            (let ((expr (read-from-string (format nil "(progn ~A)" file-contents)))) \
		              (generate-c-standalone expr :output-file "habu.c"))))' \
		--quit 2>&1 | grep -v "^;"
	@echo "Generated C code: habu.c"
	$(CC) $(CFLAGS) -Wno-unused-value -Iruntime -o habu habu.c $(RUNTIME_OBJS) $(LDLIBS)
	@echo "✓ Habu Lisp built ($(shell ls -lh habu | awk '{print $$5}'))"
	@echo ""
	@echo "Usage:"
	@echo "  ./habu              # Launch REPL"
	@echo "  echo '(+ 1 2)' | ./habu   # Evaluate expression"

# Clean Habu artifacts
clean-habu:
	rm -f habu habu.c

# Update clean target to include Habu
clean: clean-habu
