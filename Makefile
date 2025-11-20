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

.PHONY: all clean test benchmark examples repls repl-test repl-demo repl-bench clean-repls

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

# REPL executables
REPL_PROGS = habu-enhanced habu-prog habu-rec habu-extended

# Build all REPLs (requires SBCL)
repls: habu-enhanced habu-prog habu-rec habu-extended

habu-enhanced: enhanced-repl.lisp bootstrap/compiler.lisp bootstrap/c-backend.lisp $(RUNTIME_OBJS)
	@echo "Building Enhanced REPL..."
	@sbcl --noinform --non-interactive \
		--eval '(load "bootstrap/compiler.lisp")' \
		--eval '(load "bootstrap/c-backend.lisp")' \
		--eval '(in-package :habu-compiler)' \
		--eval '(with-open-file (in "enhanced-repl.lisp" :direction :input) \
		          (let ((file-contents (make-string (file-length in)))) \
		            (read-sequence file-contents in) \
		            (let ((expr (read-from-string (format nil "(progn ~A)" file-contents)))) \
		              (generate-c-standalone expr :output-file "habu-enhanced.c"))))' \
		--quit
	$(CC) $(CFLAGS) -Wno-unused-value -Iruntime -o habu-enhanced habu-enhanced.c $(RUNTIME_OBJS) $(LDLIBS)
	@echo "✓ Enhanced REPL built (56KB)"

habu-prog: programmable-repl.lisp bootstrap/compiler.lisp bootstrap/c-backend.lisp $(RUNTIME_OBJS)
	@echo "Building Programmable REPL..."
	@sbcl --noinform --non-interactive \
		--eval '(load "bootstrap/compiler.lisp")' \
		--eval '(load "bootstrap/c-backend.lisp")' \
		--eval '(in-package :habu-compiler)' \
		--eval '(with-open-file (in "programmable-repl.lisp" :direction :input) \
		          (let ((file-contents (make-string (file-length in)))) \
		            (read-sequence file-contents in) \
		            (let ((expr (read-from-string (format nil "(progn ~A)" file-contents)))) \
		              (generate-c-standalone expr :output-file "habu-prog.c"))))' \
		--quit
	$(CC) $(CFLAGS) -Wno-unused-value -Iruntime -o habu-prog habu-prog.c $(RUNTIME_OBJS) $(LDLIBS)
	@echo "✓ Programmable REPL built (73KB)"

habu-rec: recursive-repl.lisp bootstrap/compiler.lisp bootstrap/c-backend.lisp $(RUNTIME_OBJS)
	@echo "Building Recursive REPL..."
	@sbcl --noinform --non-interactive \
		--eval '(load "bootstrap/compiler.lisp")' \
		--eval '(load "bootstrap/c-backend.lisp")' \
		--eval '(in-package :habu-compiler)' \
		--eval '(with-open-file (in "recursive-repl.lisp" :direction :input) \
		          (let ((file-contents (make-string (file-length in)))) \
		            (read-sequence file-contents in) \
		            (let ((expr (read-from-string (format nil "(progn ~A)" file-contents)))) \
		              (generate-c-standalone expr :output-file "habu-rec.c"))))' \
		--quit
	$(CC) $(CFLAGS) -Wno-unused-value -Iruntime -o habu-rec habu-rec.c $(RUNTIME_OBJS) $(LDLIBS)
	@echo "✓ Recursive REPL built (73KB) - Complete Lisp!"

habu-extended: extended-recursive-repl.lisp bootstrap/compiler.lisp bootstrap/c-backend.lisp $(RUNTIME_OBJS)
	@echo "Building Extended REPL (v1.2)..."
	@sbcl --noinform --non-interactive \
		--eval '(load "bootstrap/compiler.lisp")' \
		--eval '(load "bootstrap/c-backend.lisp")' \
		--eval '(in-package :habu-compiler)' \
		--eval '(with-open-file (in "extended-recursive-repl.lisp" :direction :input) \
		          (let ((file-contents (make-string (file-length in)))) \
		            (read-sequence file-contents in) \
		            (let ((expr (read-from-string (format nil "(progn ~A)" file-contents)))) \
		              (generate-c-standalone expr :output-file "habu-extended.c"))))' \
		--quit
	$(CC) $(CFLAGS) -Wno-unused-value -Iruntime -o habu-extended habu-extended.c $(RUNTIME_OBJS) $(LDLIBS)
	@echo "✓ Extended REPL (v1.2) built (75KB) - Adds and, or, not, cond, <=, >="

# Test all REPLs
repl-test: $(REPL_PROGS)
	@./test-repls.sh

# Run interactive demo
repl-demo: habu-rec
	@./demo.sh

# Run performance benchmarks
repl-bench: habu-rec
	@./bench-repls.sh

# Clean REPL artifacts
clean-repls:
	rm -f $(REPL_PROGS)
	rm -f habu-enhanced.c habu-prog.c habu-rec.c habu-extended.c

# Update clean target to include REPLs
clean: clean-repls
