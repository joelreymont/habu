# Integration Tests Makefile

CC = gcc
CFLAGS = -Wall -O2
PARENT = ..

# Common dependencies
DEPS = $(PARENT)/runtime-minimal.c $(PARENT)/ir-generation.c $(PARENT)/code-generation.c $(PARENT)/encoders.c

# Test targets
TESTS = test-integration-simple test-integration-arithmetic test-integration-comparisons test-integration-if test-reader

.PHONY: all clean

all: $(TESTS)

test-integration-simple: test-integration-simple.c $(DEPS)
	$(CC) $(CFLAGS) -o $@ $^

test-integration-arithmetic: test-integration-arithmetic.c $(DEPS)
	$(CC) $(CFLAGS) -o $@ $^

test-integration-comparisons: test-integration-comparisons.c $(DEPS)
	$(CC) $(CFLAGS) -o $@ $^

test-integration-if: test-integration-if.c $(DEPS)
	$(CC) $(CFLAGS) -o $@ $^

test-reader: test-reader.c $(PARENT)/runtime-minimal.c $(PARENT)/reader.c
	$(CC) $(CFLAGS) -o $@ $^

clean:
	rm -f $(TESTS) *.o
