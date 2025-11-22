# Habu Bootstrap Compiler Makefile

CC = gcc
CFLAGS = -Wall -O2
SOURCES = habu-bootstrap.c runtime-minimal.c ir-generation.c code-generation.c encoders.c
OBJECTS = $(SOURCES:.c=.o)
TARGET = habu-bootstrap

# Test sources
TEST_DIR = tests
TESTS = test-integration-simple test-integration-arithmetic test-integration-comparisons test-integration-if

.PHONY: all clean test tests

all: $(TARGET)

$(TARGET): $(OBJECTS)
	$(CC) $(CFLAGS) -o $@ $^

%.o: %.c
	$(CC) $(CFLAGS) -c -o $@ $<

# Run the bootstrap compiler with examples
run: $(TARGET)
	./$(TARGET)

# Build and run all tests
test: tests
	@echo "Running all integration tests..."
	@cd $(TEST_DIR) && ./test-integration-simple
	@cd $(TEST_DIR) && ./test-integration-arithmetic
	@cd $(TEST_DIR) && ./test-integration-comparisons
	@cd $(TEST_DIR) && ./test-integration-if
	@echo ""
	@echo "All tests passed!"

# Build all test binaries
tests:
	@cd $(TEST_DIR) && $(MAKE) all

clean:
	rm -f $(OBJECTS) $(TARGET)
	@cd $(TEST_DIR) && $(MAKE) clean

# Help
help:
	@echo "Habu Bootstrap Compiler"
	@echo ""
	@echo "Targets:"
	@echo "  all     - Build the bootstrap compiler (default)"
	@echo "  run     - Run the compiler with example programs"
	@echo "  test    - Build and run all integration tests"
	@echo "  tests   - Build test binaries only"
	@echo "  clean   - Remove built files"
	@echo "  help    - Show this help"
