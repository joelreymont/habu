# Contributing to Habu REPL

Thank you for your interest in contributing to Habu Lisp! This guide will help you understand how to build, test, and modify the REPL implementations.

## Table of Contents

- [Getting Started](#getting-started)
- [Project Structure](#project-structure)
- [Building the REPLs](#building-the-repls)
- [Testing](#testing)
- [Development Workflow](#development-workflow)
- [Code Style](#code-style)
- [Adding Features](#adding-features)
- [Documentation](#documentation)

## Getting Started

### Prerequisites

- **SBCL** (Steel Bank Common Lisp) - for compiling Lisp to C
- **GCC or Clang** - for compiling C to native code
- **Make** - for build automation
- **Git** - for version control

### Quick Start

```bash
# Clone the repository
git clone <repository-url>
cd habu

# Build all REPLs
make repls

# Run tests
make repl-test

# Try the complete Lisp REPL
./habu-rec
```

## Project Structure

```
habu/
├── runtime/              # C runtime (minimal)
│   ├── runtime.c        # Core primitives (GC, cons, etc.)
│   ├── gc.c             # Garbage collector
│   ├── lineedit.c       # Line editing for REPL
│   └── habu.h           # Header file
├── bootstrap/           # Compilation infrastructure
│   ├── compiler.lisp    # Lisp-to-internal compiler
│   └── c-backend.lisp   # Internal-to-C code generator
├── enhanced-repl.lisp   # REPL with quote & symbols (235 lines)
├── programmable-repl.lisp  # + let & lambda (282 lines)
├── recursive-repl.lisp  # + defun & recursion (320 lines)
├── stdlib.lisp          # Standard library functions
├── examples.lisp        # Example programs
├── test-repls.sh        # Automated test suite
├── demo.sh              # Interactive demonstration
└── Makefile             # Build automation
```

## Building the REPLs

### Build Individual REPLs

```bash
# Enhanced REPL (quote, symbols, if, lists)
make habu-enhanced

# Programmable REPL (+ let, lambda, closures)
make habu-prog

# Recursive REPL (+ defun, recursion) - Complete Lisp
make habu-rec
```

### Build All REPLs

```bash
make repls
```

### How Compilation Works

1. **SBCL reads the Lisp source** (`*-repl.lisp`)
2. **Habu compiler** transforms Lisp to internal representation
3. **C backend** generates C code (`habu-*.c`)
4. **GCC/Clang** compiles C to native executable
5. **Links with runtime** (`runtime/*.c`)

### Manual Compilation

If you need to compile manually:

```bash
# Step 1: Generate C code
sbcl --load /tmp/compile-prog.lisp --quit

# Step 2: Compile C to executable
gcc -O2 -Iruntime -o habu-prog habu-prog.c runtime/*.c -lm
```

## Testing

### Run All Tests

```bash
make repl-test
# or directly:
./test-repls.sh
```

### Run Interactive Demo

```bash
make repl-demo
# or directly:
./demo.sh
```

### Manual Testing

```bash
# Start a REPL
./habu-rec

# Try some expressions
habu> (+ 2 3)
5

habu> (defun factorial (n) (if (= n 0) 1 (* n (factorial (- n 1)))))
<symbol>

habu> (factorial 5)
120
```

### Test a Specific Feature

```bash
# Create test file
cat > /tmp/my-test.txt <<'EOF'
(defun square (x) (* x x))
(square 7)
EOF

# Run through REPL
./habu-rec < /tmp/my-test.txt
```

## Development Workflow

### 1. Make Changes to REPL Source

Edit one of the REPL files:
- `enhanced-repl.lisp` - Basic REPL
- `programmable-repl.lisp` - With let/lambda
- `recursive-repl.lisp` - Complete Lisp

### 2. Rebuild

```bash
make habu-rec  # or habu-enhanced, habu-prog
```

### 3. Test

```bash
./test-repls.sh
```

### 4. Commit

```bash
git add <modified-files>
git commit -m "Description of changes"
```

## Code Style

### Lisp Code

- **Indentation**: 2 spaces per level
- **Line length**: Prefer < 80 characters
- **Comments**: Use `;;;; ` for major sections, `;;; ` for descriptions
- **Naming**: Use `kebab-case` for functions and variables
- **Predicates**: End with `?` (e.g., `null?`, `even?`)

Example:

```lisp
;;;; List Utilities

;;; Sum all elements in a list
(defun sum (lst)
  (if (= lst 0) 0
    (+ (car lst) (sum (cdr lst)))))

;;; Check if list is empty
(defun null? (x)
  (= x 0))
```

### C Code

The C runtime follows these conventions:
- **Indentation**: 4 spaces
- **Naming**: `snake_case` for functions, `UPPER_CASE` for macros
- **Prefix**: All runtime functions start with `habu_`
- **Comments**: Use `/* */` for documentation

## Adding Features

### Adding a New Operator

If you want to add a new operator (e.g., `modulo`):

1. **Add to evaluator** in `recursive-repl.lisp`:

```lisp
(defun eval-expr (expr env)
  ...
  (if (symbol=? first (make-symbol (quote "modulo")))
    (- (car args) (* (car (cdr args)) (/ (car args) (car (cdr args)))))
    ...))
```

2. **Rebuild and test**:

```bash
make habu-rec
./habu-rec
habu> (modulo 17 5)
2
```

### Adding a C Primitive

If you need a new C primitive (try to avoid this!):

1. **Implement in `runtime/runtime.c`**:

```c
habu_value_t habu_my_primitive(habu_value_t arg) {
    // Implementation
    return result;
}
```

2. **Declare in `runtime/habu.h`**:

```c
habu_value_t habu_my_primitive(habu_value_t arg);
```

3. **Add codegen in `bootstrap/c-backend.lisp`**:

```lisp
((and (consp expr) (eq (car expr) 'my-primitive))
 (format nil "habu_my_primitive(~A)"
         (habu-expr-to-c (second expr) indent)))
```

4. **Use in Lisp**:

```lisp
(my-primitive some-arg)
```

### Adding to Standard Library

Edit `stdlib.lisp`:

```lisp
;;; Your new function
(defun my-utility (x y)
  (+ x y))
```

Users can load it by copy/pasting into the REPL.

### Adding Examples

Edit `examples.lisp`:

```lisp
;;;; My New Examples Section

;;; Example function
(defun cool-algorithm (n)
  ...)
```

## Documentation

### Updating Documentation

When adding features, update:

- `README_REPL.md` - If it affects quick start
- `QUICK_REFERENCE.md` - Add syntax reference
- `REPL_FINAL_STATUS.md` - Update feature list
- `REPL_CHANGELOG.md` - Add to changelog

### Documentation Style

- Use clear, concise language
- Include code examples
- Explain both "what" and "why"
- Test all examples

## Common Tasks

### Add a Test Case

Edit `test-repls.sh`:

```bash
# Test my new feature
echo -e "${BLUE}Testing my feature...${NC}"
cat > /tmp/test-myfeature.txt <<'EOF'
(my-new-function 42)
EOF

RESULT=$(./habu-rec < /tmp/test-myfeature.txt 2>&1)
if echo "$RESULT" | grep -q "expected-output"; then
    echo -e "${GREEN}✓ My feature: PASS${NC}"
else
    echo -e "${RED}✗ My feature: FAIL${NC}"
    exit 1
fi
```

### Debug a REPL Issue

1. **Generate C code** to inspect:

```bash
make habu-rec
less habu-rec.c  # See generated code
```

2. **Add debug prints** in Lisp:

```lisp
(defun debug-me (x)
  (progn
    (print-value x)
    (println)
    x))
```

3. **Compile and test**:

```bash
make habu-rec
./habu-rec
habu> (debug-me 42)
42
42
```

### Clean Build Artifacts

```bash
make clean         # Clean all build artifacts
make clean-repls   # Clean only REPL artifacts
```

## Performance Tips

### Tail Recursion

The REPL doesn't optimize tail calls. Avoid deep recursion:

```lisp
;; BAD - Deep recursion
(defun factorial (n)
  (if (= n 0) 1
    (* n (factorial (- n 1)))))

;; BETTER - Use accumulator (still limited by stack)
(defun factorial (n)
  (factorial-helper n 1))

(defun factorial-helper (n acc)
  (if (= n 0) acc
    (factorial-helper (- n 1) (* n acc))))
```

### List Construction

Build lists front-to-back with cons:

```lisp
;; GOOD - cons onto front
(defun range (start end)
  (if (> start end) nil
    (cons start (range (+ start 1) end))))

;; BAD - append is expensive
(defun range-bad (start end)
  (if (> start end) nil
    (append (range-bad start (- end 1)) (list end))))
```

## Getting Help

- **Documentation**: See `README_REPL.md`, `QUICK_REFERENCE.md`
- **Examples**: Check `examples.lisp`, `stdlib.lisp`
- **Issues**: Report bugs via GitHub issues
- **Questions**: Ask in discussions

## Contribution Checklist

Before submitting changes:

- [ ] Code follows style guide
- [ ] All tests pass (`./test-repls.sh`)
- [ ] Documentation updated
- [ ] Examples work correctly
- [ ] Commit messages are clear
- [ ] Changes are minimal and focused

## Thank You!

Your contributions help make Habu Lisp better for everyone. Whether it's fixing bugs, adding features, improving documentation, or suggesting ideas - all contributions are valued!

**Welcome to the Habu Lisp community!** 🎉
