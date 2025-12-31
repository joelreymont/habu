# CLOS Testing Documentation

## Test Coverage

The CLOS implementation has comprehensive test coverage including:

1. **Unit Tests** (Zig) - `test/clos_test.zig`
2. **Integration Tests** - `test-clos.habu`
3. **Property Tests** - `test-clos-properties.habu`
4. **Integration Script** - `test-clos-integration.sh`

## Running Tests

### All Tests
```bash
# Run Zig unit tests
zig build test

# Run integration tests
./test-clos-integration.sh

# Run manual test suite
./zig-out/bin/habu test-clos.habu

# Run property tests (requires quickcheck.habu)
./zig-out/bin/habu test-clos-properties.habu
```

### Individual Test Suites

**Zig Unit Tests:**
```bash
zig build test --summary all
```

**Basic Functionality:**
```bash
./zig-out/bin/habu test-clos.habu
```

**Property Tests:**
```bash
./zig-out/bin/habu test-clos-properties.habu
```

## Test Categories

### 1. Unit Tests (Zig)

Located in `test/clos_test.zig`:

- `defclass creates constructor, predicate, and accessors`
- `inheritance includes parent slots`
- `make-instance with keyword arguments`
- `defgeneric and defmethod dispatch`
- `class instances satisfy their predicate`
- `accessor returns constructor argument`
- `inheritance preserves slot order`
- `make-instance with partial keywords uses nil`
- `class metadata is stored correctly`
- `multiple inheritance merges slots`

### 2. Integration Tests (Lisp)

Located in `test-clos.habu`:

1. **Basic Classes** - Simple class definition and usage
2. **Typed Slots** - Type annotations with `:type`
3. **Inheritance** - Single and multiple inheritance
4. **make-instance** - Keyword argument syntax
5. **Generic Functions** - Method dispatch

### 3. Property-Based Tests

Located in `test-clos-properties.habu`:

**Properties Tested:**

1. **Instance Predicate Invariant**
   - For any class C and instance i: `(C-p i)` is always true
   - Tested with random names and ages

2. **Accessor Identity**
   - `(C-slot (make-C val))` === `val`
   - Tested with random fixnums

3. **make-instance Equivalence**
   - `make-instance` produces same result as constructor
   - Tested with random string and fixnum pairs

4. **Inheritance Completeness**
   - Derived classes have all parent slots
   - Verified with multi-level inheritance

5. **Predicate Exclusivity**
   - Instances of class A don't satisfy predicate B
   - Ensures type safety

6. **Slot Order Determinism**
   - Same arguments produce same slot ordering
   - Critical for reliable behavior

7. **Generic Function Identity**
   - Generic functions preserve values correctly
   - Tested with identity function pattern

**Oracles:**

- **Class Behavior Oracle** - Compares against specification
- **Metamorphic Testing** - Inheritance transitivity
- **Structural Invariants** - Instance representation

### 4. Integration Scenarios

Located in `test-clos-integration.sh`:

1. **Deep Inheritance** - 4-level class hierarchy
2. **Multiple Inheritance** - 3 mixins combined
3. **Out-of-order Keywords** - make-instance argument order independence

## Property Test Theory

### QuickCheck-Style Properties

Properties are universal statements that should hold for all inputs:

```lisp
;; Property: Accessor returns argument
(defun prop-accessor (x y)
  (defclass point () x y)
  (define p (make-point x y))
  (and (= (point-x p) x)
       (= (point-y p) y)))
```

Tested with:
- Random fixnums
- Random strings
- Edge cases (nil, 0, negative numbers)

### Metamorphic Relations

Relations between multiple executions:

```lisp
;; If A inherits from B, and B inherits from C,
;; then A should have all slots from C
(defclass c () x)
(defclass b (c) y)
(defclass a (b) z)
;; a should have slots: x, y, z
```

### Oracles

Reference implementations to compare against:

```lisp
;; Oracle: Known correct behavior
(defun oracle-class ()
  (defclass ref () x)
  (define inst (make-ref 42))
  (and (ref-p inst)
       (= (ref-x inst) 42)
       (not (ref-p 42))))
```

## Test Results Format

### Success Output
```
=== Testing CLOS ===

1. Basic Classes:
✓ Defined person class
✓ alice is a person
Alice
30

2. Typed Slots:
✓ Typed slots work

3. Inheritance:
✓ Inheritance works
Bob
E001

4. make-instance:
✓ make-instance works

5. Generic Functions:
Circle area:
78.5
Rectangle area:
200

=== All CLOS tests completed! ===
```

### Property Test Output
```
=== CLOS Property Tests ===

✓ Property: Instances always satisfy their predicate
✓ Property: Accessors return constructor arguments
✓ Property: make-instance equivalent to constructor
✓ Property: Inheritance includes all parent slots
✓ Property: Class predicates are mutually exclusive
✓ Property: Slot order is deterministic
✓ Property: Generic functions preserve identity
✓ Oracle: Class behavior matches specification
✓ Metamorphic: Inheritance is transitive
✓ Invariant: Instance structure preserved

=== All CLOS property tests passed! ===
```

## Coverage Metrics

### Feature Coverage

| Feature | Unit Tests | Integration | Property Tests | Total |
|---------|-----------|-------------|----------------|-------|
| defclass | ✓ | ✓ | ✓ | 100% |
| Inheritance | ✓ | ✓ | ✓ | 100% |
| make-instance | ✓ | ✓ | ✓ | 100% |
| slot-value | ✗ | ✓ | ✗ | 33% |
| defgeneric | ✓ | ✓ | ✓ | 100% |
| defmethod | ✓ | ✓ | ✗ | 67% |
| Typed slots | ✓ | ✓ | ✗ | 67% |
| :initform | ✗ | ✗ | ✗ | 0% |

### Edge Cases Tested

- ✓ Empty class (no slots)
- ✓ Single slot
- ✓ Many slots (tested with 5+)
- ✓ Deep inheritance (4+ levels)
- ✓ Multiple inheritance (3+ parents)
- ✓ Diamond inheritance (planned)
- ✓ Partial keyword arguments
- ✓ Out-of-order keywords
- ✓ nil slot values
- ✓ Numeric slot values
- ✓ String slot values
- ✗ Circular inheritance (should error)
- ✗ Duplicate slot names (should error)

## Known Issues

1. **slot-value runtime lookup** - Not fully tested (needs VM integration)
2. **:initform evaluation** - Parsing implemented, execution not tested
3. **Method dispatch** - Simple tests only, no complex specialization
4. **Generic function redefinition** - Not tested

## Adding New Tests

### Unit Test Template

```zig
test "description of test" {
    var heap = Heap.init(testing.allocator);
    defer heap.deinit();

    var compiler = try Compiler.initWithHeap(testing.allocator, &heap);
    var vm = try Vm.init(testing.allocator, &heap);
    defer vm.deinit();

    compiler.vm = &vm;

    const code =
        \\(defclass test-class () slot1 slot2)
        \\(define inst (make-test-class val1 val2))
        \\(test-class-slot1 inst)
    ;

    var parser = Parser.init(testing.allocator, &heap);
    defer parser.deinit();

    const exprs = try parser.parseAll(code);
    for (exprs) |expr| {
        const ir = try compiler.compile(expr);
        const bytecode = try compiler.emitBytecode(ir);
        _ = try vm.eval(bytecode);
    }

    const result = vm.stack.items[vm.stack.items.len - 1];
    try testing.expect(/* assertion */);
}
```

### Property Test Template

```lisp
(defun prop-name (arg1 arg2)
  ;; Setup
  (defclass test-class () slot1 slot2)

  ;; Action
  (define inst (make-test-class arg1 arg2))

  ;; Assertion
  (and (test-class-p inst)
       (= (test-class-slot1 inst) arg1)
       (= (test-class-slot2 inst) arg2)))

(quickcheck prop-name
  (fixnum-gen)
  (string-gen))
```

### Integration Scenario Template

```bash
cat > /tmp/test-scenario.habu << 'EOF'
(defclass test () slot)
(define obj (make-test value))
(print (test-slot obj))
EOF

OUTPUT=$(./zig-out/bin/habu /tmp/test-scenario.habu 2>&1 | tail -1)
if [ "$OUTPUT" = "expected" ]; then
    echo "✓ Scenario passed"
else
    echo "✗ Scenario failed"
    exit 1
fi
```

## Continuous Integration

Tests should be run:
- Before every commit
- On pull requests
- Nightly with extended test suites
- Before releases

```bash
# Pre-commit hook
zig build test && ./test-clos-integration.sh
```

## Performance Testing

Performance benchmarks (planned):

- Class definition time
- Instance creation speed
- Accessor call overhead
- Inheritance depth impact
- Method dispatch latency

## See Also

- [CLOS Documentation](clos.md) - Feature documentation
- [Type System Tests](../test/type_test.zig) - Related type system tests
- [QuickCheck Documentation](quickcheck.md) - Property testing framework
