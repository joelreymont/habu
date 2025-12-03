# Habu Run Tests Command

Run the Habu test suite via ASDF and report results.

## Arguments
- `$ARGUMENTS` - Optional: "quick" for just running tests, or nothing for full output

## Workflow

1. **Run Tests via ASDF**
   ```bash
   sbcl --noinform --non-interactive \
     --eval '(require :asdf)' \
     --eval '(push #P"/Users/joel/Work/habu/bootstrap/" asdf:*central-registry*)' \
     --eval '(asdf:test-system :habu)'
   ```

2. **Analyze Results**
   The test system reports:
   - Core Compiler Tests (arithmetic, comparisons, let bindings, etc.)
   - Keyword Argument Tests (6 tests for &key support)
   - Package Tests (1 passes, 4 skipped in bootstrap mode)

3. **Interpret Output**
   - `[PASS]` - Test succeeded
   - `[FAIL]` - Test failed (shows expected vs actual)
   - `[SKIP]` - Test skipped (usually requires native mode)

## Test Files

Tests are defined in `bootstrap/habu.asd` as the `habu/tests` system:
- `bootstrap/test-harness.lisp` - Test utilities (HABU-TEST package)
- `tests/test-core.lisp` - Core compiler tests (37 tests)
- `tests/test-keyword-args.lisp` - Keyword argument tests (6 tests)
- `tests/test-packages.lisp` - Package system tests (1 pass, 4 skipped)

## Expected Output

```
=== Core Compiler Tests ===
[PASS] add = 42
[PASS] sub = 42
...

=== Keyword Argument Tests ===
[PASS] kw-default = 10
...

=== Package Tests ===
[PASS] pkg-simple = 42
[SKIP] pkg-cross-call: cross-package calls require native reader
...

TOTAL: 44 passed, 0 failed, 4 skipped
All tests PASSED!
```

## Adding New Tests

1. Create `tests/test-feature.lisp` using HABU-TEST package
2. Add to `bootstrap/habu.asd` in the habu/tests system
3. Tests auto-run when loaded via ASDF
