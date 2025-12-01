# Habu Run Tests Command

Run the Habu test suite and report results with semantic context.

## Arguments
- `$ARGUMENTS` - Optional: specific test name or pattern (e.g., "closures", "test_self*.lisp")

## Workflow

1. **Identify Tests**
   - If no argument: run all tests in `tests/` directory
   - If pattern given: match files against pattern
   - List tests to be run

2. **Run Tests**
   For each test file:
   - Load `bootstrap/compiler-sbcl.lisp`, `macho.lisp`, etc.
   - Execute the test
   - Capture exit code and output

3. **Results Summary**
   ```
   PASS: test_arithmetic.lisp (exit 0)
   PASS: test_closures.lisp (exit 0)
   FAIL: test_recursive.lisp (exit 139 - SIGSEGV)
   ```

4. **Failure Analysis**
   For each failing test:
   - Report which specific assertion failed (if output available)
   - For crashes, use /habu-debug workflow
   - Provide function context from .map file

## Output Format

```
HABU TEST SUITE
===============

Running <N> tests...

RESULTS:
  [PASS] test_arithmetic.lisp
  [PASS] test_closures.lisp
  [FAIL] test_recursive.lisp - SIGSEGV at read-list-elems+0x48

Summary: <passed>/<total> tests passed

FAILURES:
  test_recursive.lisp:
    Signal: SIGSEGV (exit 139)
    Location: read-list-elems + 0x48
    Cause: Stack overflow in recursive reader
```

## Example Usage
```
/habu-run-tests
/habu-run-tests closures
/habu-run-tests test_self*.lisp
```
