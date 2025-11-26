# Habu Test Framework Specification

## Overview
A comprehensive testing framework for Habu Lisp that supports unit tests, integration tests, property-based testing, and coverage analysis.

## Current Test Framework (test-harness.lisp)

### Existing Features
```lisp
(test-group "Group Name"
  (test-case test-name
    (assert-compiles-both expr)))
```

**Statistics**:
- Color-coded output (✓/✗)
- Pass/fail counts
- Exit code for CI/CD
- 120 tests currently

## Enhanced Test Framework Design

### 1. Core Test Macros

```lisp
;;; Define a test
(deftest test-name (suite-name)
  "Optional documentation string"
  (:tags :unit :arithmetic)
  (:timeout 5.0)  ; seconds
  (body...))

;;; Define test suite
(defsuite suite-name
  "Suite documentation"
  (:setup (lambda () ...))     ; Run before each test
  (:teardown (lambda () ...))  ; Run after each test
  (:before-suite (lambda () ...))  ; Run once before suite
  (:after-suite (lambda () ...)))  ; Run once after suite

;;; Run tests
(run-tests)                    ; Run all tests
(run-tests :suite 'arithmetic) ; Run specific suite
(run-tests :tag :unit)         ; Run tests by tag
(run-tests :pattern "arith")   ; Run tests matching pattern
(run-tests :failed)            ; Re-run failed tests

;;; Test status
(test-status)                  ; Show summary
(test-report :format :html)    ; Generate report
```

### 2. Assertion Functions

```lisp
;;; Basic assertions
(assert-true expr)
(assert-false expr)
(assert-equal expected actual &optional message)
(assert-not-equal expected actual)
(assert-eq expected actual)      ; Pointer equality
(assert-eql expected actual)     ; Number/char equality

;;; Numeric assertions
(assert-= expected actual &optional epsilon)
(assert-< a b)
(assert-<= a b)
(assert-> a b)
(assert->= a b)

;;; Type assertions
(assert-type expected-type actual)
(assert-typep actual type-spec)

;;; Collection assertions
(assert-member item list)
(assert-length expected list)
(assert-empty collection)
(assert-not-empty collection)

;;; Exception assertions
(assert-error error-type (body...))
(assert-no-error (body...))
(assert-signals condition-type (body...))

;;; Compiler-specific assertions
(assert-compiles expr &optional arch)
(assert-compiles-both expr)
(assert-compile-error expr)
(assert-code-size expr expected-size &optional delta)

;;; Custom assertions
(assert-match pattern value)      ; Pattern matching
(assert-approx-equal expected actual tolerance)
```

### 3. Test Organization

```lisp
;;; File: test/arithmetic-test.lisp
(in-package :habu-test)

(defsuite arithmetic-tests
  "Tests for arithmetic operations"
  (:before-suite
   (lambda ()
     (format t "~%Starting arithmetic tests~%"))))

(deftest test-addition (arithmetic-tests)
  "Test basic addition"
  (:tags :unit :arithmetic)
  (assert-equal 3 (+ 1 2))
  (assert-equal 0 (+ 0 0))
  (assert-equal -1 (+ -2 1)))

(deftest test-subtraction (arithmetic-tests)
  "Test basic subtraction"
  (:tags :unit :arithmetic)
  (assert-equal 1 (- 3 2))
  (assert-equal 0 (- 5 5)))

;;; Parameterized tests
(deftest test-addition-parameterized (arithmetic-tests)
  (:tags :unit :arithmetic)
  (dolist (case '((1 2 3) (0 0 0) (-1 1 0)))
    (destructuring-bind (a b expected) case
      (assert-equal expected (+ a b)))))
```

### 4. Integration Tests

```lisp
(defsuite integration-tests
  "Integration tests for full compilation pipeline")

(deftest test-full-compilation (integration-tests)
  "Test complete compilation from source to execution"
  (:tags :integration)
  (let* ((source '(defun factorial (n)
                    (if (<= n 1)
                        1
                        (* n (factorial (- n 1))))))
         (compiled (compile-expression source)))
    (assert-not-empty compiled)
    (assert-compiles '(factorial 5))))

(deftest test-closure-capture (integration-tests)
  "Test closure environment capture"
  (:tags :integration :closures)
  (assert-compiles '(let ((x 10))
                      ((lambda (y) (+ x y)) 5))))
```

### 5. Property-Based Testing

```lisp
;;; QuickCheck-style testing
(defproperty prop-addition-commutative (arithmetic-tests)
  "Addition is commutative"
  (:forall ((a :fixnum) (b :fixnum)))
  (assert-equal (+ a b) (+ b a)))

(defproperty prop-reverse-involution (list-tests)
  "Reversing twice returns original"
  (:forall ((lst :list-of :fixnum)))
  (assert-equal lst (reverse (reverse lst))))

;;; Custom generators
(defgenerator :fixnum ()
  (- (random 1000) 500))

(defgenerator :list-of (element-type)
  (let ((length (random 20)))
    (loop repeat length collect (generate element-type))))

;;; Property test configuration
(setf *property-test-count* 100)    ; Number of random tests
(setf *property-shrink-attempts* 50) ; Shrinking iterations
```

### 6. Performance Tests

```lisp
(deftest test-compilation-speed (performance-tests)
  "Ensure compilation is reasonably fast"
  (:tags :performance)
  (:timeout 1.0)
  (time
    (dotimes (i 100)
      (compile-expression '(+ 1 2)))))

(deftest test-memory-usage (performance-tests)
  "Test memory consumption"
  (:tags :performance :memory)
  (let ((before (gc-stats)))
    (dotimes (i 1000)
      (compile-expression '(let ((x 1)) x)))
    (let ((after (gc-stats)))
      (assert-< (- (allocated after) (allocated before))
                1000000)))) ; Less than 1MB
```

### 7. Coverage Analysis

```lisp
;;; Enable coverage
(enable-coverage)

;;; Run tests with coverage
(run-tests-with-coverage)

;;; Coverage report
(coverage-report :format :text)
(coverage-report :format :html :output "coverage.html")

;;; Coverage statistics
(coverage-stats)
; => (:lines 1234 :covered 1100 :percentage 89.2)

;;; Identify uncovered code
(show-uncovered-code 'emit-x86_64)
```

### 8. Test Fixtures

```lisp
(defsuite database-tests
  "Tests requiring database"
  (:setup
   (lambda ()
     (setf *test-db* (make-test-database))))
  (:teardown
   (lambda ()
     (close-database *test-db*)
     (setf *test-db* nil))))

(deftest test-query (database-tests)
  "Test database query"
  (let ((result (query *test-db* "SELECT * FROM test")))
    (assert-not-empty result)))
```

### 9. Mocking and Stubbing

```lisp
;;; Mock a function
(with-mocks ((compile-expression (lambda (expr) '(#x90))))
  (deftest test-with-mock (compiler-tests)
    (assert-equal '(#x90) (compile-expression '(+ 1 2)))))

;;; Stub return values
(with-stub (gc-stats :returns '(:allocated 1000))
  (deftest test-memory (memory-tests)
    (assert-equal 1000 (allocated (gc-stats)))))

;;; Verify function calls
(with-spy (emit-x86_64)
  (compile-expression '(+ 1 2))
  (assert-called emit-x86_64 :times 1))
```

### 10. Continuous Integration Support

```lisp
;;; Run tests for CI
(defun ci-test-runner ()
  "Run all tests suitable for CI"
  (let ((results (run-tests :tags '(:unit :integration))))
    (when (> (failed-count results) 0)
      (test-report :format :junit :output "test-results.xml")
      (exit 1))
    (exit 0)))

;;; Parallel test execution
(run-tests :parallel t :workers 4)
```

### 11. Test Output Formats

```lisp
;;; Text output (current)
========================================
Habu Compiler Test Suite
========================================

Arithmetic Tests
================
✓ test-addition
✓ test-subtraction
✗ test-division
  Expected: 5
  Actual:   4
  at test-arithmetic.lisp:42

Total:  120
Passed: 119/120 (99.2%)
Failed: 1/120 (0.8%)

;;; JUnit XML output (for CI)
<testsuites>
  <testsuite name="arithmetic-tests" tests="10" failures="1">
    <testcase name="test-addition" time="0.001"/>
    <testcase name="test-division" time="0.002">
      <failure>Expected 5, got 4</failure>
    </testcase>
  </testsuite>
</testsuites>

;;; HTML output
<html>
  <head><title>Test Results</title></head>
  <body>
    <h1>Test Results</h1>
    <div class="summary">
      <span class="passed">119 passed</span>
      <span class="failed">1 failed</span>
    </div>
    ...
  </body>
</html>

;;; JSON output
{
  "total": 120,
  "passed": 119,
  "failed": 1,
  "suites": [
    {
      "name": "arithmetic-tests",
      "tests": [
        {"name": "test-addition", "status": "pass", "time": 0.001},
        {"name": "test-division", "status": "fail", "time": 0.002,
         "error": "Expected 5, got 4"}
      ]
    }
  ]
}
```

### 12. Test Discovery

```lisp
;;; Auto-discover tests in directory
(discover-tests "test/")

;;; Load test files
(load-tests "test/*.lisp")

;;; Test naming convention
; Files: *-test.lisp or test-*.lisp
; Functions: test-* or *-test
```

## Implementation Plan

### Phase 1: Enhanced Core (Week 1-2)
- [ ] Implement defsuite and deftest macros
- [ ] Add comprehensive assertion functions
- [ ] Setup/teardown support
- [ ] Test tagging system
- [ ] Multiple output formats (text, JUnit, HTML)

### Phase 2: Advanced Features (Week 3-4)
- [ ] Property-based testing (QuickCheck)
- [ ] Test fixtures
- [ ] Mocking and stubbing
- [ ] Parameterized tests
- [ ] Test discovery

### Phase 3: Quality & Performance (Week 5-6)
- [ ] Coverage analysis tool
- [ ] Parallel test execution
- [ ] Performance testing utilities
- [ ] CI/CD integration
- [ ] Test report generation

### Phase 4: Expansion (Week 7-8)
- [ ] Expand to 1000+ tests
- [ ] Cross-platform tests (x86_64, ARM64)
- [ ] Regression test suite
- [ ] Stress tests
- [ ] Integration test suite

## File Structure

```
test/
├── framework/
│   ├── core.lisp           # Core test macros
│   ├── assertions.lisp     # Assertion functions
│   ├── fixtures.lisp       # Fixture support
│   ├── mocking.lisp        # Mocking/stubbing
│   ├── property.lisp       # Property-based testing
│   ├── coverage.lisp       # Coverage analysis
│   └── reporters.lisp      # Output formatters
├── unit/
│   ├── arithmetic-test.lisp
│   ├── comparison-test.lisp
│   ├── bitwise-test.lisp
│   ├── boolean-test.lisp
│   ├── control-test.lisp
│   ├── variables-test.lisp
│   ├── functions-test.lisp
│   ├── macro-test.lisp
│   ├── gc-test.lisp
│   └── ...
├── integration/
│   ├── compilation-test.lisp
│   ├── closure-test.lisp
│   ├── recursion-test.lisp
│   └── ...
├── performance/
│   ├── compilation-speed-test.lisp
│   ├── memory-usage-test.lisp
│   └── ...
├── regression/
│   ├── issue-001-test.lisp
│   └── ...
└── run-all-tests.lisp      # Main test runner
```

## Example Usage

```lisp
;;; Run all tests
$ sbcl --script test/run-all-tests.lisp

;;; Run specific suite
$ sbcl --eval "(run-tests :suite 'arithmetic-tests)"

;;; Run with coverage
$ sbcl --eval "(run-tests-with-coverage)"

;;; CI mode
$ sbcl --script test/ci-runner.lisp

;;; Generate HTML report
$ sbcl --eval "(test-report :format :html :output 'report.html')"
```

## Success Criteria

- [ ] 1000+ test cases
- [ ] 90%+ code coverage
- [ ] All tests pass on both x86_64 and ARM64
- [ ] < 60 seconds total test execution time
- [ ] Property-based tests for all core operations
- [ ] Integration tests for all major features
- [ ] Automated test discovery
- [ ] CI/CD integration working
- [ ] HTML and JUnit XML reports
- [ ] Coverage reports generated automatically
