# Habu Benchmarking Harness Specification

## Overview
A comprehensive benchmarking framework for measuring and tracking performance of the Habu Lisp compiler and runtime.

## Goals
1. **Measure Performance**: Accurately measure execution time, memory usage, and other metrics
2. **Track Regressions**: Detect performance regressions across commits
3. **Compare Implementations**: Compare different approaches and optimizations
4. **Optimize Development**: Guide optimization efforts with data
5. **Cross-Platform**: Measure performance on x86_64 and ARM64

## Benchmark Framework Design

### 1. Core Benchmark Macros

```lisp
;;; Define a benchmark
(defbenchmark benchmark-name (suite-name)
  "Benchmark documentation"
  (:warmup 10)        ; Warmup iterations
  (:iterations 1000)  ; Measurement iterations
  (:timeout 30.0)     ; Timeout in seconds
  (:baseline baseline-name) ; Compare against baseline
  (body...))

;;; Benchmark suites
(defbenchmark-suite suite-name
  "Suite documentation"
  (:before-suite (lambda () ...))
  (:after-suite (lambda () ...)))

;;; Run benchmarks
(run-benchmarks)                        ; Run all benchmarks
(run-benchmarks :suite 'arithmetic)     ; Run specific suite
(run-benchmarks :pattern "compile")     ; Pattern matching
(run-benchmarks :tag :memory)           ; By tag

;;; Compare benchmarks
(compare-benchmarks :baseline "v0.1" :current "HEAD")
(benchmark-diff "baseline.json" "current.json")
```

### 2. Measurement Functions

```lisp
;;; Time measurement
(benchmark-time (body...)
  => (:mean 1.234e-6       ; Mean time in seconds
      :median 1.230e-6      ; Median
      :stddev 0.050e-6      ; Standard deviation
      :min 1.200e-6         ; Minimum
      :max 1.300e-6         ; Maximum
      :iterations 1000))    ; Number of iterations

;;; Memory measurement
(benchmark-memory (body...)
  => (:allocated 1024      ; Bytes allocated
      :peak-usage 2048      ; Peak memory usage
      :gc-count 5           ; Number of GC cycles
      :gc-time 0.001))      ; Time spent in GC

;;; Combined measurement
(benchmark-all (body...)
  => (:time ... :memory ... :custom ...))
```

### 3. Statistical Analysis

```lisp
;;; Outlier detection
(detect-outliers measurements)
=> (:outliers (1.5e-6 2.0e-6)
    :clean (1.2e-6 1.3e-6 1.25e-6 ...))

;;; Confidence intervals
(confidence-interval measurements :confidence 0.95)
=> (:lower 1.20e-6 :upper 1.28e-6)

;;; Hypothesis testing
(t-test baseline current :alpha 0.05)
=> (:t-statistic 2.5 :p-value 0.012 :significant t)

;;; Regression detection
(detect-regression baseline current :threshold 0.10)
=> (:regression t :change 15.5% :severity :high)
```

### 4. Benchmark Suites

#### 4.1 Arithmetic Benchmarks

```lisp
(defbenchmark-suite arithmetic-benchmarks
  "Benchmarks for arithmetic operations")

(defbenchmark bench-addition (arithmetic-benchmarks)
  "Benchmark integer addition"
  (:iterations 10000)
  (+ 42 58))

(defbenchmark bench-multiplication (arithmetic-benchmarks)
  "Benchmark integer multiplication"
  (:iterations 10000)
  (* 123 456))

(defbenchmark bench-division (arithmetic-benchmarks)
  "Benchmark integer division"
  (:iterations 10000)
  (/ 1000 7))

(defbenchmark bench-mixed-arithmetic (arithmetic-benchmarks)
  "Benchmark mixed arithmetic expression"
  (:iterations 5000)
  (+ (* 3 4) (/ 20 2)))

(defbenchmark bench-nested-arithmetic (arithmetic-benchmarks)
  "Benchmark deeply nested arithmetic"
  (:iterations 1000)
  (+ (* (- 10 5) (/ 20 4)) (+ 3 (* 2 5))))
```

#### 4.2 Memory Benchmarks

```lisp
(defbenchmark-suite memory-benchmarks
  "Benchmarks for memory operations")

(defbenchmark bench-cons-allocation (memory-benchmarks)
  "Benchmark cons cell allocation"
  (:iterations 1000)
  (:tags :memory :allocation)
  (cons 1 2))

(defbenchmark bench-list-creation (memory-benchmarks)
  "Benchmark list creation"
  (:iterations 1000)
  (list 1 2 3 4 5 6 7 8 9 10))

(defbenchmark bench-gc-pressure (memory-benchmarks)
  "Benchmark GC under allocation pressure"
  (:iterations 100)
  (:tags :memory :gc)
  (dotimes (i 1000)
    (cons i i)))

(defbenchmark bench-large-structure (memory-benchmarks)
  "Benchmark large data structure creation"
  (:iterations 10)
  (let ((result nil))
    (dotimes (i 10000)
      (push (cons i (* i i)) result))
    result))
```

#### 4.3 Function Call Benchmarks

```lisp
(defbenchmark-suite function-call-benchmarks
  "Benchmarks for function calls")

(defbenchmark bench-lambda-call (function-call-benchmarks)
  "Benchmark lambda call overhead"
  (:iterations 10000)
  ((lambda (x) (* x 2)) 42))

(defbenchmark bench-named-function-call (function-call-benchmarks)
  "Benchmark named function call"
  (:iterations 10000)
  (:before (defun double (x) (* x 2)))
  (double 42))

(defbenchmark bench-closure-call (function-call-benchmarks)
  "Benchmark closure call with captured variables"
  (:iterations 10000)
  (let ((multiplier 2))
    ((lambda (x) (* x multiplier)) 42)))

(defbenchmark bench-recursive-call (function-call-benchmarks)
  "Benchmark recursive function call"
  (:iterations 100)
  (:before (defun factorial (n)
             (if (<= n 1) 1 (* n (factorial (- n 1))))))
  (factorial 10))

(defbenchmark bench-tail-recursive-call (function-call-benchmarks)
  "Benchmark tail-recursive call (when TCO implemented)"
  (:iterations 100)
  (:before (defun sum-to (n &optional (acc 0))
             (if (zerop n) acc (sum-to (- n 1) (+ acc n)))))
  (sum-to 1000))
```

#### 4.4 Compilation Benchmarks

```lisp
(defbenchmark-suite compilation-benchmarks
  "Benchmarks for compilation speed")

(defbenchmark bench-compile-simple (compilation-benchmarks)
  "Benchmark simple expression compilation"
  (:iterations 1000)
  (compile-expression '(+ 1 2)))

(defbenchmark bench-compile-complex (compilation-benchmarks)
  "Benchmark complex expression compilation"
  (:iterations 100)
  (compile-expression '(let ((x 10) (y 20))
                         (if (< x y)
                             (+ x y)
                             (* x y)))))

(defbenchmark bench-compile-lambda (compilation-benchmarks)
  "Benchmark lambda compilation"
  (:iterations 100)
  (compile-expression '((lambda (x y) (+ (* x x) (* y y))) 3 4)))

(defbenchmark bench-compile-with-optimization (compilation-benchmarks)
  "Benchmark compilation with optimizations"
  (:iterations 100)
  (:tags :optimization)
  (with-optimization-level 3
    (compile-expression '(+ (* 2 3) (/ 10 2)))))

(defbenchmark bench-code-size (compilation-benchmarks)
  "Measure generated code size"
  (:metric :size)
  (length (compile-expression '(let ((x 1) (y 2)) (+ x y)))))
```

#### 4.5 List Processing Benchmarks

```lisp
(defbenchmark-suite list-processing-benchmarks
  "Benchmarks for list operations")

(defbenchmark bench-car-cdr (list-processing-benchmarks)
  "Benchmark car/cdr access"
  (:iterations 10000)
  (:before (setf *test-list* (list 1 2 3 4 5)))
  (car (cdr *test-list*)))

(defbenchmark bench-list-traversal (list-processing-benchmarks)
  "Benchmark list traversal"
  (:iterations 1000)
  (:before (setf *test-list* (loop for i from 1 to 100 collect i)))
  (dolist (x *test-list*) x))

(defbenchmark bench-mapcar (list-processing-benchmarks)
  "Benchmark mapcar"
  (:iterations 1000)
  (:before (setf *test-list* (loop for i from 1 to 100 collect i)))
  (mapcar (lambda (x) (* x 2)) *test-list*))

(defbenchmark bench-reduce (list-processing-benchmarks)
  "Benchmark reduce"
  (:iterations 1000)
  (:before (setf *test-list* (loop for i from 1 to 100 collect i)))
  (reduce #'+ *test-list*))

(defbenchmark bench-reverse (list-processing-benchmarks)
  "Benchmark list reversal"
  (:iterations 1000)
  (:before (setf *test-list* (loop for i from 1 to 100 collect i)))
  (reverse *test-list*))
```

#### 4.6 Macro Expansion Benchmarks

```lisp
(defbenchmark-suite macro-benchmarks
  "Benchmarks for macro expansion")

(defbenchmark bench-simple-macro (macro-benchmarks)
  "Benchmark simple macro expansion"
  (:iterations 1000)
  (:before (defmacro double (x) `(* ,x 2)))
  (macroexpand '(double 42)))

(defbenchmark bench-backquote (macro-benchmarks)
  "Benchmark backquote expansion"
  (:iterations 1000)
  (let ((x 1) (y 2))
    `(+ ,x ,y)))

(defbenchmark bench-complex-macro (macro-benchmarks)
  "Benchmark complex macro with multiple expansions"
  (:iterations 100)
  (:before (defmacro with-temp ((var value) &body body)
             `(let ((,var ,value))
                (unwind-protect
                    (progn ,@body)
                  (cleanup ,var)))))
  (macroexpand '(with-temp (x 10) (print x))))
```

#### 4.7 I/O Benchmarks

```lisp
(defbenchmark-suite io-benchmarks
  "Benchmarks for I/O operations")

(defbenchmark bench-read-expression (io-benchmarks)
  "Benchmark reading s-expressions"
  (:iterations 1000)
  (with-input-from-string (s "(+ 1 2)")
    (read s)))

(defbenchmark bench-print-expression (io-benchmarks)
  "Benchmark printing s-expressions"
  (:iterations 1000)
  (with-output-to-string (s)
    (print '(+ 1 2) s)))

(defbenchmark bench-format-string (io-benchmarks)
  "Benchmark format string processing"
  (:iterations 1000)
  (format nil "Result: ~A" 42))

(defbenchmark bench-file-read (io-benchmarks)
  "Benchmark file reading"
  (:iterations 10)
  (:before (create-test-file "test.lisp" 1000))
  (:after (delete-file "test.lisp"))
  (with-open-file (f "test.lisp")
    (read-file-to-list f)))
```

### 5. Performance Regression Testing

```lisp
;;; Save baseline
(save-benchmark-baseline "baseline-v0.1.json")

;;; Compare against baseline
(defun detect-regressions ()
  (let* ((baseline (load-baseline "baseline-v0.1.json"))
         (current (run-benchmarks))
         (regressions nil))
    (dolist (bench current)
      (let ((base (find-benchmark bench baseline)))
        (when base
          (let ((change (percentage-change (time base) (time bench))))
            (when (> change *regression-threshold*)
              (push (list (name bench) change) regressions))))))
    regressions))

;;; Regression configuration
(setf *regression-threshold* 0.10)  ; 10% slowdown = regression
(setf *improvement-threshold* 0.20) ; 20% speedup = improvement
```

### 6. Benchmark Reports

```lisp
;;; Text report
================================================================================
Habu Lisp Benchmark Suite - 2025-01-15 14:32:10
================================================================================

Arithmetic Benchmarks
---------------------
bench-addition              1.234 μs/op  ±  0.050 μs  (1000 iterations)
bench-multiplication        1.456 μs/op  ±  0.062 μs  (1000 iterations)
bench-division              2.123 μs/op  ±  0.145 μs  (1000 iterations)

Memory Benchmarks
-----------------
bench-cons-allocation       0.234 μs/op    256 bytes allocated
bench-list-creation         0.456 μs/op   1024 bytes allocated
bench-gc-pressure          12.345 ms/op     15 GC cycles, 2.3ms GC time

Compilation Benchmarks
----------------------
bench-compile-simple       15.234 μs/op  ±  0.523 μs
bench-compile-complex     123.456 μs/op  ±  5.234 μs

Summary
-------
Total benchmarks:       45
Total time:            12.3 seconds
Fastest:               bench-addition (1.234 μs)
Slowest:               bench-large-structure (234.5 ms)
Memory allocated:       4.5 MB
GC cycles:             127
GC time:               156 ms (1.3%)

;;; HTML report with charts
<html>
  <head><title>Benchmark Results</title></head>
  <body>
    <h1>Habu Benchmark Results</h1>
    <div class="summary">...</div>
    <canvas id="chart"></canvas> <!-- Time series chart -->
    <table class="results">...</table>
  </body>
</html>

;;; JSON export
{
  "timestamp": "2025-01-15T14:32:10Z",
  "commit": "abc123",
  "benchmarks": [
    {
      "name": "bench-addition",
      "suite": "arithmetic-benchmarks",
      "iterations": 1000,
      "time": {
        "mean": 1.234e-6,
        "median": 1.230e-6,
        "stddev": 0.050e-6,
        "min": 1.200e-6,
        "max": 1.300e-6
      },
      "memory": {
        "allocated": 0,
        "gc-count": 0
      }
    }
  ]
}

;;; CSV export for analysis
Name,Suite,Mean(μs),Stddev(μs),Iterations,Allocated(bytes)
bench-addition,arithmetic,1.234,0.050,1000,0
bench-multiplication,arithmetic,1.456,0.062,1000,0
...

;;; Comparison report
Comparing baseline-v0.1.json to current

Regressions (slower):
  bench-compile-complex   +15.3%  (123.4 μs → 142.3 μs)  ⚠
  bench-gc-pressure       +22.1%  (12.3 ms → 15.0 ms)    ⚠⚠

Improvements (faster):
  bench-addition          -5.2%   (1.30 μs → 1.23 μs)    ✓
  bench-car-cdr           -12.3%  (0.45 μs → 0.39 μs)    ✓✓

No significant change:
  bench-multiplication    +0.5%   (1.45 μs → 1.46 μs)
  ...

Overall: 2 regressions, 2 improvements, 41 unchanged
```

### 7. Profiling Integration

```lisp
;;; Profile a benchmark
(profile-benchmark 'bench-compile-complex)
=> Function Call Profile:
   emit-x86_64         45.2%  (127 calls, 5.6 μs/call)
   parse               23.1%  (89 calls,  3.2 μs/call)
   emit-arm64          18.5%  (127 calls, 1.8 μs/call)
   ...

;;; Hotspot detection
(find-hotspots 'bench-compile-complex)
=> (:hotspots
    ((:function emit-x86_64 :percentage 45.2)
     (:function parse :percentage 23.1)))

;;; Memory profiling
(profile-memory 'bench-large-structure)
=> Allocation Profile:
   cons                1.2 MB  (45.2%)
   make-array          0.8 MB  (30.1%)
   make-string         0.5 MB  (18.9%)
   ...
```

### 8. Continuous Performance Monitoring

```lisp
;;; Run benchmarks on every commit
(defun ci-benchmark-runner ()
  "Run benchmarks for CI"
  (let ((results (run-benchmarks)))
    ;; Save results
    (save-results results (format nil "bench-~A.json" (git-commit)))

    ;; Check for regressions
    (when-let ((regressions (detect-regressions)))
      (format t "~%⚠ Performance regressions detected:~%")
      (dolist (reg regressions)
        (format t "  - ~A: +~,1F%~%" (first reg) (second reg)))
      (exit 1))

    ;; Generate report
    (benchmark-report :format :html :output "benchmark-report.html")
    (exit 0)))

;;; Track performance over time
(defun performance-history (benchmark-name &optional (days 30))
  "Show performance trend"
  (let ((results (load-benchmark-history benchmark-name days)))
    (plot-performance-graph results)))
```

### 9. Comparison with Other Implementations

```lisp
;;; Compare with SBCL, CCL, etc.
(defun compare-implementations ()
  (let ((implementations '(:habu :sbcl :ccl :ecl)))
    (dolist (impl implementations)
      (run-benchmarks :implementation impl))
    (generate-comparison-report)))

;;; Benchmark result
Implementation Comparison - bench-addition
-------------------------------------------
SBCL:    0.234 μs/op  (baseline)
CCL:     0.456 μs/op  (+94.9%)
Habu:    1.234 μs/op  (+427.4%)
ECL:     2.345 μs/op  (+902.1%)
```

## Implementation Plan

### Phase 1: Core Framework (Week 1-2)
- [ ] Basic benchmark macros (defbenchmark, run-benchmarks)
- [ ] Time measurement with statistics
- [ ] Simple text output
- [ ] 20 initial benchmarks (arithmetic, compilation)

### Phase 2: Advanced Metrics (Week 3)
- [ ] Memory measurement
- [ ] GC statistics
- [ ] Statistical analysis (outliers, confidence intervals)
- [ ] 40+ benchmarks (add memory, function calls)

### Phase 3: Reporting (Week 4)
- [ ] HTML reports with charts
- [ ] JSON/CSV export
- [ ] Baseline comparison
- [ ] Regression detection
- [ ] 60+ benchmarks

### Phase 4: Integration (Week 5)
- [ ] Profiling integration
- [ ] CI/CD integration
- [ ] Performance history tracking
- [ ] Automated regression detection
- [ ] 80+ benchmarks

### Phase 5: Expansion (Week 6)
- [ ] 100+ comprehensive benchmarks
- [ ] Cross-implementation comparison
- [ ] Performance optimization guided by benchmarks
- [ ] Documentation and examples

## File Structure

```
benchmark/
├── framework/
│   ├── core.lisp          # Core benchmark macros
│   ├── measurement.lisp   # Time/memory measurement
│   ├── statistics.lisp    # Statistical analysis
│   ├── profiling.lisp     # Profiling integration
│   └── reporters.lisp     # Report generation
├── suites/
│   ├── arithmetic-bench.lisp
│   ├── memory-bench.lisp
│   ├── compilation-bench.lisp
│   ├── function-call-bench.lisp
│   ├── list-processing-bench.lisp
│   ├── macro-bench.lisp
│   ├── io-bench.lisp
│   └── ...
├── baselines/
│   ├── baseline-v0.1.json
│   ├── baseline-v0.2.json
│   └── ...
├── reports/
│   ├── benchmark-report.html
│   ├── regression-report.txt
│   └── ...
└── run-benchmarks.lisp    # Main benchmark runner
```

## Example Usage

```bash
# Run all benchmarks
$ sbcl --script benchmark/run-benchmarks.lisp

# Run specific suite
$ sbcl --eval "(run-benchmarks :suite 'arithmetic-benchmarks)"

# Save baseline
$ sbcl --eval "(save-benchmark-baseline 'baseline-v0.2.json')"

# Compare with baseline
$ sbcl --eval "(compare-benchmarks :baseline 'v0.1' :current 'HEAD')"

# CI mode
$ sbcl --script benchmark/ci-benchmark-runner.lisp

# Generate HTML report
$ sbcl --eval "(benchmark-report :format :html :output 'report.html')"

# Profile benchmark
$ sbcl --eval "(profile-benchmark 'bench-compile-complex)"
```

## Success Criteria

- [ ] 100+ benchmark cases covering all major operations
- [ ] Benchmarks run in < 5 minutes total
- [ ] Accurate timing (< 5% variance on repeated runs)
- [ ] Memory measurement working
- [ ] GC statistics captured
- [ ] Statistical analysis (mean, median, stddev, outliers)
- [ ] Regression detection (> 10% slowdown flagged)
- [ ] HTML reports with charts generated
- [ ] JSON/CSV export for external analysis
- [ ] Baseline comparison working
- [ ] CI integration (fail on regression)
- [ ] Performance history tracking (30 days)
- [ ] Profiling integration
- [ ] Cross-platform benchmarks (x86_64, ARM64)
- [ ] Documentation and examples

## Metrics to Track

### Performance Metrics
- Execution time (mean, median, stddev, min, max)
- Memory allocated
- Peak memory usage
- GC cycles and time
- Code size (bytes)
- Compilation speed (LOC/sec)

### Regression Metrics
- Percentage change from baseline
- Statistical significance (p-value)
- Severity (minor < 10%, major < 25%, critical >= 25%)

### Quality Metrics
- Test stability (variance across runs)
- Coverage (% of code benchmarked)
- Performance trends (improving/regressing/stable)
