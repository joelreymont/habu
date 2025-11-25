# Habu Profiler

Function-level profiler for measuring execution time of Habu functions.

## Quick Start

```lisp
;; Enable profiling for a function
(profile my-function)

;; Define the function (will be instrumented)
(defun my-function (x)
  (* x x))

;; Call it - prints timing info
(my-function 5)
;; Output: PROFILE: MY-FUNCTION 1234

;; Disable profiling
(unprofile my-function)
```

## API Reference

### Compile-Time Forms

#### `(profile fn1 fn2 ...)`
Enable profiling for one or more functions. Functions defined after this form will have timing instrumentation added.

```lisp
(profile factorial fibonacci)
(defun factorial (n) ...)  ;; Will be profiled
(defun fibonacci (n) ...)  ;; Will be profiled
```

#### `(unprofile fn1 fn2 ...)`
Disable profiling for specified functions. Subsequent definitions will not be instrumented.

```lisp
(unprofile factorial)
(defun factorial (n) ...)  ;; Will NOT be profiled
```

### Programmatic API

#### `(profile-function name)`
Add a function name to the profiled functions list.

```lisp
(habu-sbcl-codegen::profile-function 'my-fn)
```

#### `(unprofile-function name)`
Remove a function name from the profiled functions list.

```lisp
(habu-sbcl-codegen::unprofile-function 'my-fn)
```

#### `(function-profiled-p name)`
Check if a function is currently being profiled.

```lisp
(habu-sbcl-codegen::function-profiled-p 'my-fn)  ;; Returns T or NIL
```

### Primitives

#### `(get-time-ns)`
Returns current time in nanoseconds as a fixnum. Useful for manual timing.

```lisp
(let ((start (get-time-ns)))
  ;; ... do work ...
  (let ((elapsed (- (get-time-ns) start)))
    (println elapsed)))
```

## Output Format

Profiled functions print timing information when they return:

```
PROFILE: <FUNCTION-NAME> <ELAPSED-NANOSECONDS>
```

Example output:
```
PROFILE: FACTORIAL 2500
PROFILE: FIBONACCI 15000
```

## Implementation Details

### How Profiling Works

1. When `(profile fn)` is encountered, the function name is added to `*profiled-functions*`
2. When `compile-defun` compiles a function in the profiled list, it wraps the body with `wrap-body-with-profile`
3. The wrapper:
   - Captures start time via `get-time-ns`
   - Executes the original function body
   - Calculates elapsed time
   - Prints profiling output
   - Returns the original result

### Wrapper Code Structure

```lisp
(let ((start (get-time-ns)))
  (let ((result (progn <original-body>)))
    (let ((elapsed (- (get-time-ns) start)))
      (print "PROFILE: ")
      (print '<function-name>)
      (print " ")
      (println elapsed))
    result))
```

### Runtime Support

- **Runtime table entry 50** (offset 400): `habu_get_time_ns`
- Timer uses `clock_gettime(CLOCK_MONOTONIC)` on POSIX systems
- Resolution: nanosecond precision (actual accuracy depends on hardware)

### State Variables

| Variable | Description |
|----------|-------------|
| `*profiled-functions*` | List of function names currently being profiled |

## Limitations

1. **Compile-time only**: Profiling is determined at compile time. You cannot dynamically enable/disable profiling for already-compiled functions.

2. **No call counting**: Current implementation only measures time, not call counts.

3. **No call graph**: Does not track caller-callee relationships.

4. **Output only**: Profiling data is printed to stdout, not stored in a data structure accessible from Habu code.

5. **Overhead**: Profiled functions have additional overhead from timing calls and printing.

## Examples

### Profile Recursive Function

```lisp
(profile factorial)

(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))

(factorial 5)
;; Output (one line per recursive call):
;; PROFILE: FACTORIAL 500
;; PROFILE: FACTORIAL 800
;; PROFILE: FACTORIAL 1100
;; PROFILE: FACTORIAL 1400
;; PROFILE: FACTORIAL 1700
;; PROFILE: FACTORIAL 2000
```

### Profile Multiple Functions

```lisp
(profile helper main-function)

(defun helper (x)
  (* x 2))

(defun main-function (x)
  (+ (helper x) 1))

(main-function 5)
;; Output:
;; PROFILE: HELPER 300
;; PROFILE: MAIN-FUNCTION 800
```

### Manual Timing

```lisp
(let ((start (get-time-ns)))
  (dotimes (i 1000)
    (some-expensive-operation))
  (let ((elapsed (- (get-time-ns) start)))
    (print "Total time: ")
    (println elapsed)))
```

## Future Enhancements

Potential improvements for a more comprehensive profiler:

1. **Call counting**: Track number of times each function is called
2. **Call graph**: Record caller-callee relationships
3. **Data structures**: Store profiling data in Habu-accessible hash tables
4. **Aggregation**: Sum total time per function across all calls
5. **Sampling**: Statistical profiling with lower overhead
6. **Report generation**: `(profile-report)` to print summary statistics
