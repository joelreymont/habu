# Habu Trace Facility

Function tracing for debugging Habu programs.

## Quick Start

```lisp
;; Enable tracing for a function
(trace my-function)

;; Define and call the function
(defun my-function (x y)
  (+ x y))

(my-function 3 4)
;; Output:
;; TRACE: (MY-FUNCTION 3 4)
;; TRACE: MY-FUNCTION => 7

;; Disable tracing
(untrace my-function)
```

## API Reference

### Compile-Time Forms

#### `(trace fn1 fn2 ...)`
Enable tracing for one or more functions. Functions defined after this form will have trace instrumentation added.

```lisp
(trace factorial fibonacci)
(defun factorial (n) ...)  ;; Will be traced
(defun fibonacci (n) ...)  ;; Will be traced
```

#### `(untrace fn1 fn2 ...)`
Disable tracing for specified functions.

```lisp
(untrace factorial)
(defun factorial (n) ...)  ;; Will NOT be traced
```

### Programmatic API

#### `(trace-function name)`
Add a function name to the traced functions list.

```lisp
(habu-sbcl-codegen::trace-function 'my-fn)
```

#### `(untrace-function name)`
Remove a function name from the traced functions list.

```lisp
(habu-sbcl-codegen::untrace-function 'my-fn)
```

#### `(function-traced-p name)`
Check if a function is currently being traced.

```lisp
(habu-sbcl-codegen::function-traced-p 'my-fn)  ;; Returns T or NIL
```

## Output Format

Traced functions print entry and exit information:

**Entry:**
```
TRACE: (<FUNCTION-NAME> <ARG1> <ARG2> ...)
```

**Exit:**
```
TRACE: <FUNCTION-NAME> => <RETURN-VALUE>
```

## Example Output

### Simple Function
```lisp
(trace add)
(defun add (a b) (+ a b))
(add 3 4)
```
Output:
```
TRACE: (ADD 3 4)
TRACE: ADD => 7
```

### Recursive Function
```lisp
(trace factorial)
(defun factorial (n)
  (if (= n 0)
      1
      (* n (factorial (- n 1)))))
(factorial 3)
```
Output:
```
TRACE: (FACTORIAL 3)
TRACE: (FACTORIAL 2)
TRACE: (FACTORIAL 1)
TRACE: (FACTORIAL 0)
TRACE: FACTORIAL => 1
TRACE: FACTORIAL => 1
TRACE: FACTORIAL => 2
TRACE: FACTORIAL => 6
```

### Multiple Functions
```lisp
(trace helper main-fn)
(defun helper (x) (* x 2))
(defun main-fn (x) (+ (helper x) 1))
(main-fn 5)
```
Output:
```
TRACE: (MAIN-FN 5)
TRACE: (HELPER 5)
TRACE: HELPER => 10
TRACE: MAIN-FN => 11
```

## Implementation Details

### How Tracing Works

1. When `(trace fn)` is encountered, the function name is added to `*traced-functions*`
2. When `compile-defun` compiles a function in the traced list, it wraps the body with `wrap-body-with-trace`
3. The wrapper:
   - Prints entry message with function name and arguments
   - Executes the original function body
   - Prints exit message with return value
   - Returns the original result

### Wrapper Code Structure

```lisp
(progn
  (print "TRACE: (")
  (print '<function-name>)
  (mapc (lambda (arg) (print " ") (print arg)) (list <args>))
  (println ")")
  (let ((result (progn <original-body>)))
    (print "TRACE: ")
    (print '<function-name>)
    (print " => ")
    (println result)
    result))
```

### State Variables

| Variable | Description |
|----------|-------------|
| `*traced-functions*` | List of function names currently being traced |

## Comparison with Profiler

| Feature | Trace | Profile |
|---------|-------|---------|
| Shows arguments | Yes | No |
| Shows return value | Yes | No |
| Shows timing | No | Yes |
| Entry/exit messages | Yes | Exit only |
| Call nesting visible | Yes | By timing |
| Overhead | Higher | Lower |

## Best Practices

1. **Trace sparingly**: Tracing adds significant overhead from printing
2. **Use for debugging**: Enable trace when investigating specific functions
3. **Recursive functions**: Trace output shows call depth naturally
4. **Multiple functions**: Trace related functions to see control flow
5. **Disable when done**: Use `untrace` to remove instrumentation

## Limitations

1. **Compile-time only**: Tracing is determined at compile time
2. **Output only**: Trace data is printed, not stored
3. **No filtering**: All calls are traced (no conditional tracing)
4. **No timestamps**: Use profiler for timing information
