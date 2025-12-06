# Linearization Pass Design

## Overview

The linearization pass converts tree-structured IR to linear IR (Three-Address Code / A-Normal Form style). This architectural change:

1. **Isolates tree recursion** to a single pass
2. **Makes codegen trivially iterative** (just loop over instructions)
3. **Opens optimization opportunities** (DCE, CSE, peephole)
4. **Enables self-hosting** by avoiding deep recursion in native Habu

## Current Architecture

```
Source → [parse] → S-expr → [compile] → Tree-IR → [lift-lambdas] → IR → [codegen] → Bytes
                                                                          ↑
                                                               Deep recursion here
```

## Proposed Architecture

```
Source → [parse] → S-expr → [compile] → Tree-IR → [lift-lambdas] → IR
       → [linearize] → Linear-IR → [codegen-linear] → Bytes
            ↑ NEW                        ↑
       One-time tree              Simple iteration
         traversal                  (no recursion)
```

## Tree IR vs Linear IR

### Tree IR (Current)
```lisp
(add (mul (var 0) (lit 2))
     (sub (var 1) (lit 3)))
```

Nested structure requires recursive traversal.

### Linear IR (Proposed)
```lisp
((load-var t0 0)      ; t0 = var[0]
 (load-lit t1 2)      ; t1 = 2
 (mul t2 t0 t1)       ; t2 = t0 * t1
 (load-var t3 1)      ; t3 = var[1]
 (load-lit t4 3)      ; t4 = 3
 (sub t5 t3 t4)       ; t5 = t3 - t4
 (add result t2 t5))  ; result = t2 + t5
```

Flat list - codegen just iterates and emits.

## Linear IR Instruction Set

### Values
| Instruction | Meaning |
|-------------|---------|
| `(load-lit dst val)` | dst = tagged literal value |
| `(load-var dst offset)` | dst = env[offset] |
| `(load-nil dst)` | dst = nil (tag 6) |
| `(load-sym dst name)` | dst = allocate symbol |
| `(load-str dst string)` | dst = allocate string |

### Arithmetic
| Instruction | Meaning |
|-------------|---------|
| `(add dst src1 src2)` | dst = src1 + src2 |
| `(sub dst src1 src2)` | dst = src1 - src2 |
| `(mul dst src1 src2)` | dst = src1 * src2 |
| `(div dst src1 src2)` | dst = src1 / src2 |
| `(mod dst src1 src2)` | dst = src1 mod src2 |

### Comparison
| Instruction | Meaning |
|-------------|---------|
| `(cmp-eq dst src1 src2)` | dst = (src1 == src2) |
| `(cmp-lt dst src1 src2)` | dst = (src1 < src2) |
| `(cmp-gt dst src1 src2)` | dst = (src1 > src2) |
| `(cmp-le dst src1 src2)` | dst = (src1 <= src2) |
| `(cmp-ge dst src1 src2)` | dst = (src1 >= src2) |

### List Operations
| Instruction | Meaning |
|-------------|---------|
| `(cons dst car-src cdr-src)` | dst = cons(car, cdr) |
| `(car dst src)` | dst = car(src) |
| `(cdr dst src)` | dst = cdr(src) |

### Control Flow
| Instruction | Meaning |
|-------------|---------|
| `(label name)` | label definition |
| `(jump label)` | unconditional jump |
| `(jump-if-nil src label)` | jump if src is nil |
| `(jump-if-not-nil src label)` | jump if src is not nil |

### Function Calls
| Instruction | Meaning |
|-------------|---------|
| `(call dst fn-name args...)` | dst = fn(args...) |
| `(funcall dst fn-temp args...)` | dst = funcall(fn, args...) |
| `(return src)` | return src |

### Variables
| Instruction | Meaning |
|-------------|---------|
| `(setq offset src)` | env[offset] = src |
| `(bind count)` | extend env by count slots |
| `(unbind count)` | shrink env by count slots |

## Linearization Algorithm

### Iterative Post-Order Traversal

```lisp
(defun linearize (ir)
  "Convert tree IR to linear IR using iterative post-order traversal"
  (let ((work-stack (list (cons :visit ir)))
        (temp-counter 0)
        (output nil)
        (temp-map nil))  ; maps IR nodes to their result temps

    (while work-stack
      (let* ((item (pop work-stack))
             (action (car item))
             (node (cdr item)))
        (case action
          ;; First visit: push emit action, then children
          (:visit
           (if (leaf-ir? node)
               (emit-leaf node)
               (progn
                 (push (cons :emit node) work-stack)
                 (push-children node work-stack))))

          ;; Second visit: all children done, emit instruction
          (:emit
           (emit-node node temp-map)))))

    (reverse output)))
```

### Key Insight

The algorithm visits each node twice:
1. **First visit (:visit)**: Check if leaf or push children
2. **Second visit (:emit)**: Children are done, emit instruction

This is post-order traversal with explicit stack - no recursion needed.

## Control Flow Linearization

### If Expression
```lisp
;; Tree IR
(if-ir test then else)

;; Linear IR
(... test instructions ...)
(jump-if-nil t-test else-label)
(... then instructions ...)
(jump end-label)
(label else-label)
(... else instructions ...)
(label end-label)
```

### While Loop
```lisp
;; Tree IR
(while-ir test body)

;; Linear IR
(label loop-start)
(... test instructions ...)
(jump-if-nil t-test loop-end)
(... body instructions ...)
(jump loop-start)
(label loop-end)
```

## Codegen from Linear IR

```lisp
(defun codegen-linear (linear-ir rtaddrs fnoffs)
  "Generate ARM64 code from linear IR - simple iteration, no recursion"
  (let ((code nil)
        (temp-offsets nil))  ; maps temps to stack offsets
    (dolist (instr linear-ir)
      (push (emit-linear-instr instr temp-offsets rtaddrs fnoffs) code))
    (apply #'append (reverse code))))

(defun emit-linear-instr (instr temp-offsets rtaddrs fnoffs)
  "Emit ARM64 code for single linear instruction"
  (case (car instr)
    (load-lit (emit-load-lit (cadr instr) (caddr instr) temp-offsets))
    (load-var (emit-load-var (cadr instr) (caddr instr) temp-offsets))
    (add (emit-add (cadr instr) (caddr instr) (cadddr instr) temp-offsets))
    ;; ... etc
    ))
```

## Temporary Management

Each temp is allocated a stack slot:
- `t0` → `[sp + 0]`
- `t1` → `[sp + 8]`
- `t2` → `[sp + 16]`
- ...

After linearization, register allocation can improve this by:
1. Keeping frequently-used temps in registers
2. Reusing slots for non-overlapping temps (liveness analysis)

## Benefits

### 1. Self-Hosting Enabled
- Linearize pass uses iterative algorithm
- Codegen is trivial loop
- No deep recursion anywhere

### 2. Optimization Opportunities
- **Dead code elimination**: Remove unused temps
- **Common subexpression elimination**: Reuse identical computations
- **Constant folding**: Evaluate at compile time
- **Peephole optimization**: Pattern-match and improve instruction sequences

### 3. Debugging
- Linear IR is easy to print and inspect
- Each instruction is independent
- Control flow is explicit (labels + jumps)

### 4. Industry Standard
- LLVM uses SSA (similar concept)
- GCC uses GIMPLE (three-address code)
- Chez Scheme uses similar nanopass architecture

## Implementation Plan

1. **Define linear IR data structures**
2. **Implement linearize function** (iterative post-order)
3. **Implement codegen-linear** (simple iteration)
4. **Wire into compilation pipeline**
5. **Test with complex expressions**
6. **Remove old recursive codegen**

## Files to Modify

- `bootstrap/codegen.lisp`: Add linearize, codegen-linear
- `bootstrap/compiler.lisp`: Wire linearize into pipeline
- `habu0.lisp`: Add linearize for Stage 1

## Testing Strategy

1. Unit tests: linearize individual IR nodes
2. Round-trip: tree IR → linear IR → bytes, compare with old codegen
3. Stress test: deeply nested expressions
4. Self-hosting: compile habu0.lisp with linearized codegen
