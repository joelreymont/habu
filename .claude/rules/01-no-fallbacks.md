# No Silent Fallbacks - CRASH LOUDLY

## Core Principle

**Silent fallbacks MASK BUGS. Always crash with a clear error message.**

When code encounters an unexpected case, it MUST fail immediately with a descriptive error. Never silently return a default value or skip an operation.

## Mandatory Patterns

### WRONG - Silent fallback
```lisp
;; BAD: Silently returns nil on unknown case
(defun handle-op (op)
  (cond
    ((eq op 'add) (do-add))
    ((eq op 'sub) (do-sub))
    (t nil)))  ; WRONG - hides bugs
```

### RIGHT - Crash with error
```lisp
;; GOOD: Crashes immediately with context
(defun handle-op (op)
  (cond
    ((eq op 'add) (do-add))
    ((eq op 'sub) (do-sub))
    (t (error "handle-op: unknown op ~S" op))))
```

## Rules

1. **Every `cond`/`case` must have an error in the default branch** unless there's a legitimate reason for a default value (document why)

2. **Never return nil/0/empty as "not found"** - use explicit error or two-value return

3. **Never silently skip operations** - if something should happen and doesn't, crash

4. **Function dispatch must error on unknown** - dispatch tables, op handlers, type handlers must crash on unrecognized input

5. **Before writing fallback code, ask: "What bug am I hiding?"**

## Examples in Habu

```lisp
;; IR handler - MUST error on unknown
(defun codegen-ir (ir)
  (let ((tag (car ir)))
    (cond
      ((eq tag 'lit) (codegen-lit ir))
      ((eq tag 'add-ir) (codegen-add ir))
      ;; ... all cases ...
      (t (error "codegen-ir: unhandled IR tag ~S" tag)))))

;; Lookup - MUST error if not found (or use explicit maybe pattern)
(defun fenv-lookup (name fenv)
  (let ((entry (assoc name fenv)))
    (if entry
        (cdr entry)
        (error "fenv-lookup: unknown function ~S" name))))
```

## Rationale

Silent fallbacks cause:
- Bugs that manifest far from their source
- Hours of debugging "impossible" states
- Production failures that could have been caught immediately
- False confidence in code correctness

A crash with a stack trace tells you exactly what went wrong and where. A silent fallback gives you corrupted state and no clues.
