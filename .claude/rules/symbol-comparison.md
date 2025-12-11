# Symbol Comparison and Dispatch Rules - MANDATORY

## ABSOLUTE RULES - NO EXCEPTIONS

### 1. NEVER Use String Comparison for Symbol Dispatch

**ALL symbol comparisons MUST use `eq` on properly interned symbols.**

String comparison (`string=`, `string-equal`, comparing `symbol-name`) is:
- **SLOW** (O(n) vs O(1))
- **WRONG** for a Lisp implementation
- **A BUG** indicating broken interning

**ENFORCEMENT:** `string-equal` in habu0.lisp currently crashes with an error message to catch violations.

### 2. Dispatch Tables vs Individual Variables

**Use dispatch tables for related data, NOT dozens of individual variables.**

```lisp
;; WRONG - proliferation of global variables for data
(defvar *kw-x0* nil)
(defvar *kw-x1* nil)
;; ... 30 more variables

;; RIGHT - single dispatch table
(defvar *register-table* nil)  ; alist of (keyword . number)
```

**EXCEPTION:** The `*op-*` variables in habu0.lisp are INTENTIONAL symbol caching for the interpreter. They cache interned habu symbols for O(1) `eq` dispatch. This is the correct pattern - symbols are interned once, cached in variables, then used with `eq`.

### 3. Use `case`/`ecase` for Dispatch When Possible

```lisp
;; For known small sets - use case
(case op
  (if ...)
  (let ...)
  (cons ...)
  (otherwise (table-lookup op)))
```

### 4. Symbols MUST Be Interned at Startup

The `make-habu-symbol-form` and `make-habu-keyword-form` macros handle cross-compilation:

```lisp
;; These macros expand at SBCL compile time to habu primitive calls
;; At runtime, they create habu symbols and register them in intern table
(setq *op-if* (make-habu-symbol-form "IF"))
(setq *kw-x0* (make-habu-keyword-form "X0"))
```

## Cross-Symbol-Table Problem (Mode 1024)

When SBCL compiles habu0:
- Quoted symbols like `'list` become SBCL symbol pointers
- Habu-read creates habu-native symbols (tag 2)
- These are DIFFERENT objects - `eq` fails

**SOLUTION: Use macros to expand string literals to habu primitive calls at compile time:**

```lisp
;; This macro expands to integer-based habu primitive calls
;; No SBCL strings in the runtime code
(make-habu-symbol-form "FOO")
→ (let ((str (make-habu-string ...)))
    (let ((sym (make-symbol-from-string str)))
      (register-in-intern-table sym)
      sym))
```

## Current Implementation

### fenv-lookup (O(1) symbol equality)
All fenv keys are habu symbols. Lookup uses `sym-eq` (which is `eq`):

```lisp
(defun fenv-lookup (sym fenv)
  (if (null fenv) nil
      (let ((entry (car fenv)))
        (if (sym-eq sym (car entry))  ; O(1) pointer comparison
            (cdr entry)
            (fenv-lookup sym (cdr fenv))))))
```

### Register Table (single dispatch table)
Register keywords use a single dispatch table populated via `define-registers`:

```lisp
(define-registers
  ("X0" 0) ("X1" 1) ... ("X30" 30)
  ("SP" 31) ("ENV" 20) ("HEAP" 28))

(defun habu0-reg (r)
  (let ((entry (assoc r *register-table* :test #'eq)))
    (if entry (cdr entry)
        (error "unknown register"))))
```

## Violations - STOP AND FIX

If you find yourself writing ANY of these, **STOP IMMEDIATELY**:

- `(string= (symbol-name x) "FOO")` → Fix interning
- `(string-equal name1 name2)` for symbols → Fix interning
- `(setq *foo* "FOO")` storing strings for symbol lookup → Use `make-habu-symbol-form`
- Multiple variables for same category data → Use dispatch table
