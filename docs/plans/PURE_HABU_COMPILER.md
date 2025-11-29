# SBCL Primitives → Pure Habu Replacements

## Critical Dependencies (Must Replace)

### 1. Multiple Value Returns (116 uses)
**SBCL**: `(multiple-value-bind (a b) (values x y) ...)`
**Pure Habu**: Use cons cells for tuples
```lisp
;; SBCL version:
(multiple-value-bind (code symtab)
    (nc-compile-program-with-symtab forms nil)
  ...)

;; Pure Habu version:
(let ((result (nc-compile-program-with-symtab forms nil)))
  (let ((code (car result))
        (symtab (cdr result)))
    ...))
```
**Strategy**: Return `(cons value1 value2)` instead of `values`

### 2. Loop Macro (7 uses)
**SBCL**: `(loop for x in list collect ...)`
**Pure Habu**: Use labels recursion
```lisp
;; SBCL version:
(loop for x in list collect (f x))

;; Pure Habu version:
(labels ((map-fn (l acc)
           (if (null l)
               (reverse acc)
               (map-fn (cdr l) (cons (f (car l)) acc)))))
  (map-fn list nil))
```

### 3. Format (54 uses)  
**SBCL**: `(format t "~A: ~D" name value)`
**Pure Habu**: Use string-concat + number-to-string
```lisp
;; SBCL version:
(format t "Compiled ~A bytes~%" (length code))

;; Pure Habu version:
(sys-write 1 "Compiled " 9)
(sys-write 1 (number-to-string (length code)) 5)
(sys-write 1 " bytes\n" 7)
```

### 4. Mapcar (44 uses)
**SBCL**: `(mapcar #'f list)`
**Pure Habu**: Implement using labels
```lisp
(defun pure-mapcar (fn lst)
  (labels ((map-iter (l acc)
             (if (null l)
                 (reverse acc)
                 (map-iter (cdr l) (cons (funcall fn (car l)) acc)))))
    (map-iter lst nil)))
```

### 5. Apply (10 uses)
**SBCL**: `(apply #'+ list)`
**Pure Habu**: Implement for common cases
```lisp
(defun pure-apply (fn lst)
  ;; Handle up to 5 args (expandable)
  (cond
    ((null lst) (funcall fn))
    ((null (cdr lst)) (funcall fn (car lst)))
    ((null (cddr lst)) (funcall fn (car lst) (cadr lst)))
    ;; ... up to needed arity
  ))
```

### 6. Fboundp (in feature checks)
**SBCL**: `(fboundp 'optimize-ir)`
**Pure Habu**: Always define all functions, or use explicit flags
```lisp
;; SBCL version:
(if (fboundp 'optimize-ir)
    (optimize-ir expr)
    expr)

;; Pure Habu version:
(if *optimize-enabled*
    (optimize-ir expr)
    expr)
```

## What We ALREADY Have ✓

- ✓ defun, let, let*, if, cond, when, unless
- ✓ labels, lambda, funcall  
- ✓ +, -, *, /, =, <, >, <=, >=
- ✓ cons, car, cdr, list, null, consp
- ✓ quote, progn
- ✓ and, or, not
- ✓ setq (mutation)
- ✓ string-concat, number-to-string
- ✓ sys-write, sys-exit (native I/O)
- ✓ native-read-file (file I/O)

## Implementation Plan

### Phase 1: Core Utilities (1 day)
```lisp
pure-mapcar
pure-apply  
pure-member
pure-assoc
pure-remove-if
pure-filter
```

### Phase 2: Replace multiple-value-bind (2-3 days)
- Change all functions returning multiple values to return cons cells
- Update all call sites to destructure cons cells
- ~116 locations to update

### Phase 3: Replace loop (1 day)
- Only 7 uses, all simple patterns
- Convert to labels recursion

### Phase 4: Replace format (1 day)
- 54 uses, mostly for debugging/verbose output
- Use sys-write + string-concat

### Phase 5: Codegen (2-3 days)
- Port ARM64 codegen to pure Habu
- No SBCL dependencies

**Total Estimate**: 7-10 days for fully pure Habu compiler

## Immediate Action

Start with Phase 1: Add missing list utilities to compiler-pure.lisp
