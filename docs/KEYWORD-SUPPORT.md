# Keyword Support in habu0 (Tag 7)

## Summary
Complete keyword support has been added to habu0. Keywords use tag 7 and are self-evaluating values stored in the keyword table.

## Implementation Components

### 1. Reader Support (Lines 313-361, 756-761)
- `intern-keyword`: Interns keywords in the keyword table (with tag 7)
- `keyword-name`: Extracts the name string from a keyword
- `make-keyword-from-string`: Creates a keyword with tag 7 from a string
- `read-keyword`: Parses `:name` syntax and returns interned keyword
- Reader recognizes `:` prefix and calls `read-keyword` (line 890)

### 2. Eval Support (Lines 1204, 1392-1394)
- Keywords are self-evaluating in `h0-eval` (line 1204)
- KEYWORDP predicate in `h0-eval` (lines 1392-1394):
  ```lisp
  ((if (symbolp op) (op=keywordp op) nil)
   (let ((arg (h0-eval (cadr expr) env fenv)))
     (if (keywordp arg) t nil)))
  ```

### 3. Compile Support (Lines 1874, 2044-2046)
- Keyword literals compile to IR: `(ir-tag-kw-lit keyword)` (line 1874)
- KEYWORDP predicate compiles to IR: `(ir-tag-keywordp arg-ir)` (lines 2044-2046)

### 4. IR Evaluation (Line 3832)
- `h0-eval-ir` handles KEYWORDP IR tag (line 3832)

### 5. Code Generation (Lines 3020-3038, 3424-3430)
- `h0-codegen-kw-lit`: Generates ARM64 code to allocate keyword with tag 7
- KEYWORDP codegen: Checks if value's tag equals 7

## Tag 7 Memory Layout
Keywords have the same memory layout as symbols:
```
[length:8 bytes][character data:N bytes]
```
The pointer is tagged with 7 (instead of 2 for symbols or 4 for strings).

## Operator Cache
Added to support fast predicate dispatch:
- `*op-keywordp*` variable (line 56)
- `op=keywordp` comparison function (line 687)
- Initialized in `init-operators` (line 1588)

## Testing
Test files created:
- `/Users/joel/Work/habu/test-keyword-simple.lisp` - Basic keyword tests
- `/Users/joel/Work/habu/test-keyword.lisp` - Comprehensive tests

## Changes Made
1. Added `(defvar *op-keywordp* nil)` at line 56
2. Added `(defun op=keywordp (sym) (eq sym *op-keywordp*))` at line 687
3. Added `(setq *op-keywordp* (intern "KEYWORDP"))` at line 1588
4. Added KEYWORDP handler in h0-eval at lines 1392-1394

All other keyword infrastructure was already present in habu0.lisp.
