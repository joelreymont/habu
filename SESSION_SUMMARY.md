# Session Summary - Bug #20 Root Cause Fix

## Fixes Implemented

### 1. funcall-ir x20 Fix (CRITICAL)
**Root Cause**: Lambdas have no prologue, but funcall-ir didn't set x20 for lambda parameter stores.
- Lambda param-stores write to `[x20 - offset]`
- funcall-ir only set x24 (closure env), not x20 (frame base)
- Result: Lambda params clobbered caller's variables at low offsets

**Solution**: 
- Calculate parameter frame space: `param-bytes = num-args * 8`, round to 16-byte alignment
- Allocate frame: `SUB sp, sp, #param-space`  
- Set x20: `ADD x20, sp, #(param-space - 8)` so param stores land in allocated area
- After call: deallocate and restore sp and x20

**Tests Passing**:
- test_path_var.lisp (string at offset 0) → exit 42 ✓
- test_fixnum_at_offset_0.lisp (fixnum at offset 0) → exit 42 ✓
- test_concat_simple.lisp (concat 2 strings) → exit 42 ✓
- test_concat_recursive.lisp (labels + concat) → exit 42 ✓
- test_simple_read.lisp (sys-read + buffer-to-string) → exit 42 ✓

### 2. number-to-string Gensym Fix
**Root Cause**: Hardcoded variable names `d1`, `d2`, `d3`, `remainder`, `remainder2`, `remainder3` caused shadowing in nested calls.

**Solution**: Use gensyms for all 10 variables in the transformation (similar to string-append fix).

## Remaining Issue

**test_recursive_read_concat.lisp** and **test_large_buffer_concat.lisp** still crash (SIGSEGV exit 139).

**Pattern that fails**:
- labels function with recursive file reading
- Each iteration: sys-read → buffer-to-string → cons to list
- After recursion: concat-string-list on accumulated chunks

**What works**:
- Simple file read (no recursion) ✓
- Recursive labels building string list (no file I/O) ✓  
- concat-string-list with manually created strings ✓
- buffer-to-string with small buffers ✓

**What fails**:
- Recursive labels + sys-read + buffer-to-string (any buffer size)

**Next investigation**: 
Isolate whether issue is:
1. Heap exhaustion from repeated buffer allocations
2. Stack overflow from deep recursion
3. Buffer size causing frame overflow
4. Interaction between nested labels and heap allocation

**Files Modified**:
- bootstrap/compiler.lisp: funcall-ir x20 setup (lines 4050-4164), number-to-string gensyms (lines 1770-1837)

**Commits**:
- 7bf3600: "Fix funcall-ir x20 and number-to-string gensyms for Bug #20"
