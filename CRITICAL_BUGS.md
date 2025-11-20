# Habu - Critical Runtime Bugs

**Date Identified**: November 20, 2024
**Priority**: CRITICAL - Memory Safety & Correctness Issues
**Status**: Comprehensive bug list compiled, fixes in progress

---

## Executive Summary

Multiple critical bugs have been identified in the runtime system that affect memory safety, GC correctness, and basic functionality. These issues must be addressed before the system can be considered reliable.

**Impact**: HIGH - Current GC implementation is unsafe and will corrupt data
**Risk**: Memory corruption, crashes, undefined behavior
**Severity**: CRITICAL - Affects core runtime functionality

---

## Category 1: GC Runtime (runtime/gc.c) - CRITICAL

### Bug 1.1: Root Registration Broken
**Location**: `runtime/gc.c:969-1009`
**Severity**: CRITICAL - Memory corruption
**Status**: 🔴 Unfixed

**Problem**:
- `gc_heap->roots` stores raw object pointers
- Only the internal array is updated during copying GC
- Callers pass `habu_value_t` by value
- No pointer-to-pointer indirection exists
- **Result**: When GC relocates an object, caller's pointer becomes stale

**Evidence**:
- Tests note missing API: `tests/test_gc.c:204-225`

**Impact**:
- Any GC evacuation immediately invalidates all user-visible pointers
- All heap objects become dangling pointers after first collection
- Guaranteed memory corruption

**Fix Required**:
1. Change root registration to use pointer-to-pointer (`habu_value_t**`)
2. Update all roots after relocation
3. Provide API for stack-based root protection
4. Update test suite

### Bug 1.2: No Production Root Registration
**Location**: Throughout codebase
**Severity**: CRITICAL - All objects treated as garbage
**Status**: 🔴 Unfixed

**Problem**:
- NO production code calls `habu_gc_add_root`
- Only test code registers roots: `tests/test_gc.c:204-290`
- Functions like `habu_cons`, `habu_make_vector`, etc. never protect return values
- **Result**: First GC collection treats EVERYTHING as garbage

**Impact**:
- All heap objects freed or moved immediately
- Any GC trigger causes total data loss
- System cannot work correctly

**Fix Required**:
1. Add root registration to all constructor functions
2. Implement stack-based scoped roots
3. Add automatic root tracking for active stack frames
4. Document root registration requirements

### Bug 1.3: Constructors Don't Root Intermediate Results
**Location**: `runtime/gc.c:1173-1184` (habu_make_symbol), others
**Severity**: CRITICAL - Use-after-free
**Status**: 🔴 Unfixed

**Problem**:
```c
// habu_make_symbol allocates symbol, then calls habu_make_string
symbol = habu_gc_alloc(SYMBOL_SIZE);
// If habu_make_string triggers GC here, symbol is not rooted!
symbol->name = habu_make_string(...);  // GC can move/free symbol
return symbol;  // Returns dangling pointer
```

**Impact**:
- Nested allocations can trigger GC mid-construction
- Partially-constructed objects not rooted
- GC reclaims or moves them
- Returned pointer is dangling

**Affected Functions**:
- `habu_make_symbol`
- `habu_make_vector` (if it allocates elements)
- `habu_make_closure`
- Any function that does multiple allocations

**Fix Required**:
1. Root object immediately after allocation
2. Unroot when fully constructed
3. Use scope-based root guards
4. Add GC-safe allocation helpers

### Bug 1.4: NULL Dereference in habu_make_closure
**Location**: `runtime/runtime.c:127-133`
**Severity**: CRITICAL - Crash on OOM
**Status**: 🔴 Unfixed

**Problem**:
```c
habu_closure_t* closure = habu_gc_alloc(sizeof(habu_closure_t));
closure->env = env;  // No NULL check! Crashes if heap exhausted
closure->params = params;
```

**Impact**:
- When heap is exhausted, `habu_gc_alloc` returns NULL
- Dereferencing NULL causes immediate crash
- Should signal out-of-memory error instead

**Fix Required**:
1. Check `habu_gc_alloc` result before dereferencing
2. Implement proper OOM handling
3. Signal error condition
4. Add NULL checks to ALL allocation sites

---

## Category 2: Common Lisp Runtime (runtime/*.lisp) - CRITICAL

### Bug 2.1: Mark-Sweep Compaction Doesn't Update Pointers
**Location**: `runtime/memory.lisp:265-297`
**Severity**: CRITICAL - Heap corruption
**Status**: 🔴 Unfixed

**Problem**:
- Compaction copies live blocks with `memmove`
- Copies raw bytes to `compact_to` address
- **Never rewrites pointer slots inside objects**
- **Result**: All cons/vector/string pointers still point to old (now invalid) locations

**Example**:
```lisp
; Before compaction: cons at 0x1000 points to car at 0x2000
; Compaction copies cons to 0x3000
; But car pointer still says 0x2000 (old location!)
; 0x2000 is now garbage memory
```

**Impact**:
- As soon as GC compacts, entire heap becomes invalid
- All pointer chasing leads to garbage
- Total data corruption

**Fix Required**:
1. Track forwarding addresses during copy
2. Update all pointer slots to new addresses
3. Implement proper pointer fixup pass
4. Add compaction tests

### Bug 2.2: No Root Registration in CL Runtime
**Location**: `runtime/memory.lisp:37-50`
**Severity**: CRITICAL - All objects freed
**Status**: 🔴 Unfixed

**Problem**:
- Root registration helpers defined but NEVER USED
- No `register-gc-root` call sites anywhere
- GC immediately frees everything not manually passed in
- **Exception**: `*symbol-table*` survives only because it's in SBCL memory, not Habu heap

**Impact**:
- Any GC trigger frees all Habu heap objects
- Data structures not properly rooted
- System relies on accidental survival

**Fix Required**:
1. Call `register-gc-root` for all global variables
2. Add automatic root tracking
3. Integrate with SBCL GC hooks if possible
4. Document rooting requirements

### Bug 2.3: Tag Value Mismatch Between CL and C Runtimes
**Location**: `runtime/memory.lisp:52-58` vs `runtime/object.h:21-27`
**Severity**: HIGH - Type confusion
**Status**: 🔴 Unfixed

**Problem**:
```lisp
; CL runtime (memory.lisp)
TAG_STRING  = #x3   ; 0011
TAG_SYMBOL  = #x2   ; 0010

// C runtime (object.h)
TAG_STRING  = 0x4   ; 0100
TAG_SYMBOL  = 0x2   ; 0010
```

**Impact**:
- Data exchanged between bootstrap and C runtime misinterpreted
- Strings tagged as 0x3 in CL read as different type in C
- Type predicates give wrong answers
- Runtime incompatibility

**Fix Required**:
1. Standardize tag values across runtimes
2. Use shared header file for tag definitions
3. Add cross-runtime tests
4. Document canonical tag scheme

---

## Category 3: Hash Tables (runtime/hash-tables.lisp) - BROKEN

### Bug 3.1: Undefined `allocate` Function
**Location**: `runtime/hash-tables.lisp:28-34, 161-170`
**Severity**: HIGH - Code doesn't compile/run
**Status**: 🔴 Unfixed

**Problem**:
```lisp
(allocate size)  ; Called but never defined!
```

**Impact**:
- Hash table code doesn't compile
- Cannot run at all
- Feature completely broken

**Fix Required**:
1. Define `allocate` function
2. OR use proper GC allocation primitives
3. Ensure allocated nodes have proper headers
4. Make nodes GC-traversable

### Bug 3.2: Wrong Header Access for Strings/Symbols
**Location**: `runtime/hash-tables.lisp:56-65, 88-97`
**Severity**: HIGH - Wrong data accessed
**Status**: 🔴 Unfixed

**Problem**:
```lisp
(sb-sys:sap-ref-64 sap 0)  ; Reads HEADER, not payload!
```

**What Should Happen**:
- For strings: Read length at correct offset
- For symbols: Read name pointer at correct offset

**What Actually Happens**:
- Reads header word (type tag + metadata)
- Misinterprets header as length/pointer
- Hash and equality completely wrong

**Impact**:
- String hashing broken
- Symbol hashing broken
- Hash tables don't work correctly

**Fix Required**:
1. Use correct offsets for string length (not 0)
2. Use correct offsets for symbol name pointer
3. Add layout documentation
4. Write tests

### Bug 3.3: Incorrect Symbol Tag Assumption
**Location**: `runtime/hash-tables.lisp:67-71`
**Severity**: MEDIUM - Wrong tag used
**Status**: 🔴 Unfixed

**Problem**:
```lisp
(= tag #x5)  ; Assumes symbol tag is 0x5
; But memory.lisp defines TAG_SYMBOL = #x2
```

**Impact**:
- Symbol detection fails
- Falls back to "other types" case
- Never examines actual symbol key
- Hash/equality silently wrong

**Fix Required**:
1. Use correct TAG_SYMBOL value
2. Centralize tag definitions
3. Add tag validation tests

### Bug 3.4: Buckets Built Without GC Support
**Location**: `runtime/hash-tables.lisp:161-189`
**Severity**: HIGH - GC cannot traverse
**Status**: 🔴 Unfixed

**Problem**:
- Buckets created via undefined `allocate`
- No headers written
- **Result**: GC cannot mark or relocate bucket nodes

**Impact**:
- Buckets become garbage
- Hash table corrupted after first GC
- Data loss

**Fix Required**:
1. Use proper GC allocation
2. Write correct headers
3. Register buckets as roots
4. Make buckets GC-traversable

---

## Category 4: Reader (runtime/reader.lisp) - INCORRECT

### Bug 4.1: Unterminated Strings Silently Accepted
**Location**: `runtime/reader.lisp:64-90`
**Severity**: MEDIUM - Incorrect error handling
**Status**: 🔴 Unfixed

**Problem**:
```lisp
"Hello world  ; Missing closing quote
; Tokenizer stops at EOF and emits :string token
; Should signal error instead
```

**Impact**:
- Syntax errors not reported
- Bogus tokens produced
- Confusing behavior for users
- Debugging difficulty

**Fix Required**:
1. Check for proper string termination
2. Signal error if EOF reached in string
3. Add comprehensive reader tests
4. Test all error cases

---

## Category 5: Region Allocator (runtime/region.c) - UNDEFINED BEHAVIOR

### Bug 5.1: Alignment Requirement Violation
**Location**: `runtime/region.c:13-30`
**Severity**: MEDIUM - Undefined behavior
**Status**: 🔴 Unfixed

**Problem**:
```c
void* habu_region_create(size_t size) {
    return aligned_alloc(16, size);  // BUG: size must be multiple of 16!
}
```

**C Standard Requirement**:
- `aligned_alloc(alignment, size)` requires `size % alignment == 0`
- Habu exposes arbitrary sizes to API
- Odd-sized requests have undefined behavior

**Impact**:
- May fail at runtime
- May return misaligned memory
- Undefined behavior per C standard
- Platform-specific failures

**Fix Required**:
```c
void* habu_region_create(size_t size) {
    // Round up to multiple of alignment
    size_t aligned_size = (size + 15) & ~15;
    return aligned_alloc(16, aligned_size);
}
```

---

## Category 6: Tests and Tooling - BROKEN/NON-PORTABLE

### Bug 6.1: Buffer Overflow in test_compiled_execution
**Location**: `tests/test_compiled_execution.c:19-50`
**Severity**: MEDIUM - Memory corruption in tests
**Status**: 🔴 Unfixed

**Problem**:
```c
// Maps only code_size + 1 bytes
exec_mem = mmap(NULL, code_size + 1, ...);

// On AArch64, appends 4-byte ret at offsets code_size..code_size+3
exec_mem[code_size] = 0xD65F03C0;  // Past mapping!
code_size += 3;

// munmap with wrong size
munmap(exec_mem, code_size + 1);  // Should be original size!
```

**Impact**:
- Writes past end of mapping
- Undefined behavior
- May crash
- May corrupt adjacent memory

**Fix Required**:
1. Calculate correct total size before mmap
2. Include ret instruction in size calculation
3. Use original size for munmap
4. Add buffer size assertions

### Bug 6.2: Hardcoded Paths in test_compiler_simple
**Location**: `tests/test_compiler_simple.c:25-55`
**Severity**: LOW - Non-portable tests
**Status**: 🔴 Unfixed

**Problem**:
```c
cd /home/user/habu/bootstrap  // Hardcoded path!
// Checks /tmp/test-*.bin
```

**Impact**:
- Only works on exact filesystem layout
- Fails on other machines
- Silently clobbers /tmp
- Not usable in most environments

**Fix Required**:
1. Use relative paths or environment variables
2. Create temporary directory properly
3. Clean up temp files
4. Make tests portable

---

## Priority Matrix

| Category | Severity | Impact | Fix Difficulty | Priority |
|----------|----------|--------|----------------|----------|
| GC Root Registration | CRITICAL | HIGH | HIGH | P0 |
| GC Pointer Updates | CRITICAL | HIGH | HIGH | P0 |
| CL Compaction Pointers | CRITICAL | HIGH | HIGH | P0 |
| NULL Check in habu_make_closure | CRITICAL | MEDIUM | LOW | P0 |
| Constructor Rooting | CRITICAL | HIGH | MEDIUM | P1 |
| Tag Value Mismatch | HIGH | MEDIUM | LOW | P1 |
| Hash Table allocate | HIGH | LOW | MEDIUM | P2 |
| Hash Table Header Access | HIGH | LOW | MEDIUM | P2 |
| Region Allocator Alignment | MEDIUM | LOW | LOW | P2 |
| Reader Unterminated Strings | MEDIUM | LOW | LOW | P3 |
| Test Buffer Overflow | MEDIUM | LOW | LOW | P3 |
| Test Portability | LOW | LOW | LOW | P4 |

---

## Recommended Fix Order

### Phase 1: Critical GC Fixes (P0)
1. ✅ Fix NULL check in `habu_make_closure`
2. ✅ Implement pointer-to-pointer root registration
3. ✅ Fix CL runtime compaction pointer updates
4. ✅ Add production root registration calls

### Phase 2: Memory Safety (P1)
5. ✅ Root intermediate results in constructors
6. ✅ Fix tag value mismatch
7. ✅ Add OOM handling throughout

### Phase 3: Feature Completion (P2)
8. ✅ Fix hash table implementation
9. ✅ Fix region allocator alignment

### Phase 4: Quality & Testing (P3-P4)
10. ✅ Fix reader error handling
11. ✅ Fix test suite issues
12. ✅ Add comprehensive tests

---

## Testing Strategy

After each fix:
1. ✅ Run existing test suite
2. ✅ Add specific test for the bug
3. ✅ Run under Valgrind/ASan
4. ✅ Verify no regressions

---

## Status Tracking

**Total Bugs**: 12 identified
**Fixed**: 0
**In Progress**: 0
**Unfixed**: 12

**Next Action**: Begin Phase 1 critical GC fixes

---

**Document Version**: 1.0
**Last Updated**: November 20, 2024
**Maintainer**: Habu Development Team
