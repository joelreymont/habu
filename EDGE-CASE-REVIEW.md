# PLAN.md Edge Case Review: Design Risks DR-001..DR-008

**Review Date**: 2026-02-25  
**Focus**: Flaky/OOM nondeterminism, GC/JIT interaction assumptions, false-positive risk in stale-pointer reports, test brittleness in candidates/stripQualifiedName coverage

---

## Edge Cases Found

### DR-001. JIT OOM Test - Nondeterministic Heap Exhaustion
- **Scenario**: What happens when the 2MB heap in `test "compileChunk JIT OOM relay falls back to interpreted execution"` is NOT exhausted during JIT execution?
- **Expected**: Test should deterministically trigger OOM in JIT path, relay error, run GC, and fall back to interpreter
- **Actual**: Test only asserts final result (`length == 100`). If JIT succeeds without OOM (e.g., GC runs early via safepoint), the OOM relay path is never exercised. Test passes but doesn't validate the feature it claims to test.
- **File**: `src/tests/integration.zig:9633-9680`
- **Test exists**: Yes, but it's a false-positive test (validates result, not the OOM→relay→fallback path)

**Evidence**:
```zig
// Line 9673: Only asserts final value, not that OOM occurred
try testing.expect(result.isFixnum());
try testing.expectEqual(@as(i64, 100), result.toFixnum());
// No assertion that JIT compilation occurred
// No assertion that OOM was triggered
// No assertion that GC ran during execution
```

**Missing validation additions**:
1. Assert `vm.jit_fns.items.len > 0` after `defun` to prove JIT compilation happened
2. Capture initial GC count: `const gc_before = heap.stats.gc_count;`
3. Assert GC delta: `try testing.expect(heap.stats.gc_count > gc_before);`
4. Add deterministic OOM trigger (e.g., reduce heap to 256KB, allocate large vector in loop)

---

### DR-002. JIT OOM Test - Flaky Heap Size Threshold
- **Scenario**: What happens when heap size choice (2MB) becomes too large or too small for future allocation patterns?
- **Expected**: Test should remain stable across code changes that alter allocation overhead
- **Actual**: Hardcoded 2MB threshold is brittle:
  - Too small → VM init fails
  - Too large → JIT allocations never trigger OOM
  - Unrelated changes to heap layout (e.g., adding metadata fields) silently break test validity
- **File**: `src/tests/integration.zig:9640` (`total_size = 2 * 1024 * 1024`)
- **Test exists**: Yes, but threshold is arbitrary and undocumented

**Missing validation additions**:
1. Document heap size rationale: `// 256KB: fits VM init + 1 cons loop iteration, triggers OOM on 2nd`
2. Add assertion that heap is near full before loop: `try testing.expect(heap.bytesUsed() > heap.total_size / 2);`
3. Use computed threshold based on VM overhead: `const heap_size = vm_init_footprint() + (target_allocs * cons_size);`

---

### DR-003. JIT OOM Test - Concurrent GC State Corruption
- **Scenario**: What happens when `jitSafepointBeforeAlloc()` triggers GC in the middle of an allocation sequence, invalidating cached `g_alloc_ptr`?
- **Expected**: JIT should refresh heap cursor after safepoint, detect stale cache, retry allocation
- **Actual**: Code has refresh path (`jitConsRefreshCache()` at line 367-369), but test doesn't validate concurrent GC scenario:
  - Multiple JIT threads calling allocators (not tested)
  - Safepoint triggering GC during inline cons fast path (not tested)
  - Cache invalidation after semispace flip (not tested)
- **File**: `src/jit/backend.zig:356-370`
- **Test exists**: No test for concurrent safepoint+GC scenario

**Concrete edge case**:
```zig
// Thread 1: JIT inline cons checks g_alloc_ptr < g_alloc_end (line 354)
// Thread 2: GC safepoint flips semispaces, updates heap.alloc_ptr
// Thread 1: Writes car/cdr to OLD g_alloc_ptr (stale pointer)
// Result: Heap corruption (writes to from-space after flip)
```

**Missing validation additions**:
1. Add test that forces safepoint during allocation burst
2. Assert heap.alloc_ptr == g_alloc_ptr after every JIT call
3. Add concurrent stress test (if threading support exists)
4. Document non-thread-safety in `backend.zig:98` comment

---

### DR-004. GC/JIT Interaction - Forwarding Pointer Race
- **Scenario**: What happens when JIT code reads a Value.raw between GC copying the object and updating all pointers?
- **Expected**: JIT should use `jitResolveForwarded()` for all heap reads
- **Actual**: `jitResolveForwarded()` exists (line 289-324) but no test validates:
  - Reads during GC-in-progress
  - Nested forwarding chains (forwarding → forwarding → live)
  - Forwarding after safepoint in arithmetic/string ops
- **File**: `src/jit/backend.zig:289-324`
- **Test exists**: No test for mid-GC reads

**Concrete edge case**:
```zig
// JIT calls jitHashGet (line 798-805)
// Line 799: table = jitResolveForwarded(Value{ .raw = table_raw })
// GC triggers between line 799 and line 801 (table.toPtr(HashTable))
// table pointer is now stale (points to from-space)
// Result: Read from evacuated memory
```

**Missing validation additions**:
1. Test that interleaves GC and JIT hash operations
2. Assert all JIT helpers call `jitResolveForwarded()` before `toPtr()`
3. Add ASAN/valgrind test mode that poisons from-space after GC
4. Document forwarding requirement in JIT helper template

---

### DR-005. GC/JIT Interaction - Write Barrier Bypass
- **Scenario**: What happens when JIT inline-cons fast path (line 346-362) bypasses `jitWriteBarrier()`?
- **Expected**: All pointer stores should trigger write barrier for generational GC
- **Actual**: Inline cons fast path writes car/cdr directly (line 354-357) without barrier. `jitWriteBarrier()` is only called from slow paths (e.g., line 833-834).
- **File**: `src/jit/backend.zig:346-362`
- **Test exists**: No test for generational GC + JIT interaction

**Concrete edge case**:
```zig
// Old object X in tenured space stores pointer Y (new object in nursery)
// JIT inline cons creates Y, stores into X.field
// No write barrier → card table not marked
// Minor GC scans nursery, misses X→Y reference
// Y is incorrectly collected
// X.field now points to freed memory
```

**Missing validation additions**:
1. Add test with old→young pointer via JIT
2. Force minor GC after JIT allocation
3. Assert card table is marked for old objects
4. Document that inline cons is only valid for fresh allocations (no inter-generational refs)

---

### DR-006. Stale Pointer False Positives - g_heap Lifecycle
- **Scenario**: What happens when `g_heap` is set in VM A, VM A is destroyed, VM B is created, and JIT code compiled by A is called in B's context?
- **Expected**: Each VM should isolate its JIT state
- **Actual**: `g_heap` is a global (line 98). If JIT function pointers outlive the VM that compiled them, they access stale heap pointer.
- **File**: `src/jit/backend.zig:98, 136-138`
- **Test exists**: Yes, partial test at `vm.zig:14070-14123` (`test "vm jit bridge lifecycle tracks owner vm"`)

**Concrete edge case**:
```zig
// VM A compiles function F, sets g_heap = &A.heap
// VM A deinit(), deallocates A.heap
// VM B init(), does NOT call setHeap() immediately
// F() is called (e.g., via lingering closure)
// F reads g_heap → dangling pointer
// Result: segfault or use-after-free
```

**Existing test gap** (vm.zig:14070):
- Test validates bridge clearing on VM deinit
- Does NOT validate that JIT functions are invalidated when VM changes
- Does NOT test cross-VM JIT call scenario

**Missing validation additions**:
1. Add test that calls JIT function after originating VM is destroyed
2. Assert crash or error (not silent corruption)
3. Add VM id field to JIT functions, validate in bridge
4. Document VM ownership requirement in CompiledFn struct

---

### DR-007. stripQualifiedName - Ambiguous :: vs : Precedence
- **Scenario**: What happens when a symbol name contains both `::` and `:` separators (e.g., `FOO:BAR::BAZ`)?
- **Expected**: Consistent stripping order (either `::` takes precedence, or leftmost match wins)
- **Actual**: `indexOf("::")` is checked first (line 29), then `indexOfScalar(':')` (line 30). For `FOO:BAR::BAZ`:
  - Line 29 finds `::` at index 7 → returns `BAZ`
  - But if intent was "package:symbol::qualifier", stripping `::` first loses context
- **File**: `src/jit/candidates.zig:28-32`
- **Test exists**: No test for mixed `::` and `:` in names

**Concrete edge case**:
```zig
const name = "PKG:FOO::INTERNAL";
const stripped = stripQualifiedName(name);
// Returns "INTERNAL" (strips :: first)
// Candidate name is now "INTERNAL"
// Chunk name is "FOO" (without qualifier)
// chunkNameMatches() compares "INTERNAL" vs "FOO" → false
// Match fails even though they're the same function
```

**Missing validation additions**:
1. Add test for `"PKG:FOO"` → `"FOO"`
2. Add test for `"PKG::FOO"` → `"FOO"`
3. Add test for `"PKG:FOO::BAR"` → document expected result
4. Add test for `"A:B:C"` → `"C"` (leftmost `:` match)
5. Add test for `"A::B::C"` → `"C"` (leftmost `::` match)

---

### DR-008. chunkNameMatches - Case-Insensitive Aliasing
- **Scenario**: What happens when candidate.local_name is `"Foo"` and chunk.name is symbol `FOO` (uppercased by intern)?
- **Expected**: Match succeeds (Common Lisp symbols are case-insensitive)
- **Actual**: `std.ascii.eqlIgnoreCase()` handles this (line 145-152), but test coverage is missing:
  - No test for mixed-case candidate vs uppercase chunk
  - No test for string chunk names (line 149-152)
  - No test for symbol vs string chunk name equivalence
- **File**: `src/jit/candidates.zig:142-158`
- **Test exists**: No test for case-insensitive matching

**Concrete edge case**:
```zig
// Candidate from IR: lambda.name = Value.nil, but name string is "my-Func"
// Chunk from cache: chunk.name = Symbol("MY-FUNC") (interned uppercase)
// chunkNameMatches() calls eqlIgnoreCase("my-func", "MY-FUNC") → true ✓
// BUT: if chunk.name is String("my-func") (not interned):
//   Line 149 matches ✓
// If chunk.name is String("MY-FUNC"):
//   Line 149 matches ✓
// If chunk.name is Symbol("my-func") (non-canonical):
//   Line 145 checks raw equality → false ✗
//   Line 146 checks eqlIgnoreCase("my-func", "my-func") → true ✓
```

**Test coverage gap**: No test validates that Symbol and String chunk names are treated equivalently.

**Missing validation additions**:
1. Add test: candidate `"foo"`, chunk name Symbol `FOO` → match
2. Add test: candidate `"foo"`, chunk name String `"FOO"` → match
3. Add test: candidate `"PKG:foo"`, chunk name Symbol `FOO` → match (local_name stripped)
4. Add test: candidate `"foo"`, chunk name Symbol `BAR` → no match
5. Add test: candidate `"foo"`, chunk name Value.nil → no match

---

## Missing Test Coverage

### Priority 1 (Correctness Risks)

1. **DR-001**: `src/tests/integration.zig:9633` - JIT OOM relay path not validated
   - Add: Assert JIT compilation occurred via `vm.jit_fns.items.len > 0`
   - Add: Assert GC ran during execution via `heap.stats.gc_count` delta
   - Add: Reduce heap to 256KB, assert OOM deterministically

2. **DR-004**: `src/jit/backend.zig:289` - No test for mid-GC forwarding resolution
   - Add: Test that interleaves GC with JIT hash/vector operations
   - Add: Assert all helpers use `jitResolveForwarded()` before `toPtr()`

3. **DR-005**: `src/jit/backend.zig:346` - No test for write barrier with inline cons
   - Add: Test old→young pointer via JIT inline cons
   - Add: Force minor GC, assert young object survives

4. **DR-006**: `src/interp/vm.zig:14070` - Cross-VM JIT call not tested
   - Add: Test that calls JIT function after VM deinit
   - Add: Assert error (not silent corruption)

### Priority 2 (Test Brittleness)

5. **DR-007**: `src/jit/candidates.zig:28` - No test for mixed `::` / `:` separators
   - Add tests for all combinations in list above

6. **DR-008**: `src/jit/candidates.zig:142` - No test for case-insensitive chunk matching
   - Add tests for Symbol/String name equivalence

7. **DR-002**: `src/tests/integration.zig:9640` - Hardcoded heap threshold undocumented
   - Add comment explaining 2MB choice
   - Add assertion that heap is near full before loop

### Priority 3 (Robustness)

8. **DR-003**: `src/jit/backend.zig:356` - No test for concurrent safepoint+GC
   - Add test forcing safepoint during allocation burst
   - Assert `heap.alloc_ptr == g_alloc_ptr` coherence

---

## Summary

**Total edge cases found**: 8 design risks (DR-001 through DR-008)

**Test coverage status**:
- DR-001: Has test, but false-positive (validates wrong thing)
- DR-002: Has test, but brittle threshold
- DR-003: No test
- DR-004: No test
- DR-005: No test
- DR-006: Partial test (doesn't cover cross-VM scenario)
- DR-007: No test
- DR-008: No test

**Risk assessment by category**:

| Risk Area | Missing Tests | Severity | Likelihood of Failure |
|-----------|---------------|----------|----------------------|
| **Flaky/OOM nondeterminism** | DR-001, DR-002, DR-003 | High | Medium (depends on heap usage patterns) |
| **GC/JIT interaction** | DR-004, DR-005 | Critical | High (every JIT call with GC) |
| **Stale pointer false-positives** | DR-006 | Medium | Low (requires specific VM lifecycle misuse) |
| **Test brittleness** | DR-007, DR-008 | Low | Medium (as more package-qualified names are added) |

**Blocking issues for production**:
- **DR-004** (forwarding race): Could cause crashes in real workloads
- **DR-005** (write barrier bypass): Silent memory corruption in generational GC
- **DR-001** (OOM test false-positive): Feature appears tested but isn't

**Recommended fix order**:
1. DR-004, DR-005 (add tests, verify correctness)
2. DR-001 (fix false-positive test)
3. DR-006 (extend existing test)
4. DR-007, DR-008 (add coverage for candidates)
5. DR-002, DR-003 (document and harden existing test)

---

## Validation Additions (Concrete Implementation)

### For DR-001 (JIT OOM Test False-Positive)

```zig
// src/tests/integration.zig:9633
test "compileChunk JIT OOM relay falls back to interpreted execution" {
    if (!build_options.use_hoist) return;
    const allocator = testing.allocator;
    
    // Reduced heap: 256KB — tight enough to force OOM deterministically
    var heap = try Heap.init(allocator, .{ .total_size = 256 * 1024 });
    defer heap.deinit();

    var vm = try Vm.init(allocator, &heap);
    defer vm.deinit();

    // ... setup code ...

    const jit_fns_before = vm.jit_fns.items.len;
    const gc_count_before = heap.stats.gc_count;

    _ = try vm.run(try compile_chunk.compileChunk(
        allocator, &heap, &vm, &comp, &chunk_pool,
        "(defun jit-alloc-loop (n)" ++
            " (declare (optimize (speed 3) (safety 0)))" ++
            " (let ((acc nil))" ++
            "   (dotimes (i n acc)" ++
            "     (setq acc (cons i acc)))))",
    ));

    // VALIDATION 1: JIT compilation occurred
    try testing.expect(vm.jit_fns.items.len > jit_fns_before);

    // Call with count that exceeds heap budget
    const result = try vm.run(try compile_chunk.compileChunk(
        allocator, &heap, &vm, &comp, &chunk_pool,
        "(length (jit-alloc-loop 500))",  // Increased to force OOM
    ));

    // VALIDATION 2: GC ran at least once (OOM triggered)
    try testing.expect(heap.stats.gc_count > gc_count_before);

    // VALIDATION 3: Result is correct (fallback succeeded)
    try testing.expect(result.isFixnum());
    try testing.expectEqual(@as(i64, 500), result.toFixnum());
}
```

### For DR-007 (stripQualifiedName Edge Cases)

```zig
// src/jit/candidates.zig: Add to test block
test "stripQualifiedName: single colon" {
    try testing.expectEqualStrings("BAR", stripQualifiedName("FOO:BAR"));
}

test "stripQualifiedName: double colon" {
    try testing.expectEqualStrings("BAR", stripQualifiedName("FOO::BAR"));
}

test "stripQualifiedName: mixed colon and double colon (double takes precedence)" {
    // Current behavior: :: found first (at index 7), returns "BAZ"
    try testing.expectEqualStrings("BAZ", stripQualifiedName("FOO:BAR::BAZ"));
}

test "stripQualifiedName: multiple single colons (leftmost match)" {
    try testing.expectEqualStrings("B:C", stripQualifiedName("A:B:C"));
}

test "stripQualifiedName: no qualifier" {
    try testing.expectEqualStrings("FOO", stripQualifiedName("FOO"));
}
```

### For DR-008 (chunkNameMatches Case-Insensitivity)

```zig
// src/jit/candidates.zig: Add to test block
test "chunkNameMatches: case insensitive symbol match" {
    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const candidate = LambdaCandidate{
        .name = "my-func",
        .local_name = "my-func",
        .name_sym = Value.nil,
        .lambda_ir = &makeLambdaIr(&.{}, &.{}, &.{}, null),
    };

    const chunk_name_sym = try heap.intern("MY-FUNC");  // Uppercase symbol
    const chunk = try createMockChunk(&heap, chunk_name_sym, 0, 0, 0, false);
    defer heap.free(...);  // Cleanup

    try testing.expect(chunkNameMatches(&candidate, Value.nil, chunk));
}

test "chunkNameMatches: string vs symbol equivalence" {
    var heap = try Heap.init(testing.allocator, .{ .total_size = 1024 * 1024 });
    defer heap.deinit();

    const candidate = LambdaCandidate{
        .name = "foo",
        .local_name = "foo",
        .name_sym = Value.nil,
        .lambda_ir = &makeLambdaIr(&.{}, &.{}, &.{}, null),
    };

    const chunk_name_string = try heap.allocBaseString("FOO");
    const chunk = try createMockChunk(&heap, chunk_name_string, 0, 0, 0, false);
    defer heap.free(...);

    try testing.expect(chunkNameMatches(&candidate, Value.nil, chunk));
}
```

---

## End of Review

This edge case review provides concrete, file:line-specific evidence for 8 design risks with validation additions. All risks are testable and have clear acceptance criteria.
