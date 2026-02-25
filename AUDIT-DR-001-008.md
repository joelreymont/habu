# STRICT Review-Plan Pass #4: Hidden Dependencies & Missing Items (DR-001..DR-008)

**Audit Date:** 2026-02-25  
**Task:** Trace hidden dependencies and missing plan items for F1, F2, F3 follow-ups  
**Methodology:** Grep adjacent call paths, check integrations, trace test patterns, verify VM/compiler bridges  

---

## Executive Summary

**Three follow-up tasks in PLAN.md have MISSING DEPENDENCIES not captured:**
- **F1 (JIT OOM test hardening)**: Requires GC stats tracking + heap state assertions
- **F2 (chunk matching tests)**: Requires Chunk allocation helpers + IR test fixtures + integration with VM state
- **F3 (macro-function intern cleanup)**: Requires builtin symbol validation + dependent test updates

**Estimated hidden work: +180-240 minutes beyond stated task scope**

---

## F1: JIT OOM Test Hardening - Hidden Dependencies

**Primary File:** `src/tests/integration.zig:9633-9680`

### DR-001: Missing JIT Compilation Assertion
- **Requirement (PLAN):** "Assert JIT compilation occurred (e.g. `vm.jit_fns` count grows)"
- **Hidden dependencies:**
  1. **Tracking structure:** `vm.jit_fns: ArrayList(struct { chunk: Value, compiled: *CompiledFn })`
     - File: `src/interp/vm.zig:548`
     - Access: `vm.jit_fns.items.len` before/after defun compilation
  2. **Registration point:** `Vm.registerJitFn(chunk, compiled)`
     - File: `src/interp/vm.zig:1968-1990`
     - Called from: `src/interp/repl.zig:3202` after successful hoist compile
  3. **Test pattern validation:** Existing tests already validate this pattern
     - File: `src/tests/integration.zig:62` (`before = vm.jit_fns.items.len`)
     - File: `src/tests/integration.zig:72` (`after = vm.jit_fns.items.len`)
     - File: `src/tests/integration.zig:177` (`try testing.expect(vm.jit_fns.items.len > before)`)
  
- **Effort:** ~5 min (pattern copy from line 62/177)

---

### DR-002: Missing GC Count Delta Assertion
- **Requirement (PLAN):** "Assert fallback path is observable via counters (gc_count delta > 0)"
- **Hidden dependencies:**
  1. **Heap stats field:** `heap.stats.gc_count: usize`
     - File: `src/runtime/heap.zig:474` (struct field definition)
     - Incremented: `src/runtime/heap.zig:1433` (`promoted_cycle = self.stats.gc_count`)
     - Incremented: `src/runtime/gc.zig` during `collectGarbage()` invocation
  2. **GC triggering point in JIT path:**
     - Error relay: `src/jit/backend.zig:562` (`jitRelayError()` sets `bridge.set_error()`)
     - Error propagation: `src/interp/vm.zig:2113-2114` (`jit_bridge_error` checked)
     - GC fallback: `src/interp/vm.zig:2119-2120` (`if (err == error.OutOfMemory) { _ = try self.collectGarbage(); return null; }`)
  3. **Heap GC method:** `pub fn collectGarbage(self: *Heap) !?Value`
     - File: `src/runtime/heap.zig:1218`
     - Returns: first survivor/moved object value or null
  4. **Existing test pattern:** See line 2584+ for error-relay test structure
     - File: `src/tests/integration.zig:2584-2608` (JIT bridge relay test)
     - Uses: `repl.eval()` + error expectation
  
- **Hidden requirement:** Must call defun **multiple times** to accumulate OOM, OR use much larger allocation in single call
  - Single call with n=100: uses ~100 cons cells (~3KB), heap is 2MB → no OOM on this host
  - Need at least 1000+ iterations or allocate 10+ MB to trigger OOM on 2MB heap
  - See: LESSONS.md line 134 ("Adding a JIT no-GC execution fence with OOM deopt")
  
- **Effort:** ~15 min (capture before gc_count, run allocation, capture after, assert delta)

---

### DR-003: Missing OOM Determinism Validation
- **Requirement (PLAN):** "Ensure deterministic pass without environment-specific timing"
- **Hidden dependencies:**
  1. **Stochastic OOM trigger points:**
     - Heap allocation patterns vary by GC cycle, nursery refill timing
     - Minor GC may trigger before major GC, reducing free space unpredictably
     - See: LESSONS.md line 264 ("Some long `zig build test` runs can stall")
  2. **Deterministic heap setup:**
     - File: `src/tests/integration.zig:9633` creates 2MB heap
     - Current test: dotimes loop from 0..100, accumulating cons cells
     - **Problem:** cons cell size is ~16 bytes, so 100 cells = ~1.6KB on a 2MB heap
     - **Fix:** Need allocation large enough to guarantee OOM on 2MB
       - Estimate: 2000+ iterations, or allocate within loop: `(allocate-string 100000)`
       - Test: `(dotimes (i 2000 acc) (cons i acc))` would need ~32KB heap space
  3. **Fallback assertion:**
     - Must prove OOM was triggered AND fallback occurred
     - Evidence: `gc_count` delta > 0 (GC ran) + no crash + result is correct
  
- **Effort:** ~30-45 min (experiment with thresholds, validate OOM triggers, lock with assertions)

---

### DR-004: OOM Relay Behavior Documentation Need
- **Requirement (implied):** Test should document that OOM in JIT → relay → GC → retry interpreted
- **Hidden files affected:**
  1. `LESSONS.md` - should document OOM relay pattern and expectations
  2. Comments in `src/interp/vm.zig:2119-2120` already mention fallback
  
- **Effort:** ~10 min (add test comment explaining relay + fallback flow)

---

## F2: Lambda Candidate Chunk Matching Tests - Hidden Dependencies

**Primary File:** `src/jit/candidates.zig:160-182`

### DR-005: Missing Chunk Creation Test Fixtures
- **Requirement (PLAN):** "Add tests for signature match success (arity/opt/key/rest all equal)"
- **Hidden dependencies:**
  1. **Chunk allocation API:** `heap.allocChunk(...)`
     - Signature: `allocChunk(code, constants, arity, opt_count, key_count, has_rest, num_locals)`
     - File: `src/runtime/heap.zig:2961-3008`
     - Returns: `Value` (chunk Value)
  2. **Chunk pointer extraction:** `chunk.toPtr(runtime.Chunk)`
     - File: `src/runtime/objects.zig` (Value methods)
     - Pattern: `chunk_val.toPtr(Chunk)` to access signature fields
  3. **Test allocator requirement:**
     - All tests use `testing.allocator` from standard library
     - File: `src/jit/candidates.zig:190` imports `const testing = std.testing`
     - Pattern: Tests must manage heap allocation/deallocation
  4. **Heap initialization in tests:**
     - Current candidates.zig tests DON'T allocate heap (they only test IR)
     - New tests need: heap setup, chunk allocation, proper cleanup
     - Pattern: See `src/tests/integration.zig:42-54` for heap init pattern
       ```zig
       var heap = try Heap.init(allocator, .{ .total_size = ... });
       defer heap.deinit();
       ```
  5. **Chunk signature matching validation:**
     - Function: `chunkSignatureMatches(lambda_ir, chunk) -> bool`
     - File: `src/jit/candidates.zig:126-131`
     - Checks: arity, opt_count, key_count, has_rest match lambda params
  
- **Hidden requirement:** Test helper function to create chunks with specific signatures
  - Cannot reuse existing test chunks (integration tests don't expose them)
  - Must call `heap.allocChunk()` directly with specific parameters
  
- **Effort:** ~45-60 min
  - 20 min: Set up heap in test block
  - 15 min: Create helper to build test chunks  
  - 20 min: Write 4+ test cases (signature match, name match, used[], mismatch)

---

### DR-006: Missing Lambda Candidate Test Fixtures
- **Requirement (PLAN):** "Include collectLambdaCandidates + findMatchingChunk integration assertion"
- **Hidden dependencies:**
  1. **Lambda IR generation:**
     - Current helper: `makeLambdaIr()` already exists (line 195)
     - Parameters: captures, optional_params, key_params, rest_param
  2. **Lambda candidate collection:**
     - Function: `collectLambdaCandidates(allocator, ir_node, out_list)`
     - File: `src/jit/candidates.zig:84-98`
     - Walks IR for `.define`, `.set_symbol_function`, `.progn` nodes
  3. **Integration test pattern:**
     - Call `collectLambdaCandidates()` on define IR
     - Call `findMatchingChunk()` with collected candidates + chunk array
     - Assert: match succeeds, used[] is mutated, returns correct chunk
  4. **Chunk name matching scenarios:**
     - Symbol name match: `chunk.name` is symbol, matches candidate name
     - String name match: `chunk.name` is string, matches case-insensitive
     - Local vs qualified: candidate has "pkg:func", local_name is "func"
     - File: `src/jit/candidates.zig:142-157` (chunkNameMatches logic)
  
- **Effort:** ~30 min (write integration test that chains collection + matching)

---

### DR-007: Missing Used Array Exclusion Tests
- **Requirement (PLAN):** "Assert `used[]` is asserted to mutate only on match"
- **Hidden dependencies:**
  1. **used array semantics:**
     - Input: `used: []bool` array, parallel to `child_chunks[]`
     - Behavior: `used[idx] = true` when chunk matches (line 173)
     - Only mutated on successful match, not on signature/name mismatch
  2. **Test requirements:**
     - Create 3 chunks with different signatures
     - Create used array [false, false, false]
     - Call findMatchingChunk for first chunk
     - Assert: used[0] = true, used[1] = false, used[2] = false
     - Call findMatchingChunk again with same signature
     - Assert: returns second chunk (first is used), used[1] = true
     - Call findMatchingChunk for mismatched signature
     - Assert: returns null, no used[] mutation
  
- **Effort:** ~20 min (write 2 test cases for used[] behavior)

---

### DR-008: Missing Signature Mismatch Rejection Tests
- **Requirement (PLAN):** "Signature mismatch rejection (arity/opt/key/rest deltas)"
- **Hidden dependencies:**
  1. **Signature comparison logic:**
     - Function: `chunkSignatureMatches(lambda_ir, chunk)`
     - File: `src/jit/candidates.zig:126-131`
     - Checks: 4 fields independently
  2. **Test matrix:**
     - ✓ arity mismatch: candidate arity=2, chunk arity=3 → no match
     - ✓ opt_count mismatch: candidate opts=1, chunk opts=0 → no match
     - ✓ key_count mismatch: candidate keys=2, chunk keys=0 → no match
     - ✓ rest_param mismatch: candidate rest=true, chunk rest=false → no match
  3. **Test assertion pattern:**
     - Create candidate with specific signature (e.g., arity=3, opt=1, key=2, rest=false)
     - Create chunks with each mismatch variant
     - Call findMatchingChunk for each mismatch
     - Assert: returns null for each
  
- **Effort:** ~25 min (4 test cases, one per field mismatch)

---

## F3: Redundant Macro-Function Intern Cleanup - Hidden Dependencies

**Primary File:** `src/compiler/compile.zig:7260-7272`

### DR-008b: Missing Builtin Symbol Validation
- **Requirement (PLAN):** "Replace `heap2.intern("MACRO-FUNCTION")` with builtin symbol value"
- **Hidden dependencies:**
  1. **Builtin symbols initialization:**
     - Struct: `Compiler.Builtins` with field `macro_function: Value`
     - File: `src/compiler/compile.zig:715-850`
     - Initialized: `src/compiler/compile.zig:1330` in `initBuiltins()`
     - Value: `try heap.intern("MACRO-FUNCTION")` (already interned once)
  2. **Current code (line 7263):**
     ```zig
     if (head.raw == b_mf.macro_function.raw)  // Identity check on interned symbol
     ```
  3. **Redundant code (line 7270):**
     ```zig
     const mf_sym = try heap2.intern("MACRO-FUNCTION");  // Redundant re-intern!
     ```
  4. **Fix location:** Replace line 7270 with:
     ```zig
     const mf_sym = b_mf.macro_function;  // Reuse builtin
     ```
  5. **Validation requirement:**
     - Must verify b_mf is non-null at this point
     - Current code: line 7262 `const b_mf = self.builtins orelse return error.UninitializedBuiltins;`
     - So b_mf is guaranteed to be initialized
  
- **Effort:** ~5 min (one-line change + verify no test regressions)

---

### DR-008c: Missing Dependent Test Updates
- **Requirement (implied):** Verify macro-function rewrite tests still pass
- **Hidden dependencies:**
  1. **Macro-function tests:**
     - File: `src/tests/integration.zig` - search for "macro.*function" tests
     - No direct macro-function rewrite tests found
     - Indirectly tested via setf + macro expansion tests
  2. **Setf tests:** 
     - File: `src/tests/integration.zig` - many setf tests
     - But none specifically for `(setf (macro-function ...) ...)`
  3. **Test requirement:**
     - After F3 change, run: `zig build test` 
     - Verify no regressions in macro/setf paths
     - Add focused test if macro-function setf is not covered
  
- **Effort:** ~15 min (test run + potential new test for macro-function setf)

---

## Execution Order & Constraints

```
F1: JIT OOM Test (DR-001..004)
    ├─ Prerequisite: Understand jitRelayError + collectGarbage flow
    ├─ Parallel: DR-001 (jit_fns assertion) + DR-002 (gc_count)
    ├─ Then: DR-003 (OOM determinism)
    └─ Then: DR-004 (documentation)
    
F2: Chunk Matching Tests (DR-005..008)
    ├─ Prerequisite: Understand heap.allocChunk + Chunk signature
    ├─ First: DR-005 (chunk fixtures + heap setup)
    ├─ Then: DR-006 (integration with candidates)
    ├─ Parallel: DR-007 (used[] tests) + DR-008 (mismatch tests)
    
F3: Macro-Function Cleanup (DR-008b..c)
    ├─ Prerequisite: Verify builtin symbol is initialized
    ├─ First: DR-008b (one-line change)
    └─ Then: DR-008c (test validation)

CRITICAL ORDERING:
  - F1 can start immediately (independent)
  - F2 requires understanding of heap/chunk ABI (blocks if F1 takes all resources)
  - F3 is standalone, lowest priority (5 min change + 10 min tests)
```

---

## Total Estimated Hidden Work

| Task | Hidden Items | Subtotal | Notes |
|------|--------------|----------|-------|
| F1 (OOM test) | DR-001,002,003,004 | 60 min | GC tracking, determinism validation |
| F2 (chunk tests) | DR-005,006,007,008 | 120 min | Heap setup, fixtures, 4 test scenarios |
| F3 (intern cleanup) | DR-008b,008c | 20 min | 1-line change + test validation |
| **TOTAL** | **8 items** | **~200 min** | Beyond stated PLAN scope |

---

## LESSONS.md Update Candidates

- **OOM relay pattern:** Document that JIT errors with OutOfMemory trigger GC + interpreted retry
- **Chunk test fixtures:** Document pattern for heap-initialized candidate matching tests
- **Builtin symbol reuse:** Document that builtin symbols should be reused, not re-interned

---

## Blockers & Risks

1. **Heap state determinism (DR-003):** OOM triggering varies by GC scheduling
   - **Mitigation:** Use deterministic allocation loop (e.g., 2000 cons cells), measure on current host, lock threshold
   
2. **Chunk test complexity (DR-005):** New tests require heap ABI knowledge
   - **Mitigation:** Provide allocChunk helper function in test block; reference heap.zig:2961
   
3. **Macro-function test coverage (DR-008c):** Setf + macro-function interaction not well-tested
   - **Mitigation:** Add explicit test: `(setf (macro-function 'foo) #'my-expander)` + call it

---

## Recommendations

1. **MUST address DR-001 + DR-002** before closing F1 (test strength requirements)
2. **Should add DR-003 + DR-004** (determinism + documentation)
3. **MUST have heap setup (DR-005)** before attempting other F2 items
4. **F3 is safe to defer** until F1/F2 complete (minimal risk)
5. **Update LESSONS.md** with OOM relay and chunk-test patterns for future reference
