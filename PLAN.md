# Deep Review Fix Plan

Fixes for issues found during deep review, ordered by severity and dependency.

## Critical

### 1. Fix JIT helpers that silently swallow OOM / overflow errors
- **Files**: `src/jit/backend.zig:347-1700` — 37 `catch return` patterns across 16 functions
- **What**: Replace error-swallowing patterns with `catch |err| jitRelayError(err)` so the bridge longjmps out and the VM retries interpreted.

#### Functions to fix (16 total):

| Function | Line | Pattern | Error type |
|----------|------|---------|------------|
| `jitCons` | 367 | `allocCons catch return 0` | OOM |
| `jitMakeHash` | 795 | `allocHashTable catch return nil` | OOM |
| `jitHashSet` | 829-830 | `math.mul catch`, `growHashTable catch` | Overflow, OOM |
| `jitHashTest` | 883 | `heap.intern catch` | OOM |
| `jitHashKeys` | 898 | `allocCons catch` (in loop) | OOM |
| `jitHashAlist` | 915-916 | `allocCons catch` (2x in loop) | OOM |
| `jitMakeVector` | 934 | `allocVector catch` | OOM |
| `jitMakeString` | 985 | `allocStringUninitialized catch` | OOM |
| `jitInternName` | 996 | `heap.intern catch` | OOM |
| `jitMakeArray1` | 1024 | `allocArray catch` | OOM |
| `jitArefN` | 1141-1144 | `math.mul/add catch` (index calc) | Overflow |
| `jitAsetN` | 1219-1222 | `math.mul/add catch` (index calc) | Overflow |
| `jitMakeArrayDynamic` | 1322 | `jitAllocArrayFromDims catch` | OOM |
| `jitStrConcat` | 1375-1389 | `math.add catch`, `allocString catch` | Overflow, OOM |
| `jitSubstring` | 1422 | `substring catch` | OOM |
| `jitFormatSimple` | 1588-1655 | multiple alloc catches | OOM |

#### Pattern (mechanical replacement):
```zig
// BEFORE:
const result = heap.allocCons(car, cdr) catch return 0;
// AFTER:
const result = heap.allocCons(car, cdr) catch |err| jitRelayError(err);
```

Use `catch |err| jitRelayError(err)` (not hardcoded error types) — relay the actual error.

#### Special case — jitFormatSimple (lines 1559-1665):
- Has `defer out.deinit(heap.backing_allocator)` at line 1604
- `longjmp` from `jitRelayError` skips `defer` → leaks the ArrayList
- **Fix**: Replace `defer` with explicit cleanup. Before each error relay, do `out.deinit(heap.backing_allocator)` then `jitRelayError(err)`. Or use a wrapper:
  ```zig
  fn jitFormatOomRelay(out: *std.ArrayList(u8), allocator: std.mem.Allocator, err: anyerror) noreturn {
      out.deinit(allocator);
      jitRelayError(err);
  }
  ```
- The fast path (line 1596 `allocBaseString catch`) has NO defer — can use `jitRelayError` directly.

#### Dead code cleanup:
- `jitAppend` line 419: `if (new_cell == 0) return 0;` — dead after jitCons longjmps. Remove.

#### NOT in scope (leave as-is):
- `g_heap orelse return 0/nil` (16 sites) — null heap is an initialization bug, not a runtime error. These could be changed to `jitRequireHeap()` (panic) in a separate item but that's a different failure mode.
- `bufPrint catch return nil` in jitFormatSimple (2 sites) — buffer overflow on 64-byte num buf for fixnum is unreachable in practice.
- `parseInt catch return nil` in jitFormatSimple (1 site) — malformed format string, not allocation.

- **Acceptance**: Zero `catch return 0` / `catch return Value.nil.raw` for allocation or math overflow errors. jitFormatSimple properly cleans up ArrayList before relay. All existing tests pass.
- **Test**: Add integration test that forces OOM during JIT execution (small heap + JIT loop that allocates) and verifies error propagation.
- **Effort**: 2-3 hours (mechanical changes across 16 functions)
- **Depends on**: Nothing

### 2. Replace `unreachable` with proper error for closure captures in JIT
- **File**: `src/jit/backend.zig:2637-2650`
- **What**:
  1. Change `translateVar` return type from `HoistValue` to `anyerror!HoistValue`
  2. Add `try` at call site line 2512: `.@"var" => |v| try self.translateVar(v),`
  3. Replace `unreachable; // TODO: closure captures` with `return error.UnsupportedIrNode`
- **Acceptance**: The `unreachable` is gone. Compile succeeds. Existing tests pass.
- **Effort**: 15 minutes
- **Depends on**: Nothing

## Major

### 3. Replace string dispatch with interned symbol comparison in compiler
- **Files**: `src/compiler/compile.zig` — 12 sites in ~8 locations (not ~30 as originally estimated)
- **What**: Pre-intern names, replace `std.mem.eql(u8, ...)` dispatch with identity comparison.

#### Sites to fix:
| Line(s) | String literal | Fix |
|---------|---------------|-----|
| 7247-7248 | `"MACRO-FUNCTION"` / `"macro-function"` | Intern once, compare by identity |
| 10432-10433 | `"CL-USER"` / `"COMMON-LISP-USER"` | Compare package pointer identity |
| 10916 | `"QUOTE"` | Use `builtins.sym_quote` |
| 16127 | `"%HABU-MACRO-ENTRY"` | Intern once |
| 16133-16134 | `"MACRO-FUNCTION"` / `"macro-function"` | Same as 7247 |
| 18106-18116 | 6 keywords × 2 cases | Intern as keywords at heap init |

#### NOT in scope (legitimate string comparisons, keep as-is):
- Lines 1425, 1768, 2439, 9220, 11680, 11737, 11912, 12419, 13656, 16065, 16086, 18374 — content equality, not dispatch
- Lines 3481, 3493 — debug trace only
- Lines 3743, 3754 — Maxima compat hacks (rare path)
- Test assertions

- **Acceptance**: All symbol/keyword dispatch uses identity comparison.
- **Risk**: Must verify intern order — some of these symbols must exist before compiler init. The heap already pre-interns many CL symbols (`builtins`), so adding ~8 more should be straightforward.
- **Effort**: 2-3 hours (fewer sites than originally estimated)
- **Depends on**: Nothing

### 4. Fix SymbolTable.put duplicate key leak
- **File**: `src/runtime/heap.zig:95-100`
- **What**: Use `getOrPut` to check for existing key before allocating:
  ```zig
  pub fn put(self: *SymbolTable, name: []const u8, sym: Value) !void {
      const result = try self.map.getOrPut(self.allocator, name);
      if (!result.found_existing) {
          result.key_ptr.* = try self.allocator.dupe(u8, name);
      }
      result.value_ptr.* = sym;
      self.version +%= 1;
  }
  ```
- **Note**: Currently only triggered during init (`"T"`, `"NIL"`), not by `intern()` which guards with `.get()` first. Low practical impact but easy correctness fix.
- **Acceptance**: `put` with existing key doesn't leak. Test with testing allocator confirms.
- **Effort**: 20 minutes
- **Depends on**: Nothing

### 5. Add unit tests for untested JIT files
- **Files**: `src/jit/candidates.zig` (177 lines, 0 tests)
- **What**: Test `isEligible`, `ineligibleReason`, `findMatchingChunk`, `collectLambdaCandidates`
- **Note**: `hoist_contract.zig` (46 lines) is a compile-time contract check — tested implicitly by `zig build test`. No separate tests needed.
- **Acceptance**: `candidates.zig` has ≥4 test blocks covering: eligible lambda, ineligible captures, ineligible optional, chunk matching.
- **Effort**: 1-2 hours
- **Depends on**: Nothing

## Minor (defer — tracked elsewhere)

### 6. Condition system stubs — tracked in `docs/cl-symbols.md`
### 7. DST detection in decode-universal-time — tracked in `docs/cl-symbols.md`
### 8. Thread safety for JIT globals — not needed until concurrency work
### 9. jitSafepointBeforeAlloc doesn't trigger GC — less critical after item 1 fix
