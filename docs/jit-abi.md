# JIT Runtime ABI (ARM64)

Current Habu JIT entrypoints are plain C ABI functions produced by Hoist and called via `CompiledFn` wrappers.

## Calling Convention
- C ABI on aarch64 (`callconv(.c)`).
- Arguments are tagged `i64` values.
- Supported direct wrappers today:
  - `fn() i64`
  - `fn(i64) i64`
  - `fn(i64, i64) i64`
  - `fn(i64, i64, i64) i64`
- Source of truth: `src/jit/backend.zig` (`CompiledFn.call0/call1/call2/call3`, `callFromValues`).

## Return Value
- Native code returns a tagged `i64`.
- VM side reinterprets the result as `Value`.

## Notes
- Legacy `JitContext`/error-union helper ABI is no longer the active path.
- Runtime patching and icache synchronization live in `src/jit/backend.zig` and `src/interp/repl.zig`.
