# JIT Runtime ABI (ARM64)

This JIT calls Zig runtime helpers that return `arith.Error!Value`.

## Calling Convention
- Zig aarch64 error-union ABI uses sret.
- `x8` points to the return buffer.
- `x0` is the error trace pointer.
- Arguments shift by one:
  - Unary: `x1 = *JitContext`, `x2 = arg0`
  - Binary: `x1 = *JitContext`, `x2 = arg0`, `x3 = arg1`

## Return Buffer Layout
- 16 bytes total
- Offset 0: `Value` (8 bytes)
- Offset 8: `anyerror` tag (u16)
- Offset 10–15: padding

## JitContext Layout
- Offset 0: `sp` (Value*)
- Offset 8: `const_pool` (Value*)
- Offset 16: `frame_base` (Value*)
- Offset 24: `stack_end` (Value*)
- Offset 32: `heap` (Heap*)
- Offset 40: `ret_buf` (RetBuf*)
- Offset 48: `err` (u16)
- Offset 56: `const_count` (usize)
- Offset 64: `err_trace` (*StackTrace)
- Offset 72: `vm` (*Vm)

Guards:
- Compile-time layout assertions in `src/jit/ctx.zig`.
