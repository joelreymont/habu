# JIT Runtime ABI (ARM64)

This JIT calls Zig runtime helpers that return `arith.Error!Value`.

## Calling Convention
- Zig aarch64 error-union ABI uses sret.
- `x0` **and** `x8` point to the return buffer.
- Arguments shift by one:
  - Unary: `x1 = *JitContext`, `x2 = arg0`
  - Binary: `x1 = *JitContext`, `x2 = arg0`, `x3 = arg1`

## Return Buffer Layout
- 16 bytes total
- Offset 0: `Value` (8 bytes)
- Offset 8: `anyerror` tag (u16)
- Offset 10–15: padding

Guards:
- Compile-time layout assertions in `src/jit/ctx.zig`.
