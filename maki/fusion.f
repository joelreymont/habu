\ maki/fusion.f - op-fusion is CONCATENATION (a concatenative-language fact).
\
\ A fused elementwise chain is just a sequence of checked tile words:
\   x g LOAD-V4  a SCALE-V4  y g LOAD-V4  ADD-V4  RELU-V4  y g STORE-V4
\ Composing the words IS the fusion: intermediates stay on the (register) stack, so
\ only the inputs load and the result stores - no global round-trips. The checker
\ types the whole sequence at once, so the fused effect is PROVEN for free. There is
\ no fusion "pass": lowering a maki/ONNX subgraph = mapping each node to its tile
\ word(s) and CONCATENATING them, framed by the input loads and the single store.
\
\ Push an op chain with FUSE-OP!, then FUSE-DRIVER writes a driver that defines the
\ fused kernel K and emits it; running bin/hb on it, the kernel CERTIFYING is the
\ proof the fusion is correct (or it fails closed). Benchmark target: hand-fused
\ Triton (parity at the memory ceiling); there is NO unfused baseline. Checked Habu;
\ load after the PTX v4 vocab + maki/eval.f (SB-* string builder) + lib/fs-mutate.f.

\ Wrapped in `package MAKI`: the words export as `MAKI:FUSE-*` / `MAKI:OP-*`, isolated
\ from the global/habu namespace (docs/forth.md "Packages"). E-FUSE stays global (a
\ cross-cutting error code, like lib/errors.f's E-*); the package body reaches it via the
\ package's global-fallback lookup.

-5002 constant E-FUSE  \ unfusible / unknown op, or chain overflow (fail-closed)

package MAKI
public

\ elementwise op codes (maki/ONNX subgraph nodes)
0 constant OP-SCALE    \ tile := tile * uniform a   (ONNX Mul-by-scalar / Gemm alpha)
1 constant OP-ADD      \ tile := tile + load(y)      (ONNX Add of the second input)
2 constant OP-RELU     \ tile := max(tile, 0)        (ONNX Relu)

\ op -> the tile word(s) it contributes to the fused body (fail-closed on unknown)
: FUSE-OP-WORDS ( n -- ptr u8 n ) {: op :}
   op OP-SCALE = if s"  a SCALE-V4"          exit then
   op OP-ADD   = if s"  y g LOAD-V4 ADD-V4"  exit then
   op OP-RELU  = if s"  RELU-V4"             exit then
   E-FUSE throw ;

\ the op chain to fuse (a checked buffer the checker types as a pointer)
16 constant FUSE-MAX
create FUSE-OPS  FUSE-MAX cells allot
variable FUSE-NOPS
: FUSE-RESET ( -- )  0 FUSE-NOPS ! ;
: FUSE-OP! ( n -- )  {: op :}
   FUSE-NOPS @ FUSE-MAX >= if E-FUSE throw then
   op  FUSE-NOPS @ cells FUSE-OPS + !  FUSE-NOPS @ 1+ FUSE-NOPS ! ;

private

\ append the fused word sequence (the WHOLE fusion) to the string builder: bind
\ inputs, make the per-thread ctx, load x, CONCATENATE each op's words, store.
\ Internal helper - NOT part of the public interface (no MAKI:FUSE-BODY export); only
\ FUSE-DRIVER uses it.
: FUSE-BODY ( -- )
   s"  {: x y a :} x GRID-CTX-V4 {: g :} x g LOAD-V4" SB-APPEND
   FUSE-NOPS @ 0 do  FUSE-OPS i cells + @  FUSE-OP-WORDS SB-APPEND  loop
   s"  y g STORE-V4" SB-APPEND ;

public

\ write a driver that defines the fused kernel K (entry SAXPY) and emits it (v4)
: FUSE-DRIVER ( -- )
   SB-RESET
   s" 256 %BLOCK" SB-APPEND  10 SB-APPEND-C
   s" : K ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> uniform<f32> -- )" SB-APPEND
   FUSE-BODY
   s"  ;" SB-APPEND  10 SB-APPEND-C
   s" CG-RESET CG-HEADER CG-ENTRY CG-OPEN CG-PARAMS 1 SPAN-REG 2 SPAN-REG 1 UNIFORM-REG K CG-RET CG-CLOSE"
      SB-APPEND  10 SB-APPEND-C
   s" bye" SB-APPEND  10 SB-APPEND-C
   s" /tmp/fuse-driver.f" SB$ WRITE-ALL ;

end-package
