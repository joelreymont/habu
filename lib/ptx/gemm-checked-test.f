\ gemm-checked-test.f - the tiled GEMM data-flow expressed as a CHECKED KERNEL: body.
\
\ This is the type-level re-expression (habu-re-express-tiled, part 1): the same data-flow
\ the unchecked raw-PTX kernel in lib/ptx/cg-matmul.f emits by hand - zero a register
\ accumulator, sweep the K dimension staging A and B blocks into shared, load tiles,
\ FMA-accumulate, finalize, store - now COMPOSED from the checked tile-DSL vocabulary and
\ VERIFIED by the checker:
\   (a) the counted loop          -> an inline checked `?do ... loop` (locals + index
\                                    accessible; the checker enforces a stack-neutral body,
\                                    i.e. the accumulator is loop-invariant).
\   (b) shared-memory staging     -> STAGE / SLOAD (lib/ptx/tile-smem.f).
\   (c) the register accumulator  -> ACC-ZERO / ACC-FMA / ACC-TILE (lib/ptx/tile-acc.f);
\                                    ACC-TILE is the completion gate before STORE.
\
\ The KERNEL: definition certifying IS the proof. What remains for a device-correct checked
\ GEMM is CODEGEN: the tile-DSL op bodies throw E-PTX-NOIMPL today, so MM-CHECKED type-checks
\ but does not yet emit; lowering it to the same PTX as cg-matmul.f (and a 2-D grid) is dotted
\ habu-tiled-gemm-codegen. The negative (a non-stack-neutral K-loop body) is gated by
\ lib/ptx/gemm-checked-neg-test.f. Load after lib/ptx/tile.f, tile-smem.f, and tile-acc.f.

T-RESET

256 %BLOCK

\ C = A * B, the tiled-GEMM data-flow as a checked KERNEL: body
KERNEL: MM-CHECKED ( span<space-global,f32,extent-n> span<space-global,f32,extent-n> span<space-global,f32,extent-n> -- )  GRID: ceil-n-256
   {: A B C :}
   A GRID-CTX {: g :}
   g ACC-ZERO                          \ acc = 0  (register accumulator)
   4 0 ?do                             \ K-reduction over BK tiles (inline checked loop)
      A g STAGE g SLOAD                 \ stage A block -> shared, load A tile
      B g STAGE g SLOAD                 \ stage B block -> shared, load B tile
      ACC-FMA                           \ acc += A_tile * B_tile
   loop
   ACC-TILE  C g STORE ;                \ finalize accumulator (completion gate), store to C

\ Clean load past this point is the positive proof: the tiled GEMM data-flow type-checks as
\ a checked KERNEL: body composed from the (a)+(b)+(c) tile-DSL capabilities.

T-REPORT
