\ mma-emit-diff.f - byte-identity harness for the TF32 mma.sync GEMM emitter.
\
\ dot habu-fp16-mma-tile. Emits EMIT-MATMUL-MMA (lib/ptx/cg-mma.f MMM) for a fixed
\ list of TF32 configs (default / SWZ / dyn / wide MFRAGS 2&4 / wide-B / 4-warp /
\ deep-stage / epilogue) with a config header line, so the whole stream can be diffed
\ before vs after a change to prove every TF32 config stays BYTE-IDENTICAL. The fp16
\ tile family adds a DTYPE dispatch; with MMA-DTYPE=0 (default) this stream must not
\ move. Device-independent (pure emit); no libcuda needed. Run:
\   bin/hb --load tools/ptx/mma-emit-diff.f > /tmp/emit.txt   (base vs branch, then diff)
\
\ BYTE-IDENTITY IS NOT EXPECTED for the DEEP-RING (stages 3-5) configs after the deferred cp.async
\ reorder (dot habu-reorder-cp-async, docs/eval-triton.md Round 9). That change moves the next-tile
\ prefetch to AFTER the compute burst in MMA-PIPE-KLOOP-MULTI only (N>=3) and drops its steady
\ wait_group literal N-1 -> N-2, so every stages 3/4/5 config here (the "deep-stage ring pipeline" rows)
\ intentionally DIFFERS base-vs-branch. The stages 1/2 pipes are UNCHANGED (MMA-PIPE-KLOOP-SINGLE and
\ MMA-PIPE-KLOOP-WITH keep issue-before-compute - the N=2 deferral was measured a -12%..-34% regression
\ and reverted), so every stages<=2 config stays byte-identical. Correctness of the reordered deep-ring
\ configs is proven ELEMENT-EXACT by tools/ptx/mma-gemm-check.f (stages 3/4/5, both warp grids), NOT by
\ an empty diff here.

require lib/errors.f
require lib/string.f
require lib/float.f
require lib/fmt.f
require src/arch/ptx/emit.f
require lib/ptx/cg.f
require lib/ptx/cg-matmul.f
require lib/ptx/cg-mma.f

package MMAEMITDIFF

: ED-RESET ( -- )                                   \ restore the committed tile defaults
   32 MMA-BK !  0 MMA-PAD !  2 MMA-STAGES !  0 MMA-DYNSMEM !  0 MMA-LMODE !
   1 MMA-MFRAGS !  8 MMA-WARPS !  0 MMA-EPILOG !  0 MMA-BLDM !  0 MMA-BPAD ! ;

\ set every knob (stack top = bpad, bottom = bk), print a header naming the config,
\ emit the MMM PTX to stdout, restore the defaults.
: ED#  ( n -- )  SB-INT s"  " SB-APPEND ;               \ append "n " to the header string builder
\ fp16 tile (MMA-DTYPE=1) with the transposed-Bs feed OFF (MMA-BTF16 defaults 0): proves the fp16
\ DEFAULT B-feed path stays BYTE-IDENTICAL across this change. Does NOT touch MMA-BTF16, so this harness
\ still loads against the pre-change (base) cg-mma.f for the base-vs-branch diff. Restores MMA-DTYPE=0.
: ED-ONE-F16 ( n n n n n n n -- )   \ bk pad stages dyn mfrags warps epilog
   MMA-EPILOG !  MMA-WARPS !  MMA-MFRAGS !  MMA-DYNSMEM !  MMA-STAGES !  MMA-PAD !  MMA-BK !
   1 MMA-DTYPE !
   SB-RESET s" === F16 bk " SB-APPEND MMA-BK @ ED#  s" pad " SB-APPEND MMA-PAD @ ED#
   s" stages " SB-APPEND MMA-STAGES @ ED#  s" dyn " SB-APPEND MMA-DYNSMEM @ ED#
   s" mfrags " SB-APPEND MMA-MFRAGS @ ED#  s" warps " SB-APPEND MMA-WARPS @ ED#
   s" epi " SB-APPEND MMA-EPILOG @ ED#  s" ===" SB-APPEND SB$ type cr
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   PTX-CAPTURE$ type
   0 MMA-DTYPE !  ED-RESET ;
\ bf16 tile (MMA-DTYPE=2, dot habu-bf16-m16n8k16-tile), transposed-Bs feed OFF: the bf16 mirror of
\ ED-ONE-F16. Emits the bf16 DEFAULT B-feed path so a later bf16 change can be regression-diffed; the
\ bf16 rows are APPENDED after the fp16 rows so the tf32(35)+fp16(6) prefix stays a byte-identical
\ base-vs-branch diff. Restores MMA-DTYPE=0.
: ED-ONE-BF16 ( n n n n n n n -- )   \ bk pad stages dyn mfrags warps epilog
   MMA-EPILOG !  MMA-WARPS !  MMA-MFRAGS !  MMA-DYNSMEM !  MMA-STAGES !  MMA-PAD !  MMA-BK !
   2 MMA-DTYPE !
   SB-RESET s" === BF16 bk " SB-APPEND MMA-BK @ ED#  s" pad " SB-APPEND MMA-PAD @ ED#
   s" stages " SB-APPEND MMA-STAGES @ ED#  s" dyn " SB-APPEND MMA-DYNSMEM @ ED#
   s" mfrags " SB-APPEND MMA-MFRAGS @ ED#  s" warps " SB-APPEND MMA-WARPS @ ED#
   s" epi " SB-APPEND MMA-EPILOG @ ED#  s" ===" SB-APPEND SB$ type cr
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   PTX-CAPTURE$ type
   0 MMA-DTYPE !  ED-RESET ;
: ED-ONE ( n n n n n n n n n n -- )   \ bk pad stages dyn mode mfrags warps epilog bldm bpad
   MMA-BPAD !  MMA-BLDM !  MMA-EPILOG !  MMA-WARPS !  MMA-MFRAGS !
   MMA-LMODE !  MMA-DYNSMEM !  MMA-STAGES !  MMA-PAD !  MMA-BK !
   SB-RESET s" === bk " SB-APPEND MMA-BK @ ED#  s" pad " SB-APPEND MMA-PAD @ ED#
   s" stages " SB-APPEND MMA-STAGES @ ED#  s" dyn " SB-APPEND MMA-DYNSMEM @ ED#
   s" mode " SB-APPEND MMA-LMODE @ ED#  s" mfrags " SB-APPEND MMA-MFRAGS @ ED#
   s" warps " SB-APPEND MMA-WARPS @ ED#  s" epi " SB-APPEND MMA-EPILOG @ ED#
   s" bldm " SB-APPEND MMA-BLDM @ ED#  s" bpad " SB-APPEND MMA-BPAD @ ED#
   s" ===" SB-APPEND SB$ type cr
   PTX-CAPTURE-ON  EMIT-MATMUL-MMA  PTX-CAPTURE-OFF
   PTX-CAPTURE$ type
   ED-RESET ;

public
: ED-ALL ( -- )
   \ base modes (MFRAGS=1, 8-warp)
   32 0 2 0 0 1 8 0 0 0 ED-ONE
   32 0 2 0 1 1 8 0 0 0 ED-ONE
   32 0 2 0 2 1 8 0 0 0 ED-ONE
   \ larger-BK / swizzle (MFRAGS=1, 8-warp)
   64 0 1 0 0 1 8 0 0 0 ED-ONE
   64 0 1 0 2 1 8 0 0 0 ED-ONE
   64 0 2 1 0 1 8 0 0 0 ED-ONE
   64 0 2 1 2 1 8 0 0 0 ED-ONE
   32 8 2 0 0 1 8 0 0 0 ED-ONE
   32 8 2 0 2 1 8 0 0 0 ED-ONE
   64 8 2 1 2 1 8 0 0 0 ED-ONE
   \ wider-M (MFRAGS=2/4, 8-warp)
   32 8 2 1 2 2 8 0 0 0 ED-ONE
   32 8 1 0 2 2 8 0 0 0 ED-ONE
   32 8 2 1 0 2 8 0 0 0 ED-ONE
   32 8 2 1 2 4 8 0 0 0 ED-ONE
   32 8 2 1 0 4 8 0 0 0 ED-ONE
   \ B-side ldmatrix transposed-Bs (8-warp)
   32 8 1 1 2 4 8 0 1 0 ED-ONE
   32 8 1 1 2 4 8 0 1 4 ED-ONE
   32 8 2 1 2 4 8 0 1 4 ED-ONE
   32 8 2 1 2 2 8 0 1 4 ED-ONE
   32 8 1 1 2 2 8 0 1 4 ED-ONE
   32 8 1 0 2 2 8 0 1 4 ED-ONE
   \ 4-warp (WARPS=4)
   32 8 1 0 2 4 4 0 0 0 ED-ONE
   32 8 2 1 2 4 4 0 0 0 ED-ONE
   32 8 2 1 0 4 4 0 0 0 ED-ONE
   32 8 1 1 2 4 4 0 1 4 ED-ONE
   32 8 2 1 2 4 4 0 1 4 ED-ONE
   32 8 1 0 2 2 4 0 0 0 ED-ONE
   \ deep-stage ring pipeline (WARPS=4, stages 3-5)
   32 8 3 1 2 4 4 0 0 0 ED-ONE
   32 8 3 1 2 2 4 0 0 0 ED-ONE
   32 8 4 1 2 2 4 0 0 0 ED-ONE
   32 8 5 1 2 2 4 0 0 0 ED-ONE
   32 8 3 1 2 4 4 0 1 4 ED-ONE
   \ shared-memory epilogue (8-warp and 4-warp)
   32 8 2 1 2 2 8 1 0 0 ED-ONE
   32 8 1 0 2 4 4 1 0 0 ED-ONE
   32 8 1 1 2 4 8 1 1 4 ED-ONE
   \ fp16 tile, transposed-Bs feed OFF (byte-identical to the pre-change fp16 default path)
   32 0 2 0 1 8 0 ED-ONE-F16
   32 0 2 1 2 8 0 ED-ONE-F16
   32 0 1 0 4 8 0 ED-ONE-F16
   32 0 2 1 4 4 0 ED-ONE-F16
   32 0 1 0 2 4 0 ED-ONE-F16
   32 0 2 1 2 8 1 ED-ONE-F16
   \ bf16 tile (dot habu-bf16-m16n8k16-tile), APPENDED after the tf32+fp16 stream so their prefix stays a
   \ byte-identical base-vs-branch diff; mirrors the six fp16 rows (only the mma dtype token differs)
   32 0 2 0 1 8 0 ED-ONE-BF16
   32 0 2 1 2 8 0 ED-ONE-BF16
   32 0 1 0 4 8 0 ED-ONE-BF16
   32 0 2 1 4 4 0 ED-ONE-BF16
   32 0 1 0 2 4 0 ED-ONE-BF16
   32 0 2 1 2 8 1 ED-ONE-BF16 ;

;package

MMAEMITDIFF:ED-ALL
