\ opt-test.f - fixtures for the sound PTX optimization passes (lib/ptx/opt.f).
\
\ Each pass has a committed before/after text fixture; opt(opt(x)) = opt(x); the
\ mul.rn+add.rn pair is proven to survive (no unsound fma fusion); and a safety
\ layer emits real kernels (saxpy / gelu / cg-mma), optimizes them, and asserts
\ the semantics survive (key instructions still present) plus the host-measurable
\ instruction-count deltas. Load after lib/ptx/opt.f. Checked Habu.

require lib/ptx/test-prelude.f
require lib/ptx/cg-activation.f
require lib/ptx/cg-matmul.f
require lib/ptx/cg-mma.f
require lib/ptx/opt-ir.f
require lib/ptx/opt.f

$20000 constant OPTT-CAP
create OPTT-SAVE OPTT-CAP allot
variable OPTT-SAVE-U

T-RESET

\ ---- per-pass helpers: parse, run one pass, render ----
: OPTT-CSE$ ( ptr u8 n -- ptr u8 n )       PTX-PARSE PTX-OPT-CSE PTX-RENDER PTX-RENDER$ ;
: OPTT-COPY$ ( ptr u8 n -- ptr u8 n )      PTX-PARSE PTX-OPT-COPYPROP PTX-RENDER PTX-RENDER$ ;
: OPTT-CONST$ ( ptr u8 n -- ptr u8 n )     PTX-PARSE PTX-OPT-CONSTFOLD PTX-RENDER PTX-RENDER$ ;
: OPTT-DCE$ ( ptr u8 n -- ptr u8 n )       PTX-PARSE PTX-OPT-DCE PTX-RENDER PTX-RENDER$ ;
: OPTT-PEEP$ ( ptr u8 n -- ptr u8 n )      PTX-PARSE PTX-OPT-PEEP PTX-RENDER PTX-RENDER$ ;

\ ---- CSE: identical rhs (cvt of same source) reused, redundant def removed ----
: OPTT-CSE ( -- )
   s\" .visible .entry K\n{\ncvt.rna.tf32.f32 %r60, %f26;\ncvt.rna.tf32.f32 %r61, %f26;\nadd.f32 %f9, %r60, %r61;\nst.global.f32 [%rd1], %f9;\n}\n" OPTT-CSE$
   s\" .visible .entry K\n{\ncvt.rna.tf32.f32 %r60, %f26;\nadd.f32 %f9, %r60, %r60;\nst.global.f32 [%rd1], %f9;\n}\n" STR= TTRUE ;

\ ---- copy-propagation: a register copy is forwarded, the mov removed ----
: OPTT-COPY ( -- )
   s\" {\nmov.f32 %f7, %f2;\nmul.rn.f32 %f8, %f7, %f3;\nst.global.f32 [%rd1], %f8;\n}\n" OPTT-COPY$
   s\" {\nmul.rn.f32 %f8, %f2, %f3;\nst.global.f32 [%rd1], %f8;\n}\n" STR= TTRUE ;

\ ---- constant-fold: a duplicate immediate mov folds to the first constant reg ----
: OPTT-CONST ( -- )
   s\" {\nmov.f32 %f2, 0f3F800000;\nmov.f32 %f3, 0f3F800000;\nadd.f32 %f9, %f2, %f3;\nst.global.f32 [%rd1], %f9;\n}\n" OPTT-CONST$
   s\" {\nmov.f32 %f2, 0f3F800000;\nadd.f32 %f9, %f2, %f2;\nst.global.f32 [%rd1], %f9;\n}\n" STR= TTRUE ;

\ ---- DCE: a pure, side-effect-free def whose dst is read nowhere is removed ----
: OPTT-DCE ( -- )
   s\" {\nmul.rn.f32 %f2, %f4, %f5;\nmul.rn.f32 %f9, %f4, %f6;\nst.global.f32 [%rd1], %f2;\n}\n" OPTT-DCE$
   s\" {\nmul.rn.f32 %f2, %f4, %f5;\nst.global.f32 [%rd1], %f2;\n}\n" STR= TTRUE ;

\ ---- peephole: self-move elimination ----
: OPTT-PEEP ( -- )
   s\" {\nadd.f32 %f5, %f1, %f2;\nmov.f32 %f5, %f5;\nmul.f32 %f9, %f5, %f3;\nst.global.f32 [%rd1], %f9;\n}\n" OPTT-PEEP$
   s\" {\nadd.f32 %f5, %f1, %f2;\nmul.f32 %f9, %f5, %f3;\nst.global.f32 [%rd1], %f9;\n}\n" STR= TTRUE ;

\ ---- idempotence: opt(opt(x)) = opt(x) ----
: OPTT-IDEM ( -- )
   s\" .visible .entry K\n{\ncvt.rna.tf32.f32 %r60, %f26;\ncvt.rna.tf32.f32 %r61, %f26;\nadd.f32 %f9, %r60, %r61;\nst.global.f32 [%rd1], %f9;\n}\n" OPT-PTX {: oa:ptr ou:n :}
   oa OPTT-SAVE ou BYTE-COPY  ou OPTT-SAVE-U !
   OPTT-SAVE OPTT-SAVE-U @ OPT-PTX
   OPTT-SAVE OPTT-SAVE-U @ STR= TTRUE ;

\ ---- fma refusal: a .rn mul+add pair is NEVER fused (rounding boundary) ----
: OPTT-FMA-REFUSED ( -- )
   s\" {\nmul.rn.f32 %f7, %f2, %f3;\nadd.rn.f32 %f8, %f7, %f4;\nst.global.f32 [%rd1], %f8;\n}\n" OPT-PTX
   2dup s" fma" CONTAINS? 0= TTRUE
   2dup s" mul.rn.f32 %f7, %f2, %f3;" CONTAINS? TTRUE
   s" add.rn.f32 %f8, %f7, %f4;" CONTAINS? TTRUE ;

\ ================= safety: real kernels keep their semantics =================
: OPTT-SAXPY ( -- )
   PTX-CAPTURE-ON PTX-EMIT-SAXPY PTX-CAPTURE-OFF PTX-CAPTURE$ {: a:ptr u:n :}
   a u PTX-PARSE PTX-INSN-COUNT {: b:n :}
   a u OPT-PTX {: oa:ptr ou:n :}
   PTX-INSN-COUNT b T=                                     \ proven kernel: no redundancy, count kept
   oa ou s" mul.rn.f32 %f4, %f1, %f2;" CONTAINS? TTRUE
   oa ou s" add.rn.f32 %f4, %f4, %f3;" CONTAINS? TTRUE
   oa ou s" st.global.f32 [%rd5], %f4;" CONTAINS? TTRUE ;

: OPTT-PUT ( n -- ) {: r:n :}   \ append a store of a result register (keeps it live)
   SB-RESET s" st.global.f32 [%rd1], " CG-S r CG-F s" ;" CG-S CG-LINE ;

: OPTT-GELU1 ( -- )                                        \ one gelu forward: no redundancy
   PTX-CAPTURE-ON CG-RESET 1 EMIT-GELU OPTT-PUT PTX-CAPTURE-OFF PTX-CAPTURE$ {: a:ptr u:n :}
   a u PTX-PARSE PTX-INSN-COUNT {: b:n :}
   a u OPT-PTX {: oa:ptr ou:n :}
   PTX-INSN-COUNT b T=
   oa ou s" ex2.approx.f32" CONTAINS? TTRUE ;              \ activation core survives

: OPTT-GELU2 ( -- )                                        \ gelu(x) recomputed twice -> CSE removes the redundant recompute
   PTX-CAPTURE-ON CG-RESET 1 EMIT-GELU OPTT-PUT 1 EMIT-GELU OPTT-PUT PTX-CAPTURE-OFF PTX-CAPTURE$ {: a:ptr u:n :}
   a u PTX-PARSE PTX-INSN-COUNT {: b:n :}
   a u OPT-PTX 2drop PTX-INSN-COUNT {: c:n :}
   b c - 0 > TTRUE ;                                       \ strictly fewer instructions

: OPTT-MMA ( -- )                                          \ loop-invariant address recompute removed; anchors survive
   PTX-CAPTURE-ON EMIT-MATMUL-MMA PTX-CAPTURE-OFF PTX-CAPTURE$ {: a:ptr u:n :}
   a u PTX-PARSE PTX-INSN-COUNT {: b:n :}
   a u OPT-PTX {: oa:ptr ou:n :}
   PTX-INSN-COUNT {: c:n :}
   b c - 0 > TTRUE                                         \ strictly fewer instructions
   oa ou s" mma.sync.aligned.m16n8k8" CONTAINS? TTRUE      \ tensor-core op preserved
   oa ou s" cvt.rna.tf32.f32" CONTAINS? TTRUE              \ tf32 conversions preserved
   oa ou s" st.global.f32 [%rd12]," CONTAINS? TTRUE ;      \ result store preserved

OPTT-CSE
OPTT-COPY
OPTT-CONST
OPTT-DCE
OPTT-PEEP
OPTT-IDEM
OPTT-FMA-REFUSED
OPTT-SAXPY
OPTT-GELU1
OPTT-GELU2
OPTT-MMA

T-REPORT
