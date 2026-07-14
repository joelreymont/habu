\ attention-roles-test.f - operand roles thread through the attention scaffold.
\
\ Regression for dot habu-attention-scaffold-erases-e03f933b. The four Q/K/V/O
\ matrix tokens are SAME-TYPED; before this change ATTN:STATE discarded them
\ (2drop 2drop) and SETUP-EMIT bound operands positionally (%rd1=Q..%rd4=O), so a
\ candidate that permuted Q/K/V/O before ATTN:START emitted BYTE-IDENTICAL PTX -
\ the role swap was a codegen no-op. STATE now packs each operand's pointer
\ register into the attnctx and each phase loads/stores through the routed role.
\
\ This pins the gap closed: two candidates differing ONLY in operand order must
\ emit DIFFERENT PTX (headline T-STR= assertion), with the specific staging/score/
\ output base registers proving the swap reaches the exact load/store; and the
\ correctly-ordered candidate must still bind %rd1..%rd4 in role order so correct
\ authoring stays byte-unchanged (lib/ptx/attention-checked-test.f pins the bytes).

require lib/ptx/test-prelude.f
require lib/ptx/cg-attention.f

package ATTN-ROLES-TEST

$4000 constant GOLD-CAP        \ attention PTX is ~2.6 KB; one capture fits
create GOLD GOLD-CAP allot
variable GOLD-U

\ the fixed checked phase pipeline (START..FINISH) is inlined after AUTHOR-OPEN so
\ each emitter's net stack effect is ( -- ); only the operand ORDER differs.
: EMIT-CORRECT ( -- )          \ Q K V O in canonical role order
   ATTN:AUTHOR-OPEN
   ATTN:START ATTN:STAGE-Q ATTN:SCORE ATTN:SOFTMAX ATTN:OUTPUT ATTN:FINISH
   ATTN:AUTHOR-CLOSE ;

: EMIT-QK-SWAP ( -- )          \ 2swap swap 2swap swaps the two DEEPEST matrices: Q<->K
   ATTN:AUTHOR-OPEN 2swap swap 2swap
   ATTN:START ATTN:STAGE-Q ATTN:SCORE ATTN:SOFTMAX ATTN:OUTPUT ATTN:FINISH
   ATTN:AUTHOR-CLOSE ;

: EMIT-OV-SWAP ( -- )          \ swap the top two matrices: V<->O (output routed into V)
   ATTN:AUTHOR-OPEN swap
   ATTN:START ATTN:STAGE-Q ATTN:SCORE ATTN:SOFTMAX ATTN:OUTPUT ATTN:FINISH
   ATTN:AUTHOR-CLOSE ;

: GOLD! ( ptr u8 n -- )        \ stash a capture into GOLD (survives the next capture)
   {: a:ptr u:n :}
   a GOLD u BYTE-COPY  u GOLD-U ! ;

: GOLD$ ( -- ptr u8 n )  GOLD GOLD-U @ ;

public

: RUN ( -- )
   T-RESET
   128 %BLOCK
   \ correct authoring -> stash the golden emission out of the reused capture buffer
   PTX-CAPTURE-ON EMIT-CORRECT PTX-CAPTURE-OFF  PTX-CAPTURE$ GOLD!
   s" correct staging binds Q=%rd1" T-LABEL  GOLD$ s" add.u64 %rd10,%rd1,%rd10;" CONTAINS? TTRUE
   s" correct score binds K=%rd2" T-LABEL    GOLD$ s" add.u64 %rd11,%rd2,%rd11;" CONTAINS? TTRUE
   s" correct output reads V=%rd3" T-LABEL   GOLD$ s" add.u64 %rd12,%rd3,%rd12;" CONTAINS? TTRUE
   s" correct output stores O=%rd4" T-LABEL  GOLD$ s" add.u64 %rd13,%rd4,%rd13;" CONTAINS? TTRUE

   \ Q/K swap: STAGE-Q now stages from %rd2 (K), SCORE reads %rd1 (Q) -> different PTX
   PTX-CAPTURE-ON EMIT-QK-SWAP PTX-CAPTURE-OFF  PTX-CAPTURE$ {: qa:ptr qu:n :}
   s" Q/K swap emits different PTX" T-LABEL   qa qu GOLD$ T-STR= 0= TTRUE
   s" Q/K swap stages from K=%rd2" T-LABEL    qa qu s" add.u64 %rd10,%rd2,%rd10;" CONTAINS? TTRUE
   s" Q/K swap scores from Q=%rd1" T-LABEL    qa qu s" add.u64 %rd11,%rd1,%rd11;" CONTAINS? TTRUE

   \ output-into-V: OUTPUT reads V-data from %rd4 (O ptr) and stores O into %rd3 (V ptr)
   PTX-CAPTURE-ON EMIT-OV-SWAP PTX-CAPTURE-OFF  PTX-CAPTURE$ {: oa:ptr ou:n :}
   s" output-into-V emits different PTX" T-LABEL  oa ou GOLD$ T-STR= 0= TTRUE
   s" output-into-V reads V-data from O=%rd4" T-LABEL  oa ou s" add.u64 %rd12,%rd4,%rd12;" CONTAINS? TTRUE
   s" output-into-V stores O into V=%rd3" T-LABEL      oa ou s" add.u64 %rd13,%rd3,%rd13;" CONTAINS? TTRUE

   T-REPORT ;

;package

ATTN-ROLES-TEST:RUN
