\ maki/saved.f - the save-vs-recompute decision (CAD-PLAN sections 9/12, dot cad-9c).
\
\ For each forward tensor an adjoint needs (maki/adjoint.f ADJ-SAVE: the INPUT for
\ relu/gelu/norms/matmul, the OUTPUT for softmax-row), decide whether to SAVE it
\ across the forward/backward boundary or RECOMPUTE it in the backward pass, under the
\ shared section-9 cost model (the same bytes the fusion planner minimises):
\
\   SAVE      = a boundary write + a backward read      = 2 * bytes(tensor)
\   RECOMPUTE = producer flops * flop-byte-ratio  +  the producer's upstream reads
\
\ Costs are byte-equivalents; the flop-byte ratio converts flops to that unit. v1
\ reads it from the store's calibration.rows (SAVED-FBR: cost|global|flop-bytes) when
\ present, else the documented default SAVED-FBR-DEFAULT - NO fabricated measurement.
\ v1 model is single-step: a producer's upstream reads are counted as-is (no transitive
\ recompute). v1 policy floor: matmul/linear operands are ALWAYS saved (recomputing a
\ contraction would redo O(M*N*K) flops - never a win), independent of the comparison.
\ A saved tensor that is a model INPUT is not recomputable and is always saved.
\
\ Backward op-kinds (OP-*-BWD) carry ADJ-SAVE = SAVE-NONE, so this pass reads the
\ whole IR and naturally decides only over forward nodes (BW-BUILD need not have run).
\ maki -> habu only; saved owns -5111..-5114.

require lib/string.f
require lib/float.f
require lib/fmt.f
require maki/op-kind.f
require maki/op-registry.f
require maki/model-ir.f
require maki/adjoint.f
require maki/traffic.f
require maki/store.f
require maki/report.f

-5111 constant E-SV-CALIB    \ malformed calibration flop-byte value on parse

package MAKI
public

\ ---- decision verdicts ------------------------------------------------------
0 constant SV-SAVE           \ write at the boundary + read in backward
1 constant SV-RECOMPUTE      \ recompute the tensor in the backward pass

\ flop-byte cost ratio: byte-equivalent cost charged per flop. v1 default 1 (one
\ flop counted as one byte-move) is CONSERVATIVE toward saving - real GPUs make a
\ flop cheaper than a byte, so a calibrated ratio is a fraction; the integer v1
\ model uses 1 and defers the fractional calibration to the store's calibration.rows.
1 constant SAVED-FBR-DEFAULT

private

\ ---- operand-ref descriptor (input slot or producer node) -------------------
: SV-REF-ROWS ( MIR:operand-ref -- CAD-KIND:rows ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-ROWS@ else r MIR-REF-NODE MIR-ROWS@ then ;
: SV-REF-COLS ( MIR:operand-ref -- CAD-KIND:cols ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-COLS@ else r MIR-REF-NODE MIR-COLS@ then ;
: SV-REF-DT ( MIR:operand-ref -- dtype ) {: r:MIR:operand-ref :}
   r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-DT@ else r MIR-REF-NODE MIR-DT@ then ;

: SV-REF-ELEMS ( MIR:operand-ref -- n )
   {: r:MIR:operand-ref :}  r SV-REF-ROWS r SV-REF-COLS SHAPE-ELEMS DIM-RAW ;
: SV-REF-BYTES ( MIR:operand-ref -- n ) {: r:MIR:operand-ref :}
   r SV-REF-ELEMS r SV-REF-DT DT-SIZE DIM-RAW * ;

\ a node ref is recomputable (a node produces it); a model input is not
: SV-RECOMPUTABLE? ( MIR:operand-ref -- bool )  MIR-REF-INPUT? 0= ;

public

\ ---- flop-byte ratio (calibration.rows override, else the documented default) --
: SV-FBR-PARSE ( ptr u8 n -- n )
   STR>NUMBER?
   MATCH option none OF E-SV-CALIB throw ENDOF some OF ENDOF ;MATCH ;

: SAVED-FBR ( -- n )
   s" cost" s" global" s" flop-bytes" CALIB-GET if SV-FBR-PARSE else 2drop SAVED-FBR-DEFAULT then ;

\ ---- the two costs (byte-equivalents) --------------------------------------
: SAVED-SAVE-COST ( MIR:operand-ref -- n )  SV-REF-BYTES 2 * ;

\ producer flops = flops/elem(producer op) * elems ; upstream = producer's input bytes
: SAVED-RECOMPUTE-COST ( MIR:operand-ref -- n ) {: ref:MIR:operand-ref :}
   ref MIR-REF-NODE {: node:CAD-KIND:node-id :}
   node MIR-OP@ OPR-FLOPS ref SV-REF-ELEMS * SAVED-FBR *
   0 node MIR-IN-COUNT@ 0 ?do
      node i MIR-INPUT-IDX MIR-IN@ SV-REF-BYTES +
   loop + ;

\ ---- the decision (floor? = the tensor is a matmul/linear operand) ----------
: SAVED-DECIDE ( MIR:operand-ref bool -- n ) {: ref:MIR:operand-ref floor:bool :}
   floor if SV-SAVE exit then
   ref SV-RECOMPUTABLE? 0= if SV-SAVE exit then
   ref SAVED-RECOMPUTE-COST  ref SAVED-SAVE-COST  < if SV-RECOMPUTE else SV-SAVE then ;

private

\ ---- report row (one decision per needed tensor) ---------------------------
: SV-REF+ ( MIR:operand-ref -- ) {: r:MIR:operand-ref :}   \ append "n<idx>" or "i<slot>"
   r MIR-REF-INPUT? if s" i" SB-APPEND r MIR-REF-SLOT SLOT>RAW SB-INT
   else s" n" SB-APPEND r MIR-REF-NODE NODE>RAW SB-INT then ;

: SV-CMP+ ( n n -- ) {: s:n r:n :}              \ the honest save-vs-recompute operator
   s r < if s" < " else s r = if s" = " else s" > " then then SB-APPEND ;

: SV-ROW$ ( MIR:operand-ref bool -- ptr u8 n ) {: ref:MIR:operand-ref floor:bool :}
   ref floor SAVED-DECIDE {: v:n :}
   SB-RESET
   v SV-SAVE = if s" backward.saved: " else s" backward.recompute: " then SB-APPEND
   ref SV-REF+
   floor if s"  (matmul operand; policy floor)" SB-APPEND SB$ exit then
   ref SV-RECOMPUTABLE? 0= if s"  (model input; not recomputable)" SB-APPEND SB$ exit then
   ref SAVED-SAVE-COST {: sc:n :}  ref SAVED-RECOMPUTE-COST {: rc:n :}
   s"  (save " SB-APPEND sc SB-INT s" B " SB-APPEND
   sc rc SV-CMP+
   s" recompute " SB-APPEND rc SB-INT s" B)" SB-APPEND SB$ ;

\ SAVE-INPUT saves every forward operand the adjoint reads (unary -> operand 0;
\ mul/matmul -> both). The policy floor rides through as the matmul-class flag.
: SV-INPUT-SAVES ( report CAD-KIND:node-id bool -- report )
   {: fn:CAD-KIND:node-id floor:bool :}
   fn MIR-IN-COUNT@ 0 ?do
      fn i MIR-INPUT-IDX MIR-IN@ floor SV-ROW$ REPORT:WARN+
   loop ;

: SV-NODE-INTO ( report CAD-KIND:node-id -- report ) {: fn:CAD-KIND:node-id :}
   fn MIR-OP@ ADJ-SAVE {: sv:n :}
   sv SAVE-NONE = if exit then
   fn MIR-OP@ OPR-CLASS CLASS-MATMUL = {: floor:bool :}
   sv SAVE-OUTPUT = if fn MIR-NODE-REF floor SV-ROW$ REPORT:WARN+ exit then
   fn floor SV-INPUT-SAVES ;

public

\ write one save-vs-recompute decision row per forward tensor an adjoint needs.
: SAVED-INTO ( report -- report )
   MIR-N@ 0 ?do  i MIR-NODE-ID SV-NODE-INTO  loop ;

;package
