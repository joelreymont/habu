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
: SV-REF-ROWS ( n -- n ) {: r:n :}  r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-ROWS@ else r MIR-ROWS@ then ;
: SV-REF-COLS ( n -- n ) {: r:n :}  r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-COLS@ else r MIR-COLS@ then ;
: SV-REF-DT   ( n -- n ) {: r:n :}  r MIR-REF-INPUT? if r MIR-REF-SLOT MIR-SLOT-DT@   else r MIR-DT@   then ;

: SV-REF-ELEMS ( n -- n ) {: r:n :}  r SV-REF-ROWS r SV-REF-COLS * ;
: SV-REF-BYTES ( n -- n ) {: r:n :}  r SV-REF-ELEMS r SV-REF-DT DT-BYTES * ;

\ a node ref is recomputable (a node produces it); a model input is not
: SV-RECOMPUTABLE? ( n -- bool )  MIR-REF-INPUT? 0= ;

public

\ ---- flop-byte ratio (calibration.rows override, else the documented default) --
: SV-FBR-PARSE ( ptr u8 n -- n )  STR>NUMBER? 0= if E-SV-CALIB throw then ;

: SAVED-FBR ( -- n )
   s" cost" s" global" s" flop-bytes" CALIB-GET if SV-FBR-PARSE else 2drop SAVED-FBR-DEFAULT then ;

\ ---- the two costs (byte-equivalents) --------------------------------------
: SAVED-SAVE-COST ( n -- n )  SV-REF-BYTES 2 * ;

\ producer flops = flops/elem(producer op) * elems ; upstream = producer's input bytes
: SAVED-RECOMPUTE-COST ( n -- n ) {: ref:n :}
   ref MIR-OP@ OPR-FLOPS  ref SV-REF-ELEMS *  SAVED-FBR *
   0  ref MIR-IN-COUNT@ 0 ?do  ref i MIR-IN@ SV-REF-BYTES +  loop  + ;

\ ---- the decision (floor? = the tensor is a matmul/linear operand) ----------
: SAVED-DECIDE ( n bool -- n ) {: ref:n floor:bool :}
   floor if SV-SAVE exit then
   ref SV-RECOMPUTABLE? 0= if SV-SAVE exit then
   ref SAVED-RECOMPUTE-COST  ref SAVED-SAVE-COST  < if SV-RECOMPUTE else SV-SAVE then ;

private

\ ---- report row (one decision per needed tensor) ---------------------------
: SV-REF+ ( n -- ) {: r:n :}                    \ append "n<idx>" or "i<slot>"
   r MIR-REF-INPUT? if s" i" SB-APPEND r MIR-REF-SLOT SB-INT
   else s" n" SB-APPEND r SB-INT then ;

: SV-CMP+ ( n n -- ) {: s:n r:n :}              \ the honest save-vs-recompute operator
   s r < if s" < " else s r = if s" = " else s" > " then then SB-APPEND ;

: SV-ROW$ ( n bool -- ptr u8 n ) {: ref:n floor:bool :}
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
: SV-INPUT-SAVES ( report n bool -- report ) {: fn:n floor:bool :}
   fn MIR-IN-COUNT@ 0 ?do  fn i MIR-IN@ floor SV-ROW$ RPT-WARN+  loop ;

: SV-NODE-INTO ( report n -- report ) {: fn:n :}
   fn MIR-OP@ ADJ-SAVE {: sv:n :}
   sv SAVE-NONE = if exit then
   fn MIR-OP@ OPR-CLASS CLASS-MATMUL = {: floor:bool :}
   sv SAVE-OUTPUT = if  fn floor SV-ROW$ RPT-WARN+  exit then
   fn floor SV-INPUT-SAVES ;

public

\ write one save-vs-recompute decision row per forward tensor an adjoint needs.
: SAVED-INTO ( report -- report )
   MIR-N@ 0 ?do  i SV-NODE-INTO  loop ;

end-package
