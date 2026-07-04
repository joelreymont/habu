\ maki/model-ir.f - the model IR node table + shape/dtype/layout keys + render.
\
\ CAD-PLAN section 4 / docs/model-cad.md Phase 1. The model is a DAG; recursive
\ unboxed ADTs are a type-families v1 non-goal, so nodes reference operands by
\ TYPED INDEX into a flat table (the lib/ptx/ir.f + tensor-value plan-store
\ pattern), never by nested value. Each node records: op-kind (maki/op-kind.f),
\ an operand window into a shared ref pool, output descriptor facts (shape/dtype/
\ layout from tensor-value descriptors), an attrs cell, a materialization flag,
\ and a reserved autograd-metadata cell.
\
\ Operand refs are tagged: a node ref is the node index (>= 0); a model-input ref
\ is -(slot+1) (< 0). Input slots carry their own descriptor facts so the render
\ and shape keys are self-contained (they survive TV-RESET; the IR denormalizes
\ the facts rather than holding tensor handles). A slot also records its base
\ alignment CLASS (tensor-value AL-*), defaulting to AL-UNKNOWN until a bound
\ buffer records a measured one (MIR-SLOT-AL!); the memory planner (maki/mem-plan.f)
\ keys vectorization off it, so an unrecorded input reports "unknown -> scalar".
\
\ Fail closed: unknown op-kind, bad operand ref, out-of-range node/slot index, bad
\ alignment class, and every capacity are named throws. The record layout never
\ leaks - callers pass or receive primitive facts only, so the store can swap to an
\ ADT family later (cad-adt-swap) without touching a caller. maki -> habu only;
\ owns -5055..-5062.

require maki/op-kind.f
require maki/op-registry.f
require maki/move-facts.f
require maki/tensor.f
require maki/tensor-value.f
require lib/string.f
require lib/float.f
require lib/fmt.f

-5055 constant E-MIR-CAP      \ node table capacity exceeded
-5056 constant E-MIR-IDX      \ node index out of range
-5057 constant E-MIR-OPKIND   \ op-kind out of range
-5058 constant E-MIR-INCAP    \ operand ref pool capacity exceeded
-5059 constant E-MIR-REF      \ operand ref names an uncommitted node / bad input slot
-5060 constant E-MIR-INSLOT   \ input-slot index / capacity out of range
-5061 constant E-MIR-STATE    \ node builder used out of order
-5062 constant E-MIR-ALIGN    \ input-slot alignment class out of range

package MAKI
public

\ ---- operand ref tagging (node index >= 0 ; input slot = -(slot+1)) ----
: MIR-IN-REF     ( n -- n )  1+ negate ;       \ slot -> ref
: MIR-REF-INPUT? ( n -- bool )  0< ;
: MIR-REF-SLOT   ( n -- n )  negate 1- ;       \ input ref -> slot
: MIR-REF-NODE   ( n -- n )  ;                 \ node ref -> node index (identity)

private

128 constant MIR-CAP          \ max nodes
384 constant MIR-INCAP        \ max operand refs across the table
64  constant MIR-IN-CAP       \ max model-input slots

\ node table: one create-array per field (keeps each field's cell independent)
create MI-OP    MIR-CAP cells allot     \ op-kind
create MI-INOFF MIR-CAP cells allot     \ operand window start in MI-INS
create MI-INCNT MIR-CAP cells allot     \ operand window length
create MI-ROWS  MIR-CAP cells allot     \ output descriptor facts
create MI-COLS  MIR-CAP cells allot
create MI-DT    MIR-CAP cells allot
create MI-LAY   MIR-CAP cells allot
create MI-ATTR  MIR-CAP cells allot     \ attrs cell (variant/axis/eps, op-typed)
create MI-MAT   MIR-CAP cells allot     \ materialization flag
create MI-AD    MIR-CAP cells allot     \ autograd metadata (reserved)
variable MIR-N

create MI-INS   MIR-INCAP cells allot   \ flat operand-ref pool
variable MIR-INS-U

\ model-input slots (their own descriptor facts)
create MI-IS-ROWS MIR-IN-CAP cells allot
create MI-IS-COLS MIR-IN-CAP cells allot
create MI-IS-DT   MIR-IN-CAP cells allot
create MI-IS-LAY  MIR-IN-CAP cells allot
create MI-IS-AL   MIR-IN-CAP cells allot     \ base alignment class (AL-*); AL-UNKNOWN default
variable MIR-IS-N

\ pending-node staging (any-arity records with the fixed-arity ref pool)
variable MIR-PEND-KIND
variable MIR-PEND-OFF
variable MIR-PEND-CNT
variable MIR-PEND-ON

: MIR-CK ( n -- n )                     \ validate a committed node index
   dup 0 < over MIR-N @ >= or if E-MIR-IDX throw then ;

: MIR-IS-CK ( n -- n )                  \ validate a model-input slot index
   dup 0 < over MIR-IS-N @ >= or if E-MIR-INSLOT throw then ;

: MIR-DT-CK ( n -- n )
   dup DT-VALID? 0= if E-MK-DTYPE throw then ;

: MIR-LAY-CK ( n -- n )
   dup dup 0 < swap LAY-N >= or if E-TV-LAYOUT throw then ;

\ an operand ref must name a committed node (>=0, < MIR-N) or a live input slot
: MIR-REF-CK ( n -- n ) {: ref:n :}
   ref MIR-REF-INPUT? if
      ref MIR-REF-SLOT MIR-IS-N @ >= if E-MIR-REF throw then
   else
      ref MIR-N @ >= if E-MIR-REF throw then
   then
   ref ;

public

\ ---- lifecycle -------------------------------------------------------------
: MIR-RESET ( -- )
   0 MIR-N !  0 MIR-INS-U !  0 MIR-IS-N !  0 MIR-PEND-ON ! ;

: MIR-N@ ( -- n )         MIR-N @ ;
: MIR-IN-SLOTS@ ( -- n )  MIR-IS-N @ ;

\ ---- checkpoint / restore (transient IR growth, e.g. the cad-9 backward pass) ----
\ MIR-MARK captures the table high-water marks; MIR-RELEASE truncates back to them
\ and clears any pending builder. A caller that appends nodes/slots and wants the IR
\ restored (gradcheck builds a throwaway backward pass) brackets its work with these.
: MIR-MARK    ( -- n n n )    MIR-N @  MIR-IS-N @  MIR-INS-U @ ;
: MIR-RELEASE ( n n n -- ) {: nn:n sn:n iu:n :}
   nn MIR-N !  sn MIR-IS-N !  iu MIR-INS-U !  0 MIR-PEND-ON ! ;

\ ---- model-input slots -----------------------------------------------------
: MIR-INPUT+ ( n n n n -- n )           \ rows cols dtype layout -- slot
   {: rows:n cols:n dt:n lay:n :}
   dt MIR-DT-CK drop  lay MIR-LAY-CK drop
   MIR-IS-N @ MIR-IN-CAP >= if E-MIR-INSLOT throw then
   MIR-IS-N @ {: s:n :}
   rows s cells MI-IS-ROWS + !
   cols s cells MI-IS-COLS + !
   dt   s cells MI-IS-DT   + !
   lay  s cells MI-IS-LAY  + !
   AL-UNKNOWN s cells MI-IS-AL + !               \ no recorded pointer yet -> conservative
   s 1+ MIR-IS-N !
   s ;

: MIR-SLOT-ROWS@ ( n -- n )  MIR-IS-CK cells MI-IS-ROWS + @ ;
: MIR-SLOT-COLS@ ( n -- n )  MIR-IS-CK cells MI-IS-COLS + @ ;
: MIR-SLOT-DT@   ( n -- n )  MIR-IS-CK cells MI-IS-DT   + @ ;
: MIR-SLOT-LAY@  ( n -- n )  MIR-IS-CK cells MI-IS-LAY  + @ ;
: MIR-SLOT-AL@   ( n -- n )  MIR-IS-CK cells MI-IS-AL   + @ ;

\ record a measured base alignment class onto an input slot (bound-buffer path)
: MIR-SLOT-AL! ( n n -- ) {: s:n al:n :}         \ slot align --
   al AL-VALID? 0= if E-MIR-ALIGN throw then
   s MIR-IS-CK drop
   al s cells MI-IS-AL + ! ;

\ ---- node builder (BEGIN op ; IN+ ref ... ; OP+ facts -> node) --------------
: MIR-OP-BEGIN ( n -- ) {: op:n :}
   MIR-PEND-ON @ if E-MIR-STATE throw then
   op 0 < op OP-N >= or if E-MIR-OPKIND throw then
   op MIR-PEND-KIND !  MIR-INS-U @ MIR-PEND-OFF !  0 MIR-PEND-CNT !  1 MIR-PEND-ON ! ;

: MIR-IN+ ( n -- ) {: ref:n :}
   MIR-PEND-ON @ 0= if E-MIR-STATE throw then
   MIR-INS-U @ MIR-INCAP >= if E-MIR-INCAP throw then
   ref MIR-REF-CK  MI-INS MIR-INS-U @ cells + !
   MIR-INS-U @ 1+ MIR-INS-U !
   MIR-PEND-CNT @ 1+ MIR-PEND-CNT ! ;

: MIR-OP+ ( n n n n n n -- n )          \ rows cols dtype layout attr mat -- node
   {: rows:n cols:n dt:n lay:n attr:n mat:n :}
   MIR-PEND-ON @ 0= if E-MIR-STATE throw then
   dt MIR-DT-CK drop  lay MIR-LAY-CK drop
   MIR-N @ MIR-CAP >= if E-MIR-CAP throw then
   MIR-N @ {: k:n :}
   MIR-PEND-KIND @ k cells MI-OP    + !
   MIR-PEND-OFF  @ k cells MI-INOFF + !
   MIR-PEND-CNT  @ k cells MI-INCNT + !
   rows k cells MI-ROWS + !
   cols k cells MI-COLS + !
   dt   k cells MI-DT   + !
   lay  k cells MI-LAY  + !
   attr k cells MI-ATTR + !
   mat  k cells MI-MAT  + !
   0    k cells MI-AD   + !
   k 1+ MIR-N !
   0 MIR-PEND-ON !
   k ;

\ ---- node accessors (each validates the node index) ------------------------
: MIR-OP@   ( n -- n )     MIR-CK cells MI-OP    + @ ;
: MIR-ROWS@ ( n -- n )     MIR-CK cells MI-ROWS  + @ ;
: MIR-COLS@ ( n -- n )     MIR-CK cells MI-COLS  + @ ;
: MIR-DT@   ( n -- n )     MIR-CK cells MI-DT    + @ ;
: MIR-LAY@  ( n -- n )     MIR-CK cells MI-LAY   + @ ;
: MIR-ATTR@ ( n -- n )     MIR-CK cells MI-ATTR  + @ ;
: MIR-AD@   ( n -- n )     MIR-CK cells MI-AD    + @ ;
: MIR-MAT@  ( n -- bool )  MIR-CK cells MI-MAT   + @ 0= 0= ;

: MIR-MAT! ( bool n -- )   MIR-CK cells MI-MAT + ! ;

: MIR-IN-COUNT@ ( n -- n )  MIR-CK cells MI-INCNT + @ ;

: MIR-IN@ ( n n -- n ) {: node:n k:n :}     \ k-th operand ref of node
   node MIR-CK drop
   k 0 < k node cells MI-INCNT + @ >= or if E-MIR-REF throw then
   MI-INS  node cells MI-INOFF + @  k +  cells + @ ;

: MIR-MAT-COUNT ( -- n )                \ materialized node count (LOWER uses this)
   0 MIR-N @ 0 ?do  i MIR-MAT@ if 1+ then  loop ;

\ ---- movement facts (attrs interpreted per maki/move-facts.f) ---------------
: MIR-MOVE? ( n -- bool )  MIR-OP@ OPR-CLASS CLASS-MOVEMENT = ;

\ the node's dissolution verdict; fail closed on a non-movement node
: MIR-MOVE-VERDICT@ ( n -- n ) {: node:n :}
   node MIR-MOVE? 0= if E-MV-NOTMOVE throw then
   node MIR-ATTR@ MV-VD@ ;

private

\ ---- dtype / layout key text (fail closed) ---------------------------------
: DT-KEY ( n -- ptr u8 n )
   case
      DT-F32  of s" f32"  endof
      DT-F16  of s" f16"  endof
      DT-BF16 of s" bf16" endof
      DT-U32  of s" u32"  endof
      DT-I32  of s" i32"  endof
      E-MK-DTYPE throw
   endcase ;

: LAY-KEY ( n -- ptr u8 n )
   case
      LAY-ROW of s" row" endof
      LAY-COL of s" col" endof
      E-TV-LAYOUT throw
   endcase ;

\ one dim: an unbound extent (0) renders "?" (shapes bind at OPTIMIZE time)
: DIM-KEY+ ( n -- )  dup 0= if drop s" ?" SB-APPEND else SB-INT then ;

: SHAPE-KEY$ ( n n -- ptr u8 n ) {: rows:n cols:n :}   \ rows cols -> "RxC"
   SB-RESET  rows DIM-KEY+  $78 SB-APPEND-C  cols DIM-KEY+  SB$ ;

public

: MIR-SHAPE-KEY  ( n -- ptr u8 n ) {: node:n :}  node MIR-ROWS@ node MIR-COLS@ SHAPE-KEY$ ;
: MIR-DTYPE-KEY  ( n -- ptr u8 n )  MIR-DT@  DT-KEY ;
: MIR-LAYOUT-KEY ( n -- ptr u8 n )  MIR-LAY@ LAY-KEY ;

: MIR-SLOT-SHAPE-KEY ( n -- ptr u8 n ) {: s:n :}  s MIR-SLOT-ROWS@ s MIR-SLOT-COLS@ SHAPE-KEY$ ;

private

\ ---- line-oriented render buffer (report.f machine-render discipline) -------
$4000 constant MO-CAP
create MO-BUF MO-CAP allot
variable MO-U

: MO-RESET ( -- )  0 MO-U ! ;
: MO-C ( n -- ) {: c:n :}
   MO-U @ MO-CAP >= if E-MIR-CAP throw then
   c MO-BUF MO-U @ + c!  MO-U @ 1+ MO-U ! ;
: MO+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while  dup a + c@ MO-C  1+  repeat drop ;
: MO-INT ( n -- )  SB-RESET SB-INT SB$ MO+ ;
: MO-NL ( -- )  $0A MO-C ;
: MO$ ( -- ptr u8 n )  MO-BUF MO-U @ ;

: MO-KEY.IDX ( ptr u8 n n -- )          \ "prefix.<idx>" (no trailing sep)
   {: idx:n :}  MO+ $2E MO-C idx MO-INT ;

\ one operand ref: node ref -> "n<idx>", input ref -> "i<slot>"
: MO-REF ( n -- ) {: ref:n :}
   ref MIR-REF-INPUT? if  s" i" MO+ ref MIR-REF-SLOT MO-INT
   else                   s" n" MO+ ref MIR-REF-NODE MO-INT  then ;

: R-INPUT ( n -- ) {: s:n :}
   s" input" s MO-KEY.IDX  s" .shape: "  MO+ s MIR-SLOT-SHAPE-KEY MO+ MO-NL
   s" input" s MO-KEY.IDX  s" .dtype: "  MO+ s MIR-SLOT-DT@ DT-KEY MO+ MO-NL
   s" input" s MO-KEY.IDX  s" .layout: " MO+ s MIR-SLOT-LAY@ LAY-KEY MO+ MO-NL ;

: R-NODE-INS ( n -- ) {: node:n :}
   s" node" node MO-KEY.IDX  s" .in:" MO+
   node MIR-IN-COUNT@ 0 ?do  $20 MO-C  node i MIR-IN@ MO-REF  loop  MO-NL ;

\ movement nodes also render their dissolution verdict (free/staged/...)
: R-NODE-MOVE ( n -- ) {: node:n :}
   node MIR-MOVE? 0= if exit then
   s" node" node MO-KEY.IDX  s" .verdict: " MO+
   node MIR-MOVE-VERDICT@ MV-VD-NAME MO+ MO-NL ;

: R-NODE ( n -- ) {: node:n :}
   s" node" node MO-KEY.IDX  s" .op: "     MO+ node MIR-OP@ OPR-NAME MO+ MO-NL
   s" node" node MO-KEY.IDX  s" .shape: "  MO+ node MIR-SHAPE-KEY  MO+ MO-NL
   s" node" node MO-KEY.IDX  s" .dtype: "  MO+ node MIR-DTYPE-KEY  MO+ MO-NL
   s" node" node MO-KEY.IDX  s" .layout: " MO+ node MIR-LAYOUT-KEY MO+ MO-NL
   s" node" node MO-KEY.IDX  s" .mat: "    MO+ node MIR-MAT@ if 1 else 0 then MO-INT MO-NL
   node R-NODE-MOVE
   node R-NODE-INS ;

public

\ machine render: "key: value" lines an agent parses by splitting on ": ".
: MIR-RENDER ( -- ptr u8 n )
   MO-RESET
   s" ir.nodes: "  MO+ MIR-N @    MO-INT MO-NL
   s" ir.inputs: " MO+ MIR-IS-N @ MO-INT MO-NL
   MIR-IS-N @ 0 ?do  i R-INPUT  loop
   MIR-N @ 0 ?do  i R-NODE  loop
   MO$ ;

end-package
