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
\ owns -5055..-5063.
\
\ The IR also carries the MODEL NAME (MIR-NAME!/MIR-NAME$), so downstream consumers
\ below the cad.f REPL layer (the golden reference-artifact store) can key a file by
\ model without a load-order cycle back into cad.f. OPTIMIZE-time shape binding
\ (maki/cad.f BIND-SHAPES) rewrites slot extents (MIR-SLOT-SHAPE!) then re-propagates
\ node extents (MIR-SHAPE!) and movement verdicts (MIR-ATTR!) in place.

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
\ -5057 (E-MIR-OPKIND) retired: MIR-OP-BEGIN takes an `opkind` family, so an
\ out-of-range op-kind is a checker reject; the code stays reserved to model-ir.
-5058 constant E-MIR-INCAP    \ operand ref pool capacity exceeded
-5059 constant E-MIR-REF      \ operand ref names an uncommitted node / bad input slot
-5060 constant E-MIR-INSLOT   \ input-slot index / capacity out of range
-5061 constant E-MIR-STATE    \ node builder used out of order
\ -5062 (E-MIR-ALIGN) retired: the align family makes an out-of-range class a
\ checker reject; the code stays reserved to model-ir.
-5063 constant E-MIR-NAME     \ model name longer than the name buffer

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
create MI-OP    MIR-CAP cells allot     \ op-kind (family value; typed slot MI-OP-AT)
create MI-INOFF MIR-CAP cells allot     \ operand window start in MI-INS
create MI-INCNT MIR-CAP cells allot     \ operand window length
create MI-ROWS  MIR-CAP cells allot     \ output descriptor facts
create MI-COLS  MIR-CAP cells allot
create MI-DT    MIR-CAP cells allot     \ dtype (family value; typed slot MI-DT-AT)
create MI-LAY   MIR-CAP cells allot     \ layout (family value; typed slot MI-LAY-AT)
create MI-ATTR  MIR-CAP cells allot     \ attrs cell (variant/axis/eps, op-typed)
create MI-MAT   MIR-CAP cells allot     \ materialization flag
create MI-AD    MIR-CAP cells allot     \ autograd metadata (reserved)
variable MIR-N

create MI-INS   MIR-INCAP cells allot   \ flat operand-ref pool
variable MIR-INS-U

\ model-input slots (their own descriptor facts)
create MI-IS-ROWS MIR-IN-CAP cells allot
create MI-IS-COLS MIR-IN-CAP cells allot
create MI-IS-DT   MIR-IN-CAP cells allot     \ dtype (family value)
create MI-IS-LAY  MIR-IN-CAP cells allot     \ layout (family value)
create MI-IS-AL   MIR-IN-CAP cells allot     \ align (family value; unknown default)
variable MIR-IS-N

\ typed slot addresses (dot habu-cad-adt-swap): the descriptor columns are
\ reachable only through these, so a raw n or a foreign family can never enter
\ or leave a descriptor cell.
: MI-OP-AT     ( n -- ptr opkind )  cells MI-OP     + ;
: MI-DT-AT     ( n -- ptr dtype )   cells MI-DT     + ;
: MI-LAY-AT    ( n -- ptr layout )  cells MI-LAY    + ;
: MI-IS-DT-AT  ( n -- ptr dtype )   cells MI-IS-DT  + ;
: MI-IS-LAY-AT ( n -- ptr layout )  cells MI-IS-LAY + ;
: MI-IS-AL-AT  ( n -- ptr align )   cells MI-IS-AL  + ;

\ model name (denormalized into the IR so the golden artifact store can key a file
\ by model below the cad.f layer without a load-order cycle). Reset with the table.
64  constant MIR-NAME-CAP
create MI-NAME MIR-NAME-CAP allot   variable MI-NAME-U

\ pending-node staging (any-arity records with the fixed-arity ref pool)
variable MIR-PEND-KIND
variable MIR-PEND-OFF
variable MIR-PEND-CNT
variable MIR-PEND-ON

\ the pending op-kind is a family value, so it rides through this typed slot (an
\ opkind cannot bind into a local); MIR-OP-BEGIN stores it, MIR-OP+ moves it to MI-OP.
: MIR-PEND-KIND-AT ( -- ptr opkind )  MIR-PEND-KIND ;

: MIR-CK ( n -- n )                     \ validate a committed node index
   dup 0 < over MIR-N @ >= or if E-MIR-IDX throw then ;

: MIR-IS-CK ( n -- n )                  \ validate a model-input slot index
   dup 0 < over MIR-IS-N @ >= or if E-MIR-INSLOT throw then ;

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
   0 MIR-N !  0 MIR-INS-U !  0 MIR-IS-N !  0 MIR-PEND-ON !  0 MI-NAME-U ! ;

: MIR-N@ ( -- n )         MIR-N @ ;
: MIR-IN-SLOTS@ ( -- n )  MIR-IS-N @ ;

\ ---- model name (single source; cad.f MODEL: sets it, golden artifacts read it) ----
: MIR-NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u MIR-NAME-CAP > if E-MIR-NAME throw then
   a MI-NAME u BYTE-COPY  u MI-NAME-U ! ;
: MIR-NAME$ ( -- ptr u8 n )  MI-NAME MI-NAME-U @ ;

\ ---- checkpoint / restore (transient IR growth, e.g. the cad-9 backward pass) ----
\ MIR-MARK captures the table high-water marks; MIR-RELEASE truncates back to them
\ and clears any pending builder. A caller that appends nodes/slots and wants the IR
\ restored (gradcheck builds a throwaway backward pass) brackets its work with these.
: MIR-MARK    ( -- n n n )    MIR-N @  MIR-IS-N @  MIR-INS-U @ ;
: MIR-RELEASE ( n n n -- ) {: nn:n sn:n iu:n :}
   nn MIR-N !  sn MIR-IS-N !  iu MIR-INS-U !  0 MIR-PEND-ON ! ;

\ ---- model-input slots -----------------------------------------------------
\ dtype/layout arrive as family values (a bad tag is a checker reject; the old
\ E-MK-DTYPE/E-TV-LAYOUT range validation is unrepresentable). Families cannot
\ bind into locals, so the descriptor stores run from the stack before the
\ extent locals bind.
: MIR-INPUT+ ( n n dtype layout -- n )           \ rows cols dtype layout -- slot
   MIR-IS-N @ MIR-IN-CAP >= if E-MIR-INSLOT throw then
   MIR-IS-N @ {: s:n :}
   s MI-IS-LAY-AT !                              \ layout (top)
   s MI-IS-DT-AT !                               \ dtype
   MAKI-ALIGN:UNKNOWN s MI-IS-AL-AT !            \ no recorded pointer yet -> conservative
   {: rows:n cols:n :}
   rows s cells MI-IS-ROWS + !
   cols s cells MI-IS-COLS + !
   s 1+ MIR-IS-N !
   s ;

: MIR-SLOT-ROWS@ ( n -- n )       MIR-IS-CK cells MI-IS-ROWS + @ ;
: MIR-SLOT-COLS@ ( n -- n )       MIR-IS-CK cells MI-IS-COLS + @ ;
: MIR-SLOT-DT@   ( n -- dtype )   MIR-IS-CK MI-IS-DT-AT  @ ;
: MIR-SLOT-LAY@  ( n -- layout )  MIR-IS-CK MI-IS-LAY-AT @ ;
: MIR-SLOT-AL@   ( n -- align )   MIR-IS-CK MI-IS-AL-AT  @ ;

\ record a measured base alignment class onto an input slot (bound-buffer path);
\ the align arrives as a family value and swaps over the slot index so the index
\ can validate + bind while the align stores from the stack.
: MIR-SLOT-AL! ( n align -- )                    \ slot align --
   swap MIR-IS-CK {: s:n :}
   s MI-IS-AL-AT ! ;

\ rebind an input slot's extents (OPTIMIZE-time shape binding, maki/cad.f)
: MIR-SLOT-SHAPE! ( n n n -- ) {: rows:n cols:n s:n :}   \ rows cols slot --
   s MIR-IS-CK drop
   rows s cells MI-IS-ROWS + !
   cols s cells MI-IS-COLS + ! ;

\ ---- node builder (BEGIN op ; IN+ ref ... ; OP+ facts -> node) --------------
\ the op-kind arrives as a family value (a bad tag is a checker reject; the old
\ E-MIR-OPKIND range validation is unrepresentable). It cannot bind into a local,
\ so it stores from the stack into the typed pending slot before any bookkeeping.
: MIR-OP-BEGIN ( opkind -- )
   MIR-PEND-ON @ if E-MIR-STATE throw then
   MIR-PEND-KIND-AT !
   MIR-INS-U @ MIR-PEND-OFF !  0 MIR-PEND-CNT !  1 MIR-PEND-ON ! ;

: MIR-IN+ ( n -- ) {: ref:n :}
   MIR-PEND-ON @ 0= if E-MIR-STATE throw then
   MIR-INS-U @ MIR-INCAP >= if E-MIR-INCAP throw then
   ref MIR-REF-CK  MI-INS MIR-INS-U @ cells + !
   MIR-INS-U @ 1+ MIR-INS-U !
   MIR-PEND-CNT @ 1+ MIR-PEND-CNT ! ;

: MIR-OP+ ( n n dtype layout n n -- n ) \ rows cols dtype layout attr mat -- node
   {: attr:n mat:n :}
   MIR-PEND-ON @ 0= if E-MIR-STATE throw then
   MIR-N @ MIR-CAP >= if E-MIR-CAP throw then
   MIR-N @ {: k:n :}
   k MI-LAY-AT !                         \ layout (top after attr/mat bound)
   k MI-DT-AT !                          \ dtype
   {: rows:n cols:n :}
   MIR-PEND-KIND-AT @ k MI-OP-AT !       \ op-kind family (staged by MIR-OP-BEGIN)
   MIR-PEND-OFF  @ k cells MI-INOFF + !
   MIR-PEND-CNT  @ k cells MI-INCNT + !
   rows k cells MI-ROWS + !
   cols k cells MI-COLS + !
   attr k cells MI-ATTR + !
   mat  k cells MI-MAT  + !
   0    k cells MI-AD   + !
   k 1+ MIR-N !
   0 MIR-PEND-ON !
   k ;

\ ---- node accessors (each validates the node index) ------------------------
: MIR-OP@   ( n -- opkind )   MIR-CK MI-OP-AT @ ;
: MIR-ROWS@ ( n -- n )       MIR-CK cells MI-ROWS  + @ ;
: MIR-COLS@ ( n -- n )       MIR-CK cells MI-COLS  + @ ;
: MIR-DT@   ( n -- dtype )   MIR-CK MI-DT-AT  @ ;
: MIR-LAY@  ( n -- layout )  MIR-CK MI-LAY-AT @ ;
: MIR-ATTR@ ( n -- n )     MIR-CK cells MI-ATTR  + @ ;
: MIR-AD@   ( n -- n )     MIR-CK cells MI-AD    + @ ;
: MIR-MAT@  ( n -- bool )  MIR-CK cells MI-MAT   + @ 0= 0= ;

: MIR-MAT! ( bool n -- )   MIR-CK cells MI-MAT + ! ;

\ re-propagated node output extents + rewritten attrs (OPTIMIZE-time re-inference)
: MIR-SHAPE! ( n n n -- ) {: rows:n cols:n node:n :}     \ rows cols node --
   node MIR-CK drop
   rows node cells MI-ROWS + !
   cols node cells MI-COLS + ! ;
: MIR-ATTR! ( n n -- ) {: attr:n node:n :}               \ attr node -- (movement re-verdict)
   node MIR-CK drop
   attr node cells MI-ATTR + ! ;

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

\ dtype/layout wire text comes from the family renders DT-KEY / LAY-KEY
\ (tensor.f / tensor-value.f): exhaustive MATCH, so a bad tag is unrepresentable.

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
