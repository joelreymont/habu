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
\ alignment class, and every capacity are named throws. Public graph handles are
\ nominal (Model-CAD V2 R3): CAD-KIND:node-id, MIR:input-slot, MIR:operand-ref,
\ MIR:input-index, and MIR:mark. Raw table positions remain behind private
\ validated refinements. maki -> habu only; owns -5055..-5063.
\
\ The IR also carries the MODEL NAME (MIR-NAME!/MIR-NAME$), so downstream consumers
\ below the cad.f REPL layer (the golden reference-artifact store) can key a file by
\ model without a load-order cycle back into cad.f. OPTIMIZE-time shape binding
\ (maki/cad.f BIND-SHAPES) rewrites slot extents (MIR-SLOT-SHAPE!) then re-propagates
\ node extents (MIR-SHAPE!) and movement verdicts (MIR-ATTR!) in place.
\
\ The IR also carries its PROVENANCE (the `prov` family): which producer populated
\ the table - MODEL: capture (maki/cad.f CAP-FINISH) or the ONNX importer
\ (maki/onnx/import.f IMPORT). Provenance lives HERE, in the shared substrate both
\ producers already require, so the consumer gates (maki/cad.f) never depend on the
\ importer and the importer never depends on the command surface. MIR-RESET clears
\ it to none atomically with the table, so a producer that throws mid-build leaves
\ NO adopted model and a stale producer's arming is unreadable (the old cad.f
\ MODEL-SET? cell survived an ONNX import - the fail-open this closes).

require maki/cad-kinds.f
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
-5061 constant E-MIR-STATE    \ node builder used out of order / bad release mark
\ -5062 (E-MIR-ALIGN) retired: the align family makes an out-of-range class a
\ checker reject; the code stays reserved to model-ir.
-5063 constant E-MIR-NAME     \ model name longer than the name buffer

package MIR
public

TYPEFAMILY input-slot 0
TYPEFAMILY operand-ref 0
TYPEFAMILY input-index 0
TYPEFAMILY ref-pos 0

PRODUCT mark 0
   FIELD nodes n
   FIELD slots n
   FIELD refs n
;PRODUCT

end-package

package MAKI
public

\ ---- model provenance (which producer populated the IR) ---------------------
\ none: no adopted model; captured: MODEL: capture; imported: ONNX importer.
\ DERIVE eq generates the typed identity compare MAKI-PROV:EQ ( prov prov -- bool ).
ENUM prov DERIVE eq
  none captured imported
;ENUM

\ named render boundary (the DT-KEY / LAY-KEY convention): exhaustive MATCH, so a
\ bad tag is unrepresentable and no throw arm exists.
: PROV-KEY ( prov -- ptr u8 n )
   MATCH prov
      none     OF s" none"     ENDOF
      captured OF s" captured" ENDOF
      imported OF s" imported" ENDOF
   ;MATCH ;

private

128 constant MIR-CAP          \ max nodes
384 constant MIR-INCAP        \ max operand refs across the table
64  constant MIR-IN-CAP       \ max model-input slots

\ node table: one array per field (keeps each field's cell independent). The
\ enum-typed columns are generative LAYOUT-BUFFER columns (dot habu-cad-adt-swap;
\ the only typed-layout pointer introduction form): each owns extent, stride,
\ bounds, and family provenance and publishes its own checked accessor, so a raw
\ n or a foreign family can never enter or leave a descriptor cell.
MIR-CAP LAYOUT-BUFFER MI-OP-AT opkind   \ op-kind column ( n -- ptr opkind )
create MI-INOFF MIR-CAP cells allot     \ operand window start in MI-INS (MIR:ref-pos)
create MI-INCNT MIR-CAP cells allot     \ operand window length
create MI-ROWS  MIR-CAP cells allot     \ output descriptor facts (CAD-KIND:rows/cols)
create MI-COLS  MIR-CAP cells allot
MIR-CAP LAYOUT-BUFFER MI-DT-AT dtype    \ dtype column ( n -- ptr dtype )
MIR-CAP LAYOUT-BUFFER MI-LAY-AT layout  \ layout column ( n -- ptr layout )
create MI-ATTR  MIR-CAP cells allot     \ attrs cell (variant/axis/eps, op-typed)
create MI-MAT   MIR-CAP cells allot     \ materialization flag
create MI-AD    MIR-CAP cells allot     \ autograd metadata (reserved)
variable MIR-N

create MI-INS   MIR-INCAP cells allot   \ flat operand-ref pool (MIR:operand-ref)
variable MIR-INS-U

\ model-input slots (their own descriptor facts)
create MI-IS-ROWS MIR-IN-CAP cells allot
create MI-IS-COLS MIR-IN-CAP cells allot
MIR-IN-CAP LAYOUT-BUFFER MI-IS-DT-AT  dtype    \ dtype column ( n -- ptr dtype )
MIR-IN-CAP LAYOUT-BUFFER MI-IS-LAY-AT layout   \ layout column ( n -- ptr layout )
MIR-IN-CAP LAYOUT-BUFFER MI-IS-AL-AT  align    \ align column (zero image = unknown)
variable MIR-IS-N

\ model name (denormalized into the IR so the golden artifact store can key a file
\ by model below the cad.f layer without a load-order cycle). Reset with the table.
64  constant MIR-NAME-CAP
create MI-NAME MIR-NAME-CAP allot   variable MI-NAME-U

\ provenance cell: a family value behind a one-slot generative buffer (a prov
\ cannot bind into a local); reset to none with the table, set by the producers.
1 LAYOUT-BUFFER MIR-PROV-V prov
: MIR-PROV-AT ( -- ptr prov )  0 MIR-PROV-V ;

\ pending-node staging (any-arity records with the fixed-arity ref pool)
1 LAYOUT-BUFFER MIR-PEND-KIND opkind
variable MIR-PEND-OFF
variable MIR-PEND-CNT
variable MIR-PEND-ON

\ the pending op-kind is a family value, so it rides through this typed slot (an
\ opkind cannot bind into a local); MIR-OP-BEGIN stores it, MIR-OP+ moves it to
\ the MI-OP-AT column.
: MIR-PEND-KIND-AT ( -- ptr opkind )  0 MIR-PEND-KIND ;

\ ---- nominal graph handles (Model-CAD V2 R3) --------------------------------
\ These private identity boundaries are the only raw representation authority;
\ each public constructor validates the raw table position first.
TRUSTED: RAW>NODE ( n -- CAD-KIND:node-id ) ;
TRUSTED: NODE>RAW ( CAD-KIND:node-id -- n ) ;
TRUSTED: RAW>SLOT ( n -- MIR:input-slot ) ;
TRUSTED: SLOT>RAW ( MIR:input-slot -- n ) ;
TRUSTED: RAW>REF ( n -- MIR:operand-ref ) ;
TRUSTED: REF>RAW ( MIR:operand-ref -- n ) ;
TRUSTED: RAW>INPUT-INDEX ( n -- MIR:input-index ) ;
TRUSTED: INPUT-INDEX>RAW ( MIR:input-index -- n ) ;
TRUSTED: RAW>REF-POS ( n -- MIR:ref-pos ) ;
TRUSTED: REF-POS>RAW ( MIR:ref-pos -- n ) ;

: NODE-RAW-CK ( n -- n )
   dup 0 < over MIR-N @ >= or if E-MIR-IDX throw then ;

: SLOT-RAW-CK ( n -- n )
   dup 0 < over MIR-IS-N @ >= or if E-MIR-INSLOT throw then ;

: NEXT-NODE ( -- CAD-KIND:node-id )
   MIR-N @ dup 0 < over MIR-CAP >= or if E-MIR-CAP throw then
   RAW>NODE ;

: NEXT-SLOT ( -- MIR:input-slot )
   MIR-IS-N @ dup 0 < over MIR-IN-CAP >= or if E-MIR-INSLOT throw then
   RAW>SLOT ;

: REF-POS-RAW-CK ( n -- n )
   dup 0 < over MIR-INCAP > or if E-MIR-REF throw then ;

: MIR-CK ( CAD-KIND:node-id -- n )
   NODE>RAW NODE-RAW-CK ;

: MIR-IS-CK ( MIR:input-slot -- n )
   SLOT>RAW SLOT-RAW-CK ;

\ an operand ref must name a committed node (>=0, < MIR-N) or a live input slot
: MIR-REF-CK ( MIR:operand-ref -- MIR:operand-ref ) {: ref:MIR:operand-ref :}
   ref REF>RAW {: raw:n :}
   raw 0< if
      raw negate 1- dup 0 < swap MIR-IS-N @ >= or if E-MIR-REF throw then
   else
      raw MIR-N @ >= if E-MIR-REF throw then
   then
   ref ;

: INOFF! ( MIR:ref-pos CAD-KIND:node-id -- ) {: off:MIR:ref-pos node:CAD-KIND:node-id :}
   node NODE>RAW {: raw:n :}
   raw 0 < raw MIR-CAP >= or if E-MIR-IDX throw then
   off raw cells MI-INOFF + ! ;

: INOFF@ ( CAD-KIND:node-id -- MIR:ref-pos )
   MIR-CK cells MI-INOFF + @ ;

: PEND-OFF! ( MIR:ref-pos -- )
   MIR-PEND-OFF ! ;

: PEND-OFF@ ( -- MIR:ref-pos )
   MIR-PEND-OFF @ ;

: REF! ( MIR:operand-ref MIR:ref-pos -- ) {: ref:MIR:operand-ref pos:MIR:ref-pos :}
   pos REF-POS>RAW REF-POS-RAW-CK drop
   ref MI-INS pos REF-POS>RAW cells + ! ;

: REF@ ( MIR:ref-pos -- MIR:operand-ref ) {: pos:MIR:ref-pos :}
   pos REF-POS>RAW REF-POS-RAW-CK drop
   MI-INS pos REF-POS>RAW cells + @ ;

public

\ ---- validated raw-index refinements (loop counters enter here) -------------
: MIR-NODE-ID ( n -- CAD-KIND:node-id )
   NODE-RAW-CK RAW>NODE ;

: MIR-SLOT-ID ( n -- MIR:input-slot )
   SLOT-RAW-CK RAW>SLOT ;

: MIR-INPUT-IDX ( n -- MIR:input-index )
   dup 0 < over MIR-INCAP >= or if E-MIR-REF throw then
   RAW>INPUT-INDEX ;

: MIR-REF-POS ( n -- MIR:ref-pos )
   REF-POS-RAW-CK RAW>REF-POS ;

\ ---- operand ref tagging (node index >= 0 ; input slot = -(slot+1)) ----
: MIR-IN-REF ( MIR:input-slot -- MIR:operand-ref )
   MIR-IS-CK 1+ negate RAW>REF ;

: MIR-NODE-REF ( CAD-KIND:node-id -- MIR:operand-ref )
   MIR-CK RAW>REF ;

: MIR-REF-INPUT? ( MIR:operand-ref -- bool )
   REF>RAW 0< ;

: MIR-REF-SLOT ( MIR:operand-ref -- MIR:input-slot ) {: ref:MIR:operand-ref :}
   ref MIR-REF-INPUT? 0= if E-MIR-REF throw then
   ref REF>RAW negate 1- {: raw:n :}
   raw 0 < raw MIR-IS-N @ >= or if E-MIR-REF throw then
   raw RAW>SLOT ;

: MIR-REF-NODE ( MIR:operand-ref -- CAD-KIND:node-id ) {: ref:MIR:operand-ref :}
   ref MIR-REF-INPUT? if E-MIR-REF throw then
   ref REF>RAW {: raw:n :}
   raw 0 < raw MIR-N @ >= or if E-MIR-REF throw then
   raw RAW>NODE ;

: MIR-NODE= ( CAD-KIND:node-id CAD-KIND:node-id -- bool )
   {: a:CAD-KIND:node-id b:CAD-KIND:node-id :}
   a NODE>RAW b NODE>RAW = ;

: MIR-SLOT= ( MIR:input-slot MIR:input-slot -- bool )
   {: a:MIR:input-slot b:MIR:input-slot :}
   a SLOT>RAW b SLOT>RAW = ;

: MIR-REF= ( MIR:operand-ref MIR:operand-ref -- bool )
   {: a:MIR:operand-ref b:MIR:operand-ref :}
   a REF>RAW b REF>RAW = ;

\ ---- lifecycle -------------------------------------------------------------
: MIR-RESET ( -- )
   0 MIR-N !  0 MIR-INS-U !  0 MIR-IS-N !  0 MIR-PEND-ON !  0 MI-NAME-U !
   MAKI-PROV:NONE MIR-PROV-AT ! ;

: MIR-N@ ( -- n )         MIR-N @ ;
: MIR-IN-SLOTS@ ( -- n )  MIR-IS-N @ ;

\ ---- provenance (a producer adopts the IR it just populated) -----------------
: MIR-PROV@ ( -- prov )   MIR-PROV-AT @ ;
: MIR-PROV! ( prov -- )   MIR-PROV-AT ! ;

\ ---- model name (single source; cad.f MODEL: sets it, golden artifacts read it) ----
: MIR-NAME! ( ptr u8 n -- ) {: a:ptr u:n :}
   u MIR-NAME-CAP > if E-MIR-NAME throw then
   a MI-NAME u BYTE-COPY  u MI-NAME-U ! ;
: MIR-NAME$ ( -- ptr u8 n )  MI-NAME MI-NAME-U @ ;

\ ---- checkpoint / restore (transient IR growth, e.g. the cad-9 backward pass) ----
\ MIR-MARK captures the table high-water marks in a nominal MIR:mark product;
\ MIR-RELEASE validates and truncates back to them and clears any pending
\ builder. A caller that appends nodes/slots and wants the IR restored
\ (gradcheck builds a throwaway backward pass) brackets its work with these.
: MIR-MARK ( -- MIR:mark )
   MIR-N @ MIR-IS-N @ MIR-INS-U @ MIR-MARK:MAKE ;

: MIR-RELEASE ( MIR:mark -- )
   MIR-MARK:UNMAKE {: nn:n sn:n iu:n :}
   nn 0 < sn 0 < or iu 0 < or if E-MIR-STATE throw then
   nn MIR-N @ > sn MIR-IS-N @ > or iu MIR-INS-U @ > or if E-MIR-STATE throw then
   nn MIR-N !
   sn MIR-IS-N !
   iu MIR-INS-U !
   0 MIR-PEND-ON ! ;

\ ---- model-input slots -----------------------------------------------------
\ dtype/layout arrive as family values (a bad tag is a checker reject; the old
\ E-MK-DTYPE/E-TV-LAYOUT range validation is unrepresentable). The descriptor
\ stores run from the stack before the extent locals bind.
: MIR-INPUT+ ( CAD-KIND:rows CAD-KIND:cols dtype layout -- MIR:input-slot )
   MIR-IS-N @ MIR-IN-CAP >= if E-MIR-INSLOT throw then
   NEXT-SLOT {: s:MIR:input-slot :}
   s SLOT>RAW {: raw:n :}
   raw MI-IS-LAY-AT !                            \ layout (top)
   raw MI-IS-DT-AT !                             \ dtype
   MAKI-ALIGN:UNKNOWN raw MI-IS-AL-AT !          \ no recorded pointer yet -> conservative
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   rows raw cells MI-IS-ROWS + !
   cols raw cells MI-IS-COLS + !
   raw 1+ MIR-IS-N !
   s ;

: MIR-SLOT-ROWS@ ( MIR:input-slot -- CAD-KIND:rows )  MIR-IS-CK cells MI-IS-ROWS + @ ;
: MIR-SLOT-COLS@ ( MIR:input-slot -- CAD-KIND:cols )  MIR-IS-CK cells MI-IS-COLS + @ ;
: MIR-SLOT-DT@   ( MIR:input-slot -- dtype )   MIR-IS-CK MI-IS-DT-AT  @ ;
: MIR-SLOT-LAY@  ( MIR:input-slot -- layout )  MIR-IS-CK MI-IS-LAY-AT @ ;
: MIR-SLOT-AL@   ( MIR:input-slot -- align )   MIR-IS-CK MI-IS-AL-AT  @ ;

\ record a measured base alignment class onto an input slot (bound-buffer path);
\ the align arrives as a family value and swaps over the slot so the slot
\ can validate + bind while the align stores from the stack.
: MIR-SLOT-AL! ( MIR:input-slot align -- )
   swap MIR-IS-CK {: raw:n :}
   raw MI-IS-AL-AT ! ;

\ rebind an input slot's extents (OPTIMIZE-time shape binding, maki/cad.f)
: MIR-SLOT-SHAPE! ( CAD-KIND:rows CAD-KIND:cols MIR:input-slot -- )
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols s:MIR:input-slot :}
   s MIR-IS-CK {: raw:n :}
   rows raw cells MI-IS-ROWS + !
   cols raw cells MI-IS-COLS + ! ;

\ ---- node builder (BEGIN op ; IN+ ref ... ; OP+ facts -> node) --------------
\ the op-kind arrives as a family value (a bad tag is a checker reject; the old
\ E-MIR-OPKIND range validation is unrepresentable). It cannot bind into a local,
\ so it stores from the stack into the typed pending slot before any bookkeeping.
: MIR-OP-BEGIN ( opkind -- )
   MIR-PEND-ON @ if E-MIR-STATE throw then
   MIR-PEND-KIND-AT !
   MIR-INS-U @ MIR-REF-POS PEND-OFF!
   0 MIR-PEND-CNT !  1 MIR-PEND-ON ! ;

: MIR-IN+ ( MIR:operand-ref -- ) {: ref:MIR:operand-ref :}
   MIR-PEND-ON @ 0= if E-MIR-STATE throw then
   MIR-INS-U @ MIR-INCAP >= if E-MIR-INCAP throw then
   ref MIR-REF-CK MIR-INS-U @ MIR-REF-POS REF!
   MIR-INS-U @ 1+ MIR-INS-U !
   MIR-PEND-CNT @ 1+ MIR-PEND-CNT ! ;

: MIR-OP+ ( CAD-KIND:rows CAD-KIND:cols dtype layout n n -- CAD-KIND:node-id )
   {: attr:n mat:n :}                    \ rows cols dtype layout attr mat -- node
   MIR-PEND-ON @ 0= if E-MIR-STATE throw then
   MIR-N @ MIR-CAP >= if E-MIR-CAP throw then
   NEXT-NODE {: k:CAD-KIND:node-id :}
   k NODE>RAW {: raw:n :}
   raw MI-LAY-AT !                       \ layout (top after attr/mat bound)
   raw MI-DT-AT !                        \ dtype
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   MIR-PEND-KIND-AT @ raw MI-OP-AT !     \ op-kind family (staged by MIR-OP-BEGIN)
   PEND-OFF@ k INOFF!
   MIR-PEND-CNT @ raw cells MI-INCNT + !
   rows raw cells MI-ROWS + !
   cols raw cells MI-COLS + !
   attr raw cells MI-ATTR + !
   mat  raw cells MI-MAT  + !
   0    raw cells MI-AD   + !
   raw 1+ MIR-N !
   0 MIR-PEND-ON !
   k ;

\ ---- node accessors (each validates the node index) ------------------------
: MIR-OP@   ( CAD-KIND:node-id -- opkind )   MIR-CK MI-OP-AT @ ;
: MIR-ROWS@ ( CAD-KIND:node-id -- CAD-KIND:rows )  MIR-CK cells MI-ROWS + @ ;
: MIR-COLS@ ( CAD-KIND:node-id -- CAD-KIND:cols )  MIR-CK cells MI-COLS + @ ;
: MIR-DT@   ( CAD-KIND:node-id -- dtype )   MIR-CK MI-DT-AT  @ ;
: MIR-LAY@  ( CAD-KIND:node-id -- layout )  MIR-CK MI-LAY-AT @ ;
: MIR-ATTR@ ( CAD-KIND:node-id -- n )     MIR-CK cells MI-ATTR  + @ ;
: MIR-AD@   ( CAD-KIND:node-id -- n )     MIR-CK cells MI-AD    + @ ;
: MIR-MAT@  ( CAD-KIND:node-id -- bool )  MIR-CK cells MI-MAT   + @ 0= 0= ;

: MIR-MAT! ( bool CAD-KIND:node-id -- )   MIR-CK cells MI-MAT + ! ;

\ re-propagated node output extents + rewritten attrs (OPTIMIZE-time re-inference)
: MIR-SHAPE! ( CAD-KIND:rows CAD-KIND:cols CAD-KIND:node-id -- )
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols node:CAD-KIND:node-id :}
   node MIR-CK {: raw:n :}
   rows raw cells MI-ROWS + !
   cols raw cells MI-COLS + ! ;

: MIR-ATTR! ( n CAD-KIND:node-id -- )
   {: attr:n node:CAD-KIND:node-id :}
   attr node MIR-CK cells MI-ATTR + ! ;

: MIR-IN-COUNT@ ( CAD-KIND:node-id -- n )  MIR-CK cells MI-INCNT + @ ;

: MIR-IN@ ( CAD-KIND:node-id MIR:input-index -- MIR:operand-ref )
   {: node:CAD-KIND:node-id k:MIR:input-index :}
   node MIR-CK {: raw:n :}
   k INPUT-INDEX>RAW {: ki:n :}
   ki 0 < ki raw cells MI-INCNT + @ >= or if E-MIR-REF throw then
   node INOFF@ REF-POS>RAW ki + MIR-REF-POS REF@ ;

: MIR-MAT-COUNT ( -- n )                \ materialized node count (LOWER uses this)
   0 MIR-N @ 0 ?do  i MIR-NODE-ID MIR-MAT@ if 1+ then  loop ;

\ ---- movement facts (attrs interpreted per maki/move-facts.f) ---------------
: MIR-MOVE? ( CAD-KIND:node-id -- bool )  MIR-OP@ OPR-CLASS CLASS-MOVEMENT = ;

\ the node's dissolution verdict; fail closed on a non-movement node
: MIR-MOVE-VERDICT@ ( CAD-KIND:node-id -- n ) {: node:CAD-KIND:node-id :}
   node MIR-MOVE? 0= if E-MV-NOTMOVE throw then
   node MIR-ATTR@ MV-VD@ ;

private

\ dtype/layout wire text comes from the family renders DT-KEY / LAY-KEY
\ (tensor.f / tensor-value.f): exhaustive MATCH, so a bad tag is unrepresentable.

\ An unbound extent (0) renders "?" (shapes bind at OPTIMIZE time); integer
\ rendering is a wire boundary.
: ROWS-KEY+ ( CAD-KIND:rows -- )
   ROWS-RAW dup 0= if drop s" ?" SB-APPEND else SB-INT then ;
: COLS-KEY+ ( CAD-KIND:cols -- )
   COLS-RAW dup 0= if drop s" ?" SB-APPEND else SB-INT then ;

: SHAPE-KEY$ ( CAD-KIND:rows CAD-KIND:cols -- ptr u8 n )
   {: rows:CAD-KIND:rows cols:CAD-KIND:cols :}
   SB-RESET rows ROWS-KEY+ $78 SB-APPEND-C cols COLS-KEY+ SB$ ;

public

: MIR-SHAPE-KEY ( CAD-KIND:node-id -- ptr u8 n )
   {: node:CAD-KIND:node-id :}
   node MIR-ROWS@ node MIR-COLS@ SHAPE-KEY$ ;

: MIR-DTYPE-KEY ( CAD-KIND:node-id -- ptr u8 n )  MIR-DT@ DT-KEY ;
: MIR-LAYOUT-KEY ( CAD-KIND:node-id -- ptr u8 n )  MIR-LAY@ LAY-KEY ;

: MIR-SLOT-SHAPE-KEY ( MIR:input-slot -- ptr u8 n )
   {: s:MIR:input-slot :}
   s MIR-SLOT-ROWS@ s MIR-SLOT-COLS@ SHAPE-KEY$ ;

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
: MO-REF ( MIR:operand-ref -- ) {: ref:MIR:operand-ref :}
   ref MIR-REF-INPUT? if
      s" i" MO+ ref MIR-REF-SLOT SLOT>RAW MO-INT
   else
      s" n" MO+ ref MIR-REF-NODE NODE>RAW MO-INT
   then ;

: R-INPUT ( MIR:input-slot -- ) {: s:MIR:input-slot :}
   s" input" s SLOT>RAW MO-KEY.IDX  s" .shape: "  MO+ s MIR-SLOT-SHAPE-KEY MO+ MO-NL
   s" input" s SLOT>RAW MO-KEY.IDX  s" .dtype: "  MO+ s MIR-SLOT-DT@ DT-KEY MO+ MO-NL
   s" input" s SLOT>RAW MO-KEY.IDX  s" .layout: " MO+ s MIR-SLOT-LAY@ LAY-KEY MO+ MO-NL ;

: R-NODE-INS ( CAD-KIND:node-id -- ) {: node:CAD-KIND:node-id :}
   s" node" node NODE>RAW MO-KEY.IDX  s" .in:" MO+
   node MIR-IN-COUNT@ 0 ?do
      $20 MO-C
      node i MIR-INPUT-IDX MIR-IN@ MO-REF
   loop
   MO-NL ;

\ movement nodes also render their dissolution verdict (free/staged/...)
: R-NODE-MOVE ( CAD-KIND:node-id -- ) {: node:CAD-KIND:node-id :}
   node MIR-MOVE? 0= if exit then
   s" node" node NODE>RAW MO-KEY.IDX  s" .verdict: " MO+
   node MIR-MOVE-VERDICT@ MV-VD-NAME MO+ MO-NL ;

: R-NODE ( CAD-KIND:node-id -- ) {: node:CAD-KIND:node-id :}
   s" node" node NODE>RAW MO-KEY.IDX  s" .op: "     MO+ node MIR-OP@ OPR-NAME MO+ MO-NL
   s" node" node NODE>RAW MO-KEY.IDX  s" .shape: "  MO+ node MIR-SHAPE-KEY  MO+ MO-NL
   s" node" node NODE>RAW MO-KEY.IDX  s" .dtype: "  MO+ node MIR-DTYPE-KEY  MO+ MO-NL
   s" node" node NODE>RAW MO-KEY.IDX  s" .layout: " MO+ node MIR-LAYOUT-KEY MO+ MO-NL
   s" node" node NODE>RAW MO-KEY.IDX  s" .mat: "    MO+ node MIR-MAT@ if 1 else 0 then MO-INT MO-NL
   node R-NODE-MOVE
   node R-NODE-INS ;

public

\ machine render: "key: value" lines an agent parses by splitting on ": ".
: MIR-RENDER ( -- ptr u8 n )
   MO-RESET
   s" ir.nodes: "  MO+ MIR-N @    MO-INT MO-NL
   s" ir.inputs: " MO+ MIR-IS-N @ MO-INT MO-NL
   MIR-IS-N @ 0 ?do  i MIR-SLOT-ID R-INPUT  loop
   MIR-N @ 0 ?do  i MIR-NODE-ID R-NODE  loop
   MO$ ;

end-package
