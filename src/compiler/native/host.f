\ host.f - implementation-owned facts for native construction calls.
\ A dictionary occurrence is the selection key; a published implementation row
\ supplies its exact code, execution contract and direct implementation edges.
\ Rows retired by code reclamation never become live again when bytes are reused.

require src/lib/prelude.f
require src/compiler/target/model.f
require src/compiler/native/fetch-check.f
require src/habu/xref.f
require src/habu/native-observer-cells.f
require src/habu/native-host-cells.f
require src/core/source-producer.f

package NHOST
public

-9440 constant E-FIRST
-9449 constant E-LAST
-9440 constant E-UNSAFE
-9441 constant E-STATE
-9442 constant E-ROW

\ A definition's host reason names what in its body the published facts
\ (contract, call sites, address sites) do not account for. SAFE: nothing.
\ MEMORY: memory operations and address-forming words (operand addresses,
\ views, string literals), admitted by provenance. FIXED: a fixed record
\ mention: a constant value compiled as an immediate, or a data address the
\ address sites carry. CAST, INDIRECT, UNCHECKED and FOREIGN: an
\ effect the facts cannot name. UNKNOWN: an op, expansion or literal the
\ classification does not model, or no facts at all. SAFE, MEMORY and FIXED
\ are admitted. Objects publish these values in section 15: append, never
\ renumber.
0 constant SAFE
1 constant CAST
2 constant MEMORY
3 constant INDIRECT
4 constant UNCHECKED
5 constant FOREIGN
6 constant UNKNOWN
7 constant FIXED

private

0 CWORK-VARIABLE IMPL-N n
DYNAMIC-BUFFER IMPL-ENTRY n
DYNAMIC-BUFFER IMPL-LEN n
DYNAMIC-BUFFER IMPL-CONTRACT CTARGET:contract
DYNAMIC-BUFFER IMPL-IN n
DYNAMIC-BUFFER IMPL-OUT n
DYNAMIC-BUFFER IMPL-TYPE-FIRST n
DYNAMIC-BUFFER IMPL-TYPES n
0 CWORK-VARIABLE IMPL-TYPE-N n
DYNAMIC-BUFFER IMPL-REASON n
DYNAMIC-BUFFER IMPL-LOC n
DYNAMIC-BUFFER IMPL-FIRST n
DYNAMIC-BUFFER IMPL-EDGES n
DYNAMIC-BUFFER IMPL-LIVE n

0 CWORK-VARIABLE EDGE-N n
DYNAMIC-BUFFER EDGE-TO n
DYNAMIC-BUFFER EDGE-SITE n
DYNAMIC-BUFFER EDGE-KIND n
DYNAMIC-BUFFER EDGE-LOC n

0 CWORK-VARIABLE ASSOC-N n
DYNAMIC-BUFFER ASSOC-ENTRY n
DYNAMIC-BUFFER ASSOC-LEN n
DYNAMIC-BUFFER ASSOC-IMPL n
DYNAMIC-BUFFER ASSOC-LIVE n

UNKNOWN CWORK-VARIABLE SOURCE-REASON n
0 CWORK-VARIABLE SOURCE-LOC n
0 CWORK-VARIABLE SOURCE-FUNS n
DYNAMIC-BUFFER SOURCE-IN n
DYNAMIC-BUFFER SOURCE-OUT n
DYNAMIC-BUFFER SOURCE-TYPE-FIRST n
DYNAMIC-BUFFER SOURCE-TYPE-READY n
DYNAMIC-BUFFER SOURCE-TYPES n
0 CWORK-VARIABLE SOURCE-TYPE-N n

\ Emission reads the producer's completed source facts after HIR construction.
0 CWORK-VARIABLE FACT-REASON n
0 CWORK-VARIABLE FACT-LOC n
0 CWORK-VARIABLE FACT-FUNS n
DYNAMIC-BUFFER FACT-IN n
DYNAMIC-BUFFER FACT-OUT n
DYNAMIC-BUFFER FACT-TYPE-FIRST n
DYNAMIC-BUFFER FACT-TYPE-READY n
DYNAMIC-BUFFER FACT-TYPES n

variable OWNER-INSTALLED
variable PRIOR-INVALIDATE
variable PRIOR-FACTS
variable KERNEL-BEGIN
variable KERNEL-END
variable SEEDED-N
CAST: AS-INVALIDATE ( n -- [ n n -- ] )
CAST: AS-FACTS ( n -- [ n ptr n n n -- n bool ] )
1 constant NUMBER-CON      \ checker.f CC-N, encoded by PRIM-SPEC:A-NUM

: ROW ( n -- n )
   {: id:n :}
   id 1 < id IMPL-N @ > or if E-ROW throw then
   id 1- ;

: RESERVE-IMPL ( n -- )
   {: count:n :}
   count IMPL-ENTRY-RESERVE
   count IMPL-LEN-RESERVE
   count IMPL-CONTRACT-RESERVE
   count IMPL-IN-RESERVE
   count IMPL-OUT-RESERVE
   count IMPL-TYPE-FIRST-RESERVE
   count IMPL-REASON-RESERVE
   count IMPL-LOC-RESERVE
   count IMPL-FIRST-RESERVE
   count IMPL-EDGES-RESERVE
   count IMPL-LIVE-RESERVE ;

: RESERVE-EDGE ( n -- )
   {: count:n :}
   count EDGE-TO-RESERVE
   count EDGE-SITE-RESERVE
   count EDGE-KIND-RESERVE
   count EDGE-LOC-RESERVE ;

: REASON$ ( n -- ptr u8 n )
   {: reason:n :}
   reason CAST = if s" source cast" exit then
   reason MEMORY = if s" source address or memory operation" exit then
   reason INDIRECT = if s" indirect invocation" exit then
   reason UNCHECKED = if s" unchecked body" exit then
   reason FOREIGN = if s" foreign call" exit then
   reason FIXED = if s" fixed record mention" exit then
   s" missing implementation facts" ;

: REPORT-ENTRY ( n -- )
   ROW IMPL-ENTRY @ s"  implementation " type . ;

: REPORT-REASON ( n n -- )
   {: reason:n loc:n :}
   s" host construction: " type reason REASON$ type
   s"  body offset " type loc . cr ;

: REFUSE ( n n n -- )
   {: id:n reason:n loc:n :}
   reason loc REPORT-REASON
   id 0<> if id REPORT-ENTRY cr then
   E-UNSAFE throw ;

public

\ A zero id means no producer ever published facts for this exact entry.
: ID-OF ( n -- n )
   {: entry:n :}
   IMPL-N @ {: count:n :}
   count 0= if 0 exit then
   0 IMPL-LIVE {: live:ptr :}
   0 IMPL-ENTRY {: entries:ptr :}
   count
   begin dup 0 > while
      dup 1- {: row:n :}
      live row cells + @ 0<> entries row cells + @ entry = and if exit then
      1-
   repeat ;

private

: MATCH-ROW? ( n ptr n n n -- bool )
   {: row:n types:ptr din:n dout:n :}
   row IMPL-TYPE-FIRST @ 0< if false exit then
   row IMPL-IN @ din <> row IMPL-OUT @ dout <> or if false exit then
   din dout + 0 ?do
      row IMPL-TYPE-FIRST @ i + IMPL-TYPES @
      types i cells + @ <> if false unloop exit then
   loop
   true ;

\ The current producer answers only for an implementation it still owns.
: MATCH-FACTS ( n ptr n n n -- n bool )
   {: entry:n types:ptr din:n dout:n :}
   entry ID-OF {: id:n :}
   id 0= if 0 false exit then
   id ROW {: row:n :}
   row types din dout MATCH-ROW? 0= if 0 false exit then
   row IMPL-LEN @ true ;

public

\ A restored process has no retained implementation facts. Its callback cell
\ is cleared with the other process-owned cells; the next source compilation
\ starts a fresh owner without reading the saved image's old dynamic buffers.
: INVALIDATE ( n n -- )
   {: first:n end:n :}
   first end >= if exit then
   PRIOR-INVALIDATE @ dup 0<> if AS-INVALIDATE first end rot execute else drop then
   KERNEL-END @ KERNEL-BEGIN @ >
   first KERNEL-END @ < and
   end KERNEL-BEGIN @ > and if
      SEEDED-N @ 0 ?do 0 i IMPL-LIVE ! loop
      0 data-base NATIVE-HOST-CELLS:SCALAR-PRIM + !
      0 data-base NATIVE-HOST-CELLS:DIVISION-PRIM + !
   then
   IMPL-N @ 0 ?do
      i IMPL-ENTRY @ end <
      i IMPL-ENTRY @ i IMPL-LEN @ + first > and if 0 i IMPL-LIVE ! then
   loop
   ASSOC-N @ 0 ?do
      i ASSOC-ENTRY @ end <
      i ASSOC-ENTRY @ i ASSOC-LEN @ + first > and if 0 i ASSOC-LIVE ! then
   loop ;

\ The digest slot follows the entire immutable kernel code interval. A marker
\ without that producer cannot claim a kernel-owned division implementation.
: KERNEL-OWNER ( -- )
   0 KERNEL-BEGIN !  0 KERNEL-END !
   SOURCE-PRODUCER:KERNEL SOURCE-PRODUCER:ROLE$
      {: digest:ptr size:n known:bool :}
   known 0= if exit then
   size 32 <> if E-STATE throw then
   data-base RBASE-CELL + @ {: base:n :}
   digest NULL-PTR BYTE-VIEW - {: end:n :}
   base 0 <= end base <= or if E-STATE throw then
   base KERNEL-BEGIN !  end KERNEL-END ! ;

\ A registry ordinal selects the exact booted occurrence. Its emitted row
\ supplies the behavior; the specification contributes only its numeric shape.
: SEED-ROW ( n n n -- )
   {: marker:n din:n dout:n :}
   marker 0< marker ndict@ > or if E-STATE throw then
   marker 1- XREF-REC {: rec:ptr :}
   rec XREF-START {: entry:n :}
   rec XREF-CODE-BYTES {: len:n :}
   entry 0 <= len 0 <= or if E-STATE throw then
   RTARGET:HOST-TARGET RTARGET:CORE
   MATCH RTARGET:core-result
      unsupported OF E-STATE throw ENDOF
      supported OF
         IMPL-N @ 1+ dup RESERVE-IMPL {: count:n :}
         count 1- {: row:n :}
         IMPL-TYPE-N @ {: first:n :}
         din dout + {: types:n :}
         first types + IMPL-TYPES-RESERVE
         types 0 ?do NUMBER-CON first i + IMPL-TYPES ! loop
         entry row IMPL-ENTRY !
         len row IMPL-LEN !
         din row IMPL-IN !  dout row IMPL-OUT !
         first row IMPL-TYPE-FIRST !
         SAFE row IMPL-REASON !  0 row IMPL-LOC !
         0 row IMPL-FIRST !  0 row IMPL-EDGES !
         1 row IMPL-LIVE !
         row IMPL-CONTRACT !
         first types + IMPL-TYPE-N !
         count IMPL-N !
      ENDOF
   ;MATCH ;

\ The scalar body is call-free. Division is partial: its guard can branch to
\ the sealed terminal (DIV-ZERO) helper and throw E-DIV-ZERO. The emitter has
\ published the marker only after both spans finished.
: SEED-SCALAR ( -- )
   data-base NATIVE-HOST-CELLS:SCALAR-PRIM + @ {: marker:n :}
   marker 0<> if marker 1 1 SEED-ROW then ;

: SEED-DIVISION ( -- )
   data-base NATIVE-HOST-CELLS:DIVISION-PRIM + @ {: marker:n :}
   marker 0= if exit then
   KERNEL-BEGIN @ 0= if E-STATE throw then
   marker 0< marker ndict@ > or if E-STATE throw then
   marker 1- XREF-REC {: rec:ptr :}
   rec XREF-START {: entry:n :}
   rec XREF-CODE-BYTES {: len:n :}
   entry KERNEL-BEGIN @ < entry KERNEL-END @ >= or if E-STATE throw then
   len 0 <= len KERNEL-END @ entry - > or if E-STATE throw then
   marker 2 1 SEED-ROW ;

: INSTALL ( -- )
   OWNER-INSTALLED @ 0<> if exit then
   data-base NATIVE-OBS-CELLS:HOST-INVALIDATE + @ PRIOR-INVALIDATE !
   data-base NATIVE-HOST-CELLS:FACTS + @ PRIOR-FACTS !
   0 IMPL-N !
   0 IMPL-TYPE-N !
   0 EDGE-N !
   0 ASSOC-N !
   KERNEL-OWNER
   SEED-SCALAR
   SEED-DIVISION
   IMPL-N @ SEEDED-N !
   ['] INVALIDATE data-base NATIVE-OBS-CELLS:HOST-INVALIDATE + xt!
   ['] MATCH-FACTS data-base NATIVE-HOST-CELLS:FACTS + xt!
   1 OWNER-INSTALLED ! ;

: CAPTURE-PREPARE ( -- )
   OWNER-INSTALLED @ 0<> if
      PRIOR-INVALIDATE @ dup 0<> if
         data-base NATIVE-OBS-CELLS:HOST-INVALIDATE + xt!
      else
         data-base NATIVE-OBS-CELLS:HOST-INVALIDATE + !
      then
      PRIOR-FACTS @ dup 0<> if
         data-base NATIVE-HOST-CELLS:FACTS + xt!
      else
         data-base NATIVE-HOST-CELLS:FACTS + !
      then
   then
   0 OWNER-INSTALLED !
   0 PRIOR-INVALIDATE !
   0 PRIOR-FACTS !
   0 KERNEL-BEGIN !  0 KERNEL-END !  0 SEEDED-N ! ;

\ The elaborator keeps the worst reason by rank while its source tape is still
\ live. Later HIR and machine rows cannot reinterpret that decision.
: SOURCE-BEGIN ( bool -- )
   INSTALL
   if UNCHECKED else SAFE then SOURCE-REASON !
   0 SOURCE-LOC !
   0 SOURCE-FUNS !
   0 SOURCE-TYPE-N !
   0 FACT-FUNS ! ;

private

\ The admitted reasons rank SAFE < MEMORY < FIXED; every refusing reason ranks
\ above them, alike.
: RANK ( n -- n )
   {: reason:n :}
   reason SAFE = if 0 exit then
   reason MEMORY = if 1 exit then
   reason FIXED = if 2 exit then
   3 ;

public

\ Keep the worst reason by rank, so an admitted reason never masks a later
\ refusing one; among refusing reasons the first keeps its location.
: SOURCE-REFUSE ( n n -- )
   {: reason:n loc:n :}
   reason RANK SOURCE-REASON @ RANK > if
      reason SOURCE-REASON !
      loc SOURCE-LOC !
   then ;

: SOURCE-REASON@ ( -- n n )
   FACT-REASON @ FACT-LOC @ ;

: SOURCE-ABANDON ( -- )
   UNKNOWN SOURCE-REASON !
   0 SOURCE-LOC !
   0 SOURCE-FUNS !
   0 FACT-FUNS ! ;

: SOURCE-ARITY+ ( n n -- )
   {: din:n dout:n :}
   SOURCE-FUNS @ 1+ {: count:n :}
   count SOURCE-IN-RESERVE
   count SOURCE-OUT-RESERVE
   count SOURCE-TYPE-FIRST-RESERVE
   count SOURCE-TYPE-READY-RESERVE
   din count 1- SOURCE-IN !
   dout count 1- SOURCE-OUT !
   -1 count 1- SOURCE-TYPE-FIRST !
   0 count 1- SOURCE-TYPE-READY !
   count SOURCE-FUNS ! ;

: SOURCE-ARITY@ ( n -- n n )
   {: ordinal:n :}
   ordinal FACT-FUNS @ >= if -1 -1 exit then
   ordinal FACT-IN @ ordinal FACT-OUT @ ;

: SOURCE-CONTRACT-START ( -- )
   SOURCE-FUNS @ 1- {: ordinal:n :}
   ordinal 0< if E-STATE throw then
   ordinal SOURCE-IN @ ordinal SOURCE-OUT @ + {: count:n :}
   count 0< if E-STATE throw then
   SOURCE-TYPE-N @ count + dup SOURCE-TYPES-RESERVE SOURCE-TYPE-N !
   SOURCE-TYPE-N @ count - ordinal SOURCE-TYPE-FIRST ! ;

: SOURCE-CONTRACT-IN ( n n -- )
   {: idx:n con:n :}
   SOURCE-FUNS @ 1- {: ordinal:n :}
   idx 0< idx ordinal SOURCE-IN @ >= or if E-STATE throw then
   con 0 <= if E-STATE throw then
   con ordinal SOURCE-TYPE-FIRST @ idx + SOURCE-TYPES ! ;

: SOURCE-CONTRACT-OUT ( n n -- )
   {: idx:n con:n :}
   SOURCE-FUNS @ 1- {: ordinal:n :}
   idx 0< idx ordinal SOURCE-OUT @ >= or if E-STATE throw then
   con 0 <= if E-STATE throw then
   con ordinal SOURCE-TYPE-FIRST @ ordinal SOURCE-IN @ + idx + SOURCE-TYPES ! ;

: SOURCE-CONTRACT-DONE ( -- )
   1 SOURCE-FUNS @ 1- SOURCE-TYPE-READY ! ;

\ The declaration's source facts are complete before emission.
\ TAKE reads this copy after emission.
: SOURCE-FREEZE ( -- )
   SOURCE-FUNS @ {: funs:n :}
   SOURCE-TYPE-N @ {: types:n :}
   funs FACT-IN-RESERVE
   funs FACT-OUT-RESERVE
   funs FACT-TYPE-FIRST-RESERVE
   funs FACT-TYPE-READY-RESERVE
   types FACT-TYPES-RESERVE
   funs 0 ?do
      i SOURCE-IN @ i FACT-IN !
      i SOURCE-OUT @ i FACT-OUT !
      i SOURCE-TYPE-FIRST @ i FACT-TYPE-FIRST !
      i SOURCE-TYPE-READY @ i FACT-TYPE-READY !
   loop
   types 0 ?do i SOURCE-TYPES @ i FACT-TYPES ! loop
   SOURCE-REASON @ FACT-REASON !
   SOURCE-LOC @ FACT-LOC !
   funs FACT-FUNS ! ;

private

: SOURCE-CONTRACT? ( n -- bool )
   dup FACT-FUNS @ >= if drop false exit then
   FACT-TYPE-READY @ 0<> ;

: SOURCE-CONTRACT-COPY ( n n -- )
   {: ordinal:n id:n :}
   ordinal SOURCE-CONTRACT? 0= if exit then
   id ROW {: row:n :}
   ordinal FACT-IN @ row IMPL-IN @ <>
   ordinal FACT-OUT @ row IMPL-OUT @ <> or if E-STATE throw then
   row IMPL-IN @ row IMPL-OUT @ + {: count:n :}
   IMPL-TYPE-N @ count + IMPL-TYPES-RESERVE
   IMPL-TYPE-N @ row IMPL-TYPE-FIRST !
   count 0 ?do
      ordinal FACT-TYPE-FIRST @ i + FACT-TYPES @
         IMPL-TYPE-N @ i + IMPL-TYPES !
   loop
   IMPL-TYPE-N @ count + IMPL-TYPE-N ! ;

: SAME-TYPES? ( n n -- bool )
   {: a:n b:n :}
   a ROW {: x:n :}
   b ROW {: y:n :}
   x IMPL-TYPE-FIRST @ {: first:n :}
   first 0< if false exit then
   x IMPL-IN @ x IMPL-OUT @ + 0= if
      y NULL-PTR 0 0 MATCH-ROW? exit
   then
   y first IMPL-TYPES x IMPL-IN @ x IMPL-OUT @ MATCH-ROW? ;

\ A host owner compares its local producer row with the active target owner's
\ live row. The current owner supplies the full emitted extent for retirement.
: TARGET-LEN? ( n n -- n bool )
   {: id:n entry:n :}
   entry ID-OF {: target-id:n :}
   target-id 0<> if
      id target-id SAME-TYPES? 0= if 0 false exit then
      target-id ROW IMPL-LEN @ true exit
   then
   data-base NATIVE-HOST-CELLS:FACTS + @ dup 0= if drop 0 false exit then
   AS-FACTS {: query :}
   id ROW {: row:n :}
   row IMPL-TYPE-FIRST @ {: first:n :}
   first 0< if 0 false exit then
   row IMPL-IN @ row IMPL-OUT @ + 0= if
      entry NULL-PTR 0 0 query execute exit
   then
   entry first IMPL-TYPES row IMPL-IN @ row IMPL-OUT @ query execute ;

public

\ The occurrence owner attaches a retained host implementation to a distinct
\ target implementation. Aliases share the target entry; later definitions do
\ not inherit the association by spelling.
: ASSOCIATE ( n n n n -- )
   {: target-slot:n target-occ:n host-slot:n host-occ:n :}
   target-slot target-occ DEF-OCC:RESOLVE {: target:ptr :}
   host-slot host-occ DEF-OCC:RESOLVE {: host:ptr :}
   host XREF-START ID-OF {: id:n :}
   id 0= if 0 UNKNOWN 0 REFUSE then
   target XREF-START {: entry:n :}
   id entry TARGET-LEN? {: len:n same:bool :}
   same 0= if id UNKNOWN 0 REFUSE then
   \ The dictionary omits a trailing return from its code length. The emitted
   \ row owns the full extent, which must retire if that return is reused.
   entry 0 <= len 0 <= or if E-STATE throw then
   ASSOC-N @ 1+ {: count:n :}
   count ASSOC-ENTRY-RESERVE
   count ASSOC-LEN-RESERVE
   count ASSOC-IMPL-RESERVE
   count ASSOC-LIVE-RESERVE
   count 1- {: row:n :}
   entry row ASSOC-ENTRY !
   len row ASSOC-LEN !
   id row ASSOC-IMPL !
   1 row ASSOC-LIVE !
   count ASSOC-N ! ;

private

\ Prepare all storage while publication may still throw. The publisher only
\ exposes the row after its bytes and dictionary record are committed.
: PREPARE ( n n CTARGET:contract n n n n -- n )
   {: entry:n len:n contract:CTARGET:contract din:n dout:n reason:n loc:n :}
   entry 0 <= len 0 <= or if E-STATE throw then
   reason SAFE = din 0 < dout 0 < or and if E-STATE throw then
   IMPL-N @ 1+ dup RESERVE-IMPL {: id:n :}
   id 1- {: row:n :}
   entry row IMPL-ENTRY !
   len row IMPL-LEN !
   contract row IMPL-CONTRACT !
   din row IMPL-IN !
   dout row IMPL-OUT !
   -1 row IMPL-TYPE-FIRST !
   reason row IMPL-REASON !
   loc row IMPL-LOC !
   EDGE-N @ row IMPL-FIRST !
   0 row IMPL-EDGES !
   0 row IMPL-LIVE !
   id IMPL-N !
   id ;

\ Edges are appended while the row is the newest unpublished implementation.
: EDGE+ ( n n n n n -- )
   {: id:n to:n site:n kind:n loc:n :}
   id IMPL-N @ <> if E-STATE throw then
   id ROW {: row:n :}
   row IMPL-LIVE @ 0<> if E-STATE throw then
   EDGE-N @ 1+ dup RESERVE-EDGE {: count:n :}
   count 1- {: edge:n :}
   to edge EDGE-TO !
   site edge EDGE-SITE !
   kind edge EDGE-KIND !
   loc edge EDGE-LOC !
   count EDGE-N !
   row IMPL-EDGES @ 1+ row IMPL-EDGES ! ;

\ Each word the source runs passes its record here before its entry is chosen.
defer RUN-OBSERVE ( ptr n -- )
: NO-RUN-OBSERVE ( ptr n -- ) drop ;
: DEFAULT-RUNS ( -- ) ['] NO-RUN-OBSERVE is RUN-OBSERVE ;
DEFAULT-RUNS
0 CWORK-VARIABLE PRIOR-SELECT n

public

\ A word the source runs at top level, by an immediate or under a top-level
\ tick. A compiler read of a fixed value selects its record without running it.
: RUN-REC ( ptr n -- n )
   {: rec:ptr :}
   rec RUN-OBSERVE
   rec XREF-START ;

\ The engine's selection callback: an already-bound code entry runs as it is.
: SELECT-CALL ( ptr n n -- n )
   {: rec:ptr entry:n :}
   rec 0= if entry exit then
   rec RUN-REC ;

\ observer sees every word the source runs, through the engine's selection
\ cell, until UNOBSERVE-RUNS puts back the callback the cell held.
: OBSERVE-RUNS ( [ ptr n -- ] -- )
   is RUN-OBSERVE
   data-base NATIVE-HOST-CELLS:SELECT + @ PRIOR-SELECT !
   ['] SELECT-CALL data-base NATIVE-HOST-CELLS:SELECT + xt! ;

: UNOBSERVE-RUNS ( -- )
   PRIOR-SELECT @ {: callback:n :}
   callback 0<> if
      callback data-base NATIVE-HOST-CELLS:SELECT + xt!
   else
      0 data-base NATIVE-HOST-CELLS:SELECT + !
   then
   0 PRIOR-SELECT !
   DEFAULT-RUNS ;

;package

NHOST:INSTALL
