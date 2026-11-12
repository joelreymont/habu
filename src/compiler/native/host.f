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
-9441 constant E-STATE
-9442 constant E-ROW

\ A definition's host reason names what in its body the published facts
\ (contract, call sites, address sites) do not account for. SAFE: nothing.
\ MEMORY: memory operations and address-forming words, admitted by provenance.
\ FIXED: a constant compiled as an immediate, or a data address the address
\ sites carry. INDIRECT: a run of an xt or text operand, which a file's closure
\ walk and source observer account for. CAST and UNCHECKED: an effect the
\ facts cannot name. UNKNOWN: an unmodeled op or literal, or no facts.
\ REBIND: a compiled `is`.
0 constant SAFE
1 constant CAST
2 constant MEMORY
3 constant INDIRECT
4 constant UNCHECKED
6 constant UNKNOWN
7 constant FIXED
8 constant REBIND

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

\ One process's install state (INSTALL): a save or a file's object writes it
\ zero, as CAPTURE-PREPARE leaves it. The engine cell INSTALL fills holds no
\ other callback, so a program whose file object kept it installed installs
\ again over its own entries.
variable OWNER-INSTALLED    OWNER-INSTALLED scratch-cell-mark
variable KERNEL-BEGIN       KERNEL-BEGIN scratch-cell-mark
variable KERNEL-END         KERNEL-END scratch-cell-mark
variable SEEDED-N           SEEDED-N scratch-cell-mark
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

\ A restored process has no retained implementation facts. Its callback cell
\ is cleared with the other process-owned cells; the next source compilation
\ starts a fresh owner without reading the saved image's old dynamic buffers.
: INVALIDATE ( n n -- )
   {: first:n end:n :}
   first end >= if exit then
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
   loop ;

\ The digest slot follows the entire immutable kernel code interval. A marker
\ without that producer cannot claim a kernel-owned division implementation.
: KERNEL-OWNER ( -- )
   0 KERNEL-BEGIN !  0 KERNEL-END !
   SOURCE-PRODUCER:KERNEL$
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
   0 IMPL-N !
   0 IMPL-TYPE-N !
   0 EDGE-N !
   KERNEL-OWNER
   SEED-SCALAR
   SEED-DIVISION
   IMPL-N @ SEEDED-N !
   ['] INVALIDATE data-base NATIVE-OBS-CELLS:HOST-INVALIDATE + xt!
   1 OWNER-INSTALLED ! ;

: CAPTURE-PREPARE ( -- )
   OWNER-INSTALLED @ 0<> if
      0 data-base NATIVE-OBS-CELLS:HOST-INVALIDATE + !
   then
   0 OWNER-INSTALLED !
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

\ The admitted reasons rank SAFE < MEMORY < INDIRECT < FIXED < REBIND, then
\ UNCHECKED, which a checker contract admits; every refusing reason ranks
\ above them, alike, so a cast or an unmodeled op still refuses after an `is`
\ or in an unchecked body.
: RANK ( n -- n )
   {: reason:n :}
   reason SAFE = if 0 exit then
   reason MEMORY = if 1 exit then
   reason INDIRECT = if 2 exit then
   reason FIXED = if 3 exit then
   reason REBIND = if 4 exit then
   reason UNCHECKED = if 5 exit then
   6 ;

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

\ The source facts are complete before emission; TAKE reads this copy after it.
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

\ Until UNOBSERVE-RUNS puts back the callback the engine's selection cell held,
\ the observer sees every word the source runs.
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
