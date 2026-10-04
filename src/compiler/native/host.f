\ host.f - implementation-owned facts for native construction calls.
\ A dictionary occurrence is the selection key; a published implementation row
\ supplies its exact code, execution contract and direct implementation edges.
\ Rows retired by code reclamation never become live again when bytes are reused.

require lib/prelude.f
require src/compiler/target/model.f
require src/habu/xref.f
require src/habu/native-observer-cells.f
require src/habu/native-host-cells.f

package NHOST
public

-9440 constant E-FIRST
-9449 constant E-LAST
-9440 constant E-UNSAFE
-9441 constant E-STATE
-9442 constant E-ROW

0 constant SAFE
1 constant CAST
2 constant MEMORY
3 constant INDIRECT
4 constant UNCHECKED
5 constant FOREIGN
6 constant UNKNOWN

private

variable IMPL-N
DYNAMIC-BUFFER IMPL-ENTRY n
DYNAMIC-BUFFER IMPL-LEN n
DYNAMIC-BUFFER IMPL-CONTRACT CTARGET:contract
DYNAMIC-BUFFER IMPL-IN n
DYNAMIC-BUFFER IMPL-OUT n
DYNAMIC-BUFFER IMPL-TYPE-FIRST n
DYNAMIC-BUFFER IMPL-TYPES n
variable IMPL-TYPE-N
DYNAMIC-BUFFER IMPL-REASON n
DYNAMIC-BUFFER IMPL-LOC n
DYNAMIC-BUFFER IMPL-FIRST n
DYNAMIC-BUFFER IMPL-EDGES n
DYNAMIC-BUFFER IMPL-LIVE n

variable EDGE-N
DYNAMIC-BUFFER EDGE-TO n
DYNAMIC-BUFFER EDGE-SITE n
DYNAMIC-BUFFER EDGE-KIND n
DYNAMIC-BUFFER EDGE-LOC n

variable ASSOC-N
DYNAMIC-BUFFER ASSOC-ENTRY n
DYNAMIC-BUFFER ASSOC-LEN n
DYNAMIC-BUFFER ASSOC-IMPL n
DYNAMIC-BUFFER ASSOC-LIVE n

variable SOURCE-REASON
variable SOURCE-LOC
UNKNOWN SOURCE-REASON !
variable SOURCE-FUNS
DYNAMIC-BUFFER SOURCE-IN n
DYNAMIC-BUFFER SOURCE-OUT n
DYNAMIC-BUFFER SOURCE-TYPE-FIRST n
DYNAMIC-BUFFER SOURCE-TYPE-READY n
DYNAMIC-BUFFER SOURCE-TYPES n
variable SOURCE-TYPE-N

variable OWNER-INSTALLED
variable PRIOR-INVALIDATE
CAST: AS-INVALIDATE ( n -- [ n -- ] )
1 constant NUMBER-CON      \ checker.f CC-N, encoded by PRIM-SPEC:A-NUM

variable WORK-N
DYNAMIC-BUFFER WORK n
DYNAMIC-BUFFER SEEN n
DYNAMIC-BUFFER PARENT n
DYNAMIC-BUFFER VIA n

variable REQUIRED
TYPED-VARIABLE EXECUTION RTARGET:execution-platform

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

: RESERVE-WALK ( -- )
   IMPL-N @ 1+ dup SEEN-RESERVE PARENT-RESERVE
   IMPL-N @ 1+ VIA-RESERVE
   EDGE-N @ 1+ WORK-RESERVE ;

: REASON$ ( n -- ptr u8 n )
   {: reason:n :}
   reason CAST = if s" source cast" exit then
   reason MEMORY = if s" source address or memory operation" exit then
   reason INDIRECT = if s" indirect invocation" exit then
   reason UNCHECKED = if s" unchecked body" exit then
   reason FOREIGN = if s" foreign call" exit then
   s" missing implementation facts" ;

: PATH ( n -- )
   {: id:n :}
   id
   begin dup 0<> while
      dup ROW {: row:n :}
      s"  implementation " type row IMPL-ENTRY @ .
      dup VIA @ dup 0<> if
         1- {: edge:n :}
         s"  call site " type edge EDGE-SITE @ .
         s"  source offset " type edge EDGE-LOC @ .
      else drop then
      cr
      PARENT @
   repeat drop ;

: REFUSE ( n n n -- )
   {: id:n reason:n loc:n :}
   s" host construction: " type reason REASON$ type
   s"  body offset " type loc . cr
   id PATH
   E-UNSAFE throw ;

: CHECK-IMPL ( n CTARGET:contract -- )
   {: id:n execution:CTARGET:contract :}
   id ROW {: row:n :}
   row IMPL-LIVE @ 0= if id UNKNOWN 0 REFUSE then
   row IMPL-CONTRACT @ execution CTARGET:SAME? 0= if id UNKNOWN 0 REFUSE then
   row IMPL-TYPE-FIRST @ 0< if id UNKNOWN 0 REFUSE then
   row IMPL-REASON @ {: reason:n :}
   reason SAFE <> if id reason row IMPL-LOC @ REFUSE then ;

: PUSH ( n -- )
   {: id:n :}
   id 0= if E-ROW throw then
   id ROW drop
   id WORK-N @ WORK !
   1 WORK-N +! ;

: POP ( -- n )
   -1 WORK-N +!
   WORK-N @ WORK @ ;

: FOLLOW ( n n -- )
   {: from:n edge:n :}
   edge EDGE-TO @ {: to:n :}
   to 1 < to IMPL-N @ > or if from UNKNOWN edge EDGE-LOC @ REFUSE then
   to SEEN @ 0<> if exit then
   from to PARENT !
   edge 1+ to VIA !
   to PUSH ;

: CLOSURE ( n CTARGET:contract -- )
   {: root:n execution:CTARGET:contract :}
   RESERVE-WALK
   IMPL-N @ 1+ 0 ?do 0 i SEEN ! 0 i PARENT ! 0 i VIA ! loop
   0 WORK-N !
   root PUSH
   begin WORK-N @ 0<> while
      POP {: id:n :}
      id SEEN @ 0= if
         1 id SEEN !
         id execution CHECK-IMPL
         id ROW {: row:n :}
         row IMPL-FIRST @ row IMPL-EDGES @ +
         row IMPL-FIRST @ ?do id i FOLLOW loop
      then
   repeat ;

public

\ A restored process has no retained implementation facts. Its callback cell
\ is cleared with the other process-owned cells; the next source compilation
\ starts a fresh owner without reading the saved image's old dynamic buffers.
: INVALIDATE ( n -- )
   {: floor:n :}
   PRIOR-INVALIDATE @ dup 0<> if AS-INVALIDATE floor swap execute else drop then
   IMPL-N @ 0 ?do
      i IMPL-ENTRY @ i IMPL-LEN @ + floor > if 0 i IMPL-LIVE ! then
   loop
   ASSOC-N @ 0 ?do
      i ASSOC-ENTRY @ i ASSOC-LEN @ + floor > if 0 i ASSOC-LIVE ! then
   loop ;

\ The boot publishes the registry ordinal of the one scalar body whose
\ emitter supplied a complete, call-free contract. Zero means that backend
\ provided no such body. A dictionary name is never used as authority.
: SEED-SCALAR ( -- )
   data-base NATIVE-HOST-CELLS:SCALAR-PRIM + @ {: marker:n :}
   marker 0= if exit then
   marker 0< marker ndict@ > or if E-STATE throw then
   marker 1- XREF-REC {: rec:ptr :}
   rec XREF-START {: entry:n :}
   rec XREF-CODE-BYTES {: len:n :}
   entry 0 <= len 0 <= or if E-STATE throw then
   RTARGET:HOST-TARGET RTARGET:CORE
   MATCH RTARGET:core-result
      unsupported OF E-STATE throw ENDOF
      supported OF
         1 RESERVE-IMPL
         2 IMPL-TYPES-RESERVE
         entry 0 IMPL-ENTRY !
         len 0 IMPL-LEN !
         1 0 IMPL-IN !  1 0 IMPL-OUT !
         0 0 IMPL-TYPE-FIRST !
         NUMBER-CON 0 IMPL-TYPES !  NUMBER-CON 1 IMPL-TYPES !
         2 IMPL-TYPE-N !
         SAFE 0 IMPL-REASON !  0 0 IMPL-LOC !
         0 0 IMPL-FIRST !  0 0 IMPL-EDGES !
         1 0 IMPL-LIVE !  1 IMPL-N !
         0 IMPL-CONTRACT !
      ENDOF
   ;MATCH ;

: INSTALL ( -- )
   OWNER-INSTALLED @ 0<> if exit then
   data-base NATIVE-OBS-CELLS:HOST-INVALIDATE + @ PRIOR-INVALIDATE !
   0 IMPL-N !
   0 IMPL-TYPE-N !
   0 EDGE-N !
   0 ASSOC-N !
   SEED-SCALAR
   ['] INVALIDATE data-base NATIVE-OBS-CELLS:HOST-INVALIDATE + xt!
   1 OWNER-INSTALLED ! ;

: CAPTURE-PREPARE ( -- )
   OWNER-INSTALLED @ 0<> if
      PRIOR-INVALIDATE @ dup 0<> if
         data-base NATIVE-OBS-CELLS:HOST-INVALIDATE + xt!
      else
         data-base NATIVE-OBS-CELLS:HOST-INVALIDATE + !
      then
   then
   0 OWNER-INSTALLED !
   0 PRIOR-INVALIDATE !
   0 REQUIRED ! ;

\ The elaborator keeps the first semantic refusal while its source tape is
\ still live. Later HIR and machine rows cannot reinterpret that decision.
: SOURCE-BEGIN ( bool -- )
   INSTALL
   if UNCHECKED else SAFE then SOURCE-REASON !
   0 SOURCE-LOC !
   0 SOURCE-FUNS !
   0 SOURCE-TYPE-N ! ;

: SOURCE-REFUSE ( n n -- )
   {: reason:n loc:n :}
   SOURCE-REASON @ SAFE <> if exit then
   reason SOURCE-REASON !
   loc SOURCE-LOC ! ;

: SOURCE-REASON@ ( -- n n )
   SOURCE-REASON @ SOURCE-LOC @ ;

: SOURCE-ABANDON ( -- )
   UNKNOWN SOURCE-REASON !
   0 SOURCE-LOC !
   0 SOURCE-FUNS ! ;

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
   ordinal SOURCE-FUNS @ >= if -1 -1 exit then
   ordinal SOURCE-IN @ ordinal SOURCE-OUT @ ;

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

private

: SOURCE-CONTRACT? ( n -- bool )
   dup SOURCE-FUNS @ >= if drop false exit then
   SOURCE-TYPE-READY @ 0<> ;

: SOURCE-CONTRACT-COPY ( n n -- )
   {: ordinal:n id:n :}
   ordinal SOURCE-CONTRACT? 0= if exit then
   id ROW {: row:n :}
   ordinal SOURCE-IN @ row IMPL-IN @ <>
   ordinal SOURCE-OUT @ row IMPL-OUT @ <> or if E-STATE throw then
   row IMPL-IN @ row IMPL-OUT @ + {: count:n :}
   IMPL-TYPE-N @ count + IMPL-TYPES-RESERVE
   IMPL-TYPE-N @ row IMPL-TYPE-FIRST !
   count 0 ?do
      ordinal SOURCE-TYPE-FIRST @ i + SOURCE-TYPES @
         IMPL-TYPE-N @ i + IMPL-TYPES !
   loop
   IMPL-TYPE-N @ count + IMPL-TYPE-N ! ;

: SAME-TYPES? ( n n -- bool )
   {: a:n b:n :}
   a ROW {: x:n :}
   b ROW {: y:n :}
   x IMPL-TYPE-FIRST @ 0< y IMPL-TYPE-FIRST @ 0< or if false exit then
   x IMPL-CONTRACT @ y IMPL-CONTRACT @ CTARGET:SAME? 0= if false exit then
   x IMPL-IN @ y IMPL-IN @ <> x IMPL-OUT @ y IMPL-OUT @ <> or if false exit then
   x IMPL-IN @ x IMPL-OUT @ + 0 ?do
      x IMPL-TYPE-FIRST @ i + IMPL-TYPES @
      y IMPL-TYPE-FIRST @ i + IMPL-TYPES @ <> if false unloop exit then
   loop
   true ;

public

\ A zero id means no producer ever published facts for this exact entry.
: ID-OF ( n -- n )
   {: entry:n :}
   IMPL-N @
   begin dup 0 > while
      dup 1- {: row:n :}
      row IMPL-LIVE @ 0<> row IMPL-ENTRY @ entry = and if exit then
      1-
   repeat ;

private

: ASSOCIATED-ID ( n -- n )
   {: entry:n :}
   ASSOC-N @
   begin dup 0 > while
      dup 1- {: row:n :}
      row ASSOC-LIVE @ 0<> row ASSOC-ENTRY @ entry = and if
         drop row ASSOC-IMPL @ exit
      then
      1-
   repeat drop
   entry ID-OF ;

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
   entry ID-OF {: target-id:n :}
   target-id 0= if id UNKNOWN 0 REFUSE then
   id target-id SAME-TYPES? 0= if id UNKNOWN 0 REFUSE then
   \ The dictionary omits a trailing return from its code length. The emitted
   \ row owns the full extent, which must retire if that return is reused.
   target-id ROW IMPL-LEN @ {: len:n :}
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

public

\ The occurrence resolves before reading its record, including on alias and
\ retired-name handles. Selection never asks the current spelling or target OS.
: SELECT-ENTRY ( n n RTARGET:execution-platform -- n )
   {: slot:n occurrence:n platform:RTARGET:execution-platform :}
   data-base NATIVE-OBS-CELLS:HOST-INVALIDATE + @ 0= if
      0 UNKNOWN 0 REFUSE
   then
   slot occurrence DEF-OCC:RESOLVE {: rec:ptr :}
   rec XREF-START {: entry:n :}
   entry ASSOCIATED-ID {: id:n :}
   id 0= if 0 UNKNOWN 0 REFUSE then
   id ROW IMPL-IN @ 0<> if id UNKNOWN 0 REFUSE then
   platform RTARGET:EXECUTION-TARGET RTARGET:CORE
   MATCH RTARGET:core-result
      supported OF id swap CLOSURE ENDOF
      unsupported OF id UNKNOWN 0 REFUSE ENDOF
   ;MATCH
   id ROW IMPL-ENTRY @ ;

: REQUIRED? ( -- bool ) REQUIRED @ 0<> ;

: SELECT-REC ( ptr n -- n )
   {: rec:ptr :}
   REQUIRED? 0= if rec XREF-START exit then
   rec DEF-OCC:SELECT EXECUTION @ SELECT-ENTRY ;

: ADMIT-ENTRY ( n -- n )
   {: entry:n :}
   REQUIRED? 0= if entry exit then
   entry ID-OF {: id:n :}
   id 0= if 0 UNKNOWN 0 REFUSE then
   id ROW IMPL-IN @ 0<> if id UNKNOWN 0 REFUSE then
   EXECUTION @ RTARGET:EXECUTION-TARGET RTARGET:CORE
   MATCH RTARGET:core-result
      supported OF id swap CLOSURE ENDOF
      unsupported OF id UNKNOWN 0 REFUSE ENDOF
   ;MATCH
   entry ;

: SELECT-CALL ( ptr n n -- n )
   {: rec:ptr entry:n :}
   rec 0= if entry ADMIT-ENTRY exit then
   rec SELECT-REC ;

: WITH-REQUIRED ( RTARGET:execution-platform [ -- ] -- )
   {: platform:RTARGET:execution-platform q :}
   REQUIRED @ {: prior:n :}
   EXECUTION @ {: previous:RTARGET:execution-platform :}
   data-base NATIVE-HOST-CELLS:SELECT + @ {: callback:n :}
   platform EXECUTION !
   1 REQUIRED !
   ['] SELECT-CALL data-base NATIVE-HOST-CELLS:SELECT + xt!
   q catch {: rc:n :}
   callback 0<> if
      callback data-base NATIVE-HOST-CELLS:SELECT + xt!
   else
      0 data-base NATIVE-HOST-CELLS:SELECT + !
   then
   previous EXECUTION !
   prior REQUIRED !
   rc 0<> if rc throw then ;

;package

NHOST:INSTALL
