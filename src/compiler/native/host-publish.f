\ host-publish.f - take owned implementation facts before the code window and
\ expose them only after native publication succeeds.

require src/compiler/native/host.f
require src/compiler/native/abi.f
require src/compiler/session/emission.f
require src/compiler/binding.f
require src/lib/le.f

package NHOST
private

0 CWORK-VARIABLE TAKEN-BASE n
0 CWORK-VARIABLE TAKEN-EDGES n
0 CWORK-VARIABLE TAKEN-TYPES n
0 CWORK-VARIABLE TAKEN-FUNS n
0 CWORK-VARIABLE TAKEN-EMISSION NART:emission
0 CWORK-VARIABLE TAKEN-AT n

: FUN-END ( NART:emission n -- n )
   {: e:NART:emission ordinal:n :}
   ordinal 1+ e NART:FUNCTIONS < if
      e ordinal 1+ NART:FUNCTION-OFFSET@ exit
   then
   e NART:SIZE ;

\ Internal function targets are resolved against the pending emission's own
\ ordered entry offsets. External targets keep the identity filed at emission.
: INTERNAL-ID ( NART:emission n n -- n )
   {: e:NART:emission at:n target:n :}
   target at < target at e NART:SIZE + >= or if 0 exit then
   target at - {: off:n :}
   e NART:FUNCTIONS 0 ?do
      e i NART:FUNCTION-OFFSET@ off = if
         TAKEN-BASE @ i + 1+ unloop exit
      then
   loop
   E-STATE throw ;

: EDGE-TARGET ( NART:emission n n -- n )
   {: e:NART:emission at:n site:n :}
   e at e site NART:CALL-TARGET@ INTERNAL-ID {: internal:n :}
   internal 0<> if internal exit then
   e site NART:CALL-IMPL@ ;

: TAKE-EDGES ( NART:emission n n n -- )
   {: e:NART:emission at:n ordinal:n id:n :}
   e ordinal NART:FUNCTION-OFFSET@ {: start:n :}
   e ordinal FUN-END {: end:n :}
   e NART:CALL-SITES 0 ?do
      e i NART:CALL-SITE@ {: site:n :}
      site start >= site end < and if
         id e at i EDGE-TARGET site start -
         e i NART:CALL-KIND@ e i NART:CALL-LOC@ EDGE+
      then
   loop ;

: FACTS ( n -- n n n n )
   {: ordinal:n :}
   SOURCE-REASON@ {: reason:n loc:n :}
   ordinal SOURCE-ARITY@ {: din:n dout:n :}
   din 0 < dout 0 < or ordinal SOURCE-CONTRACT? 0= or if
      din dout UNKNOWN 0 exit
   then
   din dout reason loc ;

: TAKE-ONE ( NART:emission n n -- )
   {: e:NART:emission at:n ordinal:n :}
   e ordinal NART:FUNCTION-OFFSET@ {: off:n :}
   e ordinal FUN-END off - {: len:n :}
   at off + len e NART:BINDING CBIND:TARGET@ ordinal FACTS
      PREPARE {: id:n :}
   ordinal id SOURCE-CONTRACT-COPY
   id TAKEN-BASE @ ordinal + 1+ <> if E-STATE throw then
   e at ordinal id TAKE-EDGES ;

: TAKE-BODY ( -- )
   TAKEN-EMISSION @ {: e:NART:emission :}
   TAKEN-AT @ {: at:n :}
   TAKEN-FUNS @ 0 ?do e at i TAKE-ONE loop ;

public

\ The publisher's observer reads these while TAKE owns the emission.  A
\ A negative constructor count means unknown; zero is a known empty contract.
: TAKEN-FUNCTIONS ( -- n ) TAKEN-FUNS @ ;

: TAKEN-FACTS ( n -- n n n n n n )
   {: ordinal:n :}
   ordinal 0 < ordinal TAKEN-FUNS @ >= or if E-STATE throw then
   TAKEN-BASE @ ordinal + {: row:n :}
   row IMPL-LEN @ row IMPL-IN @ row IMPL-OUT @
   row IMPL-REASON @ row IMPL-LOC @
   row IMPL-TYPE-FIRST @ 0< if -1 else
      row IMPL-IN @ row IMPL-OUT @ + then ;

: TAKEN-TYPE@ ( n n -- n )
   {: ordinal:n idx:n :}
   ordinal 0 < ordinal TAKEN-FUNS @ >= or if E-STATE throw then
   TAKEN-BASE @ ordinal + {: row:n :}
   row IMPL-TYPE-FIRST @ 0< if E-STATE throw then
   idx 0 < idx row IMPL-IN @ row IMPL-OUT @ + >= or if E-STATE throw then
   row IMPL-TYPE-FIRST @ idx + IMPL-TYPES @ ;

: ABANDON-TAKEN ( -- )
   TAKEN-FUNS @ 0= if exit then
   TAKEN-BASE @ IMPL-N !
   TAKEN-EDGES @ EDGE-N !
   TAKEN-TYPES @ IMPL-TYPE-N !
   0 TAKEN-FUNS ! ;

\ No code window has opened. Every reserve and row copy that may throw happens
\ before the optional observer runs. An incomplete take is abandoned on refusal.
: TAKE ( NART:emission n -- )
   {: e:NART:emission at:n :}
   TAKEN-FUNS @ 0<> if E-STATE throw then
   IMPL-N @ TAKEN-BASE !
   EDGE-N @ TAKEN-EDGES !
   IMPL-TYPE-N @ TAKEN-TYPES !
   e NART:FUNCTIONS dup 0 <= if E-STATE throw then TAKEN-FUNS !
   e TAKEN-EMISSION !
   at TAKEN-AT !
   [: TAKE-BODY ;] catch {: rc:n :}
   0 TAKEN-AT !
   rc 0<> if ABANDON-TAKEN rc throw then ;

\ Function rows are seven little-endian cells followed by the scalar
\ constructor cells.  The call rows are five cells: site, kind, reference,
\ source location and whether the emitting implementation was known.  The
\ caller resolves and patches references before invoking this preparation.
0 CWORK-VARIABLE IMPORT-BASE n
0 CWORK-VARIABLE IMPORT-SIZE n
0 CWORK-VARIABLE IMPORT-END n
0 CWORK-VARIABLE IMPORT-FUNS n
0 CWORK-VARIABLE IMPORT-FUNCTION-P ptr u8
0 CWORK-VARIABLE IMPORT-FUNCTION-U n
0 CWORK-VARIABLE IMPORT-CALL-P ptr u8
0 CWORK-VARIABLE IMPORT-CALL-U n
0 CWORK-VARIABLE IMPORT-CURSOR n
DYNAMIC-BUFFER IMPORT-ROWS n

: IMPORT-ROOM ( n -- )
   {: bytes:n :}
   bytes 0 < bytes IMPORT-FUNCTION-U @ IMPORT-CURSOR @ - > or
      if E-STATE throw then ;

: IMPORT-TYPES ( ptr u8 n n -- )
   {: row:ptr id:n count:n :}
   id 1- {: target:n :}
   IMPL-TYPE-N @ count + IMPL-TYPES-RESERVE
   IMPL-TYPE-N @ target IMPL-TYPE-FIRST !
   count 0 ?do
      row i cells + LE:U64@ IMPL-TYPE-N @ i + IMPL-TYPES !
   loop
   IMPL-TYPE-N @ count + IMPL-TYPE-N ! ;

: IMPORT-EDGE-TARGET ( n n -- n )
   {: target:n known:n :}
   IMPORT-FUNS @ 0 ?do
      IMPORT-FUNCTION-P @ i IMPORT-ROWS @ + LE:U64@
         IMPORT-BASE @ + target = if
         TAKEN-BASE @ i + 1+ unloop exit
      then
   loop
   target IMPORT-BASE @ >=
   target IMPORT-BASE @ - IMPORT-SIZE @ < and
      if E-STATE throw then
   known 0= if 0 exit then
   target ID-OF ;

: CHECK-IMPORT-ONE ( n -- )
   {: ordinal:n :}
   7 cells IMPORT-ROOM
   IMPORT-CURSOR @ ordinal IMPORT-ROWS !
   IMPORT-FUNCTION-P @ IMPORT-CURSOR @ + {: row:ptr :}
   row LE:U64@ {: off:n :}
   row 8 + LE:U64@ {: len:n :}
   row 48 + LE:U64@ {: types:n :}
   types -1 <
   types IMPORT-FUNCTION-U @ IMPORT-CURSOR @ - 7 cells - CELL / > or
      if E-STATE throw then
   off IMPORT-END @ <> len 0 <= or
   len IMPORT-SIZE @ off - > or if E-STATE throw then
   off len + IMPORT-END !
   7 cells types 0 max cells + IMPORT-CURSOR +! ;

: CHECK-IMPORT-TABLE ( -- )
   IMPORT-FUNS @ IMPORT-ROWS-RESERVE
   IMPORT-FUNS @ 0 ?do i CHECK-IMPORT-ONE loop
   IMPORT-CURSOR @ IMPORT-FUNCTION-U @ <> if E-STATE throw then
   IMPORT-END @ IMPORT-SIZE @ <> if E-STATE throw then ;

: IMPORT-EDGES ( n n -- )
   {: id:n off:n :}
   id 1- IMPL-LEN @ {: len:n :}
   IMPORT-CALL-U @ 5 cells / 0 ?do
      IMPORT-CALL-P @ i 5 cells * + {: call:ptr :}
      call LE:U64@ {: site:n :}
      site 0 < site IMPORT-SIZE @ >= or if E-STATE throw then
      site off >= site off len + < and if
         call 16 + LE:U64@ call 32 + LE:U64@
            IMPORT-EDGE-TARGET {: to:n :}
         id to site off - call 8 + LE:U64@
            call 24 + LE:U64@ EDGE+
      then
   loop ;

: IMPORT-ONE ( ptr u8 n -- )
   {: row:ptr ordinal:n :}
   row LE:U64@ {: off:n :}
   row 8 + LE:U64@ {: len:n :}
   row 16 + LE:U64@ {: din:n :}
   row 24 + LE:U64@ {: dout:n :}
   row 32 + LE:U64@ {: reason:n :}
   row 40 + LE:U64@ {: loc:n :}
   row 48 + LE:U64@ {: types:n :}
   IMPORT-BASE @ off + len NABI:BINDING CBIND:TARGET@
      din dout reason loc PREPARE {: id:n :}
   types -1 > if
      types din dout + <> if E-STATE throw then
      row 7 cells + id types IMPORT-TYPES
   then
   id TAKEN-BASE @ ordinal + 1+ <> if E-STATE throw then
   id off IMPORT-EDGES ;

: IMPORT-BODY ( -- )
   CHECK-IMPORT-TABLE
   IMPORT-FUNS @ 0 ?do
      IMPORT-FUNCTION-P @ i IMPORT-ROWS @ + i IMPORT-ONE
   loop ;

: PREPARE-IMPORT ( n n ptr u8 n ptr u8 n -- )
   {: base:n size:n funcs:ptr funcu:n calls:ptr callu:n :}
   TAKEN-FUNS @ 0<> if E-STATE throw then
   size 0 <= funcu 8 < or
   callu 0 < or callu 5 cells mod 0<> or if E-STATE throw then
   funcs LE:U64@ {: count:n :}
   count 0 <= count funcu 8 - 7 cells / > or if E-STATE throw then
   IMPL-N @ TAKEN-BASE !
   EDGE-N @ TAKEN-EDGES !
   IMPL-TYPE-N @ TAKEN-TYPES !
   count TAKEN-FUNS !
   base IMPORT-BASE ! size IMPORT-SIZE ! 0 IMPORT-END !
   count IMPORT-FUNS !
   funcs IMPORT-FUNCTION-P ! funcu IMPORT-FUNCTION-U !
   calls IMPORT-CALL-P ! callu IMPORT-CALL-U !
   8 IMPORT-CURSOR !
   ['] IMPORT-BODY catch {: rc:n :}
   rc 0<> if ABANDON-TAKEN rc throw then ;

\ PREPARE wrote all cells; the code publisher only flips liveness now.
: PUBLISH-TAKEN ( -- )
   TAKEN-FUNS @ 0 ?do
      1 TAKEN-BASE @ i + IMPL-LIVE !
   loop
   0 TAKEN-FUNS ! ;

;package
