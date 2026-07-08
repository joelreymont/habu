\ maki/eval-repair-mech.f - mechanical checker-guided repairer (package EVAL).
\
\ MECH-REPAIR closes the authoring loop without an author: candidate ->
\ checker verdict -> structured repair packet (tools/repair-packet-core.f
\ RP-PACKET over the checker's own JSON diagnostic) -> packet-directed source
\ edit -> re-check, up to MECH-MAX-ROUNDS rejections. Every candidate revision
\ runs through the shared EVAL metric engine (maki/eval-repair-loop.f
\ REPAIR-STEP), so REPAIR-ROUNDS@ / REPAIR-TOKENS@ after a mechanical run are
\ directly comparable to the author-trajectory columns in maki/eval-repair.f.
\
\ What each packet class actually carries decides what is mechanical:
\ - remove_producer: token + byte_start/byte_end name the excess producer in
\   the candidate -> delete that span. token_index 0 flags the definition-name
\   token (unconsumed-input surplus, nothing deletable) -> unrepairable.
\ - fix_return_stack: return_stack.expected/actual rows plus the flagged
\   transfer token. Surplus (actual row deeper than expected) -> delete the
\   flagged transfer; a deficit carries no insertable token -> unrepairable.
\ - add_producer: the packet names only the flagged consumer and the
\   expected/actual rows; it has no producer token or insertion point ->
\   unrepairable (packet-capability gap, tracked by dot).
\ - fix_type: the packet has expected/actual rows and a prose suggestion but
\   no typed replacement token -> unrepairable (gap, tracked by dot).
\ Any other class (unknown_rejection, rewrite_uncheckable, ...) has no modeled
\ edit and reports unrepairable.

require lib/errors.f
require lib/memory.f
require tools/argv.f
require tools/json.f
require tools/repair-packet-core.f
require maki/eval-repair-loop.f

\ eval-repair-mech owns error codes -5240..-5243.
-5240 constant E-MECH-SRC-CAP   \ candidate exceeds the working-source capacity
-5241 constant E-MECH-PKT-CAP   \ repair packet exceeds the packet buffer
-5242 constant E-MECH-PACKET    \ packet missing a required field
-5243 constant E-MECH-NUM       \ non-decimal byte in a packet number field

package EVAL

$2000 constant MECH-SRC-CAP
$8000 constant MECH-DIAG-CAP
$8000 constant MECH-PKT-CAP
64 constant MECH-CLASS-CAP
128 constant MECH-TOK-CAP

variable MECH-SRC-A   variable MECH-SRC-U
variable MECH-DIAG-A
variable MECH-PKT-A   variable MECH-PKT-U
variable MECH-CLASS-U variable MECH-TOK-U
variable MECH-BS      variable MECH-BE      variable MECH-TIX
variable MECH-RS-EXP  variable MECH-RS-ACT
variable MECH-D

create MECH-CLASS-BUF MECH-CLASS-CAP allot
create MECH-TOK-BUF MECH-TOK-CAP allot

: MECH-PTR-U8-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: MECH-PTR-U8@ ( ptr a -- ptr u8 )
   MECH-PTR-U8-FIELD @ ;

: MECH-PTR-U8! ( ptr u8 ptr a -- )
   MECH-PTR-U8-FIELD ! ;

: MECH-BUF@ ( ptr a n -- ptr u8 ) {: slot:ptr cap:n :}
   slot @ 0= if cap MEM-ALLOC-BYTES drop slot MECH-PTR-U8! then
   slot MECH-PTR-U8@ ;

: MECH-SRC ( -- ptr u8 )
   MECH-SRC-A MECH-SRC-CAP MECH-BUF@ ;

: MECH-DIAG ( -- ptr u8 )
   MECH-DIAG-A MECH-DIAG-CAP MECH-BUF@ ;

: MECH-PKT ( -- ptr u8 )
   MECH-PKT-A MECH-PKT-CAP MECH-BUF@ ;

: MECH-SRC! ( ptr u8 n -- ) {: a:ptr u:n :}
   u MECH-SRC-CAP > if E-MECH-SRC-CAP throw then
   a MECH-SRC u BYTE-COPY
   u MECH-SRC-U ! ;

public

: MECH-SOURCE$ ( -- ptr u8 n )
   MECH-SRC MECH-SRC-U @ ;

: MECH-CLASS$ ( -- ptr u8 n )
   MECH-CLASS-BUF MECH-CLASS-U @ ;

private

\ run the current candidate with JSON diagnostics captured into MECH-DIAG
: MECH-CHECK ( -- n )
   s" <mech>" DIAG-FILE!
   1 1 0 DIAG-ORIGIN!
   0 0= DIAG-JSON!
   MECH-DIAG MECH-DIAG-CAP DIAG-BUFFER!
   MECH-SOURCE$ CHECK-CANDIDATE! ;

\ restore the engine diagnostic defaults (off, <input>, origin 1 1 0)
: MECH-CHECK-DONE ( -- )
   DIAG-BUFFER-OFF
   s" <input>" DIAG-FILE!
   1 1 0 DIAG-ORIGIN!
   0 0= 0= DIAG-JSON! ;

\ captured diagnostic JSONL -> repair-packet bytes -> MECH-PKT
: MECH-PACKET! ( -- )
   DIAG-BUFFER$ 2dup RP-COUNT >r RP-FIRST r> RP-PACKET {: a:ptr u:n :}
   u MECH-PKT-CAP > if E-MECH-PKT-CAP throw then
   a MECH-PKT u BYTE-COPY
   u MECH-PKT-U ! ;

\ non-negative decimal (packet number fields) -> n
: MECH-DEC ( ptr u8 n -- n ) {: a:ptr u:n :}
   0 MECH-D !
   0 begin dup u < while
      dup a + c@
      dup 48 < over 57 > or if E-MECH-NUM throw then
      MECH-D @ 10 * + 48 - MECH-D !
      1+
   repeat drop
   MECH-D @ ;

\ required packet field
: MECH-REQ ( n ptr u8 n -- n )
   JSON-GET dup -1 = if E-MECH-PACKET throw then ;

: MECH-CLASS! ( ptr u8 n -- ) {: a:ptr u:n :}
   u MECH-CLASS-CAP > if E-MECH-PACKET throw then
   a MECH-CLASS-BUF u BYTE-COPY
   u MECH-CLASS-U ! ;

: MECH-TOK! ( ptr u8 n -- ) {: a:ptr u:n :}
   u MECH-TOK-CAP > if E-MECH-PACKET throw then
   a MECH-TOK-BUF u BYTE-COPY
   u MECH-TOK-U ! ;

: MECH-TOK$ ( -- ptr u8 n )
   MECH-TOK-BUF MECH-TOK-U @ ;

\ packet number field -> n
: MECH-NUM@ ( n ptr u8 n -- n )
   MECH-REQ JSON-NUMBER$ MECH-DEC ;

\ return-stack row -> cell count (null row -> 0)
: MECH-ROW-CELLS ( n -- n ) {: node:n :}
   node JSON-NULL? if 0 exit then
   node JSON-STRING$ COUNT-TOKS ;

: MECH-RS! ( n -- ) {: rs:n :}
   rs s" expected" MECH-REQ MECH-ROW-CELLS MECH-RS-EXP !
   rs s" actual" MECH-REQ MECH-ROW-CELLS MECH-RS-ACT ! ;

\ parse MECH-PKT and pull the machine-actionable fields
: MECH-EXTRACT ( -- )
   MECH-PKT MECH-PKT-U @ JSON-PARSE {: root:n :}
   root s" repair_class" MECH-REQ JSON-STRING$ MECH-CLASS!
   root s" token" MECH-REQ JSON-STRING$ MECH-TOK!
   root s" token_index" MECH-NUM@ MECH-TIX !
   root s" byte_start" MECH-NUM@ MECH-BS !
   root s" byte_end" MECH-NUM@ MECH-BE !
   root s" return_stack" MECH-REQ MECH-RS! ;

\ capture the rejection diagnostic, build its packet, parse the fields.
\ The re-check verdict is dropped: the loop already holds the rejection from
\ REPAIR-STEP; this run only exists to capture the diagnostic.
: MECH-DIAGNOSE ( -- )
   MECH-CHECK drop
   MECH-PACKET!
   MECH-CHECK-DONE
   MECH-EXTRACT ;

: MECH-SPAN$ ( -- ptr u8 n )
   MECH-SRC MECH-BS @ +  MECH-BE @ MECH-BS @ - ;

\ span sanity: inside the source, nonempty, and not the definition-name token
\ (token_index 0: the surplus has no deletable body producer)
: MECH-SPAN-VALID? ( -- bool )
   MECH-TIX @ 0 >
   MECH-BS @ 0 >= and
   MECH-BE @ MECH-BS @ > and
   MECH-BE @ MECH-SRC-U @ <= and ;

\ valid span whose source bytes equal the packet token
: MECH-SPAN-OK? ( -- bool )
   MECH-SPAN-VALID? 0= if 0 0= 0= exit then
   MECH-SPAN$ MECH-TOK$ STR= ;

\ delete [byte_start, byte_end) from the working source
: MECH-DELETE ( -- )
   MECH-BE @ begin dup MECH-SRC-U @ < while
      dup MECH-SRC + c@
      over MECH-BE @ - MECH-BS @ + MECH-SRC + c!
      1+
   repeat drop
   MECH-SRC-U @ MECH-BE @ - MECH-BS @ + MECH-SRC-U ! ;

: MECH-REMOVE? ( -- bool )
   MECH-CLASS$ s" remove_producer" STR= ;

: MECH-RSTACK? ( -- bool )
   MECH-CLASS$ s" fix_return_stack" STR= ;

\ surplus return-stack transfer: actual row deeper than expected
: MECH-RS-SURPLUS? ( -- bool )
   MECH-RS-ACT @ MECH-RS-EXP @ > ;

\ one packet-directed edit; false = the class carries too little to edit
\ (add_producer: no producer token; fix_type: no replacement token; others:
\ no modeled edit)
: MECH-APPLY ( -- bool )
   MECH-REMOVE? if
      MECH-SPAN-OK? dup if MECH-DELETE then
      exit
   then
   MECH-RSTACK? if
      MECH-RS-SURPLUS? MECH-SPAN-OK? and dup if MECH-DELETE then
      exit
   then
   0 0= 0= ;

public

0 constant MECH-GREEN          \ certified within the round cap
1 constant MECH-UNREPAIRABLE   \ packet class carries too little to edit
2 constant MECH-CAPPED         \ MECH-MAX-ROUNDS rejections, gave up
8 constant MECH-MAX-ROUNDS     \ rejection budget of one mechanical run

\ the mechanical loop: candidate -> REPAIR-STEP (shared metric engine) -> if
\ rejected: packet -> edit -> re-check. Read REPAIR-ROUNDS@ / REPAIR-TOKENS@ /
\ REPAIR-GREEN? after this exactly as after an author-trajectory run.
: MECH-REPAIR ( ptr u8 n -- n )
   MECH-SRC!
   REPAIR-RESET
   begin
      MECH-SOURCE$ REPAIR-STEP
      REPAIR-GREEN? if MECH-GREEN exit then
      REPAIR-ROUNDS@ MECH-MAX-ROUNDS >= if MECH-CAPPED exit then
      MECH-DIAGNOSE
      MECH-APPLY 0= if MECH-UNREPAIRABLE exit then
   again ;

end-package
