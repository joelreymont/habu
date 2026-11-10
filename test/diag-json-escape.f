\ diag-json-escape.f - a --json-errors packet escapes every control byte. RFC
\ 8259 section 7 forbids a raw byte below 0x20 in a JSON string, and a client
\ refuses the whole packet for one. A control byte is a blank inside a stack
\ effect, so none reaches a token, and `( n<c>jesc-nosuch -- n )` carries each
\ c from 0x00 to 0x1F into an unknown-signature-type packet's source text.
\ Every stream must parse with the strict JSONL reader, its "definition_source"
\ must decode back to the bytes written and its "token" to jesc-nosuch alone.

require src/lib/string.f
require test/gate-common.f
require src/lib/jsonl.f
require tools/check-core.f

package DIAG-JSON-ESCAPE

32 constant CONTROL-END                  \ the first byte a JSON string holds raw

variable BYTE
variable POS
variable COUNT
TYPED-VARIABLE READER-STORAGE JSON:cursor

\ The byte sits between the type names n and jesc-nosuch, so an escape that
\ swallowed or split a neighbour would not decode back to it.
: FIXTURE! ( n -- )
   {: c:n :}
   c BYTE !
   GE-SRC-RESET
   s" : JESC-BAD ( n" GE-SRC+
   c GE-SRC-C
   s" jesc-nosuch -- n ) dup ;" GE-SRC-LINE ;

: FAIL ( ptr u8 n -- )
   {: msg:ptr msgu:n :}
   s" control byte " type BYTE @ .
   msg msgu GE-FAIL ;

: CHECK-ACT ( -- )
   GE-SRC-BUF GE-SRC-U @ s" json-escape.f" LSP-CHECK:SOURCE
   LSP-CHECK:RUN dup 0 <> if throw then drop ;

: CHECK-JSON ( -- )
   LSP-CHECK:RESET
   s" json-errors" LSP-CHECK:OPT
   [: CHECK-ACT ;] GE-CAPTURE-ACTION
   GT-RC@ 0= if s" the check tool accepted the rejected fixture" FAIL then ;

\ "token" precedes "definition_source" in the packet, and READ-FIND-KEY only
\ reads forward.
: TOKEN-AND-SOURCE ( ptr u8 n -- )
   READER-STORAGE -rot JSON:READ-INIT JSON:READ-NEXT drop
   s" token" JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE s" packet has no token field" FAIL then
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE s" packet token field is not a string" FAIL then
   s" jesc-nosuch" JSON:READ-STR-EQ? 0= if JSON:READ-CLOSE
      s" packet token field does not decode to jesc-nosuch alone" FAIL
   then JSON:READ-SKIP-VALUE
   s" definition_source" JSON:READ-FIND-KEY 0= if JSON:READ-CLOSE
      s" packet has no definition_source field" FAIL
   then
   JSON:READ-TOKEN JSON:T-STR <> if JSON:READ-CLOSE
      s" packet definition_source field is not a string" FAIL
   then
   SB-RESET  s" JESC-BAD ( n" SB-APPEND  BYTE @ SB-APPEND-C
   s" jesc-nosuch -- n ) dup " SB-APPEND
   SB$ JSON:READ-STR-EQ? 0= if JSON:READ-CLOSE
      s" packet definition_source field does not decode to the source written" FAIL
   then JSON:READ-CLOSE ;

: BLANK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 ?do
      a i + c@ dup 32 = over 9 = or over 13 = or swap 10 = or
      0= if false unloop exit then
   loop true ;

: ROW ( ptr u8 n bool -- )
   {: a:ptr u:n first:bool :}
   READER-STORAGE a u JSON:READ-INIT JSON:READ-NEXT JSON:T-OBJ <> if
      JSON:READ-CLOSE s" packet stream has a non-object" FAIL
   then
   JSON:READ-SKIP-VALUE JSON:READ-NEXT JSON:T-END <> if JSON:READ-CLOSE E-JSON-READ-TRAILING throw then
   JSON:READ-CLOSE
   first if a u TOKEN-AND-SOURCE then ;

\ Every row of the stream parses strictly; the first object is the packet.
: STREAM ( -- )
   GT-ERR$ {: a:ptr u:n :}
   0 POS !
   0 COUNT !
   begin POS @ u < while
      a u POS @ JSONL:SPLIT-LINE drop {: line:ptr lu:n next:n :}
      line lu BLANK? 0= if
         line lu COUNT @ 0= ROW
         COUNT @ 1+ COUNT !
      then
      next POS !
   repeat
   COUNT @ 0= if s" stream holds no packet" FAIL then ;

: ONE ( n -- )
   FIXTURE!
   CHECK-JSON
   STREAM ;

: ALL ( -- )
   s" every control byte in a packet's source text" GT-PROGRESS-RUN
   CONTROL-END 0 ?do i ONE loop
   s" every control byte in a packet's source text" GT-PROGRESS-PASS ;

public

: RUN ( -- )
   s" hb-diag-json-escape" GT-START
   ALL
   GT-CLEANUP
   s" PASS: --json-errors control-byte escapes" type cr ;

;package

DIAG-JSON-ESCAPE:RUN
