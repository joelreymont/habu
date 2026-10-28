\ diag-json-escape.f - a --json-errors packet escapes every control byte.
\
\ RFC 8259 section 7 forbids a raw byte below 0x20 inside a JSON string, and a
\ client's parser refuses the whole packet for one. A byte below 0x20 inside a
\ stack effect does not end its token, so `( n<c>-- n )` carries every byte c
\ from 0x00 to 0x1F into the token of an unknown-signature-type packet.
\
\ Every packet stream must parse with the strict JSONL reader, and its "token"
\ must decode to the four bytes n<c>--.

require src/lib/string.f
require test/gate-common.f
require src/lib/jsonl.f
require tools/check-core.f

package DIAG-JSON-ESCAPE

32 constant CONTROL-END                  \ the first byte a JSON string holds raw

variable BYTE
variable POS
variable COUNT
TYPED-VARIABLE READER-STORAGE JR:cursor

\ The byte sits inside the token n<c>--, so an escape that swallowed or split a
\ neighbour would not decode back to it.
: FIXTURE! ( n -- )
   {: c:n :}
   c BYTE !
   GE-SRC-RESET
   s" : JESC-BAD ( n" GE-SRC+
   c GE-SRC-C
   s" -- n ) dup ;" GE-SRC-LINE ;

: FAIL ( ptr u8 n -- )
   {: msg:ptr msgu:n :}
   s" control byte " type BYTE @ .
   msg msgu GE-FAIL ;

: CHECK-ACT ( -- )
   GE-SRC-BUF GE-SRC-U @ s" json-escape.f" CHECK:SOURCE
   CHECK:RUN dup 0 <> if throw then drop ;

: CHECK-JSON ( -- )
   CHECK:RESET
   s" json-errors" CHECK:OPT
   [: CHECK-ACT ;] GE-CAPTURE-ACTION
   GT-RC@ 0= if s" the check tool accepted the rejected fixture" FAIL then ;

: TOKEN-FIELD ( ptr u8 n -- )
   READER-STORAGE -rot JR:INIT JR:NEXT drop
   s" token" JR:FIND-KEY 0= if JR:CLOSE s" packet has no token field" FAIL then
   JR:TOKEN JR:T-STR <> if JR:CLOSE s" packet token field is not a string" FAIL then
   SB-RESET  s" n" SB-APPEND  BYTE @ SB-APPEND-C  s" --" SB-APPEND
   SB$ JR:STR-EQ? 0= if JR:CLOSE
      s" packet token field does not decode to n<c>--" FAIL
   then JR:CLOSE ;

: BLANK? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 ?do
      a i + c@ dup 32 = over 9 = or over 13 = or swap 10 = or
      0= if false unloop exit then
   loop true ;

: ROW ( ptr u8 n bool -- )
   {: a:ptr u:n first:bool :}
   READER-STORAGE a u JR:INIT JR:NEXT JR:T-OBJ <> if
      JR:CLOSE s" packet stream has a non-object" FAIL
   then
   JR:SKIP-VALUE JR:NEXT JR:T-END <> if JR:CLOSE E-JR-TRAILING throw then
   JR:CLOSE
   first if a u TOKEN-FIELD then ;

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
   s" every control byte in a packet token" GT-PROGRESS-RUN
   CONTROL-END 0 ?do i ONE loop
   s" every control byte in a packet token" GT-PROGRESS-PASS ;

public

: RUN ( -- )
   s" hb-diag-json-escape" GT-START
   ALL
   GT-CLEANUP
   s" PASS: --json-errors control-byte escapes" type cr ;

;package

DIAG-JSON-ESCAPE:RUN
