\ diag-json-escape.f - a --json-errors packet escapes every control byte.
\
\ RFC 8259 section 7 forbids a raw byte below 0x20 inside a JSON string, and a
\ client's parser refuses the whole packet for one. A byte below 0x20 inside a
\ stack effect does not end its token, so `( n<c>-- n )` carries every byte c
\ from 0x00 to 0x1F into the token of an unknown-signature-type packet.
\
\ Every packet stream must parse with the strict JSONL reader, and its "token"
\ must decode to the four bytes n<c>--.

require test/gate-common.f
require tools/json.f
require tools/check-core.f

package DIAG-JSON-ESCAPE

32 constant CONTROL-END                  \ the first byte a JSON string holds raw

variable BYTE

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
   CHECK:RUN throw ;

: CHECK-JSON ( -- )
   CHECK:RESET
   s" json-errors" CHECK:OPT
   [: CHECK-ACT ;] GE-CAPTURE-ACTION OUTCOME:EXITED GT-OUTCOME!
   GT-RC@ 0= if s" the check tool accepted the rejected fixture" FAIL then ;

: TOKEN-FIELD ( n -- )
   {: root:n :}
   root s" token" JSON-GET {: node:n :}
   node -1 = if s" packet has no token field" FAIL then
   node JSON-KIND J-STR <> if s" packet token field is not a string" FAIL then
   SB-RESET  s" n" SB-APPEND  BYTE @ SB-APPEND-C  s" --" SB-APPEND
   node JSON-STRING$ SB$ STR= 0= if
      s" packet token field does not decode to n<c>--" FAIL
   then ;

\ Every row of the stream parses strictly; the first object is the packet.
: STREAM ( -- )
   GT-ERR$ JSONL-START-STRICT
   JSONL-NEXT-OBJECT {: root:n :}
   root -1 = if s" stream holds no packet" FAIL then
   root TOKEN-FIELD
   begin JSONL-NEXT-OBJECT -1 <> while repeat ;

: STRICT ( -- )
   [: STREAM ;] catch 0 <> if s" packet stream is not strict JSONL" FAIL then ;

: ONE ( n -- )
   FIXTURE!
   CHECK-JSON
   STRICT ;

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
