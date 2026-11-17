\ json-only-core.f - JSON diagnostic line filter core.

require src/lib/error.f
require src/lib/jsonl.f
require src/lib/string.f
require src/lib/fd.f
require src/core/exit-status.f

10 constant JSON-ONLY-LF-C

create JSON-ONLY-LF 1 allot
TYPED-VARIABLE JSON-ONLY-READER JSON:cursor

variable JSON-ONLY-JSON-FD
variable JSON-ONLY-PROSE-FD
TYPED-VARIABLE JSON-ONLY-SOURCE ^u8
variable JSON-ONLY-SOURCE-U
variable JSON-ONLY-POS
TYPED-VARIABLE JSON-ONLY-LINE ^u8
variable JSON-ONLY-LINE-U
variable JSON-ONLY-ROW-OBJECT

: JSON-ONLY-FDS! ( fd fd -- ) {: jsonfd prosefd :}
   jsonfd JSON-ONLY-JSON-FD !
   prosefd JSON-ONLY-PROSE-FD ! ;

: JSON-ONLY-DEFAULT-FDS ( -- )
   FD:STDOUT FD:STDERR JSON-ONLY-FDS! ;

: JSON-ONLY-WRITE ( n ^u8 n -- ) {: fd a:^ u :}
   u 0= IF exit THEN
   fd a u write u <> IF s" json-only: write failed" EXIT-STATUS:IOERR die THEN ;

: JSON-ONLY-LF$ ( -- ^u8 n )
   JSON-ONLY-LF-C JSON-ONLY-LF c!
   JSON-ONLY-LF 1 ;

: JSON-ONLY-PROSE ( ^u8 n -- )
   {: a:^ u :}
   JSON-ONLY-PROSE-FD @ a u JSON-ONLY-WRITE ;

: JSON-ONLY-JSON-LINE ( -- )
   JSON-ONLY-JSON-FD @ JSON-ONLY-LINE @ JSON-ONLY-LINE-U @ JSON-ONLY-WRITE
   JSON-ONLY-JSON-FD @ JSON-ONLY-LF$ JSON-ONLY-WRITE ;

: JSON-ONLY-OBJECT ( -- )
   JSON-ONLY-READER JSON-ONLY-LINE @ JSON-ONLY-LINE-U @ JSON:READ-INIT
   JSON:READ-NEXT {: kind:n :}
   begin JSON:READ-NEXT dup JSON:T-END <> while drop repeat drop
   JSON:READ-CLOSE
   kind JSON:T-OBJ = JSON-ONLY-ROW-OBJECT ! ;

: JSON-ONLY-OBJECT? ( -- bool )
   [: JSON-ONLY-OBJECT ;] catch
   dup 0= if drop JSON-ONLY-ROW-OBJECT @ 0<> exit then
   dup E-JSON-READ-MALFORMED = over E-JSON-READ-STRING = or
   over E-JSON-READ-ESCAPE = or over E-JSON-READ-SURROGATE = or
   over E-JSON-READ-TRAILING = or over E-JSON-READ-NUMBER = or
   over E-JSON-READ-EOF = or over E-JSON-READ-COLON = or
   over E-JSON-READ-COMMA = or if drop false exit then
   throw ;

: JSON-ONLY-NEXT? ( -- bool )
   begin
      JSON-ONLY-SOURCE @ JSON-ONLY-SOURCE-U @ JSON-ONLY-POS @ JSONL:SPLIT-LINE
      0= if 2drop drop false exit then
      JSON-ONLY-POS !
      TRIM JSON-ONLY-LINE-U ! JSON-ONLY-LINE !
      JSON-ONLY-LINE-U @ 0 > if JSON-ONLY-OBJECT? if true exit then then
   again ;

: JSON-ONLY-START? ( ^u8 n -- bool )
   JSON-ONLY-SOURCE-U ! JSON-ONLY-SOURCE !
   0 JSON-ONLY-POS !
   JSON-ONLY-NEXT? ;

: JSON-ONLY-EMIT-FOUND ( -- )
   begin
      JSON-ONLY-JSON-LINE
      JSON-ONLY-NEXT? 0=
   until ;

: JSON-ONLY-FILTER ( ^u8 n -- )
   2dup JSON-ONLY-START? IF
      2drop
      JSON-ONLY-EMIT-FOUND
   ELSE
      JSON-ONLY-PROSE
   THEN ;
