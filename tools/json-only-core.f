\ json-only-core.f - JSON diagnostic line filter core.

require src/lib/error.f
require src/lib/json.f
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

: JSON-ONLY-WRITE ( n str -- ) {: fd s:str :}
   s STR:LEN 0= IF exit THEN
   fd s write s STR:LEN <> IF
      s" json-only: write failed" EXIT-STATUS:IOERR die
   THEN ;


: JSON-ONLY-LF$ ( -- str )
   JSON-ONLY-LF-C JSON-ONLY-LF c!
   JSON-ONLY-LF 1 >str ;

: JSON-ONLY-PROSE ( str -- )
   {: s:str :}
   JSON-ONLY-PROSE-FD @ s JSON-ONLY-WRITE ;

: JSON-ONLY-JSON-LINE ( -- )
   JSON-ONLY-JSON-FD @ JSON-ONLY-LINE @ JSON-ONLY-LINE-U @ >str
   JSON-ONLY-WRITE
   JSON-ONLY-JSON-FD @ JSON-ONLY-LF$ JSON-ONLY-WRITE ;

: JSON-ONLY-OBJECT ( -- )
   JSON-ONLY-READER JSON-ONLY-LINE @ JSON-ONLY-LINE-U @ >str
   JSON:READ-INIT
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
      JSON-ONLY-SOURCE @ JSON-ONLY-SOURCE-U @ >str
      JSON-ONLY-POS @ JSONL:SPLIT-LINE
      0= if drop drop false exit then
      JSON-ONLY-POS !
      STR:TRIM dup STR:LEN JSON-ONLY-LINE-U ! STR:DATA JSON-ONLY-LINE !
      JSON-ONLY-LINE-U @ 0 > if JSON-ONLY-OBJECT? if true exit then then
   again ;

: JSON-ONLY-START? ( str -- bool )
   dup STR:LEN JSON-ONLY-SOURCE-U ! STR:DATA JSON-ONLY-SOURCE !
   0 JSON-ONLY-POS !
   JSON-ONLY-NEXT? ;

: JSON-ONLY-EMIT-FOUND ( -- )
   begin
      JSON-ONLY-JSON-LINE
      JSON-ONLY-NEXT? 0=
   until ;

: JSON-ONLY-FILTER ( str -- )
   dup JSON-ONLY-START? IF
      drop
      JSON-ONLY-EMIT-FOUND
   ELSE
      JSON-ONLY-PROSE
   THEN ;
