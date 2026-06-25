\ json-only-core.f - JSON diagnostic line filter core.
\ Load after lib/errors.f, lib/memory.f, and tools/json.f.

\ Checked filter. Raw fd primitives are used through checked effects.

10 constant JSON-ONLY-LF-C
74 constant JSON-ONLY-E-IO

create JSON-ONLY-LF 1 allot

variable JSON-ONLY-JSON-FD
variable JSON-ONLY-PROSE-FD

: JSON-ONLY-FDS! ( fd fd -- ) {: jsonfd prosefd :}
   jsonfd JSON-ONLY-JSON-FD !
   prosefd JSON-ONLY-PROSE-FD ! ;

: JSON-ONLY-DEFAULT-FDS ( -- )
   1 >FD 2 >FD JSON-ONLY-FDS! ;

: JSON-ONLY-WRITE ( n ptr u8 n -- ) {: fd a:ptr u :}
   u 0= IF exit THEN
   fd a u write u <> IF s" json-only: write failed" JSON-ONLY-E-IO die THEN ;

: JSON-ONLY-LF$ ( -- ptr u8 n )
   JSON-ONLY-LF-C JSON-ONLY-LF c!
   JSON-ONLY-LF 1 ;

: JSON-ONLY-PROSE ( ptr u8 n -- )
   {: a:ptr u :}
   JSON-ONLY-PROSE-FD @ a u JSON-ONLY-WRITE ;

: JSON-ONLY-JSON-LINE ( -- )
   JSON-ONLY-JSON-FD @ JSONL-LA@ JSONL-LU @ JSON-ONLY-WRITE
   JSON-ONLY-JSON-FD @ JSON-ONLY-LF$ JSON-ONLY-WRITE ;

: JSON-ONLY-NEXT? ( -- bool )
   JSONL-NEXT-OBJECT dup -1 = IF drop 0 0= 0= ELSE drop 0 0= THEN ;

: JSON-ONLY-START? ( ptr u8 n -- bool )
   JSONL-START
   JSON-ONLY-NEXT? ;

: JSON-ONLY-EMIT-FOUND ( -- )
   begin
      JSON-ONLY-JSON-LINE
      JSON-ONLY-NEXT? 0=
   until ;

: JSON-ONLY-FILTER ( ptr u8 n -- )
   2dup JSON-ONLY-START? IF
      2drop
      JSON-ONLY-EMIT-FOUND
   ELSE
      JSON-ONLY-PROSE
   THEN ;
