\ json-only-core.f - JSON diagnostic line filter core.
\ Load after lib/errors.f, lib/memory.f, and tools/json.f.

\ Checked filter. Raw fd primitives are used through checked effects.

10 constant JSON-ONLY-LF-C
74 constant JSON-ONLY-E-IO

create JSON-ONLY-LF 1 allot

variable JSON-ONLY-JSON-FD
variable JSON-ONLY-PROSE-FD
variable JSON-ONLY-JSON-A
variable JSON-ONLY-JSON-CAP
variable JSON-ONLY-JSON-U
variable JSON-ONLY-PROSE-A
variable JSON-ONLY-PROSE-CAP
variable JSON-ONLY-PROSE-U
variable JSON-ONLY-BUF-ON

: JSON-ONLY-BUF-PTR-FIELD ( ptr a -- ptr ptr u8 )
   0 ptr-field ;

: JSON-ONLY-BUF-PTR@ ( ptr a -- ptr u8 )
   JSON-ONLY-BUF-PTR-FIELD @ ;

: JSON-ONLY-BUF-PTR! ( ptr u8 ptr a -- )
   JSON-ONLY-BUF-PTR-FIELD ! ;

: JSON-ONLY-JSON-A@ ( -- ptr u8 )
   JSON-ONLY-JSON-A JSON-ONLY-BUF-PTR@ ;

: JSON-ONLY-PROSE-A@ ( -- ptr u8 )
   JSON-ONLY-PROSE-A JSON-ONLY-BUF-PTR@ ;

: JSON-ONLY-JSON-A! ( ptr u8 -- )
   JSON-ONLY-JSON-A JSON-ONLY-BUF-PTR! ;

: JSON-ONLY-PROSE-A! ( ptr u8 -- )
   JSON-ONLY-PROSE-A JSON-ONLY-BUF-PTR! ;

: JSON-ONLY-FDS! ( fd fd -- ) {: jsonfd prosefd :}
   jsonfd JSON-ONLY-JSON-FD !
   prosefd JSON-ONLY-PROSE-FD ! ;

: JSON-ONLY-DEFAULT-FDS ( -- )
   1 >FD 2 >FD JSON-ONLY-FDS! ;

: JSON-ONLY-BUFFERS! ( ptr u8 n ptr u8 n -- ) {: json:ptr jsonu:n prose:ptr proseu:n :}
   json JSON-ONLY-JSON-A!
   jsonu JSON-ONLY-JSON-CAP !
   0 JSON-ONLY-JSON-U !
   prose JSON-ONLY-PROSE-A!
   proseu JSON-ONLY-PROSE-CAP !
   0 JSON-ONLY-PROSE-U !
   0 0= JSON-ONLY-BUF-ON ! ;

: JSON-ONLY-BUFFERS-OFF ( -- )
   0 0= 0= JSON-ONLY-BUF-ON ! ;

: JSON-ONLY-JSON$ ( -- ptr u8 n )
   JSON-ONLY-JSON-A@ JSON-ONLY-JSON-U @ ;

: JSON-ONLY-PROSE$ ( -- ptr u8 n )
   JSON-ONLY-PROSE-A@ JSON-ONLY-PROSE-U @ ;

: JSON-ONLY-BUF-COPY ( ptr u8 ptr u8 n -- )
   {: a:ptr dst:ptr u:n :}
   0 begin dup u < while
      dup a + c@ over dst + c!
      1+
   repeat drop ;

: JSON-ONLY-APPEND ( ptr u8 n ptr u8 n ptr n -- )
   {: a:ptr u:n dst:ptr cap:n lenp:ptr :}
   lenp @ u + cap > IF s" json-only: output buffer full" JSON-ONLY-E-IO die THEN
   a dst lenp @ + u JSON-ONLY-BUF-COPY
   lenp @ u + lenp ! ;

: JSON-ONLY-JSON-APPEND ( ptr u8 n -- )
   JSON-ONLY-JSON-A@ JSON-ONLY-JSON-CAP @ JSON-ONLY-JSON-U JSON-ONLY-APPEND ;

: JSON-ONLY-PROSE-APPEND ( ptr u8 n -- )
   JSON-ONLY-PROSE-A@ JSON-ONLY-PROSE-CAP @ JSON-ONLY-PROSE-U JSON-ONLY-APPEND ;

: JSON-ONLY-WRITE ( n ptr u8 n -- ) {: fd a:ptr u :}
   u 0= IF exit THEN
   fd a u write u <> IF s" json-only: write failed" JSON-ONLY-E-IO die THEN ;

: JSON-ONLY-LF$ ( -- ptr u8 n )
   JSON-ONLY-LF-C JSON-ONLY-LF c!
   JSON-ONLY-LF 1 ;

: JSON-ONLY-PROSE ( ptr u8 n -- )
   {: a:ptr u :}
   JSON-ONLY-BUF-ON @ IF a u JSON-ONLY-PROSE-APPEND exit THEN
   JSON-ONLY-PROSE-FD @ a u JSON-ONLY-WRITE ;

: JSON-ONLY-JSON-LINE ( -- )
   JSON-ONLY-BUF-ON @ IF
      JSONL-LA@ JSONL-LU @ JSON-ONLY-JSON-APPEND
      JSON-ONLY-LF$ JSON-ONLY-JSON-APPEND
      exit
   THEN
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
