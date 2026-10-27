\ json-only-core.f - JSON diagnostic line filter core.

require src/lib/errors.f
require src/lib/memory.f
require src/lib/jsonl.f
require src/lib/string.f

\ Checked filter. Raw fd primitives are used through checked effects.

10 constant JSON-ONLY-LF-C
74 constant JSON-ONLY-E-IO

create JSON-ONLY-LF 1 allot
TYPED-VARIABLE JSON-ONLY-READER JR:cursor

variable JSON-ONLY-JSON-FD
variable JSON-ONLY-PROSE-FD
variable JSON-ONLY-JSON-A
variable JSON-ONLY-JSON-CAP
variable JSON-ONLY-JSON-U
variable JSON-ONLY-PROSE-A
variable JSON-ONLY-PROSE-CAP
variable JSON-ONLY-PROSE-U
variable JSON-ONLY-BUF-ON
TYPED-VARIABLE JSON-ONLY-SOURCE ptr u8
variable JSON-ONLY-SOURCE-U
variable JSON-ONLY-POS
TYPED-VARIABLE JSON-ONLY-LINE ptr u8
variable JSON-ONLY-LINE-U
variable JSON-ONLY-ROW-OBJECT

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

\ The two buffers are the caller's and cannot grow, so text that does not fit
\ is refused whole by E-DIAG-CAPACITY, as the all-errors core refuses its
\ report buffer, and the buffer keeps everything before it.
: JSON-ONLY-BUF-ROOM ( n n ptr n -- )
   {: need:n cap:n lenp:ptr :}
   lenp @ need + cap > IF E-DIAG-CAPACITY throw THEN ;

: JSON-ONLY-APPEND ( ptr u8 n ptr u8 n ptr n -- )
   {: a:ptr u:n dst:ptr cap:n lenp:ptr :}
   u cap lenp JSON-ONLY-BUF-ROOM
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
      JSON-ONLY-LINE-U @ 1+ JSON-ONLY-JSON-CAP @ JSON-ONLY-JSON-U JSON-ONLY-BUF-ROOM
      JSON-ONLY-LINE @ JSON-ONLY-LINE-U @ JSON-ONLY-JSON-APPEND
      JSON-ONLY-LF$ JSON-ONLY-JSON-APPEND
      exit
   THEN
   JSON-ONLY-JSON-FD @ JSON-ONLY-LINE @ JSON-ONLY-LINE-U @ JSON-ONLY-WRITE
   JSON-ONLY-JSON-FD @ JSON-ONLY-LF$ JSON-ONLY-WRITE ;

: JSON-ONLY-OBJECT ( -- )
   JSON-ONLY-READER JSON-ONLY-LINE @ JSON-ONLY-LINE-U @ JR:INIT
   JR:NEXT {: kind:n :}
   begin JR:NEXT dup JR:T-END <> while drop repeat drop
   JR:CLOSE
   kind JR:T-OBJ = JSON-ONLY-ROW-OBJECT ! ;

: JSON-ONLY-OBJECT? ( -- bool )
   [: JSON-ONLY-OBJECT ;] catch
   dup 0= if drop JSON-ONLY-ROW-OBJECT @ 0<> exit then
   dup E-JR-MALFORMED = over E-JR-STRING = or
   over E-JR-ESCAPE = or over E-JR-SURROGATE = or
   over E-JR-TRAILING = or over E-JR-NUMBER = or
   over E-JR-EOF = or over E-JR-COLON = or
   over E-JR-COMMA = or if drop false exit then
   throw ;

: JSON-ONLY-NEXT? ( -- bool )
   begin
      JSON-ONLY-SOURCE @ JSON-ONLY-SOURCE-U @ JSON-ONLY-POS @ JSONL:SPLIT-LINE
      0= if 2drop drop false exit then
      JSON-ONLY-POS !
      TRIM JSON-ONLY-LINE-U ! JSON-ONLY-LINE !
      JSON-ONLY-LINE-U @ 0 > if JSON-ONLY-OBJECT? if true exit then then
   again ;

: JSON-ONLY-START? ( ptr u8 n -- bool )
   JSON-ONLY-SOURCE-U ! JSON-ONLY-SOURCE !
   0 JSON-ONLY-POS !
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
