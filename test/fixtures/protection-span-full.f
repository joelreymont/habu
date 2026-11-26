\ protection-span-full.f - fills the image's code space with ordinary
\ definitions to one empty word below the ceiling, checks a body and a long name
\ that would reach the ceiling are rejected where an empty word still fits, then
\ that `:`, `create` and `constant` are each rejected E-CODE-SPACE-FULL at the
\ ceiling. A word that would pass the ceiling is rejected EXIT-STATUS:FULL and
\ leaves nothing. `2 0 ?do loop` is much code per token read, so the fill takes
\ seconds.
\
\ The fill places bodies of 100 units, then of half as many, down to one unit,
\ each while the room after it keeps PST-MARGIN. A body's bytes depend on the
\ target's code and alignment, so one of each size is placed first and its
\ bytes are read off cp@. The room left need not be a whole number of empty
\ words, so one empty word with a long name takes the remainder
\ (CODE-SPACE:SPAN gives its name's bytes) before empty words fill the room
\ down to one.

require src/lib/test/assert.f
require src/lib/test/eval.f

$800 constant PST-CAP
create PST-SRC PST-CAP allot
variable PST-U
variable PST-N
8 constant PST-LEVELS                     \ 100 units halved down to 0
create PST-BYTES PST-LEVELS cells allot   \ the bytes a body of each level places
variable PST-LEVEL

: PST+ ( str -- ) {: s:str :}
   s STR:LEN 0 ?do
      s STR:DATA i + c@ PST-SRC PST-U @ + i + c!
   loop
   PST-U @ s STR:LEN + PST-U ! ;

: PST-TEXT ( n -- ) {: units:n :}
   0 PST-U !  s" : PST00000 ( -- )" PST+
   units 0 ?do s"  2 0 ?do loop" PST+ loop  s"  ;" PST+ ;

: PST-DIGIT! ( n n -- ) {: k:n off:n :}
   k 10 mod [char] 0 + PST-SRC off + c! ;

: PST-DEF ( -- n )
   PST-N @ {: k:n :}
   k 10000 / 5 PST-DIGIT!  k 1000 / 6 PST-DIGIT!  k 100 / 7 PST-DIGIT!
   k 10 / 8 PST-DIGIT!  k 9 PST-DIGIT!  k 1 + PST-N !
   PST-SRC PST-U @ >str TEST-EVAL:RC ;

: PST-PUT ( -- )
   PST-DEF {: rc:n :}  rc 0<> if rc throw then ;

: PST-ROOM ( -- n ) CODE-SPACE:CEILING cp@ - ;

: PST-AT ( n -- ) {: k:n :}
   k PST-LEVEL !  100 k 0 ?do 2 / loop PST-TEXT ;
: PST-BYTES@ ( n -- n ) cells PST-BYTES + @ ;
: PST-SLOT ( -- n ) PST-LEVELS 1 - PST-BYTES@ ;

: PST-MEASURE ( -- )
   PST-LEVELS 0 ?do
      i PST-AT  cp@ {: c:n :}  PST-PUT  cp@ c - i cells PST-BYTES + !
   loop ;

\ Room for the long-named word, whose name spans less than two empty words,
\ and the two empty words the fill ends with.
: PST-MARGIN ( -- n ) PST-SLOT 4 * ;

: PST-FITS? ( -- bool )
   PST-LEVEL @ PST-BYTES@ PST-MARGIN + PST-ROOM <= ;
: PST-BODIES ( -- )
   PST-LEVELS 1 - 0 ?do
      i PST-AT  begin PST-FITS? while PST-PUT repeat
   loop ;

\ The first name length whose span leaves the room a whole number of empty
\ words, 0 when it is one already.
: PST-NAME-LEN ( -- n )
   PST-ROOM PST-SLOT mod {: part:n :}
   part 0= if 0 exit then
   1 begin dup CODE-SPACE:SPAN PST-SLOT mod part <> over PST-MARGIN < and
   while 1 + repeat ;
: PST-LONG ( n -- ) {: len:n :}
   0 PST-U !  s" : PST" PST+
   len 3 - 0 ?do [char] X PST-SRC PST-U @ + c!  PST-U @ 1 + PST-U ! loop
   s"  ( -- ) ;" PST+  PST-SRC PST-U @ >str TEST-EVAL:RC 0 T= ;

: PST-FILL ( -- )
   PST-MEASURE  PST-BODIES
   PST-NAME-LEN {: len:n :}  len 0<> if len PST-LONG then
   PST-LEVELS 1 - PST-AT
   begin PST-ROOM PST-SLOT > while PST-PUT repeat ;

: PST-FULL ( -- )
   PST-ROOM PST-SLOT T=
   1 PST-TEXT PST-DEF EXIT-STATUS:FULL T=
   s" : PST-LONG-NAME ( -- ) ;" TEST-EVAL:RC EXIT-STATUS:FULL T=
   0 PST-TEXT PST-DEF 0 T=
   s" : PST-A ( -- ) ;" TEST-EVAL:RC EXIT-STATUS:FULL T=
   s" create PST-B" TEST-EVAL:RC EXIT-STATUS:FULL T=
   s" 1 constant PST-C" TEST-EVAL:RC EXIT-STATUS:FULL T=
   T-REPORT ;

PST-FILL PST-FULL
