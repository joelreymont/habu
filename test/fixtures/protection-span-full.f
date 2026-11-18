split-str
\ protection-span-full.f - fills the image's code space with ordinary
\ definitions to one empty word below the ceiling, checks a long name whose
\ copy would reach the ceiling is rejected where an empty word still fits, then
\ that `:`, `create` and `constant` are each rejected E-CODE-SPACE-FULL at the
\ ceiling. A word that would pass the ceiling is rejected EXIT-STATUS:FULL and
\ leaves nothing, so the body halves. `2 0 ?do loop` is much code per token
\ read, so the fill takes seconds.

require src/lib/test.f

$800 constant PST-CAP
create PST-SRC PST-CAP allot
variable PST-U
variable PST-N
variable PST-UNITS
variable PST-SLOT                         \ the bytes an empty word places

: PST+ ( str -- ) {: a:^ u:n :}
   u 0 ?do a i + c@ PST-SRC PST-U @ + i + c! loop  PST-U @ u + PST-U ! ;

: PST-TEXT ( n -- ) {: units:n :}
   units PST-UNITS !  0 PST-U !  s" : PST00000 ( -- )" PST+
   units 0 ?do s"  2 0 ?do loop" PST+ loop  s"  ;" PST+ ;

: PST-DIGIT! ( n n -- ) {: k:n off:n :}
   k 10 mod [char] 0 + PST-SRC off + c! ;

: PST-DEF ( -- n )
   PST-N @ {: k:n :}
   k 10000 / 5 PST-DIGIT!  k 1000 / 6 PST-DIGIT!  k 100 / 7 PST-DIGIT!
   k 10 / 8 PST-DIGIT!  k 9 PST-DIGIT!  k 1 + PST-N !
   PST-SRC PST-U @ TEST-EVAL:RC ;

: PST-ROOM ( -- n ) CODE-SPACE:CEILING cp@ - ;

: PST-STEP ( n -- ) {: rc:n :}
   rc 0 = if exit then
   rc EXIT-STATUS:FULL <>  PST-UNITS @ 0 = or if rc throw then
   PST-UNITS @ 2 / PST-TEXT ;

: PST-FILL ( -- )
   cp@ {: c:n :}  0 PST-TEXT PST-DEF 0 T=  cp@ c - PST-SLOT !
   100 PST-TEXT
   begin PST-ROOM PST-SLOT @ > while PST-DEF PST-STEP repeat ;

: PST-FULL ( -- )
   PST-ROOM PST-SLOT @ T=
   s" : PST-LONG-NAME ( -- ) ;" TEST-EVAL:RC EXIT-STATUS:FULL T=
   0 PST-TEXT PST-DEF 0 T=
   s" : PST-A ( -- ) ;" TEST-EVAL:RC EXIT-STATUS:FULL T=
   s" create PST-B" TEST-EVAL:RC EXIT-STATUS:FULL T=
   s" 1 constant PST-C" TEST-EVAL:RC EXIT-STATUS:FULL T=
   T-REPORT ;

PST-FILL PST-FULL
