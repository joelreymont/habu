split-str
\ protection-span-full.f - fills the image's code space with ordinary
\ definitions, then checks `:`, `create` and `constant` are each rejected
\ E-CODE-SPACE-FULL at the ceiling. A word that would pass the ceiling throws
\ E-NPUB-ROOM and leaves nothing, so the body halves; an empty word is one code
\ slot, so the last lands on the ceiling. `2 0 ?do loop` is much code per token
\ read, so the fill takes seconds.

require src/lib/test.f

$800 constant PST-CAP
create PST-SRC PST-CAP allot
variable PST-U
variable PST-N
variable PST-UNITS

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
   rc E-NPUB-ROOM <>  PST-UNITS @ 0 = or if rc throw then
   PST-UNITS @ 2 / PST-TEXT ;

: PST-FILL ( -- )
   100 PST-TEXT
   begin PST-ROOM 0 > while PST-DEF PST-STEP repeat ;

: PST-FULL ( -- )
   s" : PST-A ( -- ) ;" TEST-EVAL:RC EXIT-STATUS:FULL T=
   s" create PST-B" TEST-EVAL:RC EXIT-STATUS:FULL T=
   s" 1 constant PST-C" TEST-EVAL:RC EXIT-STATUS:FULL T=
   T-REPORT ;

PST-FILL PST-FULL
