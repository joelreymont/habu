\ stored-type-param.f - a type variable that occurs only where a type is stored,
\ behind a pointer or in a family slot the family's width does not read, stands
\ for any stored type, a two-cell record included, while the word's body needs
\ no one-cell value of it. The refusals land at the call, never at the word.
\ A cast's stored variable takes any such type but one that may hold a linear
\ value, an atom or a width, at every binding, through a generic caller too.
require src/lib/test.f
require src/lib/string.f
require src/core/bytes.f

DEFLINEAR stored-type-lin

\ The recursive callers reopen RC-CAST at run time to reach its private words.
package RC-CAST
DEFLINEAR RC-CAST:tok
private
LINEAR: ERASE ( RC-CAST:tok -- n )
CAST: >ELEM ( ptr u8 -- ptr t )
: TOK-STEP ( ptr [ -- RC-CAST:tok ] -- ) @ execute ERASE drop ;
: N-STEP ( ptr [ -- n ] -- ) @ execute drop ;
public
TYPED-VARIABLE NV n
;package

\ Public: the refused callers compile at run time, outside the package.
package STORED

public

STRUCTURE turn DERIVE addr FIELD alive bool FIELD angle r ;STRUCTURE
STRUCTURE node DERIVE addr FIELD kind n ;STRUCTURE
NEWTYPE ev<a>
\ A phantom argument: no field holds `a`.
STRUCTURE run<a> DERIVE addr FIELD count n FIELD step n ;STRUCTURE
\ A width argument: `item` holds an `a` by value.
STRUCTURE box<a> DERIVE addr FIELD item a FIELD rest n ;STRUCTURE
STRUCTURE lrec DERIVE addr FIELD hold stored-type-lin ;STRUCTURE

: SAME ( ptr t -- ptr t ) ;
: READ ( ptr t -- t ) @ ;
: POKE ( t ptr t -- ) ! ;
: STEP ( ptr t -- ptr t ) 8 + ;
defer P ( ptr a -- ptr a )
: VIA ( ptr t -- ptr t ) P ;
\ The callee's one-cell `a` reaches `t` through SAME's stored one.
: VIA2 ( ptr t -- ptr t ) P SAME ;
: IN-BOX ( box<t> -- box<t> ) ;
: LOCAL-READ ( ptr t -- ptr t )
   {: p:ptr :}
   p @ drop p ;
TYPED-VARIABLE TA turn

private

CAST: N>EV-TURN ( n -- ev<turn> )
CAST: EV-TURN>N ( ev<turn> -- n )
CAST: N>EV-N ( n -- ev<n> )
CAST: EV-N>N ( ev<n> -- n )
CAST: N>EV-NODE ( n -- ev<ptr node> )
CAST: EV-NODE>N ( ev<ptr node> -- n )
CAST: >ELEM ( ptr u8 -- ptr t )
CAST: ELEM> ( ptr t -- ptr u8 )

TYPED-VARIABLE TV turn
TYPED-VARIABLE PV ptr turn
TYPED-VARIABLE RT run<turn>
TYPED-VARIABLE RN run<n>
2 TYPED-BUFFER NB n
32 BUFFER: ELEMS

: PASS ( ev<t> -- ev<t> ) ;
: COUNT ( ptr run<t> -- n ) STORED-RUN:COUNT @ ;
: KEEP ( ptr t ptr ptr t -- ) ! ;
: OUTER ( ptr t -- ptr t ) SAME ;
: NULL? ( ptr t -- bool ) NULL-PTR = ;
: OLD-STEP ( ptr a -- ptr a ) 8 + ;
: LOCAL-DROP ( ptr t -- )
   {: p:ptr :} ;
: LOCAL-KEEP ( ptr t -- ptr t )
   {: p:ptr :}
   p ;

\ A `=` over two `ptr turn` is refused: the pointer row of `=` takes one cell.
: ADDR ( ptr turn -- n ) NULL-PTR - ;
: TV? ( ptr turn -- bool ) ADDR  TV ADDR = ;

CAST: QC->ELEM ( ptr u8 -- ptr t )
public
\ QC-AT's `a` takes only a cast's stored types; QC-ANY's takes any.
: QC-AT ( ptr u8 -- ptr a ) QC->ELEM ;
defer QC-ANY ( ptr u8 -- ptr a )
defer QC-D ( ptr u8 -- ptr a )
: RUN-Q ( R [ R -- S ] -- S ) execute ;
defer QR-D ( ptr u8 -- ptr [ R -- S ] )
private

\ The admitted callers: each loads only if its record type is taken.
: PASS-TURN ( -- n ) 16 N>EV-TURN PASS EV-TURN>N ;
: PASS-N ( -- n ) 8 N>EV-N PASS EV-N>N ;
: PASS-NODE ( -- n ) 24 N>EV-NODE PASS EV-NODE>N ;
: SAME-TURN ( -- bool ) TV SAME TV? ;
: COUNT-TURN ( -- n ) 5 RT STORED-RUN:COUNT !  RT COUNT ;
: COUNT-N ( -- n ) 6 RN STORED-RUN:COUNT !  RN COUNT ;
: KEEP-TURN ( -- bool ) TV PV KEEP  PV @ TV? ;
: OUTER-TURN ( -- bool ) TV OUTER TV? ;
: NULL-TURN ( -- bool ) TV NULL? ;
: OLD-STEP-N ( -- bool ) 0 NB OLD-STEP  1 NB = ;
: LOCAL-TURN ( -- bool ) TV LOCAL-DROP  TV LOCAL-KEEP TV? ;

\ Each stores through >ELEM and reads back through ELEM> >ELEM.
: ELEM-N ( -- bool ) 7 ELEMS >ELEM !  ELEMS >ELEM ELEM> >ELEM @ 7 = ;
: ELEM-R ( -- bool )
   0.5 ELEMS >ELEM !  ELEMS >ELEM ELEM> >ELEM @ 0.5 f= ;
: ELEM-U8 ( -- bool )
   200 ELEMS >ELEM c!  ELEMS >ELEM ELEM> >ELEM c@ 200 = ;
: ELEM-PTR ( -- bool )
   ELEMS 16 + ELEMS >ELEM !  ELEMS >ELEM ELEM> >ELEM @  ELEMS 16 + = ;
: TURN? ( ptr turn -- bool )
   {: p:ptr :}
   p STORED-TURN:ALIVE @  p STORED-TURN:ANGLE @ 0.25 f=  and ;
: ELEM-TURN ( -- bool )
   true ELEMS >ELEM STORED-TURN:ALIVE !
   0.25 ELEMS >ELEM STORED-TURN:ANGLE !
   ELEMS >ELEM ELEM> >ELEM TURN? ;

70 constant CHECK-RC                 \ the engine refusing a definition it cannot certify

8192 constant DIAG-CAP
create DIAG-BUF DIAG-CAP allot

: ARM ( -- )   \ capture the next diagnostic as JSON instead of printing it
   DIAG-BUF DIAG-CAP DIAG-BUFFER!  true DIAG-JSON! ;

: DISARM ( -- )
   false DIAG-JSON!  DIAG-BUFFER-OFF ;

: HAS? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;

\ The refusal is a mismatch at the call token the string names.
: MISMATCH-AT ( ptr u8 n -- )
   HAS?  S\" \"code\":\"E-MISMATCH\"" HAS? ;

: CASE-PASS ( -- )
   s" a record, a number and a pointer pass through ev<t>" T-LABEL
   PASS-TURN 16 T=  PASS-N 8 T=  PASS-NODE 24 T= ;

: CASE-POINTER ( -- )
   s" ptr t, ptr run<t> and ptr ptr t take a two-cell record" T-LABEL
   SAME-TURN TTRUE  COUNT-TURN 5 T=  COUNT-N 6 T=  KEEP-TURN TTRUE
   OUTER-TURN TTRUE ;

: CASE-NULL ( -- )
   s" a ptr t compared with the null takes a record pointer" T-LABEL
   NULL-TURN TFALSE ;

: CASE-LOCAL ( -- )
   s" a ptr local takes a record pointer until a fetch through it" T-LABEL
   LOCAL-TURN TTRUE
   ARM
   [: s" : STORED-R11 ( ptr STORED:turn -- ptr STORED:turn ) STORED:LOCAL-READ ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"token\":\"STORED:LOCAL-READ\"" MISMATCH-AT
   DISARM ;

: CASE-OLD-STEP ( -- )
   s" a pointer step over ptr n is unchanged" T-LABEL
   OLD-STEP-N TTRUE ;

: CASE-READ ( -- )
   s" a body that fetches a t takes no record" T-LABEL
   ARM
   [: s" : STORED-R1 ( ptr STORED:turn -- STORED:turn ) STORED:READ ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"token\":\"STORED:READ\"" MISMATCH-AT
   DISARM ;

: CASE-POKE ( -- )
   s" nor one that stores a t" T-LABEL
   ARM
   [: s" : STORED-R2 ( STORED:turn ptr STORED:turn -- ) STORED:POKE ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"token\":\"STORED:POKE\"" MISMATCH-AT
   DISARM ;

: CASE-STEP ( -- )
   s" nor one that steps the pointer" T-LABEL
   ARM
   [: s" : STORED-R3 ( ptr STORED:turn -- ptr STORED:turn ) STORED:STEP ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"token\":\"STORED:STEP\"" MISMATCH-AT
   DISARM ;

: CASE-VIA ( -- )
   s" nor one that hands it to a one-cell variable" T-LABEL
   ARM
   [: s" : STORED-R4 ( ptr STORED:turn -- ptr STORED:turn ) STORED:VIA ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"token\":\"STORED:VIA\"" MISMATCH-AT
   DISARM ;

: CASE-VIA2 ( -- )
   s" nor one that does so before a stored-type call" T-LABEL
   ARM
   [: s" : STORED-R5 ( ptr STORED:turn -- ptr STORED:turn ) STORED:VIA2 ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"token\":\"STORED:VIA2\"" MISMATCH-AT
   DISARM ;

: CASE-RECURSE ( -- )
   s" a recursive call takes an ordinary variable" T-LABEL
   ARM
   [: s" : STORED-R9 ( ptr t ptr t n -- ) dup 0 > IF drop 2drop STORED:TA STORED:TA 0 RECURSE EXIT THEN drop swap @ swap ! ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"token\":\"RECURSE\"" MISMATCH-AT
   DISARM
   ARM
   [: s" : STORED-R10 ( ptr t ptr t n -- ) dup 0 = IF drop swap @ swap ! EXIT THEN drop 2drop STORED:TA STORED:TA 0 RECURSE ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"token\":\"RECURSE\"" MISMATCH-AT
   DISARM ;

: CASE-WIDTH-SLOT ( -- )
   s" a slot the family's width reads takes no record" T-LABEL
   ARM
   [: s" : STORED-R6 ( STORED:box<STORED:turn> -- STORED:box<STORED:turn> ) STORED:IN-BOX ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"token\":\"STORED:IN-BOX\"" MISMATCH-AT
   DISARM ;

: CASE-LINEAR ( -- )
   s" a record with a linear member is not a stored type" T-LABEL
   ARM
   [: s" : STORED-R7 ( ptr STORED:lrec -- ptr STORED:lrec ) STORED:SAME ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"token\":\"STORED:SAME\"" MISMATCH-AT
   DISARM ;

: CASE-OPEN ( -- )
   s" nor is a record open in the caller's own variable" T-LABEL
   ARM
   [: s" : STORED-R8 ( ptr STORED:box<a> -- ptr STORED:box<a> ) STORED:SAME ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"token\":\"STORED:SAME\"" MISMATCH-AT
   DISARM ;

: CASE-ELEM ( -- )
   s" a cast's stored variable takes n, r, u8, ptr u8 and a record" T-LABEL
   ELEM-N TTRUE  ELEM-R TTRUE  ELEM-U8 TTRUE  ELEM-PTR TTRUE  ELEM-TURN TTRUE ;

\ The refusal is the code the string names, at the call of STORED:AT.
: CAST-AT ( ptr u8 n -- )
   HAS?  S\" \"token\":\"STORED:AT\"" HAS? ;

: CASE-ELEM-LINEAR ( -- )
   s" nor a type that may hold a linear value, through a generic word" T-LABEL
   ARM
   [: s" : STORED-R12 ( ptr u8 -- ptr stored-type-lin ) STORED:AT ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"code\":\"E-CAST-LINEAR\"" CAST-AT
   DISARM
   \ The stored kind refuses lrec before the cast does, as it does in CASE-LINEAR.
   ARM
   [: s" : STORED-R13 ( ptr u8 -- ptr STORED:lrec ) STORED:AT ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"token\":\"STORED:AT\"" MISMATCH-AT
   DISARM ;

: CASE-ELEM-CLASS ( -- )
   s" nor an atom or a width" T-LABEL
   ARM
   [: s" : STORED-R14 ( ptr u8 -- ptr xt ) STORED:AT ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"code\":\"E-CAST-CLASS\"" CAST-AT
   DISARM
   ARM
   [: s" : STORED-R15 ( ptr u8 -- ptr width<n> ) STORED:AT ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"code\":\"E-CAST-CLASS\"" CAST-AT
   DISARM ;

: CASE-IS-STORED ( -- )
   s" is keeps a cast's stored-type restriction" T-LABEL
   ARM
   [: s" : STORED-QC1 ( -- ) [: STORED:QC-AT ;] is STORED:QC-D ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"code\":\"E-CAST-LINEAR\"" HAS?
   DISARM
   [: s" : STORED-QC2 ( -- ) [: STORED:QC-ANY ;] is STORED:QC-D ;" evaluate-closed ;]
      0 TTHROWSQ ;

256 constant RC-CAP
RC-CAP BUFFER: RC-SRC
variable RC-LEN

\ The texts below are joined from pieces that each fit a line; RC-CAP holds
\ the longest.
: RC+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   a  RC-SRC RC-LEN @ +  u BYTE-COPY
   RC-LEN @ u + RC-LEN ! ;

\ A word in RC-CAST whose recursive call's instance of t the step word binds,
\ before the body hands t to the cast >ELEM.
: RC-RECURSE$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: na:ptr nu:n sa:ptr su:n :}
   0 RC-LEN !
   s" package RC-CAST : " RC+  na nu RC+
   s"  ( ptr u8 n -- ptr t ) 0 <> IF dup 0 RECURSE " RC+  sa su RC+
   s"  THEN >ELEM ; ;package" RC+
   RC-SRC RC-LEN @ ;

: RC-OTHER$ ( -- ptr u8 n )
   0 RC-LEN !
   s" : RC-OTHER ( ptr t n -- ptr t ) dup 0 = IF drop EXIT THEN " RC+
   s" drop RC-CAST:NV 0 RECURSE drop ;" RC+
   RC-SRC RC-LEN @ ;

: CASE-RC-LINEAR ( -- )
   s" a recursive call meets the restriction the body adds later" T-LABEL
   ARM
   [: s" RC-FORGE" s" TOK-STEP" RC-RECURSE$ evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"code\":\"E-CAST-LINEAR\"" HAS?
   DISARM
   [: s" RC-KEEP" s" N-STEP" RC-RECURSE$ evaluate-closed ;] 0 TTHROWSQ ;

: CASE-RC-OTHER ( -- )
   s" a recursive call at another instance the body leaves free" T-LABEL
   [: RC-OTHER$ evaluate-closed ;] 0 TTHROWSQ ;

: CASE-ELEM-ROWS ( -- )
   s" nor a quotation whose rows end in different tails" T-LABEL
   ARM
   [: s" : STORED-R16 ( ptr u8 -- ptr [ R -- S ] ) STORED:AT ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"code\":\"E-CAST-LINEAR\"" HAS?
   DISARM
   ARM
   [: s" : STORED-R17 ( ptr u8 -- n ) STORED:AT @ STORED:RUN-Q ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"code\":\"E-CAST-LINEAR\"" HAS?
   DISARM
   ARM
   [: s" : STORED-R18 ( -- ) [: STORED:AT ;] is STORED:QR-D ;" evaluate-closed ;]
      CHECK-RC TTHROWSQ
   S\" \"code\":\"E-CAST-LINEAR\"" HAS?
   DISARM
   [: s" : STORED-QOK1 ( ptr u8 -- ptr [ -- n ] ) STORED:AT ;" evaluate-closed ;]
      0 TTHROWSQ
   [: s" : STORED-QOK2 ( ptr u8 -- ) STORED:AT [: 42 ;] swap ! ;" evaluate-closed ;]
      0 TTHROWSQ ;

public

\ A generic word that passes the cast's stored variable on to its caller.
: AT ( ptr u8 -- ptr a ) >ELEM ;

: RUN ( -- )
   T-RESET
   CASE-PASS
   CASE-POINTER
   CASE-NULL
   CASE-LOCAL
   CASE-OLD-STEP
   CASE-READ
   CASE-POKE
   CASE-STEP
   CASE-VIA
   CASE-VIA2
   CASE-RECURSE
   CASE-WIDTH-SLOT
   CASE-LINEAR
   CASE-OPEN
   CASE-ELEM
   CASE-ELEM-LINEAR
   CASE-ELEM-CLASS
   CASE-IS-STORED
   CASE-RC-LINEAR
   CASE-RC-OTHER
   CASE-ELEM-ROWS
   T-REPORT ;

;package

STORED:RUN
