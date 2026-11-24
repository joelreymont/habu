split-str
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
CAST: >ELEM ( ^u8 -- ^t )
: TOK-STEP ( ^[ -- RC-CAST:tok ] -- ) @ execute ERASE drop ;
: N-STEP ( ^[ -- n ] -- ) @ execute drop ;
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
\ A quotation argument: qb's width is the quotation's one cell.
STRUCTURE qb<a> FIELD f [ -- a ] ;STRUCTURE
\ A width argument `mark`'s and `set`'s offsets and the stride read.
STRUCTURE held<a> DERIVE addr FIELD value a BUFFER mark 5 FIELD set bool ;STRUCTURE
\ A declared type of one cell.
ENUM dir north south ;ENUM

: SAME ( ^t -- ^t ) ;
: READ ( ^t -- t ) @ ;
: POKE ( t ^t -- ) ! ;
: STEP ( ^t -- ^t ) 8 + ;
defer P ( ^a -- ^a )
: VIA ( ^t -- ^t ) P ;
\ The callee's one-cell `a` reaches `t` through SAME's stored one.
: VIA2 ( ^t -- ^t ) P SAME ;
: IN-BOX ( box<t> -- box<t> ) ;
: LOCAL-READ ( ^t -- ^t )
   {: p:^ :}
   p @ drop p ;
: Q-DROP ( qb<t> -- n ) drop 1 ;
: Q-ID ( qb<t> -- qb<t> ) ;
: Q-NIP ( qb<t> qb<t> -- qb<t> ) nip ;
: Q-DUP ( qb<t> -- qb<t> qb<t> ) dup ;
: Q-LOCAL ( qb<t> -- qb<t> )
   {: q:qb<t> :}
   q ;
: Q-ROW ( [ -- t ] -- [ -- t ] ) ;
: RUN-AT ( R ^t [ R ^t -- S ] -- S ) execute ;
defer Q-DEFER ( qb<t> -- n )
TYPED-VARIABLE TA turn
4 TYPED-BUFFER HELDS held<turn>
\ Each reads one cell through its open receiver.
: SET-OF ( ^held<b> -- ^bool ) STORED-HELD:SET ;
: FETCH-ONE ( ^t -- n ) @ drop 0 ;
: Q-FETCH ( [ -- ^t ] -- n ) execute @ drop 0 ;

private

CAST: N>EV-TURN ( n -- ev<turn> )
CAST: EV-TURN>N ( ev<turn> -- n )
CAST: N>EV-N ( n -- ev<n> )
CAST: EV-N>N ( ev<n> -- n )
CAST: N>EV-NODE ( n -- ev<^node> )
CAST: EV-NODE>N ( ev<^node> -- n )
CAST: >ELEM ( ^u8 -- ^t )
CAST: ELEM> ( ^t -- ^u8 )

TYPED-VARIABLE TV turn
TYPED-VARIABLE PV ^turn
TYPED-VARIABLE RT run<turn>
TYPED-VARIABLE RN run<n>
2 TYPED-BUFFER NB n
32 BUFFER: ELEMS

: PASS ( ev<t> -- ev<t> ) ;
: COUNT ( ^run<t> -- n ) STORED-RUN:COUNT @ ;
: KEEP ( ^t ^^t -- ) ! ;
: OUTER ( ^t -- ^t ) SAME ;
: NULL? ( ^t -- bool ) NULL-PTR = ;
: OLD-STEP ( ^a -- ^a ) 8 + ;
: LOCAL-DROP ( ^t -- )
   {: p:^ :} ;
: LOCAL-KEEP ( ^t -- ^t )
   {: p:^ :}
   p ;

: TV? ( ^turn -- bool ) TV = ;

CAST: QC->ELEM ( ^u8 -- ^t )
public
\ QC-AT's `a` takes only a cast's stored types; QC-ANY's takes any.
: QC-AT ( ^u8 -- ^a ) QC->ELEM ;
defer QC-ANY ( ^u8 -- ^a )
defer QC-D ( ^u8 -- ^a )
: RUN-Q ( R [ R -- S ] -- S ) execute ;
defer QR-D ( ^u8 -- ^[ R -- S ] )
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
: RUN-AT-TURN ( -- bool ) TV [: TV? ;] RUN-AT ;

\ Each stores through >ELEM and reads back through ELEM> >ELEM.
: ELEM-N ( -- bool ) 7 ELEMS >ELEM !  ELEMS >ELEM ELEM> >ELEM @ 7 = ;
: ELEM-R ( -- bool )
   0.5 ELEMS >ELEM !  ELEMS >ELEM ELEM> >ELEM @ 0.5 f= ;
: ELEM-U8 ( -- bool )
   200 ELEMS >ELEM c!  ELEMS >ELEM ELEM> >ELEM c@ 200 = ;
: ELEM-PTR ( -- bool )
   ELEMS 16 + ELEMS >ELEM !  ELEMS >ELEM ELEM> >ELEM @  ELEMS 16 + = ;
: TURN? ( ^turn -- bool )
   {: p:^ :}
   p STORED-TURN:ALIVE @  p STORED-TURN:ANGLE @ 0.25 f=  and ;
: ELEM-TURN ( -- bool )
   true ELEMS >ELEM STORED-TURN:ALIVE !
   0.25 ELEMS >ELEM STORED-TURN:ANGLE !
   ELEMS >ELEM ELEM> >ELEM TURN? ;

\ Each reaches a held<turn> through the generic accessors and AT, at the
\ numbers turn's two cells give.
: HELD-FILL ( -- )
   1 HELDS {: h :}
   true h STORED-HELD:VALUE STORED-TURN:ALIVE !
   0.25 h STORED-HELD:VALUE STORED-TURN:ANGLE !
   true h STORED-HELD:SET ! ;
: HELD-READ ( -- bool )
   0 HELDS 1 STORED-HELD:AT {: h :}
   h STORED-HELD:SET @  h STORED-HELD:VALUE TURN?  and ;
: HELD-ADDR ( ^held<turn> -- n ) NULL-PTR - ;
: HELD-STEP ( -- bool ) 0 HELDS 1 STORED-HELD:AT  1 HELDS = ;
\ The offset of mark's span in a held<turn>.
: MARK-AT ( -- n ) 0 HELDS STORED-HELD:MARK SPAN:$ drop NULL-PTR -  0 HELDS HELD-ADDR - ;


: ARM ( -- )   \ capture the next diagnostic as JSON instead of printing it
   REJECTION:CAPTURE  true REJECTION:JSON ! ;

: DISARM ( -- )
   false REJECTION:JSON !  REJECTION:BUFFER-OFF ;

: HAS? ( ^u8 n -- )
   REJECTION:BUFFER$ 2swap STR:CONTAINS? TTRUE ;

\ The refusal is a mismatch at the call token the string names.
: MISMATCH-AT ( str -- )
   HAS?  S\" \"code\":\"E-MISMATCH\"" HAS? ;

: CASE-PASS ( -- )
   s" a record, a number and a pointer pass through ev<t>" T-LABEL
   PASS-TURN 16 T=  PASS-N 8 T=  PASS-NODE 24 T= ;

: CASE-POINTER ( -- )
   s" ^t, ^run<t> and ^^t take a two-cell record" T-LABEL
   SAME-TURN TTRUE  COUNT-TURN 5 T=  COUNT-N 6 T=  KEEP-TURN TTRUE
   OUTER-TURN TTRUE ;

: CASE-HELD ( -- )
   s" a generic member call adds its instance's offset or stride" T-LABEL
   HELD-FILL  HELD-READ TTRUE  HELD-STEP TTRUE ;

: CASE-HELD-SPAN ( -- )
   s" a generic BUFFER member's span starts at its instance's offset" T-LABEL
   MARK-AT  [type] turn width>n T= ;

: CASE-HELD-TOP ( -- )
   s" a top-level member call whose instance moves the member is rejected" T-LABEL
   ARM
   [: s" 0 STORED:HELDS STORED-HELD:SET drop" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"code\":\"E-LAYOUT-VALUE\"" HAS?
   S\" \"token\":\"STORED-HELD:SET\"" HAS?
   s" move the call inside a definition, which uses the receiver's layout" HAS?
   DISARM ;

: CASE-HELD-SIZE ( -- )
   s" a family whose width reads a parameter has no BYTES or CELLS" T-LABEL
   STORED-RUN:BYTES 16 T=  STORED-RUN:CELLS 2 T=
   ARM
   [: s" STORED-HELD:BYTES drop" evaluate-closed ;]
      70 TTHROWSQ
   [: s" STORED-HELD:CELLS drop" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"E-UNDEFINED\",\"message\":\"cannot find word `STORED-HELD:BYTES`" HAS?
   S\" \"E-UNDEFINED\",\"message\":\"cannot find word `STORED-HELD:CELLS`" HAS?
   DISARM ;

: CASE-ONE-CELL-MEMBER ( -- )
   s" a generic member call through an open receiver takes no record" T-LABEL
   ARM
   [: s" : STORED-R21 ( ^STORED:held<STORED:turn> -- ^bool ) STORED:SET-OF ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:SET-OF\"" MISMATCH-AT
   s" `stored:turn` is 2 cells" HAS?
   DISARM ;

: CASE-ONE-CELL-FETCH ( -- )
   s" nor does a one-cell fetch through ^t" T-LABEL
   ARM
   [: s" : STORED-R22 ( ^STORED:turn -- n ) STORED:FETCH-ONE ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:FETCH-ONE\"" MISMATCH-AT
   s" `stored:turn` is 2 cells" HAS?
   DISARM ;

: CASE-ONE-CELL-ENUM ( -- )
   s" nor a declared type that fits one cell" T-LABEL
   ARM
   [: s" : STORED-R23 ( ^STORED:dir -- n ) STORED:FETCH-ONE ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:FETCH-ONE\"" MISMATCH-AT
   s" `stored:dir` is a declared type, not a plain cell" HAS?
   DISARM ;

: CASE-NULL ( -- )
   s" a ^t compared with the null takes a record pointer" T-LABEL
   NULL-TURN TFALSE ;

: CASE-POINTER-KINDS ( -- )
   s" `=` none of whose rows fit names each row and the closest row's types" T-LABEL
   ARM
   [: s" : STORED-R24 ( ^STORED:turn ^STORED:node -- bool ) = ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"message\":\"`=` takes n n or ^a ^a; the stack holds ^stored:turn ^stored:node\"" HAS?
   S\" \"label\":\"takes n n or ^a ^a\"" HAS?
   S\" \"expected\":\"^stored:node ^stored:node\",\"found\":\"^stored:turn ^stored:node\"" HAS?
   S\" \"help\":\"give `=` ^stored:node ^stored:node: convert or replace the value before it\"" HAS?
   DISARM ;

: CASE-SUB-KINDS ( -- )
   s" `-` given a record pointer and n gives the offset row's reason" T-LABEL
   ARM
   [: s" : STORED-R26 ( ^STORED:turn n -- ^STORED:turn ) - ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"message\":\"`a` is stepped by an offset; `stored:turn` is not a plain cell type\"" HAS?
   S\" \"label\":\"steps by an offset\"" HAS?
   S\" \"expected\":\"^a n\",\"found\":\"^stored:turn n\",\"family\":\"stored:turn\"" HAS?
   S\" \"help\":\"declare the word for the plain cell type it steps, or step `stored:turn` by its size\"" HAS?
   S\" \"note\":\"`-` takes n n or ^b n or ^b ^c or ^b ^b\"" HAS?
   DISARM ;

: CASE-SUB-NAMES ( -- )
   s" `-` names its forms after the record's letters, the same in each list" T-LABEL
   ARM
   [: s" : STORED-R27 ( n -- n ) NULL-PTR - ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"message\":\"`-` takes n n or ^c n or ^c ^d or ^c ^c; the stack holds n ^b\"" HAS?
   S\" \"label\":\"takes n n or ^c n or ^c ^d or ^c ^c\"" HAS?
   DISARM ;

: CASE-SUB-HELP ( -- )
   s" `-` none of whose rows fit helps with the closest row alone" T-LABEL
   ARM
   [: s" : STORED-R38 ( n ^a -- n ) drop NULL-PTR - ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"message\":\"`-` takes n n or ^c n or ^c ^d or ^c ^c; the stack holds n ^b\"" HAS?
   S\" \"label\":\"takes n n or ^c n or ^c ^d or ^c ^c\"" HAS?
   S\" \"help\":\"give `-` ^a ^b: convert or replace the value before it\"" HAS?
   DISARM ;

: CASE-SUB-FRESH ( -- )
   s" `-` lists each form once, its letters given in order" T-LABEL
   ARM
   [: s" : STORED-R35 ( -- n ) - ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"note\":\"`-` takes n n or ^a n or ^a ^b or ^a ^a\"" HAS?
   DISARM ;

: CASE-ADD-KINDS ( -- )
   s" `+` given two record pointers, a row no value of which fits, names each form" T-LABEL
   ARM
   [: s" : STORED-R25 ( ^STORED:turn ^STORED:node -- ^STORED:turn ) + ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"message\":\"`+` takes n n or ^a n or n ^a; the stack holds ^stored:turn ^stored:node\"" HAS?
   S\" \"label\":\"takes n n or ^a n or n ^a\"" HAS?
   S\" \"expected\":\"n n\",\"found\":\"^stored:turn ^stored:node\"" HAS?
   S\" \"help\":\"give `+` n n: convert or replace the value before it\"" HAS?
   DISARM ;

: CASE-LOCAL ( -- )
   s" a ^ local takes a record pointer until a fetch through it" T-LABEL
   LOCAL-TURN TTRUE
   ARM
   [: s" : STORED-R11 ( ^STORED:turn -- ^STORED:turn ) STORED:LOCAL-READ ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:LOCAL-READ\"" MISMATCH-AT
   DISARM ;

: CASE-OLD-STEP ( -- )
   s" a pointer step over ^n is unchanged" T-LABEL
   OLD-STEP-N TTRUE ;

: CASE-READ ( -- )
   s" a body that fetches a t takes no record" T-LABEL
   ARM
   [: s" : STORED-R1 ( ^STORED:turn -- STORED:turn ) STORED:READ ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:READ\"" MISMATCH-AT
   DISARM ;

: CASE-POKE ( -- )
   s" nor one that stores a t" T-LABEL
   ARM
   [: s" : STORED-R2 ( STORED:turn ^STORED:turn -- ) STORED:POKE ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:POKE\"" MISMATCH-AT
   DISARM ;

: CASE-STEP ( -- )
   s" nor one that steps the pointer" T-LABEL
   ARM
   [: s" : STORED-R3 ( ^STORED:turn -- ^STORED:turn ) STORED:STEP ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:STEP\"" MISMATCH-AT
   DISARM ;

: CASE-VIA ( -- )
   s" nor one that hands it to a one-cell variable" T-LABEL
   ARM
   [: s" : STORED-R4 ( ^STORED:turn -- ^STORED:turn ) STORED:VIA ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:VIA\"" MISMATCH-AT
   DISARM ;

: CASE-VIA2 ( -- )
   s" nor one that does so before a stored-type call" T-LABEL
   ARM
   [: s" : STORED-R5 ( ^STORED:turn -- ^STORED:turn ) STORED:VIA2 ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:VIA2\"" MISMATCH-AT
   DISARM ;

: CASE-RECURSE ( -- )
   s" a recursive call takes an ordinary variable" T-LABEL
   ARM
   [: s" : STORED-R9 ( ^t ^t n -- ) dup 0 > IF drop 2drop STORED:TA STORED:TA 0 RECURSE EXIT THEN drop swap @ swap ! ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"RECURSE\"" MISMATCH-AT
   DISARM
   ARM
   [: s" : STORED-R10 ( ^t ^t n -- ) dup 0 = IF drop swap @ swap ! EXIT THEN drop 2drop STORED:TA STORED:TA 0 RECURSE ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"RECURSE\"" MISMATCH-AT
   DISARM ;

: CASE-WIDTH-SLOT ( -- )
   s" a slot the family's width reads takes no record" T-LABEL
   ARM
   [: s" : STORED-R6 ( STORED:box<STORED:turn> -- STORED:box<STORED:turn> ) STORED:IN-BOX ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:IN-BOX\"" MISMATCH-AT
   DISARM ;

: CASE-LINEAR ( -- )
   s" a record with a linear member is not a stored type" T-LABEL
   ARM
   [: s" : STORED-R7 ( ^STORED:lrec -- ^STORED:lrec ) STORED:SAME ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:SAME\"" MISMATCH-AT
   DISARM ;

: CASE-OPEN ( -- )
   s" nor is a record open in the caller's own variable" T-LABEL
   ARM
   [: s" : STORED-R8 ( ^STORED:box<a> -- ^STORED:box<a> ) STORED:SAME ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:SAME\"" MISMATCH-AT
   DISARM ;

: CASE-QUOT-FIELD ( -- )
   s" a quotation field's argument takes a record" T-LABEL
   [: s" : STORED-QF1 ( STORED:qb<STORED:turn> -- n ) STORED:Q-DROP ;" evaluate-closed ;]
      0 TTHROWSQ
   [: s" : STORED-QF2 ( STORED:qb<STORED:turn> -- STORED:qb<STORED:turn> ) STORED:Q-ID ;" evaluate-closed ;]
      0 TTHROWSQ
   [: s" : STORED-QF3 ( STORED:qb<STORED:turn> STORED:qb<STORED:turn> -- STORED:qb<STORED:turn> ) STORED:Q-NIP ;" evaluate-closed ;]
      0 TTHROWSQ
   [: s" : STORED-QF4 ( STORED:qb<STORED:turn> -- STORED:qb<STORED:turn> STORED:qb<STORED:turn> ) STORED:Q-DUP ;" evaluate-closed ;]
      0 TTHROWSQ
   [: s" : STORED-QF5 ( STORED:qb<STORED:turn> -- STORED:qb<STORED:turn> ) STORED:Q-LOCAL ;" evaluate-closed ;]
      0 TTHROWSQ ;

: CASE-QUOT-ROW ( -- )
   s" a quotation's own row takes no record, nor does a defer's field" T-LABEL
   ARM
   [: s" : STORED-R19 ( [ -- STORED:turn ] -- [ -- STORED:turn ] ) STORED:Q-ROW ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:Q-ROW\"" MISMATCH-AT
   DISARM
   ARM
   [: s" : STORED-R20 ( STORED:qb<STORED:turn> -- n ) STORED:Q-DEFER ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:Q-DEFER\"" MISMATCH-AT
   DISARM ;

: CASE-QUOT-PTR ( -- )
   s" a pointer in a quotation's row takes a record" T-LABEL
   RUN-AT-TURN TTRUE ;

: CASE-QUOT-FETCH ( -- )
   s" nor does a fetch through the ^t a quotation yields" T-LABEL
   ARM
   [: s" : STORED-R39 ( -- n ) [: STORED:TA ;] STORED:Q-FETCH ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:Q-FETCH\"" MISMATCH-AT
   s" `stored:turn` is 2 cells" HAS?
   DISARM ;

: CASE-ELEM ( -- )
   s" a cast's stored variable takes n, r, u8, ^u8 and a record" T-LABEL
   ELEM-N TTRUE  ELEM-R TTRUE  ELEM-U8 TTRUE  ELEM-PTR TTRUE  ELEM-TURN TTRUE ;

\ The refusal is the code the string names, at the call of STORED:AT.
: CAST-AT ( str -- )
   HAS?  S\" \"token\":\"STORED:AT\"" HAS? ;

: CASE-ELEM-LINEAR ( -- )
   s" nor a type that may hold a linear value, through a generic word" T-LABEL
   ARM
   [: s" : STORED-R12 ( ^u8 -- ^stored-type-lin ) STORED:AT ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"code\":\"E-CAST-LINEAR\"" CAST-AT
   DISARM
   \ The stored kind refuses lrec before the cast does, as it does in CASE-LINEAR.
   ARM
   [: s" : STORED-R13 ( ^u8 -- ^STORED:lrec ) STORED:AT ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"token\":\"STORED:AT\"" MISMATCH-AT
   DISARM ;

: CASE-ELEM-CLASS ( -- )
   s" nor an atom or a width" T-LABEL
   ARM
   [: s" : STORED-R14 ( ^u8 -- ^xt ) STORED:AT ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"code\":\"E-CAST-CLASS\"" CAST-AT
   DISARM
   ARM
   [: s" : STORED-R15 ( ^u8 -- ^width<n> ) STORED:AT ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"code\":\"E-CAST-CLASS\"" CAST-AT
   DISARM ;

: CASE-IS-STORED ( -- )
   s" is keeps a cast's stored-type restriction" T-LABEL
   ARM
   [: s" : STORED-QC1 ( -- ) [: STORED:QC-AT ;] is STORED:QC-D ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"code\":\"E-CAST-LINEAR\"" HAS?
   DISARM
   [: s" : STORED-QC2 ( -- ) [: STORED:QC-ANY ;] is STORED:QC-D ;" evaluate-closed ;]
      0 TTHROWSQ ;

256 constant RC-CAP
RC-CAP BUFFER: RC-SRC
variable RC-LEN

\ The texts below are joined from pieces that each fit a line; RC-CAP holds
\ the longest.
: RC+ ( str -- )
   {: a:^ u:n :}
   a  RC-SRC RC-LEN @ +  u BYTE-COPY
   RC-LEN @ u + RC-LEN ! ;

\ A word in RC-CAST whose recursive call's instance of t the step word binds,
\ before the body hands t to the cast >ELEM.
: RC-RECURSE$ ( str str -- str )
   {: na:^ nu:n sa:^ su:n :}
   0 RC-LEN !
   s" package RC-CAST : " RC+  na nu RC+
   s"  ( ^u8 n -- ^t ) 0 <> IF dup 0 RECURSE " RC+  sa su RC+
   s"  THEN >ELEM ; ;package" RC+
   RC-SRC RC-LEN @ ;

: RC-OTHER$ ( -- str )
   0 RC-LEN !
   s" : RC-OTHER ( ^t n -- ^t ) dup 0 = IF drop EXIT THEN " RC+
   s" drop RC-CAST:NV 0 RECURSE drop ;" RC+
   RC-SRC RC-LEN @ ;

: CASE-RC-LINEAR ( -- )
   s" a recursive call meets the restriction the body adds later" T-LABEL
   ARM
   [: s" RC-FORGE" s" TOK-STEP" RC-RECURSE$ evaluate-closed ;]
      70 TTHROWSQ
   S\" \"code\":\"E-CAST-LINEAR\"" HAS?
   S\" \"token\":\"RECURSE\"" HAS?
   DISARM
   [: s" RC-KEEP" s" N-STEP" RC-RECURSE$ evaluate-closed ;] 0 TTHROWSQ ;

: CASE-RC-OTHER ( -- )
   s" a recursive call at another instance the body leaves free" T-LABEL
   [: RC-OTHER$ evaluate-closed ;] 0 TTHROWSQ ;

: CASE-ELEM-ROWS ( -- )
   s" nor a quotation whose rows end in different tails" T-LABEL
   ARM
   [: s" : STORED-R16 ( ^u8 -- ^[ R -- S ] ) STORED:AT ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"code\":\"E-CAST-LINEAR\"" HAS?
   DISARM
   ARM
   [: s" : STORED-R17 ( ^u8 -- n ) STORED:AT @ STORED:RUN-Q ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"code\":\"E-CAST-LINEAR\"" HAS?
   DISARM
   ARM
   [: s" : STORED-R18 ( -- ) [: STORED:AT ;] is STORED:QR-D ;" evaluate-closed ;]
      70 TTHROWSQ
   S\" \"code\":\"E-CAST-LINEAR\"" HAS?
   DISARM
   [: s" : STORED-QOK1 ( ^u8 -- ^[ -- n ] ) STORED:AT ;" evaluate-closed ;]
      0 TTHROWSQ
   [: s" : STORED-QOK2 ( ^u8 -- ) STORED:AT [: 42 ;] swap ! ;" evaluate-closed ;]
      0 TTHROWSQ ;

public

\ A generic word that passes the cast's stored variable on to its caller.
: AT ( ^u8 -- ^a ) >ELEM ;

: RUN ( -- )
   T-RESET
   CASE-PASS
   CASE-POINTER
   CASE-HELD
   CASE-HELD-SPAN
   CASE-HELD-TOP
   CASE-HELD-SIZE
   CASE-ONE-CELL-MEMBER
   CASE-ONE-CELL-FETCH
   CASE-ONE-CELL-ENUM
   CASE-NULL
   CASE-POINTER-KINDS
   CASE-SUB-KINDS
   CASE-SUB-NAMES
   CASE-SUB-HELP
   CASE-SUB-FRESH
   CASE-ADD-KINDS
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
   CASE-QUOT-FIELD
   CASE-QUOT-ROW
   CASE-QUOT-PTR
   CASE-QUOT-FETCH
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
