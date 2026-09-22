\ Standalone process: retain the compiler while replacing its dictionary prefix.
\ Tier-neutral by design: the subject is the dictionary prefix a reset retains
\ and the checker that survives it; no compiler tier moves that boundary.
require src/core/prefix-boundary.f
: PF-OLD-SIGNATURE ( -- bool ) 0 0= ;

\ The retained checker must not resolve this discarded package shadow while
\ compiling the replacement package's reference to the global primitive.
package PF-SHADOW
public
: CELLS ( n -- n n ) dup ;
;package

package PREFIX-DECLARATIONS-TEST
private

variable PREFIX-END
variable PROT-XT
variable LAYOUT-XT

: HELPER-XT ( ptr u8 n -- n )
   OWNER-API-PRI-WID XREF-FIND-WL
   dup XREF-FOUND? 0= if s" resident helper is missing" 76 die then
   XREF-START ;

: SAVE-BOUNDARY ( -- )
   CORE-PREFIX:FIRST-RECORD PREFIX-END !
   s" (PROT-SPAN)" HELPER-XT PROT-XT !
   s" (LP2VEXEC)" HELPER-XT LAYOUT-XT ! ;

: CHECK-BOUNDARY ( -- )
   ndict@ PREFIX-END @ <> if s" reset kept the wrong dictionary prefix" 76 die then
   s" (PROT-SPAN)" HELPER-XT PROT-XT @ <>
   s" (LP2VEXEC)" HELPER-XT LAYOUT-XT @ <> or if
      s" reset changed a resident helper" 76 die
   then
   s" IMK-NDICT0" 0 XREF-FIND-WL-INDEX -1 <>
   s" SEQ" 0 XREF-FIND-WL-INDEX -1 <> or if
      s" reset retained source-prefix records" 76 die
   then ;

\ Exercise overrides on primitive IDs, which survive source-name retirement.
\ Test the live type rule: artifact output rows are serialized only at freeze.
TRUSTED: OVERRIDE-PRIMITIVES ( -- )
   s" dup" s" bool -- bool bool" TRUST
   s" PF-BOOL-DUP ( bool -- bool bool ) dup" CHECK-CANDIDATE! -1 <> if
      s" boolean override did not accept its declared input" 76 die then
   s" PF-NUM-DUP ( n -- n n ) dup" CHECK-CANDIDATE! 0<> if
      s" signature override did not reject a different input" 76 die then
   s" throw" CHECKER-UNDEFINE
   s" dup" CHECKER-DEFER
   s" throw" CTL-DEAD? if s" control override was not recorded" 76 die then
   s" PF-PRIM-DEFER ( -- ) [: dup ;] is dup" CHECK-CANDIDATE! -1 <> if
      s" primitive defer override was not recorded" 76 die
   then ;

: CHECK-PRIMITIVES ( -- )
   s" PF-NUM-DUP ( n -- n n ) dup" CHECK-CANDIDATE! -1 <> if
      s" source signature override survived reset" 76 die then
   s" PF-BOOL-DUP ( bool -- bool bool ) dup" CHECK-CANDIDATE! -1 <> if
      s" primitive dup lost its polymorphic effect" 76 die then
   s" throw" CTL-DEAD? 0= if s" primitive throw lost its control effect" 76 die then
   s" PF-PRIM-DEFER ( -- ) [: dup ;] is dup" CHECK-CANDIDATE! 0<> if
      s" primitive inherited a source defer flag" 76 die
   then ;

TRUSTED: RESET ( -- )
   SAVE-BOUNDARY
   0 set-check
   0 set-top-check
   OVERRIDE-PRIMITIVES
   CHECKER-RESET-SOURCE
   CHECK-PRIMITIVES
   CHECKER-RESET-SOURCE
   CHECK-PRIMITIVES
   CORE-PREFIX:FIRST-RECORD seed-ndict!
   CHECK-BOUNDARY ;

: LOAD-CORE ( -- )
   s" src/core/util.f" included
   s" src/core/cell.f" included
   s" src/core/exec-vector.f" included ;

TRUSTED: REPLAY ( -- n )
   s" variable V 40 constant BASE create ROW 2 cells allot : PF-OLD-SIGNATURE ( -- n ) 2 ; : PF-STORE ( -- ) BASE V ! PF-OLD-SIGNATURE V +! V @ ROW ! ; : PF-READ ( -- n ) ROW @ ; : PF-IMM? ( ptr u8 n -- n ) tok-imm? ; PF-STORE PF-READ" evaluate ;

TRUSTED: REPLAY-SHADOW ( -- n )
   s" package PF-SHADOW public : OFFSET ( n -- n ) cells ; ;package 3 PF-SHADOW:OFFSET" evaluate ;

TRUSTED: REPLAY-DEFER ( -- n )
   s" defer PF-DEFER ( -- n ) : PF-SET ( -- ) [: 42 ;] is PF-DEFER ; PF-SET PF-DEFER" evaluate ;

TRUSTED: REPLAY-USING ( -- n )
   s" package PF-LIB public : ANSWER ( -- n ) 44 ; ;package using PF-LIB : PF-CALL-ANSWER ( -- n ) ANSWER ; PF-CALL-ANSWER ;using" evaluate ;

variable EFFECTS
variable DEFERS
variable RAWS
variable USINGS
create TARGET-OWNER 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 , 0 ,

: RECORD-EFFECT ( ptr u8 n ptr u8 n -- ) 2drop 2drop 1 EFFECTS +! ;
: RECORD-DEFER ( ptr u8 n -- ) 2drop 1 DEFERS +! ;
: RECORD-RAW ( ptr u8 n ptr u8 n -- ) 2drop 2drop 1 RAWS +! ;
: RECORD-USING ( ptr u8 n -- ) 2drop 1 USINGS +! ;

\ Test-owned target uses actual compiled callbacks, hidden with its package.
: INSTALL-TARGET ( -- )
   ['] RECORD-EFFECT TARGET-OWNER NCOMP-DISPATCH:DECL-EFFECT-OFF + xt!
   ['] RECORD-DEFER TARGET-OWNER NCOMP-DISPATCH:DECL-DEFER-OFF + xt!
   ['] RECORD-RAW TARGET-OWNER NCOMP-DISPATCH:DECL-RAW-OFF + xt!
   ['] RECORD-USING TARGET-OWNER NCOMP-DISPATCH:DECL-USING-OFF + xt!
   TARGET-OWNER data-base NCOMP-DISPATCH:TARGET-DECL-CELL + 0 ptr-field ! ;

TRUSTED: REPLAY-TARGET ( -- n )
   s" variable PF-RAW-TARGET using PF-LIB ;using defer PF-LATE ( -- n ) : PF-LATE-SET ( -- ) [: 43 ;] is PF-LATE ; PF-LATE-SET PF-LATE" evaluate ;

: RUN ( -- )
   s" CHECKER-RESET-SOURCE" 0 search-wl 0<> if
      s" checker reset is visible outside the engine" 76 die
   then
   s" PF-BAD-RESET ( -- ) CHECKER-RESET-SOURCE" CHECK-CANDIDATE! 0<> if
      s" ordinary code can reset checker state" 76 die
   then
   RESET
   LOAD-CORE
   REPLAY-SHADOW 24 <> if s" discarded package shadow hid primitive cells" 76 die then
   REPLAY 42 <> if s" native prefix declarations failed" 76 die then
   REPLAY-DEFER 42 <> if s" pending defer metadata missed the compiler" 76 die then
   s" PF-BAD-DEFER ( -- ) [: 0 0= ;] is PF-DEFER" CHECK-CANDIDATE! 0<> if
      s" pending defer accepted a mismatched quotation" 76 die
   then
   REPLAY-USING 44 <> if s" using context missed the active compiler" 76 die then
   INSTALL-TARGET
   REPLAY-TARGET 43 <>
   EFFECTS @ 1 <> or DEFERS @ 1 <> or RAWS @ 1 <> or USINGS @ 1 <> or if
      s" declaration metadata missed the target checker" 76 die
   then ;

' RUN
;package
execute
