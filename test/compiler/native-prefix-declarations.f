\ Standalone process: retain the compiler while replacing its dictionary prefix.
: PF-OLD-SIGNATURE ( -- bool ) 0 0= ;

package PREFIX-DECLARATIONS-TEST
private

TRUSTED: RESET ( -- )
   0 set-check
   0 set-top-check
   CHECKER-RESET-SOURCE
   IMK-NDICT0 @ 1 - seed-ndict! ;

: LOAD-CORE ( -- )
   s" src/core/util.f" included
   s" src/core/cell.f" included
   s" src/core/exec-vector.f" included ;

TRUSTED: REPLAY ( -- n )
   s" variable V 40 constant BASE create ROW 2 cells allot : PF-OLD-SIGNATURE ( -- n ) 2 ; : PF-STORE ( -- ) BASE V ! PF-OLD-SIGNATURE V +! V @ ROW ! ; : PF-READ ( -- n ) ROW @ ; : PF-IMM? ( ptr u8 n -- n ) tok-imm? ; PF-STORE PF-READ" evaluate ;

TRUSTED: REPLAY-DEFER ( -- n )
   s" defer PF-DEFER ( -- n ) : PF-SET ( -- ) [: 42 ;] is PF-DEFER ; PF-SET PF-DEFER" evaluate ;

TRUSTED: REPLAY-USING ( -- n )
   s" package PF-LIB public : ANSWER ( -- n ) 44 ; ;package using PF-LIB : PF-CALL-ANSWER ( -- n ) ANSWER ; PF-CALL-ANSWER ;using" evaluate ;

variable EFFECTS
variable DEFERS
variable RAWS
variable USINGS
create TARGET-OWNER NCOMP-DISPATCH:DECL-BYTES allot

: RECORD-EFFECT ( ptr u8 n ptr u8 n -- ) 2drop 2drop 1 EFFECTS +! ;
: RECORD-DEFER ( ptr u8 n -- ) 2drop 1 DEFERS +! ;
: RECORD-RAW ( ptr u8 n ptr u8 n -- ) 2drop 2drop 1 RAWS +! ;
: RECORD-USING ( ptr u8 n -- ) 2drop 1 USINGS +! ;

\ Test-owned target uses actual compiled callbacks, hidden with its package.
TRUSTED: INSTALL-TARGET ( -- )
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
