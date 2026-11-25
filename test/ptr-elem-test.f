\ ptr-elem-test.f - a pointer's element type is part of the checker type.
\ `^u8` and `^n` are refused for each other in both directions at every
\ place types meet; value widening (u8 -> n) is unchanged. Each case is a whole
\ definition certified by CHECK-QUIET-CANDIDATE!, and each reject has a control
\ differing only in the element type, so refusing all pointer traffic fails.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f

package PTR-ELEM-TEST

\ ---- words the candidate definitions call ----------------------------------
public

: TAKE-U8 ( ^u8 -- n )   c@ ;
: TAKE-CELL ( ^n -- n )   @ ;
: TAKE-PP ( ^^u8 -- ^u8 )   @ ;
: TAKE-VAR ( ^a -- ^a ) ;
: WANT-U32 ( u32 -- n ) ;

private

\ Verdicts CHECK-QUIET-CANDIDATE! answers.
-1 constant CERTIFIED
0 constant REFUSED

: REJECTS ( str -- )
   CHECK-QUIET-CANDIDATE! REFUSED T= ;

: CERTIFIES ( str -- )
   CHECK-QUIET-CANDIDATE! CERTIFIED T= ;

\ ---- direct call ------------------------------------------------------------
: DIRECT ( -- )
   s" PE-PASS8-DIRECT ( ^n -- n ) TAKE-U8" REJECTS
   s" PE-PASSN-DIRECT ( ^u8 -- n ) TAKE-CELL" REJECTS
   s" PE-SAME8-DIRECT ( ^u8 -- n ) TAKE-U8" CERTIFIES
   s" PE-SAMEN-DIRECT ( ^n -- n ) TAKE-CELL" CERTIFIES ;

\ ---- qualified call ---------------------------------------------------------
\ A PKG:WORD call resolves through a different path than a bare call, so the
\ element check has to hold there too.
: QUALIFIED ( -- )
   s" PE-PASS8-QUAL ( ^n -- n ) PTR-ELEM-TEST:TAKE-U8" REJECTS
   s" PE-PASSN-QUAL ( ^u8 -- n ) PTR-ELEM-TEST:TAKE-CELL" REJECTS
   s" PE-SAME8-QUAL ( ^u8 -- n ) PTR-ELEM-TEST:TAKE-U8" CERTIFIES
   s" PE-SAMEN-QUAL ( ^n -- n ) PTR-ELEM-TEST:TAKE-CELL" CERTIFIES ;

\ ---- typed local ------------------------------------------------------------
: TYPED-LOCAL ( -- )
   s" PE-PASS8-LOCAL ( ^n -- n ) {: p:^ :} p TAKE-U8" REJECTS
   s" PE-PASSN-LOCAL ( ^u8 -- n ) {: p:^ :} p TAKE-CELL" REJECTS
   s" PE-SAME8-LOCAL ( ^u8 -- n ) {: p:^ :} p TAKE-U8" CERTIFIES
   s" PE-SAMEN-LOCAL ( ^n -- n ) {: p:^ :} p TAKE-CELL" CERTIFIES ;

\ ---- quotation reached through a call chain ---------------------------------
\ A quotation's effect is unified where it is executed; `catch` forces it to be
\ stack-preserving, so the pointer round-trips on the data stack.
: QUOTATION ( -- )
   s" PE-PASS8-QUOT ( ^n -- ^n ) [: dup TAKE-U8 drop ;] catch drop" REJECTS
   s" PE-PASSN-QUOT ( ^u8 -- ^u8 ) [: dup TAKE-CELL drop ;] catch drop" REJECTS
   s" PE-SAME8-QUOT ( ^u8 -- ^u8 ) [: dup TAKE-U8 drop ;] catch drop" CERTIFIES
   s" PE-SAMEN-QUOT ( ^n -- ^n ) [: dup TAKE-CELL drop ;] catch drop" CERTIFIES ;

\ ---- control-flow join -----------------------------------------------------
: JOIN ( -- )
   s" PE-JOIN-CONSUME ( ^u8 ^n bool -- n ) IF drop ELSE nip THEN TAKE-U8" REJECTS
   s" PE-JOIN-MIXED-OUT ( ^u8 ^n bool -- ^u8 ) IF drop ELSE nip THEN" REJECTS
   s" PE-JOIN-MIXED-OUT-N ( ^u8 ^n bool -- ^n ) IF drop ELSE nip THEN" REJECTS
   s" PE-JOIN-SAME ( ^u8 ^u8 bool -- n ) IF drop TAKE-U8 ELSE nip TAKE-U8 THEN" CERTIFIES
   s" PE-JOIN-SAME-OUT ( ^u8 ^u8 bool -- ^u8 ) IF drop ELSE nip THEN" CERTIFIES ;

\ ---- memory access through the wrong element -------------------------------
\ Cell `@` over a byte span is the width rule's refusal; byte `c@` over a cell
\ span is the pointee rule's.
: MEMORY-ACCESS ( -- )
   s" PE-CELL-FETCH-BYTES ( ^u8 -- n ) @" REJECTS
   s" PE-BYTE-FETCH-CELLS ( ^n -- n ) c@" REJECTS
   s" PE-BYTE-STORE-CELLS ( n ^n -- ) c!" REJECTS
   s" PE-BYTE-FETCH-BYTES ( ^u8 -- n ) c@" CERTIFIES
   s" PE-CELL-FETCH-CELLS ( ^n -- n ) @" CERTIFIES ;

\ ---- widening is a value rule, not a pointee rule --------------------------
\ Every concrete element pair stays distinct under a pointer, `n` and `cell`
\ included.
: WIDENING ( -- )
   s" PE-WIDEN-VALUE ( u8 -- n ) WANT-U32" CERTIFIES
   s" PE-WIDEN-VALUE-N ( u32 -- n ) TAKE-CELL drop 0" REJECTS
   s" PE-WIDEN-POINTEE-U32 ( ^u32 -- n ) TAKE-CELL" REJECTS
   s" PE-WIDEN-POINTEE-U8 ( ^u8 -- n ) TAKE-CELL" REJECTS
   s" PE-WIDEN-POINTEE-CELL ( ^cell -- n ) TAKE-CELL" REJECTS ;

\ ---- unchanged behaviour ---------------------------------------------------
: UNCHANGED ( -- )
   s" PE-PP-SAME ( ^^u8 -- ^u8 ) TAKE-PP" CERTIFIES
   s" PE-PP-MIXED ( ^^n -- ^u8 ) TAKE-PP" REJECTS
   s" PE-PP-DEPTH ( ^u8 -- ^u8 ) TAKE-PP" REJECTS
   s" PE-VAR-FROM-U8 ( ^u8 -- ^u8 ) TAKE-VAR" CERTIFIES
   s" PE-VAR-FROM-N ( ^n -- ^n ) TAKE-VAR" CERTIFIES
   s" PE-BARE-N ( n -- n ) TAKE-U8" REJECTS
   s" PE-BARE-N-CELL ( n -- n ) TAKE-CELL" REJECTS ;

public

: RUN ( -- )
   T-RESET
   DIRECT
   QUALIFIED
   TYPED-LOCAL
   QUOTATION
   JOIN
   MEMORY-ACCESS
   WIDENING
   UNCHANGED
   T-REPORT ;

RUN

;package
