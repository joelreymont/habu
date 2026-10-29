\ ptr-elem-test.f - a pointer's element type is part of the checker type.
\ `ptr u8` and `ptr n` are refused for each other in both directions at every
\ place types meet; value widening (u8 -> n) is unchanged. Each case is a whole
\ definition certified by CHECK-QUIET-CANDIDATE!, and each reject has a control
\ differing only in the element type, so refusing all pointer traffic fails.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f

package PTR-ELEM-TEST

\ ---- words the candidate definitions call ----------------------------------
public

: TAKE-U8 ( ptr u8 -- n )   c@ ;
: TAKE-CELL ( ptr n -- n )   @ ;
: TAKE-PP ( ptr ptr u8 -- ptr u8 )   @ ;
: TAKE-VAR ( ptr a -- ptr a ) ;
: WANT-U32 ( u32 -- n ) ;

private

\ Verdicts CHECK-QUIET-CANDIDATE! answers.
-1 constant CERTIFIED
0 constant REFUSED

: REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! REFUSED T= ;

: CERTIFIES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! CERTIFIED T= ;

\ ---- direct call ------------------------------------------------------------
: DIRECT ( -- )
   s" PE-PASS8-DIRECT ( ptr n -- n ) TAKE-U8" REJECTS
   s" PE-PASSN-DIRECT ( ptr u8 -- n ) TAKE-CELL" REJECTS
   s" PE-SAME8-DIRECT ( ptr u8 -- n ) TAKE-U8" CERTIFIES
   s" PE-SAMEN-DIRECT ( ptr n -- n ) TAKE-CELL" CERTIFIES ;

\ ---- qualified call ---------------------------------------------------------
\ A PKG:WORD call resolves through a different path than a bare call, so the
\ element check has to hold there too.
: QUALIFIED ( -- )
   s" PE-PASS8-QUAL ( ptr n -- n ) PTR-ELEM-TEST:TAKE-U8" REJECTS
   s" PE-PASSN-QUAL ( ptr u8 -- n ) PTR-ELEM-TEST:TAKE-CELL" REJECTS
   s" PE-SAME8-QUAL ( ptr u8 -- n ) PTR-ELEM-TEST:TAKE-U8" CERTIFIES
   s" PE-SAMEN-QUAL ( ptr n -- n ) PTR-ELEM-TEST:TAKE-CELL" CERTIFIES ;

\ ---- typed local ------------------------------------------------------------
: TYPED-LOCAL ( -- )
   s" PE-PASS8-LOCAL ( ptr n -- n ) {: p:ptr :} p TAKE-U8" REJECTS
   s" PE-PASSN-LOCAL ( ptr u8 -- n ) {: p:ptr :} p TAKE-CELL" REJECTS
   s" PE-SAME8-LOCAL ( ptr u8 -- n ) {: p:ptr :} p TAKE-U8" CERTIFIES
   s" PE-SAMEN-LOCAL ( ptr n -- n ) {: p:ptr :} p TAKE-CELL" CERTIFIES ;

\ ---- quotation reached through a call chain ---------------------------------
\ A quotation's effect is unified where it is executed; `catch` forces it to be
\ stack-preserving, so the pointer round-trips on the data stack.
: QUOTATION ( -- )
   s" PE-PASS8-QUOT ( ptr n -- ptr n ) [: dup TAKE-U8 drop ;] catch drop" REJECTS
   s" PE-PASSN-QUOT ( ptr u8 -- ptr u8 ) [: dup TAKE-CELL drop ;] catch drop" REJECTS
   s" PE-SAME8-QUOT ( ptr u8 -- ptr u8 ) [: dup TAKE-U8 drop ;] catch drop" CERTIFIES
   s" PE-SAMEN-QUOT ( ptr n -- ptr n ) [: dup TAKE-CELL drop ;] catch drop" CERTIFIES ;

\ ---- control-flow join -----------------------------------------------------
: JOIN ( -- )
   s" PE-JOIN-CONSUME ( ptr u8 ptr n bool -- n ) IF drop ELSE nip THEN TAKE-U8" REJECTS
   s" PE-JOIN-MIXED-OUT ( ptr u8 ptr n bool -- ptr u8 ) IF drop ELSE nip THEN" REJECTS
   s" PE-JOIN-MIXED-OUT-N ( ptr u8 ptr n bool -- ptr n ) IF drop ELSE nip THEN" REJECTS
   s" PE-JOIN-SAME ( ptr u8 ptr u8 bool -- n ) IF drop TAKE-U8 ELSE nip TAKE-U8 THEN" CERTIFIES
   s" PE-JOIN-SAME-OUT ( ptr u8 ptr u8 bool -- ptr u8 ) IF drop ELSE nip THEN" CERTIFIES ;

\ ---- memory access through the wrong element -------------------------------
\ Cell `@` over a byte span is the width rule's refusal; byte `c@` over a cell
\ span is the pointee rule's.
: MEMORY-ACCESS ( -- )
   s" PE-CELL-FETCH-BYTES ( ptr u8 -- n ) @" REJECTS
   s" PE-BYTE-FETCH-CELLS ( ptr n -- n ) c@" REJECTS
   s" PE-BYTE-STORE-CELLS ( n ptr n -- ) c!" REJECTS
   s" PE-BYTE-FETCH-BYTES ( ptr u8 -- n ) c@" CERTIFIES
   s" PE-CELL-FETCH-CELLS ( ptr n -- n ) @" CERTIFIES ;

\ ---- widening is a value rule, not a pointee rule --------------------------
\ Every concrete element pair stays distinct under a pointer, `n` and `cell`
\ included.
: WIDENING ( -- )
   s" PE-WIDEN-VALUE ( u8 -- n ) WANT-U32" CERTIFIES
   s" PE-WIDEN-VALUE-N ( u32 -- n ) TAKE-CELL drop 0" REJECTS
   s" PE-WIDEN-POINTEE-U32 ( ptr u32 -- n ) TAKE-CELL" REJECTS
   s" PE-WIDEN-POINTEE-U8 ( ptr u8 -- n ) TAKE-CELL" REJECTS
   s" PE-WIDEN-POINTEE-CELL ( ptr cell -- n ) TAKE-CELL" REJECTS ;

\ ---- unchanged behaviour ---------------------------------------------------
: UNCHANGED ( -- )
   s" PE-PP-SAME ( ptr ptr u8 -- ptr u8 ) TAKE-PP" CERTIFIES
   s" PE-PP-MIXED ( ptr ptr n -- ptr u8 ) TAKE-PP" REJECTS
   s" PE-PP-DEPTH ( ptr u8 -- ptr u8 ) TAKE-PP" REJECTS
   s" PE-VAR-FROM-U8 ( ptr u8 -- ptr u8 ) TAKE-VAR" CERTIFIES
   s" PE-VAR-FROM-N ( ptr n -- ptr n ) TAKE-VAR" CERTIFIES
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
