\ ptr-elem-test.f - a pointer's element type is part of the checker type.
\ Run: bin/hb --load test/ptr-elem-test.f
\
\ WHAT THIS PINS. `ptr u8` and `ptr n` are different types. The checker must
\ refuse to pass one where the other is declared, in BOTH directions, at every
\ place a type meets a type: a direct call, a qualified (PKG:WORD) call, a typed
\ local, a quotation reached through a call chain, and a control-flow join. It
\ must also refuse a byte fetch through a cell pointer, which is the mirror of
\ the cell fetch through a byte pointer that the width rule already refused.
\
\ WHY THE TEST LOOKS LIKE THIS. Every case is a whole candidate DEFINITION
\ handed to the checker (CHECK-QUIET-CANDIDATE!, verdict -1 certified /
\ 0 refused / 1 unresolvable), so what is measured is the real certification of
\ real source through the real unifier - not a re-implementation of the rule.
\ Each reject is paired with a positive control that differs only in the element
\ type, so a checker that simply refused all pointer traffic would fail the
\ suite instead of passing it.
\
\ WHAT MUST KEEP WORKING. Value-position integer widening (u8 -> u16 -> u32 ->
\ n) is unchanged: widening is a rule about values on the stack, not about what
\ a pointer points at. Nested pointers (`ptr ptr u8`) and a polymorphic pointee
\ (`ptr a`) also keep behaving as before.
\
\ THE DEFECT THIS SUITE WOULD HAVE CAUGHT. Before the fix, CON-OK? applied its
\ strict-pointee guard only to the width lattice, so the two generic-`n` rules
\ underneath it still fired inside a pointee: `n` met `u8` and `ptr n` silently
\ satisfied `ptr u8`. `PASS8-DIRECT` below certified with exit 0.

require lib/errors.f
require lib/string.f
require lib/test.f
require test/checker-assert.f

package PTR-ELEM-TEST

\ ---- words the candidate definitions call ----------------------------------
\ One pair per element type, each doing the access its element allows: c@ on a
\ byte pointer, @ on a cell pointer. Nothing here is a fixture stand-in - these
\ are the ordinary checked words whose declared parameter types the candidates
\ have to satisfy.
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
\ The measured defect and its mirror. Both operands are concrete pointers whose
\ elements differ, so neither direction may certify.
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
\ Binding the pointer to a named local and calling through the local must not
\ launder the element type.
: TYPED-LOCAL ( -- )
   s" PE-PASS8-LOCAL ( ptr n -- n ) {: p:ptr :} p TAKE-U8" REJECTS
   s" PE-PASSN-LOCAL ( ptr u8 -- n ) {: p:ptr :} p TAKE-CELL" REJECTS
   s" PE-SAME8-LOCAL ( ptr u8 -- n ) {: p:ptr :} p TAKE-U8" CERTIFIES
   s" PE-SAMEN-LOCAL ( ptr n -- n ) {: p:ptr :} p TAKE-CELL" CERTIFIES ;

\ ---- quotation reached through a call chain ---------------------------------
\ A quotation's effect is unified where it is executed, not where it is written.
\ `catch` is the checked executor whose contract also forces the quotation to be
\ stack-preserving, so the pointer travels in and back out on the data stack -
\ the element type has to survive that round trip.
: QUOTATION ( -- )
   s" PE-PASS8-QUOT ( ptr n -- ptr n ) [: dup TAKE-U8 drop ;] catch drop" REJECTS
   s" PE-PASSN-QUOT ( ptr u8 -- ptr u8 ) [: dup TAKE-CELL drop ;] catch drop" REJECTS
   s" PE-SAME8-QUOT ( ptr u8 -- ptr u8 ) [: dup TAKE-U8 drop ;] catch drop" CERTIFIES
   s" PE-SAMEN-QUOT ( ptr n -- ptr n ) [: dup TAKE-CELL drop ;] catch drop" CERTIFIES ;

\ ---- control-flow join -----------------------------------------------------
\ Two arms that leave pointers with different elements cannot be joined: the
\ join is a unification like any other. The positive control proves the join
\ itself still works when both arms agree.
: JOIN ( -- )
   s" PE-JOIN-CONSUME ( ptr u8 ptr n bool -- n ) IF drop ELSE nip THEN TAKE-U8" REJECTS
   s" PE-JOIN-MIXED-OUT ( ptr u8 ptr n bool -- ptr u8 ) IF drop ELSE nip THEN" REJECTS
   s" PE-JOIN-MIXED-OUT-N ( ptr u8 ptr n bool -- ptr n ) IF drop ELSE nip THEN" REJECTS
   s" PE-JOIN-SAME ( ptr u8 ptr u8 bool -- n ) IF drop TAKE-U8 ELSE nip TAKE-U8 THEN" CERTIFIES
   s" PE-JOIN-SAME-OUT ( ptr u8 ptr u8 bool -- ptr u8 ) IF drop ELSE nip THEN" CERTIFIES ;

\ ---- memory access through the wrong element -------------------------------
\ The two mirrors. Cell `@` over a byte span was already refused by the width
\ rule; byte `c@` over a cell span is the direction the pointee rule closes.
: MEMORY-ACCESS ( -- )
   s" PE-CELL-FETCH-BYTES ( ptr u8 -- n ) @" REJECTS
   s" PE-BYTE-FETCH-CELLS ( ptr n -- n ) c@" REJECTS
   s" PE-BYTE-STORE-CELLS ( n ptr n -- ) c!" REJECTS
   s" PE-BYTE-FETCH-BYTES ( ptr u8 -- n ) c@" CERTIFIES
   s" PE-CELL-FETCH-CELLS ( ptr n -- n ) @" CERTIFIES ;

\ ---- widening is a value rule, not a pointee rule --------------------------
\ u8 widens to n as a VALUE, and every other concrete element pair stays
\ distinct under a pointer - including the two 64-bit generic spellings `n` and
\ `cell`, which the leaked generic-n rules used to make interchangeable.
: WIDENING ( -- )
   s" PE-WIDEN-VALUE ( u8 -- n ) WANT-U32" CERTIFIES
   s" PE-WIDEN-VALUE-N ( u32 -- n ) TAKE-CELL drop 0" REJECTS
   s" PE-WIDEN-POINTEE-U32 ( ptr u32 -- n ) TAKE-CELL" REJECTS
   s" PE-WIDEN-POINTEE-U8 ( ptr u8 -- n ) TAKE-CELL" REJECTS
   s" PE-WIDEN-POINTEE-CELL ( ptr cell -- n ) TAKE-CELL" REJECTS ;

\ ---- unchanged behaviour ---------------------------------------------------
\ A nested byte pointer still matches itself and still refuses a nested cell
\ pointer; a polymorphic pointee still binds either concrete element; and a bare
\ integer where a pointer is declared still rejects (the pre-existing control
\ from the original report).
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
