\ native-wide-mem.f - production multi-cell typed loads and stores.

require test/compiler/native-eval-fixture.f
require lib/errors.f
require lib/string.f
require lib/test.f
require lib/ieee754.f
require lib/adt/option.f
require src/compiler/native/compiler.f

package NWM
private

public

\ ---- the families the cases move ---------------------------------------------
\ One cell, two and three. ONE is here because it is the other half of the
\ minimal pair the diagnosis reduced to: a one-field record's `@` compiled before
\ this leaf and must go on compiling, and it is the row that says the width is
\ being READ rather than a wide lowering being taken unconditionally.
STRUCTURE w1 0
  FIELD a n
;STRUCTURE

STRUCTURE w2 0
  FIELD a n
  FIELD b n
;STRUCTURE

STRUCTURE w3 0
  FIELD a n
  FIELD b n
  FIELD c n
;STRUCTURE

\ lib/report.f's own record, field for field: a header span, its length, and an
\ alignment code. It is the production shape this leaf exists for, and its first
\ field being a POINTER is what makes an exchanged slot a wrong string.
STRUCTURE col 0
  FIELD ha ptr u8
  FIELD hn n
  FIELD al n
;STRUCTURE

\ A TAGGED family, whose value carries a tag on top: reading one out of memory
\ has to leave a bundle a dispatch will accept, which is the composition with the
\ landed MATCH work.
SUMTYPE bx 0
  VARIANT b0 ;VARIANT
  VARIANT b2 n n ;VARIANT
;SUMTYPE

\ And a PARAMETRIC family this package owns, instantiated with a two-cell
\ argument: `opt2<pt>` occupies three cells where its declaration reserves two,
\ so its width is one the registry cannot answer and only the checker knows. It
\ is the composition with the landed construction work - a value built with pads
\ added at the site, stored whole, and read back whole.
PRODUCT pt 0
  FIELD x n
  FIELD y n
;PRODUCT

ENUM opt2 1
   VARIANT n2 ;VARIANT
   VARIANT s2 FIELD value a ;VARIANT
;ENUM

private

\ ---- where the values live ---------------------------------------------------
\ One slot per family is enough for every case; the index is still a parameter
\ everywhere, because an accessor that took none would compile the address as a
\ constant and stop being the shape lib/report.f writes.
4 TYPED-BUFFER W1-AT w1
4 TYPED-BUFFER W2-AT w2
4 TYPED-BUFFER W3-AT w3
4 TYPED-BUFFER COL-AT col
4 TYPED-BUFFER BX-AT bx
4 TYPED-BUFFER OP-AT opt2<pt>
variable SC                            \ one ordinary cell: the scalar access's own home

create BYTE-SLOT 1 allot
variable REAL-SLOT

: E-BYTE-RT ( u8 -- u8 ) BYTE-SLOT c! BYTE-SLOT c@ ;
: E-REAL-RT ( r -- r ) REAL-SLOT ! REAL-SLOT @ ;

\ A narrow test boundary that views one physical cell of W2 as a scalar. The
\ checked wide accessors below remain the production path under test.
\ Retirement owner: habu-typed-defining-words-aa224eb5.
TRUSTED: W2-SCALAR-AT ( n -- ptr n )
   0 W2-AT swap cells + ;

: W2-SCALAR! ( n n -- )
   W2-SCALAR-AT ! ;

: W2-SCALAR@ ( n -- n )
   W2-SCALAR-AT @ ;

\ ---- the production programs under test --------------------------------------
: E-L1 ( n -- n )
   W1-AT @ NWM-W1:UNMAKE 3 * ;

: E-L2 ( n -- n )
   W2-AT @ NWM-W2:UNMAKE 3 * swap 5 * + ;

: E-L3 ( n -- n )
   W3-AT @ NWM-W3:UNMAKE 5 * swap 11 * + swap 17 * + ;

: E-S1 ( n n -- )
   {: x:n k:n :} x NWM-W1:MAKE k W1-AT ! ;

: E-S2 ( n n n -- )
   {: x:n y:n k:n :} x y NWM-W2:MAKE k W2-AT ! ;

: E-S3 ( n n n n -- )
   {: x:n y:n z:n k:n :} x y z NWM-W3:MAKE k W3-AT ! ;

\ Store and read back in one body, which is what a round trip through the same
\ compilation proves: the cells came back in the order they went in.
: E-RT2 ( n n n -- n )
   {: x:n y:n k:n :} x y NWM-W2:MAKE k W2-AT !  k E-L2 ;

\ Read, change one cell, write back, read again. It is the shape every mutable
\ record in the tree is used through, and the one where a load and a store that
\ disagreed about the order would still answer a plausible number.
: E-LMS ( n n -- n )
   {: d:n k:n :}
   k W3-AT @ NWM-W3:UNMAKE {: a:n b:n c:n :}
   a d + b c NWM-W3:MAKE k W3-AT !
   k E-L3 ;

\ ---- lib/report.f's three rows, reduced --------------------------------------
\ COL+ stores a whole record through a bounds-checked buffer; COL-HDR@ reads it
\ back and keeps the header; COL-AL@ reads it back and keeps the alignment. All
\ three refused before this leaf, and they are the acceptance.
: E-COL+ ( ptr u8 n n n -- )
   {: h:ptr u:n al:n k:n :} h u al NWM-COL:MAKE k COL-AT ! ;

: E-COL-HDR ( n -- ptr u8 n )
   COL-AT @ NWM-COL:UNMAKE drop ;

: E-COL-AL ( n -- n )
   COL-AT @ NWM-COL:UNMAKE nip nip ;

\ ---- a bundle read out of memory and dispatched over -------------------------
: E-MKBX ( n -- bx )
   dup 0 > if  dup 3 *  swap 5 *  NWM-BX:B2  else  drop NWM-BX:B0  then ;

: E-SBX ( n n -- )
   {: v:n k:n :} v E-MKBX k BX-AT ! ;

: E-LBX ( n -- n )
   BX-AT @ MATCH bx
      b0 OF 0 ENDOF
      b2 OF 7 * swap 11 * + ENDOF
   ;MATCH ;

\ ---- a construction wider than its declaration, stored and read back ---------
: E-MKOP ( n -- opt2<pt> )
   dup 0 > if  dup 3 *  swap 5 *  NWM-PT:MAKE NWM-OPT2:S2  else  drop NWM-OPT2:N2  then ;

: E-SOP ( n n -- )
   {: v:n k:n :} v E-MKOP k OP-AT ! ;

: E-LOP ( n -- n )
   OP-AT @ MATCH opt2
      n2 OF 0 ENDOF
      s2 OF NWM-PT:UNMAKE 7 * swap 11 * + ENDOF
   ;MATCH ;

\ ---- the four bodies that move the access's own offset -----------------------
\ TWO ACCESSES OF DIFFERENT WIDTHS IN ONE BODY. Their widths are filed under
\ their own tokens, so a reader keyed on the definition, on the family or on the
\ order the accesses appear in gives one of them the other's number and moves the
\ wrong cells with every count agreeing.
: E-MIX ( n -- n )
   dup W2-AT @ NWM-W2:UNMAKE 3 * swap 5 * +
   swap W3-AT @ NWM-W3:UNMAKE 7 * swap 11 * + swap 13 * +
   + ;

\ A SCALAR ACCESS BESIDE A WIDE ONE. The scalar `@` has no width fact at all, so
\ what it must get is the absent answer - one cell - and it must get it while a
\ wide fact for another token is in the same table.
: E-SCW ( n n -- n )
   {: v:n k:n :}
   v SC !
   SC @ 3 *
   k W2-AT @ NWM-W2:UNMAKE 5 * swap 7 * + + ;

\ A COMMENT IN FRONT OF THE ACCESS. A parenthesised comment is not a token and
\ the tape has no row for it, but its BYTES are in the text the checker read - so
\ it moves the access's offset and moves it for the checker and the tape alike.
\ The comment writes `@` and `!` so a reader that scanned text rather than
\ consulting the table would find the wrong one first.
: E-CMT ( n -- n )
   ( this comment writes @ and ! and is not a token )
   W2-AT @ NWM-W2:UNMAKE 3 * swap 5 * + ;

\ A STRING LITERAL IN FRONT OF IT, for the same reason and one step harder: the
\ reader SPENDS the payload rather than tokenising it, so the literal is one tape
\ row whose bytes are many.
: E-STR ( n -- n )
   s" @ ! @ ! @" 2drop
   W2-AT @ NWM-W2:UNMAKE 3 * swap 5 * + ;


\ Dynamic compilation is needed only for the checker-refusal probe.
: EV-RC ( ptr u8 n -- n )
   NATIVE-EVAL:DEFINE-RC ;

: LOAD-CASE ( -- )
   s" one-, two- and three-cell loads preserve cell order" T-LABEL
   7 0 E-S1  0 E-L1 21 T=
   3 4 0 E-S2  0 E-L2 27 T=
   2 3 4 0 E-S3  0 E-L3 87 T= ;

: NARROW-STORE-CASE ( -- )
   s" guarded byte and real stores preserve their values" T-LABEL
   $AB E-BYTE-RT $AB T=
   $8000000000000000 IEEE754:BITS>F64 E-REAL-RT
   IEEE754:F64>BITS $8000000000000000 T=
   $7FF8000000001234 IEEE754:BITS>F64 E-REAL-RT
   IEEE754:F64>BITS $7FF8000000001234 T= ;

: STORE-CASE ( -- )
   s" wide stores replace every cell" T-LABEL
   9 9 0 E-S2
   3 4 0 E-S2  0 E-L2 27 T=
   9 9 9 0 E-S3
   2 3 4 0 E-S3  0 E-L3 87 T= ;

: PHYSICAL-CASE ( -- )
   s" scalar-seeded slots are read in declared wide-field order" T-LABEL
   3 0 W2-SCALAR!
   4 1 W2-SCALAR!
   0 E-L2 27 T=

   s" a wide store writes those same physical scalar slots" T-LABEL
   7 11 0 E-S2
   0 W2-SCALAR@ 7 T=
   1 W2-SCALAR@ 11 T= ;

: ROUNDTRIP-CASE ( -- )
   s" store-load and read-change-write-read round trips work" T-LABEL
   3 4 0 E-RT2 27 T=
   2 3 4 0 E-S3
   5 0 E-LMS 172 T= ;

: COL-CASE ( -- )
   s" lib/report's pointer-bearing three-cell record round trips" T-LABEL
   s" hdr" 1 0 E-COL+
   0 E-COL-HDR s" hdr" T$=
   0 E-COL-AL 1 T= ;

: DISPATCH-CASE ( -- )
   s" a stored sum bundle feeds MATCH" T-LABEL
   6 0 E-SBX  0 E-LBX 408 T=
   0 0 E-SBX  0 E-LBX 0 T= ;

: WIDE-INST-CASE ( -- )
   s" a stored construction wider than its declaration round trips" T-LABEL
   6 0 E-SOP  0 E-LOP 408 T=
   0 0 E-SOP  0 E-LOP 0 T= ;

: OFFSET-CASE ( -- )
   s" distinct accesses keep their own checker-filed widths" T-LABEL
   3 4 0 E-S2
   2 3 4 0 E-S3
   0 E-MIX 114 T=
   5 0 E-SCW 56 T=
   0 E-CMT 27 T=
   0 E-STR 27 T= ;

: REFUSED-CASE ( -- )
   s" a cell load through a byte pointer remains a checker refusal" T-LABEL
   NELAB:REFUSED-RESET
   s" : NWM-BYTEP ( ptr u8 n -- n ) drop @ ;" EV-RC 70 T=
   NELAB:REFUSED-ROW -1 T= ;

public

: MAIN ( -- )
   T-RESET
   LOAD-CASE
   STORE-CASE
   NARROW-STORE-CASE
   PHYSICAL-CASE
   ROUNDTRIP-CASE
   COL-CASE
   DISPATCH-CASE
   WIDE-INST-CASE
   OFFSET-CASE
   REFUSED-CASE
   T-REPORT
   s" native-wide-mem: ok" type cr ;

;package

NWM:MAIN
