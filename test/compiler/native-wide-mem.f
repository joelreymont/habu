\ native-wide-mem.f - `@` and `!` through a pointer to a multi-cell family,
\ from source text to executed machine code.
\
\     bin/hb --load test/compiler/native-wide-mem.f
\
\ WHAT IS UNDER TEST. A value of a layout family is W flat cells, so reading one
\ out of memory is W loads at consecutive addresses and writing one is W stores.
\ The chain's `@` and `!` moved exactly one cell whatever the checker had
\ certified, which is why lib/report.f's three-field `col` in a TYPED-BUFFER
\ refused: `COL-AT @` is a three-cell load to the checker and was a one-cell load
\ here, and the two missing cells surfaced wherever they were first counted - at
\ the call after them (E-NELAB-CALL) or at the return (E-NELAB-ARITY).
\
\ THE WIDTH IS NOT A NEW FACT AND THAT IS THE WHOLE DESIGN. src/core/checker.f
\ files it as it certifies the access, and src/habu/habu2.f's pass-2 recompiler
\ reads it back at the token's SOURCE BYTE OFFSET to emit its own wide access. So
\ the chain asks the same table at the same key (src/compiler/native/dict.f
\ MEM-CELLS), and the four cases below whose bodies MOVE that offset - a comment
\ in front of the access, a string literal in front of it, a second access of a
\ different width, a scalar access beside a wide one - are what a reader keyed on
\ anything else answers wrongly.
\
\ NOTHING HERE IS A MODEL OF THE CHAIN. Every case states one body TWICE - once
\ for the ENGINE to compile as an ordinary definition, and once as source handed
\ to NMIGRATE:DEFINE, which compiles it through every stage and publishes the
\ chain's code under a second name - and then executes both and compares what
\ they answer. That is test/compiler/native-match.f's discipline and this file
\ keeps it.
\
\ AND THE CELL ORDER NEEDS THE TWO COMPILERS CROSSED, which is the sharpest
\ instrument in this file. A store and a load that BOTH walk the cells the wrong
\ way round answer every value correctly, so a fixture that writes and reads with
\ one compiler cannot see the order at all. The four crossed cases write with one
\ and read with the other: the engine's own wide access reads memory upwards from
\ the base and pushes as it goes (src/habu/habu2.f EMIT-P2-FETCH), so a chain
\ that walked either direction differently answers the fields exchanged. The
\ `col` pair crosses them with a POINTER in the first slot and a small integer in
\ the last, so an exchange is a wrong string rather than an arithmetic near-miss.
\
\ WEIGHTS, NOT SUMS. Every payload the cases combine is weighted with a distinct
\ odd factor, because a commutative combination answers the same number whichever
\ cell came back where and would prove only that the right NUMBER of cells moved.
\
\ WHAT A COPIED BODY MAY NOT HOLD. How many cells an access moves is a fact about
\ a TOKEN, filed under its offset into the definition being certified, and
\ src/compiler/native/inline.f records spellings and kinds rather than offsets -
\ so a caller splicing such a row would elaborate the callee's tokens against its
\ own definition's facts. The recorder refuses to keep such a body
\ (NELAB:SPLICEABLE?), and the pair at the foot of this file is what says so: two
\ accessors identical in every dimension but the width of what they read, one
\ recorded and one not.
\
\ WHAT IS NOT UNDER TEST, MEASURED AND SAID HERE SO NOBODY RE-DERIVES IT. The
\ engine emits a tag-domain validation program in front of EVERY layout fetch,
\ one cell wide or ten (src/core/layout-valid.f FETCH-BUILD), and the chain has
\ no reader for the certificate that program comes from - at any width. `@`
\ through a pointer to a ONE-CELL enum compiled here before this leaf and was
\ unvalidated before it; nothing about that changes at width two, so it is a
\ capability of its own and not this file's subject.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/adt/option.f
require src/compiler/native/migrate.f
require src/compiler/native/dict.f
require src/compiler/native/inline.f

package NWM
private

18 constant REGS

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

\ ---- the bodies the engine compiles ------------------------------------------
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

\ ---- the source the chain is given -------------------------------------------
\ Character for character the body above it, so a difference between the two
\ columns can only come from the two compilers and never from two programs.
: L1$ ( -- ptr u8 n )
   s" : C-L1 ( n -- n ) W1-AT @ NWM-W1:UNMAKE 3 * ;" ;

: L2$ ( -- ptr u8 n )
   s" : C-L2 ( n -- n ) W2-AT @ NWM-W2:UNMAKE 3 * swap 5 * + ;" ;

: L3$ ( -- ptr u8 n )
   s" : C-L3 ( n -- n ) W3-AT @ NWM-W3:UNMAKE 5 * swap 11 * + swap 17 * + ;" ;

: S1$ ( -- ptr u8 n )
   s" : C-S1 ( n n -- ) {: x:n k:n :} x NWM-W1:MAKE k W1-AT ! ;" ;

: S2$ ( -- ptr u8 n )
   s" : C-S2 ( n n n -- ) {: x:n y:n k:n :} x y NWM-W2:MAKE k W2-AT ! ;" ;

: S3$ ( -- ptr u8 n )
   s" : C-S3 ( n n n n -- ) {: x:n y:n z:n k:n :} x y z NWM-W3:MAKE k W3-AT ! ;" ;

: RT2$ ( -- ptr u8 n )
   s" : C-RT2 ( n n n -- n ) {: x:n y:n k:n :} x y NWM-W2:MAKE k W2-AT ! k E-L2 ;" ;

: LMS$ ( -- ptr u8 n )
   s" : C-LMS ( n n -- n ) {: d:n k:n :} k W3-AT @ NWM-W3:UNMAKE {: a:n b:n c:n :} a d + b c NWM-W3:MAKE k W3-AT ! k E-L3 ;" ;

: COLPUT$ ( -- ptr u8 n )
   s" : C-COL+ ( ptr u8 n n n -- ) {: h:ptr u:n al:n k:n :} h u al NWM-COL:MAKE k COL-AT ! ;" ;

: COLHDR$ ( -- ptr u8 n )
   s" : C-COL-HDR ( n -- ptr u8 n ) COL-AT @ NWM-COL:UNMAKE drop ;" ;

: COLAL$ ( -- ptr u8 n )
   s" : C-COL-AL ( n -- n ) COL-AT @ NWM-COL:UNMAKE nip nip ;" ;

: SBX$ ( -- ptr u8 n )
   s" : C-SBX ( n n -- ) {: v:n k:n :} v E-MKBX k BX-AT ! ;" ;

: LBX$ ( -- ptr u8 n )
   s" : C-LBX ( n -- n ) BX-AT @ MATCH bx b0 OF 0 ENDOF b2 OF 7 * swap 11 * + ENDOF ;MATCH ;" ;

: SOP$ ( -- ptr u8 n )
   s" : C-SOP ( n n -- ) {: v:n k:n :} v E-MKOP k OP-AT ! ;" ;

: LOP$ ( -- ptr u8 n )
   s" : C-LOP ( n -- n ) OP-AT @ MATCH opt2 n2 OF 0 ENDOF s2 OF NWM-PT:UNMAKE 7 * swap 11 * + ENDOF ;MATCH ;" ;

: MIX$ ( -- ptr u8 n )
   s" : C-MIX ( n -- n ) dup W2-AT @ NWM-W2:UNMAKE 3 * swap 5 * + swap W3-AT @ NWM-W3:UNMAKE 7 * swap 11 * + swap 13 * + + ;" ;

: SCW$ ( -- ptr u8 n )
   s" : C-SCW ( n n -- n ) {: v:n k:n :} v SC ! SC @ 3 * k W2-AT @ NWM-W2:UNMAKE 5 * swap 7 * + + ;" ;

: CMT$ ( -- ptr u8 n )
   s" : C-CMT ( n -- n ) ( this comment writes @ and ! and is not a token ) W2-AT @ NWM-W2:UNMAKE 3 * swap 5 * + ;" ;

: STR$ ( -- ptr u8 n )
   S\" : C-STR ( n -- n ) s\q @ ! @ ! @\q 2drop W2-AT @ NWM-W2:UNMAKE 3 * swap 5 * + ;" ;

\ ---- the pair the recorder has to tell apart ---------------------------------
\ Two accessors alike in every dimension but the width of what they read: one
\ token of body, one input, and a row's ceiling far above either. The address
\ arrives as an ARGUMENT rather than off a buffer word, because a buffer word is
\ a CALL and no recorded body holds one - a pair written the other way round is
\ two bodies neither of which is recorded, and the width would decide nothing.
: ACC1$ ( -- ptr u8 n )
   s" : C-ACC1 ( ptr w1 -- w1 ) @ ;" ;

: ACC2$ ( -- ptr u8 n )
   s" : C-ACC2 ( ptr w2 -- w2 ) @ ;" ;

\ ---- what the checker must go on refusing ------------------------------------
\ A byte pointer is a byte span, so a cell `@` over one is a checker error and
\ was one before this leaf. It is here because the width lookup is asked at every
\ `@`: a lookup that answered for a token the checker never certified an access
\ on would compile a body the engine rejected. The case asserts the ENGINE's
\ rejection AND that the elaborator recorded no refusal of its own.
: BYTEP$ ( -- ptr u8 n )
   s" : C-BYTEP ( ptr u8 n -- n ) drop @ ;" ;

\ ---- driving one migration where its refusal can be read ---------------------
\ A checked `catch` takes a stack-neutral quotation and a quotation cannot read
\ the enclosing word's locals, so what the migration needs is parked first.
variable M-A   variable M-U   variable M-IN   variable M-OUT

: MIGRATE-RC ( -- n )
   [: M-A @ M-U @ M-IN @ M-OUT @ REGS NMIGRATE:DEFINE ;] catch ;

: TRY ( ptr u8 n n n -- n ) {: a:ptr u:n in:n out:n :}   \ typed-local-lint: allow-bare-local
   a M-A !  u M-U !  in M-IN !  out M-OUT !
   NELAB:REFUSED-RESET
   MIGRATE-RC ;

\ ---- the migrations, and what each one answered ------------------------------
\ They run HERE, inside the package block, for the two reasons
\ test/compiler/native-match.f gives: the chain publishes by evaluating the
\ source text, so a twin migrated after `;package` would land outside the package
\ and a body naming a package family would not resolve at all; and the twins have
\ to exist before the comparisons below are compiled against their names.
variable RC-L1  variable RC-L2  variable RC-L3
variable RC-S1  variable RC-S2  variable RC-S3
variable RC-RT2 variable RC-LMS
variable RC-COLPUT variable RC-COLHDR variable RC-COLAL
variable RC-SBX variable RC-LBX
variable RC-SOP variable RC-LOP
variable RC-MIX variable RC-SCW variable RC-CMT variable RC-STR
variable RC-ACC1 variable RC-ACC2
variable RC-BYTEP variable ROW-BYTEP
variable INL-ACC1 variable INL-ACC2

\ Whether the routine this spelling names has a recorded body a caller could copy.
: RECORDED? ( ptr u8 n -- n )
   NDICT:CALL-TARGET {: entry:n :}
   entry 0= if 0 exit then
   entry NINL:KNOWN? if 1 else 0 then ;

: RUN-THE-MIGRATIONS ( -- )
   L1$ 1 1 TRY RC-L1 !
   L2$ 1 1 TRY RC-L2 !
   L3$ 1 1 TRY RC-L3 !
   S1$ 2 0 TRY RC-S1 !
   S2$ 3 0 TRY RC-S2 !
   S3$ 4 0 TRY RC-S3 !
   RT2$ 3 1 TRY RC-RT2 !
   LMS$ 2 1 TRY RC-LMS !
   COLPUT$ 4 0 TRY RC-COLPUT !
   COLHDR$ 1 2 TRY RC-COLHDR !
   COLAL$ 1 1 TRY RC-COLAL !
   SBX$ 2 0 TRY RC-SBX !
   LBX$ 1 1 TRY RC-LBX !
   SOP$ 2 0 TRY RC-SOP !
   LOP$ 1 1 TRY RC-LOP !
   MIX$ 1 1 TRY RC-MIX !
   SCW$ 2 1 TRY RC-SCW !
   CMT$ 1 1 TRY RC-CMT !
   STR$ 1 1 TRY RC-STR ! ;

: RUN-THE-PAIR ( -- )
   ACC1$ 1 1 TRY RC-ACC1 !
   s" C-ACC1" RECORDED? INL-ACC1 !
   ACC2$ 1 2 TRY RC-ACC2 !
   s" C-ACC2" RECORDED? INL-ACC2 ! ;

: RUN-THE-REFUSALS ( -- )
   BYTEP$ 3 1 TRY RC-BYTEP !  NELAB:REFUSED-ROW ROW-BYTEP ! ;

RUN-THE-MIGRATIONS
RUN-THE-PAIR
RUN-THE-REFUSALS

\ ---- executing both publications ---------------------------------------------
\ Every case below calls the engine's word and the chain's word on the same input
\ and compares the two answers. Both are ordinary checked calls: the chain
\ published its words before this file's own definitions were compiled.

: COMPILED-CASE ( -- )
   s" every wide access the chain refused before this leaf compiles" T-LABEL
   RC-L1 @ 0 T=  RC-L2 @ 0 T=  RC-L3 @ 0 T=
   RC-S1 @ 0 T=  RC-S2 @ 0 T=  RC-S3 @ 0 T=
   RC-RT2 @ 0 T=  RC-LMS @ 0 T=

   s" including lib/report.f's own three rows" T-LABEL
   RC-COLPUT @ 0 T=  RC-COLHDR @ 0 T=  RC-COLAL @ 0 T=

   s" and the four bodies whose access does not stand where a counter would put it" T-LABEL
   RC-MIX @ 0 T=  RC-SCW @ 0 T=  RC-CMT @ 0 T=  RC-STR @ 0 T=

   s" and the two that compose with a dispatch and with a wide construction" T-LABEL
   RC-SBX @ 0 T=  RC-LBX @ 0 T=  RC-SOP @ 0 T=  RC-LOP @ 0 T= ;

: AGREE-LOAD ( -- )
   s" a one-cell record's load still answers what the engine answers" T-LABEL
   7 0 E-S1  0 E-L1  0 C-L1 T=
   0 C-L1 21 T=

   s" a two-cell load answers it, weighted so an exchange would show" T-LABEL
   3 4 0 E-S2  0 E-L2  0 C-L2 T=
   0 C-L2 27 T=

   s" and a three-cell load" T-LABEL
   2 3 4 0 E-S3  0 E-L3  0 C-L3 T=
   0 C-L3 87 T= ;

: AGREE-STORE ( -- )
   s" a two-cell store puts back what the engine's store puts back" T-LABEL
   3 4 0 E-S2   0 E-L2 {: e:n :}
   9 9 0 E-S2                                    \ overwrite, so a store that did nothing is caught
   3 4 0 C-S2   0 E-L2  e T=

   s" and a three-cell store" T-LABEL
   2 3 4 0 E-S3   0 E-L3 {: e3:n :}
   9 9 9 0 E-S3
   2 3 4 0 C-S3   0 E-L3  e3 T=

   s" and a one-cell store" T-LABEL
   9 0 E-S1
   7 0 C-S1  0 E-L1 21 T= ;

: CROSSED-CASE ( -- )
   s" what the chain writes, the engine reads back in the same slots" T-LABEL
   9 9 9 0 E-S3
   2 3 4 0 C-S3
   0 E-L3  87 T=

   s" and what the engine writes, the chain reads back in the same slots" T-LABEL
   9 9 9 0 E-S3
   2 3 4 0 E-S3
   0 C-L3  87 T=

   s" the same crossed both ways over a record whose first slot is a pointer" T-LABEL
   s" hdr" 1 0 C-COL+
   0 E-COL-HDR s" hdr" T$=
   0 E-COL-AL 1 T=

   s" and the other way round" T-LABEL
   s" other" 0 0 E-COL+
   0 C-COL-HDR s" other" T$=
   0 C-COL-AL 0 T= ;

: ROUNDTRIP-CASE ( -- )
   s" a store and a load in one body answer the same either compiler" T-LABEL
   3 4 0 E-RT2  3 4 0 C-RT2 T=
   3 4 0 C-RT2 27 T=

   s" and so does read-change-write-read" T-LABEL
   2 3 4 0 E-S3   5 0 E-LMS {: e:n :}
   2 3 4 0 E-S3   5 0 C-LMS  e T=
   2 3 4 0 E-S3   5 0 C-LMS  172 T= ;

: DISPATCH-CASE ( -- )
   s" a bundle read out of memory is one value a dispatch accepts" T-LABEL
   6 0 E-SBX  0 E-LBX  0 C-LBX T=
   0 C-LBX 408 T=

   s" including the payload-free variant" T-LABEL
   0 0 E-SBX  0 E-LBX  0 C-LBX T=
   0 C-LBX 0 T=

   s" and the chain's own store of one feeds the engine's dispatch" T-LABEL
   0 0 E-SBX
   6 0 C-SBX  0 E-LBX 408 T= ;

\ THE ONE CASE WHOSE READER IS NOT THE ENGINE, AND THE MEASUREMENT THAT SAYS WHY.
\ `opt2<pt>` occupies three cells where its declaration reserves two, and the
\ ENGINE cannot read one back out of a TYPED-BUFFER at all: its own store is
\ right - the chain reads back exactly what it wrote - and its own LOAD ends the
\ process with `hb: bad layout tag` (ENGINE-ERROR:BAD-TAG, 85). The reason is the
\ validation program in front of that load rather than the load itself:
\ src/core/layout-valid.f QUEUE-SUM takes the tag's slot from the family's
\ DECLARED slot count (`fam TFAM-SLOTS@`), which at a wider instantiation is a
\ PAYLOAD cell, so the guard tests the wrong cell against the tag domain. It is
\ pre-existing and has nothing to do with this leaf: the twenty-line reproducer
\ below holds on master with no migration in it at all, and both wide accesses in
\ it are the engine's own.
\
\     PRODUCT pt 0 FIELD x n FIELD y n ;PRODUCT
\     ENUM opt2 1 VARIANT n2 ;VARIANT VARIANT s2 FIELD value a ;VARIANT ;ENUM
\     4 TYPED-BUFFER OP-AT opt2<pt>
\     : MK ( n -- opt2<pt> ) dup 3 * swap 5 * PT:MAKE OPT2:S2 ;
\     : PUT ( n n -- ) {: v:n k:n :} v MK k OP-AT ! ;
\     : GET ( n -- n ) OP-AT @ MATCH opt2 n2 OF 0 ENDOF s2 OF PT:UNMAKE 7 * swap 11 * + ENDOF ;MATCH ;
\     6 0 PUT  0 GET       \ hb: bad layout tag, exit 85
\
\ SO THE STORE IS STILL A DIFFERENTIAL AND THE LOAD IS STILL EXECUTED, with the
\ chain's own load as the reader for both columns: what the ENGINE's store leaves
\ and what the CHAIN's store leaves are read back by one word and compared, which
\ is the whole of what a store fixture can say. The load's own second column is
\ what the defect above takes away, and this case will grow one the day it is
\ fixed.
: WIDE-INST-CASE ( -- )
   s" the chain compiles both halves of a wide instantiation through memory" T-LABEL
   RC-SOP @ 0 T=  RC-LOP @ 0 T=

   s" and reads back a value the engine's store left, payload and tag" T-LABEL
   6 0 E-SOP  0 C-LOP 408 T=

   s" its empty variant too, whose pads the construction site added" T-LABEL
   0 0 E-SOP  0 C-LOP 0 T=

   s" and the chain's own store leaves the same three cells the engine's does" T-LABEL
   0 0 E-SOP
   6 0 C-SOP  0 C-LOP 408 T=
   6 0 E-SOP
   0 0 C-SOP  0 C-LOP 0 T= ;

: OFFSET-CASE ( -- )
   s" two accesses of different widths in one body each get their own" T-LABEL
   3 4 0 E-S2  2 3 4 0 E-S3
   0 E-MIX  0 C-MIX T=
   0 C-MIX 114 T=

   s" a scalar access beside a wide one gets one cell" T-LABEL
   3 4 0 E-S2
   5 0 E-SCW  5 0 C-SCW T=
   5 0 C-SCW 56 T=

   s" a comment in front of the access moves its offset for both readers" T-LABEL
   3 4 0 E-S2
   0 E-CMT  0 C-CMT T=
   0 C-CMT 27 T=

   s" and so does a string literal's payload" T-LABEL
   0 E-STR  0 C-STR T=
   0 C-STR 27 T= ;

: REFUSED-CASE ( -- )
   s" a cell load through a byte pointer is still the checker's refusal" T-LABEL
   RC-BYTEP @ 0 T<>

   s" and the elaborator recorded no refusal of its own" T-LABEL
   ROW-BYTEP @ -1 T= ;

: RECORD-CASE ( -- )
   s" a one-cell accessor is recorded, so its callers copy it" T-LABEL
   RC-ACC1 @ 0 T=
   INL-ACC1 @ 1 T=

   s" and the wide one beside it is not, so its callers call it" T-LABEL
   RC-ACC2 @ 0 T=
   INL-ACC2 @ 0 T= ;

public

: MAIN ( -- )
   T-RESET
   COMPILED-CASE
   AGREE-LOAD
   AGREE-STORE
   CROSSED-CASE
   ROUNDTRIP-CASE
   DISPATCH-CASE
   WIDE-INST-CASE
   OFFSET-CASE
   REFUSED-CASE
   RECORD-CASE
   T-REPORT
   s" native-wide-mem: ok" type cr ;

;package

NWM:MAIN
