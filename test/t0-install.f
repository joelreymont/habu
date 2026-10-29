\ t0-install.f - T0 as the runtime compiler on arm64: T0:INSTALL stores
\ T0:COMPILE in the cell the tier 1 `:` dispatches through, so every definition
\ this file loads after `1 set-tier`, the test words included, is compiled by
\ T0 through the normal Habu loop. The fixtures cover `:` and TRUSTED: bodies,
\ a signature whose `)` touches its last type, quotations, strings, `char` and
\ `[char]` of an ASCII and a UTF-8 character and `char` past a word of that
\ name, locals, loops, case and recursion, and each is run for its result. The
\ does> definers cover a clause, an empty clause whose child keeps its address
\ kind, quotations nested in the parent and one in the clause, clause locals
\ and a definer chained on another, each through a child it made. A body
\ names a created word, which T0 folds into the body as the address the
\ interpreter pushes. `is` binds a defer from a body, which a caller then
\ reaches, re-points it with the keyword in capitals, binds it from inside a
\ quotation, and binds it before a MATCH, whose facts sit past its operand; a
\ nested quotation that binds a defer spelled as a parsing word loads, its skip
\ stepping over the operand. The MATCH rows dispatch over an enum and a sum
\ type whose `construct`ed values carry pads, with a cell under each value
\ proving its width; a MATCH nested in an arm, one in a case arm, one in a
\ does> clause, and one over option<pt>, whose `none` comes from a call that
\ pushes the pad the instantiation adds; a `construct` at wrap<pt> pushes that
\ pad itself; a MATCH over a 65-cell payload; one in a quotation after
\ character literals and a string, whose facts sit at the ordinals those tokens
\ advance; and a hostile tag, in a child, reaches T0's trap and exits
\ ENGINE-ERROR:BAD-TAG. The wide rows run each stack word T0 compiles over a
\ two-cell product with a cell under the result, a swap in a does> parent and
\ in its clause, a 2swap and a 2over in a does> parent whose deepest operand is
\ two cells, a three-cell product among one-cell values, a swap of two
\ eight-cell values, whose copies fill the virtual stack, and a local bound
\ from a forty-cell value, whose deepest cell lies past a pre-index load's
\ reach; `@` and `!` of a two-, a three- and a sixteen-cell value through typed
\ slots, with the cells beside each store unchanged and the fetch reading cells
\ written through a raw view, the sixteen cells spilling the virtual stack, and
\ a store and fetch in one body. The checker refuses every other operation over
\ a wide value, so no row reaches E-T0-BUNDLE. A fetch from a sum-type and an
\ enum slot checks the stored tag and loads the value with the cell under it
\ kept; a forged tag fetched in a child, from an enum slot and from a product's
\ enum field, ends the process `hb: bad layout tag` with ENGINE-ERROR:BAD-TAG.
\ The share rows run bodies whose copies share a vreg with their source, each
\ with a cell under its result: in-place arithmetic, a negate and loads on a
\ copy, a copy parked on the return stack, comparisons of a copy, and fifteen
\ copies that fill the virtual stack and evict a shared vreg. The fuse rows run
\ each compare straight into `if`, `while` and `until`, which branch on its
\ condition codes, both ways and over counted loops, with a cell under each
\ result.
\ A CALLBACK: declaration compiled by T0 is handed to libc's qsort, which sorts
\ a row through it, and its C entry lies between its accessor's code and the
\ next definition's: the stub T0 appended to the accessor's emission. The
\ refusal rows evaluate definitions T0 refuses with its own codes, which tier 1
\ accepts or refuses otherwise: a non-neutral immediate in a TRUSTED: body
\ (E-T0-IMMEDIATE), `is` naming a primitive in a TRUSTED: body (E-T0-DEFER; the
\ checker refuses it in a checked body), a counted string past 255 bytes in a
\ does> clause (E-T0-LITERAL; the checker refuses a non-neutral immediate in a
\ clause even under TRUSTED:), after which the checker's rollback depth is back
\ where it was and the definer's name defined cleanly compiles and its child
\ runs, so the refusal rolled back the checker frame, and a definition while a
\ target shadow is open (E-T0-SHADOW); a tick of an undefined word is the
\ engine's undefined refusal, as under tier 1; a definition after them proves a
\ refusal leaves the compiler idle.
\
\ PROCESS BOUNDARY. T0:INSTALL replaces the dispatch for the rest of the
\ process and nothing restores it, so this file is its suite row's only file:
\ a file loaded after it would be compiled by T0.

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require src/lib/ffi-abi.f
require src/lib/ffi-callback.f
require src/lib/task.f
require src/lib/adt/option.f
require src/compiler/target/model.f
require src/compiler/target/native-binding.f
require src/compiler/native/shadow.f
require src/habu/t0-arm64.f
require src/habu/t0.f

CTARGET-ARCH:AARCH64 T0-EMIT:SELECT
T0:INSTALL
1 set-tier

package T0-INSTALL-TEST
private

: SQUARE ( n -- n ) dup * ;
TRUSTED: TWICE ( n -- n ) 2 * ;
: APPLY ( n [ n -- n ] -- n ) execute ;
: ADD3 ( n -- n ) [: 3 + ;] APPLY ;
: GREETING ( -- ptr u8 n ) s" hello" ;
: COUNTED-LEN ( -- n ) c" abcd" c@ ;
: CH-ONE ( -- n ) char 1 ;
: CH-UP ( -- n ) CHAR A ;
: CH-HI ( -- n ) [char] é ;
: SIG-TIGHT ( n -- n) 1+ ;

: SPREAD ( n n -- n )
   {: a:n b:n :}
   a 10 * b - ;

: SUM-BELOW ( n -- n ) 0 swap 0 ?do i + loop ;
: EVENS ( n -- n ) 0 swap 0 do 1+ 2 +loop ;

: HALVINGS ( n -- n )
   0 swap
   begin dup 1 > while 2 / swap 1+ swap repeat
   drop ;

: PICK-CODE ( n -- n )
   case
      1 of 10 endof
      2 of 20 endof
      30 swap
   endcase ;

: FACT ( n -- n ) dup 1 > if dup 1- recurse * then ;

\ ---- does> definers -----------------------------------------------------------
: KEEPER ( n -- ) create , does> ( -- n ) @ ;
42 KEEPER K42

: CELLER ( n -- ) create , does> ( -- ptr n ) ;
7 CELLER C7
s" C7" XREF-FIND-CURRENT-INDEX XREF-REQUIRE-INDEX constant C7-IX

: TWICER ( n -- ) [: [: 2 * ;] execute ;] execute create , does> ( -- n ) @ [: 1+ ;] execute ;
5 TWICER T10

: SCALER ( n -- ) create , does> ( n -- n ) @ {: x:n k:n :} x k * ;
6 SCALER S6

: BASE-D ( n -- ) create , does> ( -- n ) @ ;
: CHAINED ( n -- ) BASE-D does> ( -- n ) @ 1+ ;
9 CHAINED N9

\ ---- fixed words ----------------------------------------------------------------
create SLAB 4 cells allot

: SLAB-AT ( -- n ) SLAB NULL-PTR - ;

\ ---- MATCH and construct --------------------------------------------------------
public

ENUM hue
   rojo
   verde
   azul
;ENUM

\ The widest variant fixes the value's cells, so `uno` carries one pad and
\ `nada` two.
SUMTYPE box
   VARIANT nada ;VARIANT
   VARIANT uno n ;VARIANT
   VARIANT duo n n ;VARIANT
;SUMTYPE

PRODUCT pt
   FIELD x n
   FIELD y n
;PRODUCT

\ A payload of 65 cells, one past the 64 a glue mask describes.
SUMTYPE big
   VARIANT small ;VARIANT
   VARIANT huge
      n n n n n n n n n n n n n
      n n n n n n n n n n n n n
      n n n n n n n n n n n n n
      n n n n n n n n n n n n n
      n n n n n n n n n n n n n
   ;VARIANT
;SUMTYPE

\ At wrap<pt> `empty` carries two pads where wrap<a> declares one: construct
\ pushes the pad the instantiation adds.
ENUM wrap<a>
   VARIANT empty ;VARIANT
   VARIANT full FIELD v a ;VARIANT
;ENUM

private

: ROJO ( -- hue ) construct hue rojo ;
: VERDE ( -- hue ) construct hue verde ;
: AZUL ( -- hue ) construct hue azul ;
: NADA ( -- box ) construct box nada ;
: UNO ( n -- box ) construct box uno ;
: DUO ( n n -- box ) construct box duo ;

: HUE-CODE ( hue -- n )
   MATCH hue
      rojo OF 10 ENDOF
      verde OF 20 ENDOF
      azul OF 30 ENDOF
   ;MATCH ;

: BOX-SUM ( box -- n )
   MATCH box
      nada OF 0 ENDOF
      uno OF ENDOF
      duo OF - ENDOF
   ;MATCH ;

\ A MATCH in an arm of another, over the value under the first.
: PAIR-CODE ( box hue -- n )
   MATCH hue
      rojo OF BOX-SUM ENDOF
      verde OF
         MATCH box
            nada OF 1 ENDOF
            uno OF 2 * ENDOF
            duo OF * ENDOF
         ;MATCH
      ENDOF
      azul OF BOX-SUM negate ENDOF
   ;MATCH ;

\ A MATCH in a case arm, and a case default that takes the value apart.
: CASE-BOX ( box n -- n )
   case
      1 of BOX-SUM endof
      2 of
         MATCH box
            nada OF -1 ENDOF
            uno OF 10 * ENDOF
            duo OF + ENDOF
         ;MATCH
      endof
      >r BOX-SUM 1000 + r>
   endcase ;

\ A MATCH in a does> clause, whose facts sit at the clause's ordinals.
: HUE-WEIGHT ( n -- ) create , does> ( hue -- n )
   @ swap MATCH hue
      rojo OF ENDOF
      verde OF 2 * ENDOF
      azul OF negate ENDOF
   ;MATCH ;
5 HUE-WEIGHT W5

\ At option<pt> `none` is three cells where option<n> is two: the call to
\ OPTION:NONE carries the pad the instantiation adds.
: SOME-PT ( n -- option<pt> )
   dup 0 > if dup 3 * swap 5 * T0--INSTALL--TEST-PT:MAKE OPTION:SOME else drop OPTION:NONE then ;

: PT-SUM ( option<pt> -- n )
   MATCH option
      none OF 0 ENDOF
      some OF T0--INSTALL--TEST-PT:UNMAKE 7 * swap 11 * + ENDOF
   ;MATCH ;

: BIG-SMALL ( -- big ) construct big small ;
: BIG-HUGE ( -- big )
   1 2 3 4 5 6 7 8 9 10 11 12 13
   14 15 16 17 18 19 20 21 22 23 24 25 26
   27 28 29 30 31 32 33 34 35 36 37 38 39
   40 41 42 43 44 45 46 47 48 49 50 51 52
   53 54 55 56 57 58 59 60 61 62 63 64 65
   construct big huge ;

: BIG-SUM ( big -- n )
   MATCH big
      small OF -1 ENDOF
      huge OF
         + + + + + + + + + + + + + + + +
         + + + + + + + + + + + + + + + +
         + + + + + + + + + + + + + + + +
         + + + + + + + + + + + + + + + +
      ENDOF
   ;MATCH ;

: WRAP-EMPTY ( -- wrap<pt> ) construct wrap empty ;
: WRAP-FULL ( n n -- wrap<pt> ) T0--INSTALL--TEST-PT:MAKE construct wrap full ;

: WRAP-SUM ( wrap<pt> -- n )
   MATCH wrap
      empty OF 0 ENDOF
      full OF T0--INSTALL--TEST-PT:UNMAKE 7 * swap 11 * + ENDOF
   ;MATCH ;

\ A character literal, `[char]` or `char`, files its operand under its own
\ ordinal and a quotation resumes at its first token's: a MATCH after both, in
\ a quotation after a string, two more literals and a quotation of them,
\ reads the facts at its own tokens; the skip over the first quotation steps
\ over each literal's operand.
: ORD-MIX ( n -- n )
   [char] x drop
   [: [char] w char v 2drop ;] drop
   [: s" ab" 2drop [char] y drop char z drop
      SOME-PT MATCH option
         none OF 0 ENDOF
         some OF T0--INSTALL--TEST-PT:UNMAKE 7 * swap 11 * + ENDOF
      ;MATCH ;] execute ;

\ ---- whole values through the stack words -----------------------------------
\ Each stack word over a value wider than a cell moves the value's every cell,
\ as the checker measured it at the token. The readers weigh each field by its
\ place, so a cell moved out of order or left behind changes the result.
public

PRODUCT trio
   FIELD p n
   FIELD q n
   FIELD r n
;PRODUCT

PRODUCT oct
   FIELD o1 n
   FIELD o2 n
   FIELD o3 n
   FIELD o4 n
   FIELD o5 n
   FIELD o6 n
   FIELD o7 n
   FIELD o8 n
;PRODUCT

PRODUCT twin
   FIELD lo oct
   FIELD hi oct
;PRODUCT

PRODUCT pent
   FIELD e1 oct
   FIELD e2 oct
   FIELD e3 oct
   FIELD e4 oct
   FIELD e5 oct
;PRODUCT

private

: PT ( n n -- pt ) T0--INSTALL--TEST-PT:MAKE ;
: PT-V ( pt -- n ) T0--INSTALL--TEST-PT:UNMAKE swap 10 * + ;
: TRIO ( n n n -- trio ) T0--INSTALL--TEST-TRIO:MAKE ;
: TRIO-V ( trio -- n ) T0--INSTALL--TEST-TRIO:UNMAKE swap 10 * + swap 100 * + ;
\ An eight-cell value 1..8 from a base: its value is the base, unless a cell moved.
: OCT ( n -- oct )
   {: b:n :} b b 1+ b 2 + b 3 + b 4 + b 5 + b 6 + b 7 + T0--INSTALL--TEST-OCT:MAKE ;
: OCT-V ( oct -- n )
   T0--INSTALL--TEST-OCT:UNMAKE
   {: a:n b:n c:n d:n e:n f:n g:n h:n :}
   b a 1+ = c a 2 + = and d a 3 + = and e a 4 + = and
   f a 5 + = and g a 6 + = and h a 7 + = and if a else -1 then ;
\ A forty-cell value 1..40 from a base, made and checked as five oct values.
: PENT ( n -- pent )
   {: b:n :} b OCT b 8 + OCT b 16 + OCT b 24 + OCT b 32 + OCT
   T0--INSTALL--TEST-PENT:MAKE ;
: PENT-V ( pent -- n )
   T0--INSTALL--TEST-PENT:UNMAKE
   {: a:oct b:oct c:oct d:oct e:oct :}
   a OCT-V {: x:n :}
   b OCT-V x 8 + = c OCT-V x 16 + = and d OCT-V x 24 + = and
   e OCT-V x 32 + = and if x else -1 then ;

\ Typed slots whose `@` and `!` move two, three and sixteen cells, and a raw
\ view of their cells: a store's neighbours prove it wrote only its own, and a
\ fetch reads cells written through the view.
4 TYPED-BUFFER PT-AT pt
4 TYPED-BUFFER TRIO-AT trio
CAST: PT>CELLS ( ptr pt -- ptr n )
CAST: TRIO>CELLS ( ptr trio -- ptr n )
: PT-CELL ( n -- ptr n ) 0 PT-AT PT>CELLS swap cells + ;
: TRIO-CELL ( n -- ptr n ) 0 TRIO-AT TRIO>CELLS swap cells + ;
: PT@ ( n -- pt ) PT-AT @ ;
: PT! ( pt n -- ) PT-AT ! ;
: TRIO@ ( n -- trio ) TRIO-AT @ ;
: TRIO! ( trio n -- ) TRIO-AT ! ;
: PT-RT ( pt n -- pt ) dup >r PT-AT ! r> PT-AT @ ;

\ Two eight-cell values side by side, from bases a and b: their values are the
\ bases, unless a cell moved.
: TWIN ( n n -- twin ) {: a:n b:n :} a OCT b OCT T0--INSTALL--TEST-TWIN:MAKE ;
: TWIN-V ( twin -- n n ) T0--INSTALL--TEST-TWIN:UNMAKE OCT-V >r OCT-V r> ;
3 TYPED-BUFFER TWIN-AT twin
CAST: TWIN>CELLS ( ptr twin -- ptr n )
: TWIN-CELL ( n -- ptr n ) 0 TWIN-AT TWIN>CELLS swap cells + ;
: TWIN@ ( n -- twin ) TWIN-AT @ ;
: TWIN! ( twin n -- ) TWIN-AT ! ;

\ Slot k's cells that hold the twin of bases a and b, counted.
: TWIN-HITS ( n n n -- n )
   {: k:n a:n b:n :}
   0 8 0 do
      k 16 * i + TWIN-CELL @ a i + = if 1+ then
      k 16 * i + 8 + TWIN-CELL @ b i + = if 1+ then
   loop ;

\ Slot k's cells written through the view as the twin of bases a and b.
: TWIN-SEED ( n n n -- )
   {: k:n a:n b:n :}
   8 0 do
      a i + k 16 * i + TWIN-CELL !
      b i + k 16 * i + 8 + TWIN-CELL !
   loop ;

\ Typed slots of a sum type and an enum: every `@` of one checks the stored
\ tag (NFETCH-CHECK:TAGS) before it loads, on a copy of the address.
4 TYPED-BUFFER BOX-AT box
4 TYPED-BUFFER HUE-AT hue
: BOX@ ( n -- box ) BOX-AT @ ;
: BOX! ( box n -- ) BOX-AT ! ;
: HUE@ ( n -- hue ) HUE-AT @ ;
: HUE! ( hue n -- ) HUE-AT ! ;

\ The probe's three: a one-cell value over and under a two-cell one.
: W-DROP ( n pt -- n ) drop ;
: W-NIP ( pt n -- n ) nip ;
: W-DUP ( pt -- pt pt ) dup ;
: W-SWAP ( pt n -- n pt ) swap ;
: W-OVER ( pt n -- pt n pt ) over ;
: W-TUCK ( n pt -- pt n pt ) tuck ;
: W-ROT ( pt n n -- n n pt ) rot ;
: W--ROT ( n n pt -- pt n n ) -rot ;
: W-2DUP ( pt n -- pt n pt n ) 2dup ;
: W-2DROP ( n pt n -- n ) 2drop ;
: W-2SWAP ( pt n n pt -- n pt pt n ) 2swap ;
: W-2OVER ( pt n n n -- pt n n n pt n ) 2over ;
: W-R ( n pt -- n pt pt ) >r r@ r> ;
: W-2R ( pt n -- pt n pt n ) 2>r 2r@ 2r> ;
: W-LOCAL ( pt n -- n pt ) {: v:pt k:n :} k v ;
: W-DEF ( n -- ) create 3 4 PT swap >r PT-V r> + , does> ( pt -- n pt ) @ swap ;
5 W-DEF W39
\ A does> parent's 2swap and 2over whose deepest operand is two cells wide,
\ each weighed by place into its child's cell.
: PS-V ( n n pt n -- n )
   {: a:n b:n v:pt c:n :}
   a 100000 * b 10000 * + v PT-V 100 * + c + ;
: PO-V ( pt n n n pt n -- n )
   {: a:pt b:n c:n d:n e:pt f:n :}
   a PT-V 100000000 * b 1000000 * + c 100000 * + d 10000 * +
   e PT-V 100 * + f + ;
: W-DEF-2SWAP ( -- ) create 3 4 PT 5 6 7 2swap PS-V , does> ( -- n ) @ ;
: W-DEF-2OVER ( -- ) create 3 4 PT 5 6 7 2over PO-V , does> ( -- n ) @ ;
W-DEF-2SWAP W-PS
W-DEF-2OVER W-PO
\ Three cells mixed with one on both sides.
: W3-ROT ( n trio n -- trio n n ) rot ;
: W3-2SWAP ( trio n pt n -- pt n trio n ) 2swap ;
: W3-R ( n trio n -- n trio n trio ) swap >r r@ swap r> ;
\ Sixteen cells: the copies fill the virtual stack and spill.
: W8-SWAP ( oct oct -- oct oct ) swap ;
\ Forty cells bound from memory, the deepest past a pre-index load's reach.
: W40-LOCAL ( pent n -- n pent ) {: v:pent k:n :} k v ;

\ ---- copies that share a vreg ---------------------------------------------------
: S-INC ( n -- n ) dup 1+ + ;
: S-TRIPLE ( n -- n ) dup dup + + ;
: S-OVER ( n n -- n n ) over + swap ;
: S-TUCK ( n n -- n n ) tuck - ;
: S-PARK ( n -- n ) dup >r 1+ r> + ;
: S-BYTES ( ptr u8 -- n ) dup c@ swap 1+ c@ + ;
: S-ABS ( n -- n ) dup 0< if negate then ;
: S-RANGE ( n -- bool ) dup 5 < swap 3 > and ;
: S-FILL ( n -- n )
   dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup
   + + + + + + + + + + + + + + + ;

\ ---- compares fused into their branches -------------------------------------
\ A compare right before `if`, `while` or `until` branches on its condition.
: FU-EQ ( n n -- n ) = if 1 else 2 then ;
: FU-NE ( n n -- n ) <> if 1 else 2 then ;
: FU-LT ( n n -- n ) < if 1 else 2 then ;
: FU-GT ( n n -- n ) > if 1 else 2 then ;
: FU-LE ( n n -- n ) <= if 1 else 2 then ;
: FU-GE ( n n -- n ) >= if 1 else 2 then ;
: FU-ZEQ ( n -- n ) 0= if 1 else 2 then ;
: FU-ZLT ( n -- n ) 0< if 1 else 2 then ;
: FU-IMM ( n -- n ) 5 < if 1 else 2 then ;
: FU-NEG ( n -- n ) -5 > if 1 else 2 then ;
: FU-LEFT ( n -- n ) 5 swap < if 1 else 2 then ;
: FU-UNDER ( n -- n n ) 7 swap 0< if 1 else 2 then ;
: FU-TWICE ( n n -- n ) < dup if drop 1 else drop 2 then ;

\ A local named `if` shadows the keyword: the compare before it stays a flag.
: FU-SHADOW ( n n -- bool n )
   {: x:n if:n :}
   x 3 < if ;

: WH-EQ ( n -- n )
   {: n:n :}
   n begin dup n = while 1+ repeat ;

: WH-NE ( n -- n )
   {: n:n :}
   0 begin dup n <> while 1+ repeat ;

: WH-LT ( n -- n )
   {: n:n :}
   0 begin dup n < while 1+ repeat ;

: WH-GT ( n -- n )
   {: n:n :}
   0 begin n over > while 1+ repeat ;

: WH-LE ( n -- n )
   {: n:n :}
   0 begin dup n <= while 1+ repeat ;

: WH-GE ( n -- n )
   {: n:n :}
   0 begin n over >= while 1+ repeat ;

: UN-EQ ( n -- n )
   {: n:n :}
   0 begin 1+ dup n = until ;

: UN-NE ( n -- n )
   {: n:n :}
   n begin 1+ dup n <> until ;

: UN-LT ( n -- n )
   {: n:n :}
   0 begin 1+ n over < until ;

: UN-GT ( n -- n )
   {: n:n :}
   0 begin 1+ dup n > until ;

: UN-LE ( n -- n )
   {: n:n :}
   0 begin 1+ n over <= until ;

: UN-GE ( n -- n )
   {: n:n :}
   0 begin 1+ dup n >= until ;

\ ---- is ------------------------------------------------------------------------
defer HOOK ( n -- n )
: INC ( n -- n ) 1+ ;
: CALL-HOOK ( n -- n ) HOOK ;
: AIM-SQUARE ( -- ) [: SQUARE ;] is HOOK ;
: AIM-INC ( -- ) [: INC ;] IS HOOK ;
: AIM-IN-QUOT ( -- ) [: [: TWICE ;] is HOOK ;] execute ;
\ The checker files the MATCH facts at the ordinal past `is`'s operand.
: AIM-MATCH ( hue -- n )
   [: SQUARE ;] is HOOK
   MATCH hue rojo OF 1 ENDOF verde OF 2 ENDOF azul OF 3 ENDOF ;MATCH ;

\ ---- a clause T0 refuses -----------------------------------------------------
\ A definer whose clause holds a counted string of 256 bytes, one past what a
\ counted length's one byte holds.
512 constant LONG-CAP
LONG-CAP BUFFER: LONG
TYPED-VARIABLE LONG-U len

: LONG+ ( ptr u8 n -- ) LONG LONG-CAP LONG-U BUF-APPEND ;

: LONG-DEFINER ( -- ptr u8 n )
   LONG-U BUF-RESET
   S\" : REDO-D ( n -- ) create , does> ( -- n ) drop c\" " LONG+
   256 0 do [char] x LONG LONG-CAP LONG-U BUF-APPEND-C loop
   S\" \" c@ ;" LONG+
   LONG LONG-U BUF-LEN@ ;

\ ---- a callback C calls ------------------------------------------------------
3 constant ROW-N
create ROW ROW-N cells allot

PROCESS-SYMBOLS
FUNCTION: QSORT qsort ( ptr u8 n n n -- )
   0 24 WRITES-BYTES                 \ ROW-N cells
;FUNCTION

\ The two cells C's comparator arguments point at, small enough to subtract.
: ROW-ORDER ( ptr u8 ptr u8 -- n )
   {: a b :}
   a CELL-VIEW @ b CELL-VIEW @ - ;

CALLBACK: T0-CMP ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
\ A second declaration in a row: it arms only if T0's compile of the first
\ ended the pending stub and cleared NCBSTUB.
CALLBACK: T0-CMP2 ( ptr u8 ptr u8 -- i32 ) 0 FALLBACK ;CALLBACK
' ROW-ORDER T0-CMP-BODY !

\ The accessor, which owns the stub, and the descriptor defined after it.
CAST: XT>N ( [ -- n ] -- n )
CAST: CB-XT>N ( [ -- FFI-CB:callback ] -- n )
TYPED-VARIABLE ACCESSOR [ -- n ]
TYPED-VARIABLE DESCRIPTOR [ -- FFI-CB:callback ]
' T0-CMP-ENTRY ACCESSOR !
' T0-CMP DESCRIPTOR !

\ ---- a hostile tag in a child ------------------------------------------------
\ PROCESS BOUNDARY. A bad tag exits the process, so these rows run their
\ fixture in a child under test/t0-child.f: T0 installed there compiles the
\ MATCH or the fetch the bad tag reaches.
$4000 constant CAP-CAP
30000 constant CHILD-MS

create ERR-BUF CAP-CAP allot
create OUT-BUF CAP-CAP allot
create PRODUCT-PATH FS-PATH-CAP allot

variable CHILD-OUT-N
variable CHILD-ERR-N
variable CHILD-RC

: PRODUCT$ ( -- ptr u8 n )
   SOURCE-ROOT:CWD$ s" bin/hb" PRODUCT-PATH JOIN-PATH {: u:n :}
   PRODUCT-PATH u ;

: PRODUCT-ENV! ( -- )
   PROC-ENV-RESET
   s" HABU_UNDER_TEST" >LEN PRODUCT$ >LEN PROC-ENV+
   s" HABU_FIXPOINT_ENGINE" >LEN PRODUCT$ >LEN PROC-ENV+
   PROC-ENV-INHERIT-MISSING ;

\ Run bin/hb on the file named by t, compiled by T0 in the child.
: CHILD-RUN ( ptr u8 n -- ) {: t:ptr u:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   s" test/t0-child.f" >LEN PROC-ARGV+
   t u >LEN PROC-ARGV+
   PRODUCT-ENV!
   s" bin/hb" >LEN
   OUT-BUF CAP-CAP >LEN
   ERR-BUF CAP-CAP >LEN
   CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME
   PROC-OUTCOME>RC RC>N CHILD-RC !
   LEN>N CHILD-ERR-N !
   LEN>N CHILD-OUT-N ! ;

public

\ An immediate that is no parse-neutral one. The checker refuses it in a
\ checked body, so the refusal row uses it in a TRUSTED: body. Public: the row
\ evaluates its text at top level.
: NOTHING ( -- ) ; immediate

private

: CALL-CASE ( -- )
   s" a : body runs" T-LABEL  7 SQUARE 49 T=
   s" a TRUSTED: body runs" T-LABEL  21 TWICE 42 T=
   s" a quotation runs through a call" T-LABEL  4 ADD3 7 T=
   s" a string literal" T-LABEL  GREETING s" hello" T$=
   s" a counted string literal" T-LABEL  COUNTED-LEN 4 T=
   s" char and [char] push their operand's first byte" T-LABEL
   CH-ONE 49 T=  CH-UP 65 T=  CH-HI 195 T=
   s" a signature closed against its last type" T-LABEL  4 SIG-TIGHT 5 T=
   s" locals bind deepest first" T-LABEL  5 8 SPREAD 42 T=
   s" ?do loop" T-LABEL  5 SUM-BELOW 10 T=
   s" do +loop" T-LABEL  7 EVENS 4 T=
   s" begin while repeat" T-LABEL  40 HALVINGS 5 T=
   s" case: an of arm" T-LABEL  2 PICK-CODE 20 T=
   s" case: the default arm" T-LABEL  9 PICK-CODE 30 T=
   s" recurse" T-LABEL  5 FACT 120 T= ;

: DOES-CASE ( -- )
   s" a does> child runs its clause" T-LABEL  K42 42 T=
   s" an empty clause leaves the body address" T-LABEL  C7 @ 7 T=
   s" an empty clause's child keeps its address kind" T-LABEL
   C7-IX XREF-REC XREF-FLAGS DKIND:ADDR and 0<> TTRUE
   s" parent quotations, nested, and a clause quotation" T-LABEL  T10 11 T=
   s" clause locals" T-LABEL  7 S6 42 T=
   s" a chained definer's clause replaces the first" T-LABEL  N9 10 T= ;

: FIXED-CASE ( -- )
   s" a folded created word is the address it pushes" T-LABEL
   SLAB-AT ['] SLAB execute NULL-PTR - T= ;

: IS-CASE ( -- )
   s" is binds a defer a caller reaches" T-LABEL  AIM-SQUARE 6 CALL-HOOK 36 T=
   s" IS re-points it" T-LABEL  AIM-INC 6 CALL-HOOK 7 T=
   s" is inside a quotation" T-LABEL  AIM-IN-QUOT 6 CALL-HOOK 12 T=
   s" a MATCH after is reads the facts past its operand" T-LABEL
   VERDE AIM-MATCH 2 T=  3 CALL-HOOK 9 T=
   s" a quotation skips is with the parsing-word spelling it names" T-LABEL
   s" package T0-IS-CHAR defer CHAR ( -- n ) : AIM-CHAR ( -- ) [: [: 5 ;] IS CHAR ;] execute ; ;package"
   TEST-EVAL:RC 0 T=
   s" a body char compiles its literal past a word named char" T-LABEL
   s" package T0-CHAR-SHADOW : char ( -- n n ) 1 2 ; : CS ( -- n ) char x ; : CS-OK ( -- ) CS 120 <> if 1 throw then ; CS-OK ;package"
   TEST-EVAL:RC 0 T= ;

: CALLBACK-CASE ( -- )
   T0-CMP TASK:SELF-CONTEXT FFI-CB:ENTRY {: fn:n :}
   s" the C entry is the stub T0 appended to the accessor" T-LABEL
   ACCESSOR @ XT>N fn <  fn DESCRIPTOR @ CB-XT>N <  and TTRUE
   s" qsort sorts through the callback" T-LABEL
   3 ROW !  1 ROW cell+ !  2 ROW 2 cells + !
   ROW BYTE-VIEW ROW-N CELL fn QSORT
   ROW @ 1 T=  ROW cell+ @ 2 T=  ROW 2 cells + @ 3 T=
   T0-CMP FFI-CB:UNBIND ;

: REFUSAL-CASE ( -- )
   s" a non-neutral immediate is refused by name" T-LABEL
   s" TRUSTED: USES-NOTHING ( -- ) T0-INSTALL-TEST:NOTHING ;" TEST-EVAL:RC
   T0:E-T0-IMMEDIATE T=
   s" the refusal names the immediate" T-LABEL
   T0:UNSUPPORTED$ s" T0-INSTALL-TEST:NOTHING" T$=
   s" is naming no deferred word is refused" T-LABEL
   s" TRUSTED: IS-PRIM ( -- ) [: 2 ;] is dup ;" TEST-EVAL:RC T0:E-T0-DEFER T=
   s" a counted string past 255 bytes in a does> clause is refused" T-LABEL
   RBF-DEPTH @ {: depth:n :}
   LONG-DEFINER TEST-EVAL:RC T0:E-T0-LITERAL T=
   s" the refusal rolled back the checker frame" T-LABEL
   RBF-DEPTH @ depth T=
   s" the refused definer's name defined cleanly compiles and its child runs" T-LABEL
   s" : REDO-D ( n -- ) create , does> ( -- n ) @ ; 3 REDO-D REDO-3 REDO-3" TEST-EVAL:N 3 T=
   s" a definition while a target shadow is open is refused" T-LABEL
   RTARGET:HOST-TARGET RTARGET:NATIVE-BINDING NSHADOW:OPEN
   s" : IN-SHADOW ( -- n ) 1 ;" TEST-EVAL:RC
   NSHADOW:CLOSE
   T0:E-T0-SHADOW T=
   s" a tick of an undefined word is the undefined-word refusal" T-LABEL
   s" TRUSTED: TICKS-NOTHING ( -- n ) ['] T0-INSTALL-NO-SUCH-WORD ;" TEST-EVAL:RC
   70 T=
   s" a definition after the refusals compiles and runs" T-LABEL
   s" : AFTER-REFUSALS ( -- n ) 6 ; AFTER-REFUSALS" TEST-EVAL:N 6 T= ;

\ Each construction's cell under it proves the pads it pushed: one too many or
\ too few moves it.
: MATCH-CASE ( -- )
   s" MATCH over an enum" T-LABEL
   ROJO HUE-CODE 10 T=  VERDE HUE-CODE 20 T=  AZUL HUE-CODE 30 T=
   s" MATCH over a sum type with a payload, built with pads" T-LABEL
   77 NADA BOX-SUM 0 T= 77 T=
   77 5 UNO BOX-SUM 5 T= 77 T=
   77 10 3 DUO BOX-SUM 7 T= 77 T=
   s" a nested MATCH" T-LABEL
   6 7 DUO ROJO PAIR-CODE -1 T=
   6 7 DUO VERDE PAIR-CODE 42 T=
   4 UNO VERDE PAIR-CODE 8 T=
   NADA VERDE PAIR-CODE 1 T=
   9 UNO AZUL PAIR-CODE -9 T=
   s" a MATCH inside case" T-LABEL
   8 2 DUO 1 CASE-BOX 6 T=
   8 2 DUO 2 CASE-BOX 10 T=
   3 UNO 2 CASE-BOX 30 T=
   NADA 2 CASE-BOX -1 T=
   5 UNO 9 CASE-BOX 1005 T=
   s" a MATCH in a does> clause" T-LABEL
   ROJO W5 5 T=  VERDE W5 10 T=  AZUL W5 -5 T=
   s" a call building a wider instantiation pushes its pads" T-LABEL
   77 0 SOME-PT PT-SUM 0 T= 77 T=
   77 3 SOME-PT PT-SUM 204 T= 77 T=
   s" MATCH over a payload past a glue mask's 64 cells" T-LABEL
   77 BIG-SMALL BIG-SUM -1 T= 77 T=
   77 BIG-HUGE BIG-SUM 2145 T= 77 T=
   s" construct pushes the pads its instantiation adds" T-LABEL
   77 WRAP-EMPTY WRAP-SUM 0 T= 77 T=
   77 3 4 WRAP-FULL WRAP-SUM 61 T= 77 T=
   s" a MATCH in a quotation after character literals and a string" T-LABEL
   77 3 ORD-MIX 204 T= 77 T=
   77 0 ORD-MIX 0 T= 77 T= ;

: FORGE-CASE ( -- )
   s" test/compiler/native-match-forge.f" CHILD-RUN
   s" a hostile tag reaches T0's trap and exits" T-LABEL
   ERR-BUF CHILD-ERR-N @ s" hb: bad hue tag" CONTAINS? TTRUE
   CHILD-RC @ ENGINE-ERROR:BAD-TAG T= ;

: WIDE-CASE ( -- )
   s" drop of a two-cell value" T-LABEL
   77 9 3 4 PT W-DROP 9 T= 77 T=
   77 3 4 PT 9 W-NIP 9 T= 77 T=
   77 3 4 PT W-DUP PT-V 34 T= PT-V 34 T= 77 T=
   s" each stack word over a two-cell value" T-LABEL
   77 3 4 PT 9 W-SWAP PT-V 34 T= 9 T= 77 T=
   77 3 4 PT 9 W-OVER PT-V 34 T= 9 T= PT-V 34 T= 77 T=
   77 9 3 4 PT W-TUCK PT-V 34 T= 9 T= PT-V 34 T= 77 T=
   77 3 4 PT 8 9 W-ROT PT-V 34 T= 9 T= 8 T= 77 T=
   77 8 9 3 4 PT W--ROT 9 T= 8 T= PT-V 34 T= 77 T=
   77 3 4 PT 9 W-2DUP 9 T= PT-V 34 T= 9 T= PT-V 34 T= 77 T=
   77 9 3 4 PT 8 W-2DROP 9 T= 77 T=
   77 3 4 PT 8 9 5 6 PT W-2SWAP 8 T= PT-V 34 T= PT-V 56 T= 9 T= 77 T=
   77 3 4 PT 7 8 9 W-2OVER 7 T= PT-V 34 T= 9 T= 8 T= 7 T= PT-V 34 T= 77 T=
   77 9 3 4 PT W-R PT-V 34 T= PT-V 34 T= 9 T= 77 T=
   77 3 4 PT 9 W-2R 9 T= PT-V 34 T= 9 T= PT-V 34 T= 77 T=
   77 3 4 PT 9 W-LOCAL PT-V 34 T= 9 T= 77 T=
   s" a two-cell value in a does> parent and clause" T-LABEL
   77 5 6 PT W39 PT-V 56 T= 39 T= 77 T=
   s" a does> parent's 2swap and 2over over a deepest two-cell value" T-LABEL
   77 W-PS 673405 T= 77 T=
   77 W-PO 3405673405 T= 77 T=
   s" a three-cell value among one-cell values" T-LABEL
   77 9 1 2 3 TRIO 8 W3-ROT 9 T= 8 T= TRIO-V 123 T= 77 T=
   77 1 2 3 TRIO 9 5 6 PT 8 W3-2SWAP 9 T= TRIO-V 123 T= 8 T= PT-V 56 T= 77 T=
   77 9 1 2 3 TRIO 8 W3-R TRIO-V 123 T= 8 T= TRIO-V 123 T= 9 T= 77 T=
   s" two eight-cell values swapped through memory" T-LABEL
   77 10 OCT 20 OCT W8-SWAP OCT-V 10 T= OCT-V 20 T= 77 T=
   s" a forty-cell local bound from memory" T-LABEL
   77 10 PENT 9 W40-LOCAL PENT-V 10 T= 9 T= 77 T=
   s" @ and ! of a two-cell value, its neighbours untouched" T-LABEL
   11 1 PT-CELL !  44 4 PT-CELL !
   77 3 4 PT 1 PT! 77 T=
   2 PT-CELL @ 3 T=  3 PT-CELL @ 4 T=  1 PT-CELL @ 11 T=  4 PT-CELL @ 44 T=
   77 1 PT@ PT-V 34 T= 77 T=
   s" a two-cell value stored and fetched in one body" T-LABEL
   77 5 6 PT 2 PT-RT PT-V 56 T= 77 T=
   s" @ and ! of a three-cell value, its neighbours untouched" T-LABEL
   22 2 TRIO-CELL !  66 6 TRIO-CELL !
   77 1 2 3 TRIO 1 TRIO! 77 T=
   3 TRIO-CELL @ 1 T=  4 TRIO-CELL @ 2 T=  5 TRIO-CELL @ 3 T=
   2 TRIO-CELL @ 22 T=  6 TRIO-CELL @ 66 T=
   77 1 TRIO@ TRIO-V 123 T= 77 T=
   s" @ and ! of a sixteen-cell value, its neighbours untouched" T-LABEL
   11 15 TWIN-CELL !  44 32 TWIN-CELL !
   77 10 20 TWIN 1 TWIN! 77 T=
   1 10 20 TWIN-HITS 16 T=  15 TWIN-CELL @ 11 T=  32 TWIN-CELL @ 44 T=
   2 30 40 TWIN-SEED
   77 2 TWIN@ TWIN-V 40 T= 30 T= 77 T=
   s" a checked fetch of a valid tag loads the value, the cell under it kept" T-LABEL
   10 3 DUO 1 BOX!  AZUL 2 HUE!
   77 1 BOX@ BOX-SUM 7 T= 77 T=
   77 2 HUE@ HUE-CODE 30 T= 77 T= ;

: SHARE-CASE ( -- )
   s" arithmetic on a copy leaves its source" T-LABEL
   77 5 S-INC 11 T= 77 T=
   77 5 S-TRIPLE 15 T= 77 T=
   77 3 4 S-OVER 3 T= 7 T= 77 T=
   77 3 4 S-TUCK -1 T= 4 T= 77 T=
   s" a copy parked on the return stack keeps its value" T-LABEL
   77 5 S-PARK 11 T= 77 T=
   s" loads through a copied address" T-LABEL
   77 s" AB" drop S-BYTES 131 T= 77 T=
   s" a negate of a compared copy" T-LABEL
   77 -5 S-ABS 5 T= 77 T=
   77 5 S-ABS 5 T= 77 T=
   s" two comparisons of one value" T-LABEL
   77 4 S-RANGE TTRUE 77 T=
   77 6 S-RANGE TFALSE 77 T=
   77 2 S-RANGE TFALSE 77 T=
   s" fifteen copies fill the virtual stack and evict a shared vreg" T-LABEL
   77 3 S-FILL 48 T= 77 T= ;

\ Each compare through `if` both ways, through `while` and `until` loops that
\ run a counted number of turns, with an immediate operand, a negative one, one
\ on the left, a literal spilled between the compare and its branch, a flag
\ read twice, which stays a flag, and a compare before a local named `if`; a
\ cell under each result.
: FUSE-CASE ( -- )
   s" each compare through if, true and false" T-LABEL
   77 3 3 FU-EQ 1 T= 77 T=   77 3 4 FU-EQ 2 T= 77 T=
   77 3 4 FU-NE 1 T= 77 T=   77 3 3 FU-NE 2 T= 77 T=
   77 3 4 FU-LT 1 T= 77 T=   77 4 3 FU-LT 2 T= 77 T=   77 3 3 FU-LT 2 T= 77 T=
   77 4 3 FU-GT 1 T= 77 T=   77 3 4 FU-GT 2 T= 77 T=   77 3 3 FU-GT 2 T= 77 T=
   77 3 3 FU-LE 1 T= 77 T=   77 3 4 FU-LE 1 T= 77 T=   77 4 3 FU-LE 2 T= 77 T=
   77 3 3 FU-GE 1 T= 77 T=   77 4 3 FU-GE 1 T= 77 T=   77 3 4 FU-GE 2 T= 77 T=
   77 0 FU-ZEQ 1 T= 77 T=   77 9 FU-ZEQ 2 T= 77 T=
   77 -1 FU-ZLT 1 T= 77 T=   77 0 FU-ZLT 2 T= 77 T=
   s" an immediate operand, a negative one and one on the left" T-LABEL
   77 4 FU-IMM 1 T= 77 T=   77 5 FU-IMM 2 T= 77 T=
   77 -4 FU-NEG 1 T= 77 T=   77 -5 FU-NEG 2 T= 77 T=
   77 6 FU-LEFT 1 T= 77 T=   77 5 FU-LEFT 2 T= 77 T=
   s" a literal spilled between the compare and its branch" T-LABEL
   77 -3 FU-UNDER 1 T= 7 T= 77 T=   77 3 FU-UNDER 2 T= 7 T= 77 T=
   s" a flag read twice before its if" T-LABEL
   77 3 4 FU-TWICE 1 T= 77 T=   77 4 3 FU-TWICE 2 T= 77 T=
   s" a compare before a local named if" T-LABEL
   77 1 9 FU-SHADOW 9 T= TTRUE 77 T=   77 4 9 FU-SHADOW 9 T= TFALSE 77 T=
   s" each compare through while" T-LABEL
   77 4 WH-EQ 5 T= 77 T=   77 4 WH-NE 4 T= 77 T=   77 4 WH-LT 4 T= 77 T=
   77 4 WH-GT 4 T= 77 T=   77 4 WH-LE 5 T= 77 T=   77 4 WH-GE 5 T= 77 T=
   s" each compare through until" T-LABEL
   77 4 UN-EQ 4 T= 77 T=   77 4 UN-NE 5 T= 77 T=   77 4 UN-LT 5 T= 77 T=
   77 4 UN-GT 5 T= 77 T=   77 4 UN-LE 4 T= 77 T=   77 4 UN-GE 4 T= 77 T= ;

\ A forged tag in an enum slot and in a product's enum field: the fetch's tag
\ check ends the child after the fixture armed it.
: FETCH-FORGED ( -- )
   OUT-BUF CHILD-OUT-N @ s" LAYOUT-VALID-ARMED" CONTAINS? TTRUE
   ERR-BUF CHILD-ERR-N @ s" hb: bad layout tag" CONTAINS? TTRUE
   CHILD-RC @ ENGINE-ERROR:BAD-TAG T= ;

: FETCH-FORGE-CASE ( -- )
   s" a fetch of a forged one-cell tag ends the process at the fetch" T-LABEL
   s" test/layout-valid-w1-bad.f" CHILD-RUN FETCH-FORGED
   s" a fetch of a forged tag in a product ends the process at the fetch" T-LABEL
   s" test/layout-valid-product-bad.f" CHILD-RUN FETCH-FORGED ;

public

: RUN ( -- )
   T-RESET
   CALL-CASE
   DOES-CASE
   FIXED-CASE
   MATCH-CASE
   FORGE-CASE
   WIDE-CASE
   SHARE-CASE
   FUSE-CASE
   FETCH-FORGE-CASE
   IS-CASE
   CALLBACK-CASE
   REFUSAL-CASE
   T-REPORT ;

;package

T0-INSTALL-TEST:RUN
