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
\ two-cell product with a cell under the result, a one-cell pair that 2>r
\ leaves for r@ and r> and >r leaves for 2r>, a swap in a does> parent and in
\ its clause, a 2swap and a 2over in a does> parent whose deepest operand is
\ two cells, a three-cell product among one-cell values, a swap of two
\ eight-cell values, whose copies fill the virtual stack, and a local bound
\ from a forty-cell value, whose deepest cell lies past a pre-index load's
\ reach, and that value with a cell through 2>r 2r@ 2r>, a constant under
\ the copy; `@` and `!` of a two-, a three- and a sixteen-cell value through
\ typed slots, with the cells beside each store unchanged and the fetch reading
\ cells written through a raw view, the sixteen cells spilling the virtual
\ stack, and a store and fetch in one body. The checker refuses every other
\ operation over a wide value, so no row reaches E-T0-BUNDLE. A fetch from a
\ sum-type and an enum slot checks the stored tag and loads the value with the
\ cell under it kept; a forged tag fetched in a child, from an enum slot and
\ from a product's enum field, ends the process `hb: bad layout tag` with
\ ENGINE-ERROR:BAD-TAG.
\ The share rows run bodies whose copies share a vreg with their source, each
\ with a cell under its result: arithmetic on a copy with a register, a
\ literal on either side and a shift count, a negate of a copy, a copy parked
\ on the return stack, loads through a copied address, comparisons of a copy
\ with zero, with a literal on either side and with a register, and fifteen
\ copies that fill the virtual stack and evict a shared vreg. The cell rows
\ run `cells` over a negative count, store and fetch through a pointer table
\ by `ptr-field`, and fold `3 cells cell+`, with a cell under each result. The
\ fuse rows run compares straight into `if`, `while` and `until`, which branch
\ on their condition codes, with a cell under each result. The join rows run
\ `if`, `else` and `then` whose arms leave the virtual stack in different
\ layouts, each both ways with a cell under its result: a call in one arm,
\ with and without the layout's bottom cells loaded back from memory, swapped
\ and rotated arms, a constant against a register and one whose goal vreg
\ another entry holds, arms that leave more or fewer registers than the
\ layout, an `exit` in an `if`, an `else`, an `of` arm, a default arm and a
\ loop with a join after each, an if/else nested in each arm, sixteen live
\ values evicted in one arm, an else arm after a first arm that held every
\ vreg, a copy live across an `if` whose arm writes one copy, and a fused
\ compare whose copy and constant settle between the compare and its branch.
\ The loop rows run `begin` loops whose values stay in registers around the
\ back edge, each with a cell under its result: a byte counted in a string, a
\ 2-cycle and a 3-cycle at the back edge, two `while` exits that leave
\ different register counts, constants below a loop, a call in the body,
\ sixteen live values evicted in the body, loops in each arm of an if/else,
\ `until` on a compare of a loop value and on a flag in the head (a loop value,
\ its top vreg, a call's in its bottom vreg), `until` on a compound flag
\ outside the head, a loop whose inputs are in memory at `begin`, `again` left
\ by `exit` with an exchange at the back edge and by a `while`, and a fused
\ `while` with a constant below.
\ The counted loop rows run `do` and `?do` loops whose values stay in
\ registers, each with a cell under its result: `+loop` counting up by a copy
\ of a loop value, a step and a `loop` scratch beside the vreg the layout gives
\ a dropped value's replacement, `?do` with equal and ordered bounds under
\ `loop` and `+loop`, `leave` from an `if` arm that swapped the values and
\ from a back edge that swaps them, nested levels with the inner one left, a
\ constant and a copy under the bounds, and a full virtual stack at `do`. The
\ index rows read `i` where its register copy may be stale, each with a cell
\ under its result: after a call whose callee runs a loop, a recursive call, a
\ join with an arm that runs a loop, an inner level that closes, `unloop`, a
\ begin loop whose body runs a loop, a `+loop` counting down after a call in
\ one arm, and `j` and `i` around a call with both levels live.
\ The division rows run `/` and `mod` from a local by the literals 1, 7, 65536,
\ 86400, -3 and 10^10, which T0 divides in place, over dividends of both signs
\ and MIN-N against the call's answers by a register divisor; by 7 on a
\ dividend shared with a copy and on one in memory; with both operands in
\ memory, which stays the call; folded by 1 and 7; and by a zero divisor,
\ from a local and after a literal dividend, which stays the call and throws
\ E-DIV-ZERO.
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

\ A one-cell pair on the return stack read by the one-cell words and the
\ reverse, in the primitive's order: 2>r leaves its top cell on top.
: N-RX ( n n -- n n n ) 2>r r@ r> r> >r >r 2r> ;
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
: W40-2R ( pent n -- n pent n pent n ) 2>r 5 2r@ 2r> ;

\ ---- copies that share a vreg ---------------------------------------------------
: S-INC ( n -- n ) dup 1+ + ;
: S-TRIPLE ( n -- n ) dup dup + + ;
: S-ADDC ( n -- n n ) dup 5 swap + ;
: S-MULC ( n -- n n ) dup 3 swap * ;
: S-MULL ( n -- n n ) dup 3 * ;
: S-SHL ( n -- n n ) dup 2 lshift ;
: S-NEG ( n -- n n ) dup negate ;
: S-PARK ( n -- n ) dup >r 1+ r> + ;
: S-BYTES ( ptr u8 -- n ) dup c@ swap 1+ c@ + ;
: S-ABS ( n -- n ) dup 0< if negate then ;
: S-RANGE ( n -- bool ) dup 5 < swap 3 > and ;
: S-LT5 ( n -- n bool ) dup 5 swap < ;
: S-LESS ( n n -- n n bool ) over over < ;
: S-FILL ( n -- n )
   dup dup dup dup dup dup dup dup dup dup dup dup dup dup dup
   + + + + + + + + + + + + + + + ;

\ ---- cell arithmetic ----------------------------------------------------------
3 PTR-U8-TABLE CA-PT
: CA-CELLS ( n -- n ) cells ;
: CA-PUT ( ptr u8 n -- ) CA-PT swap ptr-field ! ;
: CA-GET ( n -- ptr u8 ) CA-PT swap ptr-field @ ;
: CA-FOLD ( -- n ) 3 cells cell+ ;

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

\ A fused branch, then an `if` on a flag no compare in the body made: it tests
\ the flag, not the condition codes the first compare left.
: FU-STALE ( n n bool -- n ) rot rot < if 1 else 2 then swap if 10 + then ;

\ A local named `if` shadows the keyword: the compare before it stays a flag.
: FU-SHADOW ( n n -- bool n )
   {: x:n if:n :}
   x 3 < if ;

: WH-LT ( n -- n )
   {: n:n :}
   0 begin dup n < while 1+ repeat ;

: UN-GE ( n -- n )
   {: n:n :}
   0 begin 1+ dup n >= until ;

\ ---- joins that keep registers ------------------------------------------------
\ Each body leaves its values in vregs at `if`, and its arms leave them in
\ other layouts that `else` and `then` reconcile.
: J-TWICE ( n -- n ) 2 * ;
: J-CALL ( n n bool -- n ) >r 1+ swap 1+ r> if J-TWICE then - ;
: J-SWAP ( n n bool -- n ) >r 1+ swap 1+ swap r> if swap then - ;
: J-ELSE ( n n bool -- n ) >r 1+ swap 1+ swap r> if 100 + else swap then - ;
: J-ROT ( n n n bool -- n ) >r 1+ rot 1+ rot 1+ rot r> if rot then - - ;
: J-CONST ( n bool -- n ) >r 1+ r> if 2 * else drop 5 then 1+ ;
: J-KONST ( n bool -- n ) >r 1+ r> if drop 5 else 2 * then 1+ ;
: J-SINK ( n n bool -- n ) if swap 1+ swap then - ;
: J-PART ( n n bool -- n ) >r 1+ r> if swap 1+ swap then - ;
: J-DEEP ( n n bool -- n ) if swap 1+ swap else J-TWICE then - ;
: J-EXIT-IF ( n bool -- n ) >r 1+ r> if 10 + exit else 2 * then 1+ ;
: J-NEST ( n n bool bool -- n )
   {: f:bool g:bool :}
   1+ swap 1+ swap
   f if g if swap else 100 + then else g if 1000 + else swap then then - ;

\ Sixteen values live across a fused `if`, the virtual stack full, and an arm
\ whose pushes evict the bottom entries to memory.
: J-FULL ( n n -- n )
   >r
   dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+
   dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+
   r> 0< if dup 1+ dup 1+ dup 1+ + + + then
   + + + + + + + + + + + + + + + ;

: J-DUP ( n bool -- n ) >r 1+ dup r> if 1+ then * ;
: J-DUP2 ( n n bool -- n ) >r 1+ swap 1+ swap r> if nip dup then + ;
\ A copy and a constant settled between the compare and its branch.
: J-FUSE ( n n -- n n n ) >r 1+ dup 7 r> 0< if + 1 else * 2 then ;

\ The goal layout's bottom cells come back from memory after the call; the
\ entries meet the layout's top cells.
: J-RAISE ( n n bool -- n ) >r 1+ swap 1+ r> if J-TWICE 1+ then - ;
\ A constant whose goal vreg another entry still holds.
: J-KGOAL ( n n bool -- n ) >r 1+ swap 1+ swap r> if drop 5 swap then - ;

\ An exit in an `if`, an `of` arm, a default arm and a loop: the join after
\ each is reached again.
: J-EXIT ( n bool bool -- n ) swap if drop drop 100 exit then if drop 7 then ;
: J-EXIT-OF ( n n n -- n n ) case 0 of swap exit endof >r over 0< if swap then r> endcase ;
: J-EXIT-CASE ( n n n -- n n ) case 0 of swap endof drop swap exit endcase over 0< if swap then ;
: J-EXIT-LOOP ( n n -- n n ) 3 0 do 1+ dup 5 > if leave then unloop exit loop over 0< if swap then ;

\ A first arm that leaves fifteen constants, each settled into a vreg, and an
\ else arm that starts from a layout holding none of them and loads its value.
: J-SPENT ( n bool -- n )
   if drop 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15
   else dup dup dup dup dup dup dup dup dup dup dup dup dup dup then
   + + + + + + + + + + + + + + ;

\ ---- loops that keep registers ------------------------------------------------
\ Each loop keeps its values in vregs at `begin`, its exits' at `while`, and the
\ back edge and the exits reconcile to them. Most bodies read their inputs from
\ locals or move them before `begin`, so the loop head holds them in vregs.
: L-SMALL ( n -- bool ) 100 < ;

\ The bytes equal to ch in a string: a `while` loop over an address and a
\ count, with a counter below them that an `if` in the body bumps.
: L-COUNT ( ptr u8 n n -- n )
   {: ch:n :}
   0 rot rot
   begin dup 0 > while
      over c@ ch = if rot 1+ rot rot then
      swap 1+ swap 1-
   repeat 2drop ;

: L-SWAP ( n n n -- n n )
   {: a:n b:n k:n :}
   a b begin swap 1+ dup k > until ;

: L-ROT ( n n n n -- n n n )
   {: a:n b:n c:n k:n :}
   a b c begin rot 1+ dup k > until ;

\ Two exits: the first with both values in vregs, the second after a call.
\ The checker refuses a second `while`, so the body is TRUSTED:.
TRUSTED: L-TWO ( n n -- n n )
   {: a:n k:n :}
   a k begin dup 0 > while over L-SMALL while swap 2 * swap 1- repeat then ;

: L-KONST ( n -- n n n n ) 1 swap 2 swap 3 swap begin 1- dup 0= until ;

: L-CALL ( n n -- n )
   {: a:n k:n :}
   a k begin swap J-TWICE swap 1- dup 0= until drop ;

\ Sixteen values live around a loop whose body evicts the bottom ones.
: L-FULL ( n n -- n )
   >r
   dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+
   dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+
   begin dup dup dup + + + 2 / r> 1- dup >r 0= until r> drop
   + + + + + + + + + + + + + + + ;

: L-NEST ( n n bool -- n )
   {: a:n b:n f:bool :}
   a b f if begin dup 100 < while swap 1+ swap 2 * repeat
   else begin 1+ dup 10 > until then - ;

: L-IF-UNTIL ( n n bool -- n n )
   {: a:n b:n f:bool :}
   a b f if begin swap 1+ swap 2 * dup 50 > until then ;

\ A loop value that is a flag, whose body's compare leaves condition codes that
\ `until` must not read.
: L-UNTIL-REG ( n bool bool -- n bool bool )
   {: k:n a:bool b:bool :}
   k a b begin rot dup 0 < if negate then 1+ rot rot swap dup until ;

\ `until` on the head's top vreg, with an exchange at the back edge.
: L-TOP-FLAG ( n bool bool -- n bool bool )
   {: k:n a:bool b:bool :}
   k a b begin rot 1+ rot rot swap over until ;

: L-DONE ( n -- bool ) 0= ;

\ `until` on a call's flag, which lands in the head's bottom vreg while the back
\ edge reloads that cell from memory.
: L-CALL-FLAG ( n n -- n n )
   {: a:n k:n :}
   a k begin swap 1+ swap 1- dup L-DONE until ;

\ `until` on a compound flag outside the head, with an exchange at the back
\ edge. The turns, counted on the return stack, also end the loop, so a back
\ edge that loses the values ends with a wrong count instead of running on.
: L-AND-FLAG ( n n -- n n n )
   {: a:n b:n :}
   0 >r a b begin swap 1+ over 5 > over 3 > and r> 1+ dup >r 20 > or until r> ;

\ The inputs still in memory at `begin`: the head holds no vregs.
: L-MEM ( n n -- n n ) begin swap 1+ dup 5 = until ;

\ `again` whose back edge exchanges the two values.
: L-AGAIN ( n n -- n n )
   {: a:n b:n :}
   a b begin swap 1+ dup 9 > if exit then again ;

\ The checker refuses a `while` closed by `again`, so the body is TRUSTED:.
TRUSTED: L-WAGAIN ( n n -- n n )
   {: a:n k:n :}
   a k begin dup 0 > while swap 1+ swap 1- again then ;

: L-WCONST ( n -- n n )
   {: a:n :}
   a begin 0 swap dup 10 < while nip 2 * repeat ;

\ ---- counted loops that keep registers ----------------------------------------
\ Each `do` keeps the values under its start and limit in vregs, and the back
\ edge and every `leave` reconcile to them, and `?do`'s skip leaves from the
\ head, already in them. A body reads its inputs from locals, so the level's
\ layout holds them in vregs.

\ The step is a copy of a value the layout holds.
: D-UP ( n n -- n n )
   {: a:n k:n :}
   a k 10 0 do swap i + swap dup +loop ;

\ The step sits in the vreg of the value the body dropped, which the layout
\ gives the constant pushed in its place.
: D-FREED ( n n n -- n n n )
   {: c:n a:n b:n :}
   c a b 20 0 do rot drop 100 rot rot swap 1+ swap dup 1+ +loop ;

\ `loop` takes its scratch vreg after the back edge settles, so the scratch
\ misses the vreg the layout gives the constant.
: D-DROP ( n n -- n n )
   {: a:n b:n :}
   a b 5 0 do drop 7 loop ;

: D-QDO ( n n n n -- n n )
   {: a:n b:n lim:n st:n :}
   a b lim st ?do swap 1+ swap 2 * loop ;

: D-QSTEP ( n n n n -- n n )
   {: a:n k:n lim:n st:n :}
   a k lim st ?do swap 1+ swap dup negate +loop ;

\ `leave` from an arm that swapped the values: that exit carries them swapped,
\ the loop's end does not.
: D-LEAVE ( n n n n -- n n )
   {: a:n b:n k:n n:n :}
   a b n 0 do swap 1+ swap 2 * dup k > if swap leave then loop ;

: D-NEST ( n n -- n n )
   {: a:n b:n :}
   a b 3 0 do
      swap 1+ swap 7 4 0 do 1+ i + j + i 2 = if leave then loop +
   loop ;

\ The back edge swaps the values, and `leave` exits from the swapped layout.
: D-SWAP ( n n -- n n )
   {: a:n b:n :}
   a b 5 0 do swap 1+ i 2 = if leave then loop ;

\ A constant and a copy under the bounds settle into vregs of their own.
: D-KONST ( n -- n n n )
   {: a:n :}
   5 a dup 3 0 do 1+ loop ;

\ Fifteen values fill the virtual stack at `do`, whose bounds evict two.
: D-FULL ( n -- n )
   dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+
   dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+
   4 0 do 1+ loop
   + + + + + + + + + + + + + + ;

: D-FULL-STEP ( n -- n )
   dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+
   dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+ dup 1+
   8 0 do 1+ 2 +loop
   + + + + + + + + + + + + + + ;

\ ---- the cached loop index ----------------------------------------------------
\ On arm64 x29 caches the innermost index beside its frame cell. Each body
\ reaches `i`, `loop` or `+loop` from code that ran another loop, a callee's,
\ a nested level's or one on another path into a join, so a stale cache shows
\ as a wrong result.

\ 3 + 4 + 5 + 6, leaving its index at 7.
: X-INNER ( -- n ) 0 7 3 do i + loop ;

: X-CALL ( n -- n ) 0 swap 0 do X-INNER drop i + loop ;

: X-REC ( n -- n )
   dup 0= if exit then
   0 over 2 + 0 do over 1- recurse drop i + loop nip ;

\ The arm that runs a loop joins the one that read `i`.
: X-JOIN ( n -- n )
   0 swap 0 do i 1 and 0<> if X-INNER drop else i drop then i + loop ;

\ The outer index after an inner level closes at its limit.
: X-NEST ( n -- n ) 0 swap 0 do 5 0 do loop i + loop ;

: X-UNLOOP ( -- n )
   5 2 do 3 0 do i 1 = if unloop i unloop exit then loop loop 0 ;

\ The begin loop's back edge arrives from a call.
: X-BEGIN ( n -- n )
   0 swap 0 do 3 begin i rot + swap X-INNER drop 1- dup 0= until drop loop ;

: X-DOWN ( -- n ) 0 0 10 do i + i 10 = if X-INNER drop then -2 +loop ;

: X-J ( -- n ) 0 3 0 do 4 0 do X-INNER drop i j 10 * + + loop loop ;

\ ---- division by a literal --------------------------------------------------
\ `/` and `mod` by a literal neither 0 nor -1 divide in place from a local and
\ fold when the dividend is a literal too; a zero divisor and DREF's register
\ stay the call of the kernel row.
: DREF ( n n -- n n ) {: a:n b:n :} a b / a b mod ;

: Q1 ( n -- n n ) {: a:n :} a 1 / a 1 mod ;
: Q7 ( n -- n n ) {: a:n :} a 7 / a 7 mod ;
: Q65536 ( n -- n n ) {: a:n :} a 65536 / a 65536 mod ;
: Q86400 ( n -- n n ) {: a:n :} a 86400 / a 86400 mod ;
: QNEG3 ( n -- n n ) {: a:n :} a -3 / a -3 mod ;
: QWIDE ( n -- n n ) {: a:n :} a 10000000000 / a 10000000000 mod ;
: QZERO ( n -- n ) {: a:n :} a 0 / ;
: MZERO ( n -- n ) {: a:n :} a 0 mod ;
\ The dividend shared with a copy the divide must leave alone.
: QSHARE ( n -- n n n ) dup 7 / over 7 mod ;
\ The dividend in memory, and both operands in memory.
: D7 ( n -- n ) 7 / ;
: M7 ( n -- n ) 7 mod ;
: DIVX ( n n -- n ) / ;

1000003 constant FOLD-X
: FD1 ( -- n n n n ) 1000003 1 / 1000003 1 mod -1000003 1 / -1000003 1 mod ;
: FD7 ( -- n n n n ) 1000003 7 / 1000003 7 mod -1000003 7 / -1000003 7 mod ;
: FDZERO ( -- n ) 7 0 / ;

21 constant NDIVS
NDIVS TYPED-BUFFER DIVS n
0 0 DIVS !  1 1 DIVS !  3 2 DIVS !  6 3 DIVS !  7 4 DIVS !  99 5 DIVS !
100 6 DIVS !  399 7 DIVS !  400 8 DIVS !  65535 9 DIVS !  65536 10 DIVS !
1000003 11 DIVS !  -1 12 DIVS !  -3 13 DIVS !  -7 14 DIVS !  -100 15 DIVS !
-400 16 DIVS !  -65535 17 DIVS !  -1000003 18 DIVS !
$7FFFFFFFFFFFFFFF 19 DIVS !  1 63 lshift 20 DIVS !

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
   s" a one-cell pair on the return stack in the primitive's order" T-LABEL
   77 1 2 N-RX 2 T= 1 T= 2 T= 77 T=
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
   s" a forty-cell value and a cell through 2>r 2r@ 2r>, a constant under the copy" T-LABEL
   77 10 PENT 9 W40-2R 9 T= PENT-V 10 T= 9 T= PENT-V 10 T= 5 T= 77 T=
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
   s" a literal on either side of a copy leaves its source" T-LABEL
   77 3 S-ADDC 8 T= 3 T= 77 T=
   77 3 S-MULC 9 T= 3 T= 77 T=
   77 3 S-MULL 9 T= 3 T= 77 T=
   s" a shift of a copy leaves its source" T-LABEL
   77 3 S-SHL 12 T= 3 T= 77 T=
   s" a negate of a copy leaves its source" T-LABEL
   77 3 S-NEG -3 T= 3 T= 77 T=
   s" a copy parked on the return stack keeps its value" T-LABEL
   77 5 S-PARK 11 T= 77 T=
   s" loads through a copied address" T-LABEL
   77 s" AB" drop S-BYTES 131 T= 77 T=
   s" a compare with zero on a copy leaves its source" T-LABEL
   77 -5 S-ABS 5 T= 77 T=
   77 5 S-ABS 5 T= 77 T=
   s" two comparisons of one value" T-LABEL
   77 4 S-RANGE TTRUE 77 T=
   s" a literal compared with a copy leaves its source" T-LABEL
   77 3 S-LT5 TFALSE 3 T= 77 T=
   s" a compare of two copies leaves both sources" T-LABEL
   77 3 4 S-LESS TTRUE 4 T= 3 T= 77 T=
   s" fifteen copies fill the virtual stack and evict a shared vreg" T-LABEL
   77 3 S-FILL 48 T= 77 T= ;

: CELLS-CASE ( -- )
   s" cells takes a negative count and ptr-field indexes a pointer table" T-LABEL
   77 -2 CA-CELLS -16 T= 77 T=
   s" AB" drop 0 CA-PUT  s" CD" drop 2 CA-PUT
   77 2 CA-GET c@ 67 T= 77 T=   77 0 CA-GET c@ 65 T= 77 T=
   s" literal operands fold" T-LABEL
   77 CA-FOLD 32 T= 77 T= ;

\ Each compare through `if` both ways, through a `while` and an `until` loop
\ whose inverted branch exits early with a wrong count, with an immediate
\ operand, a negative one, one on the left, a literal spilled between the
\ compare and its branch, a flag read twice, which stays a flag, an `if` on a
\ flag no compare made after a fused one, and a compare before a local named
\ `if`; a cell under each result.
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
   s" an if on a flag no compare made, after a fused if" T-LABEL
   77 3 4 false FU-STALE 1 T= 77 T=   77 4 3 true FU-STALE 12 T= 77 T=
   s" a compare before a local named if" T-LABEL
   77 1 9 FU-SHADOW 9 T= TTRUE 77 T=   77 4 9 FU-SHADOW 9 T= TFALSE 77 T=
   s" a compare through while and through until" T-LABEL
   77 4 WH-LT 4 T= 77 T=   77 4 UN-GE 4 T= 77 T= ;

\ Each join both ways, with a cell under each result.
: JOIN-CASE ( -- )
   s" a call in one arm" T-LABEL
   77 3 10 true J-CALL 3 T= 77 T=   77 3 10 false J-CALL 7 T= 77 T=
   77 3 10 true J-RAISE 2 T= 77 T=   77 3 10 false J-RAISE 7 T= 77 T=
   s" swapped and rotated arms" T-LABEL
   77 3 10 true J-SWAP 7 T= 77 T=   77 3 10 false J-SWAP -7 T= 77 T=
   77 3 10 true J-ELSE -107 T= 77 T=   77 3 10 false J-ELSE 7 T= 77 T=
   77 1 10 100 true J-ROT -88 T= 77 T=   77 1 10 100 false J-ROT 92 T= 77 T=
   s" a constant against a register" T-LABEL
   77 3 true J-CONST 9 T= 77 T=   77 3 false J-CONST 6 T= 77 T=
   77 3 true J-KONST 6 T= 77 T=   77 3 false J-KONST 9 T= 77 T=
   77 3 10 true J-KGOAL 1 T= 77 T=   77 3 10 false J-KGOAL -7 T= 77 T=
   s" arms that leave different register counts" T-LABEL
   77 3 10 true J-SINK -6 T= 77 T=   77 3 10 false J-SINK -7 T= 77 T=
   77 3 10 true J-PART -7 T= 77 T=   77 3 10 false J-PART -8 T= 77 T=
   77 3 10 true J-DEEP -6 T= 77 T=   77 3 10 false J-DEEP -17 T= 77 T=
   s" an exit in an if, an else, an of, a default arm and a loop" T-LABEL
   77 5 true true J-EXIT 100 T= 77 T=   77 5 false true J-EXIT 7 T= 77 T=
   77 5 false false J-EXIT 5 T= 77 T=
   77 3 true J-EXIT-IF 14 T= 77 T=   77 3 false J-EXIT-IF 9 T= 77 T=
   77 3 10 0 J-EXIT-OF 3 T= 10 T= 77 T=   77 -1 0 2 J-EXIT-OF -1 T= 0 T= 77 T=
   77 3 10 2 J-EXIT-OF 10 T= 3 T= 77 T=
   77 3 -4 0 J-EXIT-CASE -4 T= 3 T= 77 T=   77 3 4 0 J-EXIT-CASE 3 T= 4 T= 77 T=
   77 3 -4 1 J-EXIT-CASE 3 T= -4 T= 77 T=
   77 -5 9 J-EXIT-LOOP -5 T= 10 T= 77 T=   77 5 9 J-EXIT-LOOP 10 T= 5 T= 77 T=
   77 3 -4 J-EXIT-LOOP -3 T= 3 T= 77 T=
   s" an if and else nested in each arm" T-LABEL
   77 3 10 true true J-NEST 7 T= 77 T=     77 3 10 true false J-NEST -107 T= 77 T=
   77 3 10 false true J-NEST -1007 T= 77 T=   77 3 10 false false J-NEST 7 T= 77 T=
   s" sixteen live values, evicted in one arm" T-LABEL
   77 1 -1 J-FULL 190 T= 77 T=   77 1 1 J-FULL 136 T= 77 T=
   s" an else arm after a first arm that held every vreg" T-LABEL
   77 4 true J-SPENT 120 T= 77 T=   77 4 false J-SPENT 60 T= 77 T=
   s" a shared vreg across an if" T-LABEL
   77 3 true J-DUP 20 T= 77 T=   77 3 false J-DUP 16 T= 77 T=
   77 3 10 true J-DUP2 22 T= 77 T=   77 3 10 false J-DUP2 15 T= 77 T=
   s" a copy and a constant settled between a compare and its branch" T-LABEL
   77 3 -1 J-FUSE 1 T= 11 T= 4 T= 77 T=   77 3 1 J-FUSE 2 T= 28 T= 4 T= 77 T= ;

\ Each loop with a cell under its result.
: LOOP-REG-CASE ( -- )
   s" a byte counted in a string" T-LABEL
   77 s" a,b,,c," [char] , L-COUNT 4 T= 77 T=   77 s" abc" [char] , L-COUNT 0 T= 77 T=
   s" a 2-cycle and a 3-cycle at the back edge" T-LABEL
   77 1 10 5 L-SWAP 11 T= 2 T= 77 T=
   77 1 10 100 50 L-ROT 101 T= 11 T= 2 T= 77 T=
   s" two exits that leave different register counts" T-LABEL
   77 3 5 L-TWO 0 T= 96 T= 77 T=   77 30 5 L-TWO 3 T= 120 T= 77 T=
   s" three constants below a loop" T-LABEL
   77 3 L-KONST 0 T= 3 T= 2 T= 1 T= 77 T=
   s" a call in the loop body" T-LABEL
   77 3 4 L-CALL 48 T= 77 T=
   s" sixteen live values around a loop" T-LABEL
   77 1 2 L-FULL 184 T= 77 T=   77 1 1 L-FULL 152 T= 77 T=
   s" loops nested in each arm of an if and else" T-LABEL
   77 3 10 true L-NEST -153 T= 77 T=   77 3 10 false L-NEST -8 T= 77 T=
   77 3 10 true L-IF-UNTIL 80 T= 6 T= 77 T=   77 3 10 false L-IF-UNTIL 10 T= 3 T= 77 T=
   s" until on a flag in the head: a loop value, its top vreg, a call's" T-LABEL
   77 0 false true L-UNTIL-REG TTRUE TFALSE 2 T= 77 T=
   77 0 true false L-UNTIL-REG TTRUE TFALSE 1 T= 77 T=
   77 0 true false L-TOP-FLAG TFALSE TTRUE 2 T= 77 T=
   77 3 4 L-CALL-FLAG 0 T= 7 T= 77 T=
   s" until on a compound flag outside the head" T-LABEL
   77 0 0 L-AND-FLAG 12 T= 6 T= 6 T= 77 T=
   s" a loop whose inputs are in memory at begin" T-LABEL
   77 0 0 L-MEM 5 T= 4 T= 77 T=
   s" again left by exit and by a while" T-LABEL
   77 0 0 L-AGAIN 10 T= 9 T= 77 T=
   77 3 4 L-WAGAIN 0 T= 7 T= 77 T=
   s" a while on a fused compare with a constant below" T-LABEL
   77 3 L-WCONST 12 T= 0 T= 77 T= ;

\ Each counted loop with a cell under its result.
: DO-REG-CASE ( -- )
   s" +loop counting up by a copy of a loop value" T-LABEL
   77 0 3 D-UP 3 T= 18 T= 77 T=
   s" a +loop step in a vreg the layout gives another value" T-LABEL
   77 7 0 4 D-FREED 4 T= 4 T= 100 T= 77 T=
   s" a loop scratch beside a constant the layout holds" T-LABEL
   77 1 2 D-DROP 7 T= 1 T= 77 T=
   s" ?do with equal and with ordered bounds" T-LABEL
   77 3 1 5 5 D-QDO 1 T= 3 T= 77 T=   77 3 1 5 2 D-QDO 8 T= 6 T= 77 T=
   77 3 2 6 6 D-QSTEP 2 T= 3 T= 77 T=   77 3 2 0 6 D-QSTEP 2 T= 7 T= 77 T=
   s" leave from inside an if" T-LABEL
   77 0 1 20 100 D-LEAVE 5 T= 32 T= 77 T=   77 0 1 1000 4 D-LEAVE 16 T= 4 T= 77 T=
   s" nested loops with values live in both, the inner one left" T-LABEL
   77 1 0 D-NEST 48 T= 4 T= 77 T=
   s" a swap at the back edge, left from the swapped layout" T-LABEL
   77 1 10 D-SWAP 3 T= 11 T= 77 T=
   s" a constant and a copy under the bounds" T-LABEL
   77 7 D-KONST 10 T= 7 T= 5 T= 77 T=
   s" a full virtual stack at do" T-LABEL
   77 1 D-FULL 124 T= 77 T=   77 1 D-FULL-STEP 124 T= 77 T= ;

\ Each read of a cached index with a cell under its result.
: IDX-CASE ( -- )
   s" i after a call whose callee runs a loop" T-LABEL
   77 4 X-CALL 6 T= 77 T=
   s" i after a recursive call that runs the loop" T-LABEL
   77 2 X-REC 6 T= 77 T=
   s" i after a join with an arm that runs a loop" T-LABEL
   77 4 X-JOIN 6 T= 77 T=
   s" i after an inner level closes" T-LABEL
   77 4 X-NEST 6 T= 77 T=
   s" i of the outer level after unloop" T-LABEL
   77 X-UNLOOP 2 T= 77 T=
   s" i in a begin loop whose body runs a loop" T-LABEL
   77 4 X-BEGIN 18 T= 77 T=
   s" i around a call in a +loop counting down" T-LABEL
   77 X-DOWN 30 T= 77 T=
   s" j and i around a call with both levels live" T-LABEL
   77 X-J 138 T= 77 T= ;

: SAME2 ( n n n n -- ) {: q:n r:n q2:n r2:n :} q q2 T= r r2 T= ;

\ Every dividend through the word against the call by divisor k.
: DIV-ROW ( [ n -- n n ] n -- ) {: k:n :}
   NDIVS 0 ?do dup i DIVS @ swap execute  i DIVS @ k DREF SAME2 loop
   drop ;

: FOLD-ROW ( n n n n n -- ) {: q:n r:n nq:n nr:n k:n :}
   q r FOLD-X k DREF SAME2  nq nr FOLD-X negate k DREF SAME2 ;

: DIV-CASE ( -- )
   s" / and mod by 1, 7, 65536, 86400, -3 and 10^10 equal the call's" T-LABEL
   [: Q1 ;] 1 DIV-ROW  [: Q7 ;] 7 DIV-ROW  [: Q65536 ;] 65536 DIV-ROW
   [: Q86400 ;] 86400 DIV-ROW  [: QNEG3 ;] -3 DIV-ROW
   [: QWIDE ;] 10000000000 DIV-ROW
   s" a dividend shared with a copy leaves the copy" T-LABEL
   77 -45 QSHARE -3 T= -6 T= -45 T= 77 T=
   s" a dividend in memory, and both operands in memory" T-LABEL
   77 -45 D7 -6 T= 77 T=  77 -45 M7 -3 T= 77 T=  77 -45 7 DIVX -6 T= 77 T=
   s" folded / and mod equal the call's" T-LABEL
   FD1 1 FOLD-ROW  FD7 7 FOLD-ROW
   s" a zero divisor throws E-DIV-ZERO" T-LABEL
   [: 7 QZERO drop ;] E-DIV-ZERO TTHROWSQ
   [: 7 MZERO drop ;] E-DIV-ZERO TTHROWSQ
   [: FDZERO drop ;] E-DIV-ZERO TTHROWSQ ;

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
   CELLS-CASE
   FUSE-CASE
   JOIN-CASE
   LOOP-REG-CASE
   DO-REG-CASE
   IDX-CASE
   DIV-CASE
   FETCH-FORGE-CASE
   IS-CASE
   CALLBACK-CASE
   REFUSAL-CASE
   T-REPORT ;

;package

T0-INSTALL-TEST:RUN
