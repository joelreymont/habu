\ native-tail.f - production tail calls with live names and wide results.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/prelude.f
require lib/adt/option.f
require src/compiler/native/dict.f
require src/compiler/native/compiler.f
require tools/codegen-tail-probe.f

package NTL-FIXTURE

public

\ ---- the callees -------------------------------------------------------------
\ A guard: it takes a value and answers nothing, which is what makes the name
\ that was handed to it cross the call while still being live afterwards.
: NTL-GUARD ( n -- )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor drop ;

\ A guard over two values, for the wide rows.
: NTL-GUARD2 ( n n -- )
   NTL-GUARD NTL-GUARD ;

\ The word in the middle: it answers a value the tail call then takes.
: NTL-RAW ( n -- n )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor ;

\ The tail callee of the narrow rows.
: NTL-ROLE ( n -- n )
   dup 17 * over 19 xor + swap 23 and + dup 29 * + 31 xor ;

\ The tail callee whose arity is NOT the caller's, which is what makes NTL-NOTAIL
\ an ordinary call rather than a branch.
: NTL-ROLE2 ( n n -- n )
   swap NTL-ROLE swap NTL-ROLE 37 * + ;

\ The tail callee of the five-wide rows, each value scaled by its own odd factor
\ so an exchange is a different answer.
: NTL-MAKE5 ( n n n n n -- n n n n n )
   {: a:n b:n c:n d:n e:n :}
   a 3 * b 5 * c 7 * d 11 * e 13 * ;

\ And of the ten-wide row.
: NTL-MAKE10 ( n n n n n n n n n n -- n n n n n n n n n n )
   {: a:n b:n c:n d:n e:n f:n g:n h:n j:n k:n :}
   a 3 * b 5 * c 7 * d 11 * e 13 * f 17 * g 19 * h 23 * j 29 * k 31 * ;

\ The tail callee that answers a double, which is the typed-result axis: the
\ result convention is the same cell of the caller's stack either way, and the
\ file it is read out of afterwards is the CALLER's business.
: NTL-REAL ( n -- r )
   dup 3 * over 5 xor + swap 7 and + dup 11 * + 13 xor
   s>f 1.5 f* 0.25 f+ ;

\ The same in the other direction: a double crosses the guard as the cell it is.
: NTL-SCALE ( r -- r )
   1.5 f* 0.25 f+ 2.0 f* 0.5 f- ;

\ A word that executes what it is handed, which is how a body makes a quotation
\ whose own last operation is a call.
: NTL-TAKE ( [ n -- n ] n -- n )
   swap execute ;

\ A callback parameter adds no sibling function and still permits a tail call.
: NTL-CALLBACK-ID ( [ -- n ] -- [ -- n ] ) ;
: NTL-CALLBACK-FORWARD ( [ -- n ] -- [ -- n ] ) NTL-CALLBACK-ID ;

\ This parent and the literal callback share a returning module contract.
: NTL-QUOT ( n -- n )
   dup [: NTL-ROLE ;] swap NTL-TAKE drop NTL-ROLE ;

\ ---- the bodies --------------------------------------------------------------
\ lib/byte-buffer.f LEN@ reduced to its shape: a local, a guard the local
\ crosses, and a tail call whose argument is what the middle word answered. The
\ local is dead at the tail call and the dialect hands it over anyway.
: NTL-LEN ( n -- n ) {: v:n :}
   v NTL-GUARD  v NTL-RAW NTL-ROLE ;

\ lib/map.f MAP-CELLS reduced: the local is the tail call's own argument, so the
\ value handed over and the value published are the same value.
: NTL-SELF ( n -- n ) {: v:n :}
   v NTL-GUARD  v NTL-ROLE ;

\ The adversarial twin: the same body with a two-argument callee, which is not
\ this routine's arity, so it is an ordinary call and the routine returns.
: NTL-NOTAIL ( n -- n ) {: v:n :}
   v NTL-GUARD  v NTL-RAW v NTL-ROLE2 ;

\ src/compiler/target.f CONTRACT and src/compiler/numeric-policy.f POLICY reduced:
\ five locals, a guard two of them cross, and a tail call taking all five.
: NTL-WIDE ( n n n n n -- n n n n n ) {: a:n b:n c:n d:n e:n :}
   c d NTL-GUARD2
   a b c d e NTL-MAKE5 ;

\ src/compiler/binding.f BIND reduced: ten values, which is what two five-cell
\ records taken apart come to.
: NTL-TEN ( n n n n n n n n n n -- n n n n n n n n n n )
   {: a:n b:n c:n d:n e:n f:n g:n h:n j:n k:n :}
   a k NTL-GUARD2
   a b c d e f g h j k NTL-MAKE10 ;

\ The typed-result axis, both ways round.
: NTL-FRESULT ( n -- r ) {: v:n :}
   v NTL-GUARD  v NTL-REAL ;

: NTL-FARG ( r -- r ) {: v:r :}
   7 NTL-GUARD  v NTL-SCALE ;

\ ---- and the site beside a wide instantiation ---------------------------------
\ WHAT A CONSTRUCTION PUTS IN A BODY THAT ALSO LEAVES THROUGH A CALL. A parametric
\ family instantiated with a multi-cell argument reserves more room than its
\ declaration does, so the compiler pushes the missing zero cells at the CALL SITE,
\ in front of the constructor's own call (src/compiler/native/elaborate.f
\ CON-PADS-PUSH). Those cells are ordinary vector values that the call it stands
\ in front of consumes, so nothing of them survives it - but they arrive at a
\ call site, which is this suite's subject, and the two features have to be
\ measured together rather than assumed disjoint.
\
\ A PADDED CONSTRUCTION CANNOT ITSELF BE THE SITE A ROUTINE LEAVES THROUGH, and
\ that is a structural fact rather than a hope. The pads make the definition leave
\ MORE cells than the constructor declares, and the elaborator calls a body a tail
\ caller only when the callee's declared output IS the definition's own - so a
\ padded construction as the last call is an ordinary call and the routine
\ returns. `( n -- option<pt> ) drop OPTION:NONE` compiles that way (measured).
\ The two shapes that CAN be written are below: the tail over a wide bundle, and
\ the padded construction whose bundle feeds an ordinary last call.
\
\ AND THE CHECKER IS WHY THERE IS NO THIRD. A construction resolves its
\ instantiation from the definition's declared OUTPUT, so a body that constructs
\ must declare that bundle; a tail caller must declare the same width in as out;
\ and a bundle cannot be taken apart into cells without a dispatch, which is
\ control flow no tail caller has yet. `( n n n -- n n n )` holding a construction
\ is refused by the checker before the chain sees it (`expected: a actual: pt<>`,
\ measured), which is the honest reason this file has two rows here and not three.
PRODUCT pt 0
   FIELD x n
   FIELD y n
;PRODUCT

\ A maker for the wide-bundle rows' input, and its reader. What is under test is
\ the routine between them.
: NTL-MKO ( n -- option<pt> )
   dup 0 > if  dup 3 *  swap 5 *  NTL--FIXTURE-PT:MAKE OPTION:SOME
         else  drop OPTION:NONE  then ;

: NTL-RD ( option<pt> -- n )
   MATCH option
      none OF 0 ENDOF
      some OF NTL--FIXTURE-PT:UNMAKE 7 * swap 11 * + ENDOF
   ;MATCH ;

\ The tail callee over the wide bundle: three cells in, three out, which is what
\ lets the routine below leave through it.
: NTL-BUMP ( option<pt> -- option<pt> )
   MATCH option
      none OF OPTION:NONE ENDOF
      some OF NTL--FIXTURE-PT:UNMAKE swap 3 * swap 5 * NTL--FIXTURE-PT:MAKE OPTION:SOME ENDOF
   ;MATCH ;

\ A tail site whose cells are a BUNDLE and not three unrelated values.
: NTL-WBUNDLE ( option<pt> -- option<pt> )
   NTL-BUMP ;

\ And the padded construction beside one: both arms build a value of the wide
\ instantiation, and the last call is over the bundle they built. Its arity is not
\ its callee's, so it returns - which is what the shape row asserts.
: NTL-PADDED ( n -- option<pt> ) {: v:n :}
   v NTL-GUARD
   v 0 > if  v 3 *  v 5 *  NTL--FIXTURE-PT:MAKE OPTION:SOME  else  OPTION:NONE  then
   NTL-BUMP ;

;package

package NTL-TEST

private

\ ---- what the published code says --------------------------------------------
: BRANCHES? ( ptr u8 n -- bool )
   NTAILPROBE:TAIL-BRANCH? ;

: RETURNS? ( ptr u8 n -- bool )
   NTAILPROBE:TRAILER-RET? ;

: CALLS ( ptr u8 n -- n )
   NTAILPROBE:CALLS ;

\ ---- the cases ---------------------------------------------------------------
: NARROW-CASE ( -- )
   s" narrow tail callers publish their result" T-LABEL
   0 NTL-FIXTURE:NTL-LEN 26503 T=
   0 NTL-FIXTURE:NTL-SELF 549 T= ;

: NOTAIL-CASE ( -- )
   s" a mismatched callee arity returns normally" T-LABEL
   0 NTL-FIXTURE:NTL-NOTAIL 46816 T= ;

: WIDE-CASE ( -- )
   s" wide tail callers preserve every result cell" T-LABEL
   2 3 5 7 11 NTL-FIXTURE:NTL-WIDE
   143 T= 77 T= 35 T= 15 T= 6 T=
   1 2 3 4 5 6 7 8 9 10 NTL-FIXTURE:NTL-TEN
   310 T= 261 T= 184 T= 133 T= 102 T=
   65 T= 44 T= 21 T= 10 T= 3 T= ;

: REAL-CASE ( -- )
   s" a tail caller publishes a double result" T-LABEL
   0 NTL-FIXTURE:NTL-FRESULT 73.75 f= TTRUE
   -1 NTL-FIXTURE:NTL-FRESULT -40.25 f= TTRUE
   0.5 NTL-FIXTURE:NTL-FARG 1.5 f= TTRUE
   -0.5 NTL-FIXTURE:NTL-FARG -1.5 f= TTRUE ;

\ THE TWO ROWS BESIDE A WIDE INSTANTIATION. The first is a tail site whose cells
\ are one BUNDLE - three cells that are one value, with the glue a construction
\ puts on them - and it is where the site's arity is not one. The second holds a
\ padded construction, whose zero cells the compiler pushes at the constructor's own
\ call site: its last call is over the bundle those cells helped build, its arity
\ is not that callee's, and it returns. Both answer through the ordinary maker
\ and reader, so a lost or misplaced pad is a wrong number here.
: BUNDLE-CASE ( -- )
   s" a tail site over a wide bundle preserves the bundle" T-LABEL
   1 NTL-FIXTURE:NTL-MKO NTL-FIXTURE:NTL-WBUNDLE NTL-FIXTURE:NTL-RD 274 T=
   s" and a padded construction in a body whose last call is over the bundle" T-LABEL
   0 NTL-FIXTURE:NTL-PADDED NTL-FIXTURE:NTL-RD 0 T=
   1 NTL-FIXTURE:NTL-PADDED NTL-FIXTURE:NTL-RD 274 T= ;

\ THE SHAPE, WHICH IS WHAT SAYS THE OPTIMISATION IS REALLY THERE. NTL-LEN holds
\ two calls - the guard and the middle word - and leaves through the third, so
\ its emitted code has exactly two branch-with-links and ends in a plain branch
\ with no return behind it. NTL-NOTAIL is the same body with the last callee's
\ arity changed: three calls, and it returns.
: SHAPE-CASE ( -- )
   s" the routine really leaves by a branch and keeps its other calls" T-LABEL
   s" NTL-FIXTURE:NTL-LEN" BRANCHES? TTRUE
   s" NTL-FIXTURE:NTL-LEN" RETURNS? TFALSE
   s" NTL-FIXTURE:NTL-LEN" CALLS 2 T=
   s" NTL-FIXTURE:NTL-SELF" BRANCHES? TTRUE
   s" NTL-FIXTURE:NTL-SELF" CALLS 1 T=
   s" NTL-FIXTURE:NTL-WIDE" BRANCHES? TTRUE
   s" NTL-FIXTURE:NTL-TEN" BRANCHES? TTRUE
   s" NTL-FIXTURE:NTL-FRESULT" BRANCHES? TTRUE
   s" NTL-FIXTURE:NTL-FARG" BRANCHES? TTRUE
   s" NTL-FIXTURE:NTL-NOTAIL" BRANCHES? TFALSE
   s" NTL-FIXTURE:NTL-NOTAIL" RETURNS? TTRUE
   s" NTL-FIXTURE:NTL-NOTAIL" CALLS 3 T=

   s" the wide-bundle site is the whole routine, and the padded one is not" T-LABEL
   s" NTL-FIXTURE:NTL-WBUNDLE" BRANCHES? TTRUE
   s" NTL-FIXTURE:NTL-WBUNDLE" RETURNS? TFALSE
   s" NTL-FIXTURE:NTL-WBUNDLE" CALLS 0 T=
   s" NTL-FIXTURE:NTL-PADDED" BRANCHES? TFALSE
   s" NTL-FIXTURE:NTL-PADDED" RETURNS? TTRUE ;

: QUOT-CASE ( -- )
   s" a quotation's last call is not the enclosing emission's tail site" T-LABEL
   0 NTL-FIXTURE:NTL-QUOT 549 T=
   [: 17 ;] NTL-FIXTURE:NTL-CALLBACK-FORWARD execute 17 T=
   s" NTL-FIXTURE:NTL-CALLBACK-FORWARD" BRANCHES? TTRUE
   s" NTL-FIXTURE:NTL-CALLBACK-FORWARD" RETURNS? TFALSE ;

public

: RUN ( -- )
   T-RESET
   NARROW-CASE
   NOTAIL-CASE
   WIDE-CASE
   REAL-CASE
   BUNDLE-CASE
   SHAPE-CASE
   QUOT-CASE
   T-REPORT ;

;package

NTL-TEST:RUN
