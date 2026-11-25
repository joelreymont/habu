\ relax.f - branch growing, package RELAX.
\
\ A body is emitted with each branch in its short form, a hole, and placed
\ with insertions, each a raw offset and a length, in raw order. FIXED+ inserts
\ a run of the one fixed length SIZE is given. BRANCH+ inserts, after a
\ branch's hole, the bytes the branch's final form has beyond it.
\
\ A placed offset is the raw one plus the insertions before it, found by binary
\ search over their prefix sums. At one offset a branch's insertion comes
\ before a fixed one. An instruction follows both (PLACED); a label follows the
\ branch's and lands on the fixed one (LANDED). A longer branch can move
\ another out of its form's reach, so SIZE asks for every length until none
\ grows, and a length never shrinks.
\
\ RESET starts a body; BRANCH+, TO! and FIXED+ file it while it is emitted;
\ SIZE sizes it, after which PLACED, LANDED and COPY read it.

require src/lib/vector.f

package RELAX
public

DYNAMIC-BUFFER BR-AT n                \ the branches: raw first byte, raw target
DYNAMIC-BUFFER BR-TO n                \ or -1 while open, and the bytes beyond
DYNAMIC-BUFFER BR-LEN n               \ the hole

private

variable NBR
variable FIXED-LEN
variable NINS                         \ the insertions, in raw order:
DYNAMIC-BUFFER INS-AT n               \ raw offset, branch or -1 for a fixed one,
DYNAMIC-BUFFER INS-BR n               \ and the lengths of those before each, with
DYNAMIC-BUFFER INS-SUM n              \ all of them last

: INS+ ( n n -- ) {: at:n br:n :}
   NINS @ {: k:n :}
   k 1 + INS-AT-RESERVE  k 1 + INS-BR-RESERVE
   at k INS-AT !  br k INS-BR !
   k 1 + NINS ! ;

: INS-LEN ( n -- n ) INS-BR @ {: br:n :}
   br 0 < if FIXED-LEN @ else br BR-LEN @ then ;

\ Orders the insertions: by offset, a branch's before a fixed one.
: INS-KEY ( n -- n ) {: k:n :}
   k INS-AT @ 2 *  k INS-BR @ 0 < if 1 + then ;

\ How many insertions have a key below n.
: INS-BELOW ( n -- n ) {: key:n :}
   0 NINS @
   begin 2dup < while
      2dup + 1 rshift {: mid:n :}
      mid INS-KEY key < if nip mid 1 + swap else drop mid then
   repeat drop ;

: SUMS ( -- )
   NINS @ {: n:n :}
   n 1 + INS-SUM-RESERVE
   0 n 0 ?do dup i INS-SUM !  i INS-LEN + loop
   n INS-SUM ! ;

public

: RESET ( -- ) 0 NINS !  0 NBR ! ;

\ A branch from raw offset at to raw offset to, -1 until TO!, whose hole of
\ hole bytes the emitter appends; answers its number.
: BRANCH+ ( n n n -- n ) {: at:n to:n hole:n :}
   NBR @ {: k:n :}
   k 1 + BR-AT-RESERVE  k 1 + BR-TO-RESERVE  k 1 + BR-LEN-RESERVE
   at k BR-AT !  to k BR-TO !  0 k BR-LEN !
   k 1 + NBR !
   at hole + k INS+
   k ;

: TO! ( n n -- ) BR-TO ! ;

: FIXED+ ( n -- ) -1 INS+ ;

: BRANCHES ( -- n ) NBR @ ;

\ Where the instruction at raw offset x lands, and where a branch to x lands,
\ from the body's start.
: PLACED ( n -- n ) {: x:n :}
   x  x 1 + 2 * INS-BELOW INS-SUM @ + ;

: LANDED ( n -- n ) {: x:n :}
   x  x 2 * 1 + INS-BELOW INS-SUM @ + ;

private

: BR-DIST ( n -- n ) {: k:n :}
   k BR-TO @ LANDED  k BR-AT @ PLACED - ;

: GROW ( [ n n -- n ] n -- bool ) {: k:n :}
   k  k BR-DIST  rot execute {: len:n :}
   len k BR-LEN @ > dup if len k BR-LEN ! then ;

: GROWN? ( [ n n -- n ] -- bool )
   false NBR @ 0 ?do over i GROW or loop nip ;

public

\ The fixed insertions' length, and the rule ( k d -- n ) giving the bytes
\ branch k takes beyond its hole, d the distance from its first byte to its
\ target.
: SIZE ( n [ n n -- n ] -- )
   swap FIXED-LEN !
   begin SUMS dup GROWN? 0= until drop ;

private

\ The source's bytes from raw offset from to raw offset to.
: PIECE ( ^VEC:vector<u8> ^VEC:vector<u8> n n -- ) {: src:^ dst:^ from:n to:n :}
   src VEC:BYTES str> drop from +  to from -  >str dst VEC:APPEND ;

\ The emitter writes a grown branch over its hole and its insertion.
: INSERT ( ^VEC:vector<u8> str n -- ) {: dst:^ fixed:str k:n :}
   k INS-BR @ 0 < if fixed dst VEC:APPEND exit then
   dst  dst VEC:COUNT k INS-LEN +  VEC:ENSURE ;

public

\ The source body onto the destination, fixed's bytes at each fixed insertion.
: COPY ( ^VEC:vector<u8> ^VEC:vector<u8> str -- ) {: src:^ dst:^ fixed:str :}
   0 NINS @ 0 ?do
      i INS-LEN 0 <> if
         src dst rot i INS-AT @ PIECE  dst fixed i INSERT  i INS-AT @
      then
   loop
   src dst rot src VEC:COUNT PIECE ;

;package
