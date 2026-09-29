\ outer.f - the outer interpreter's readers, written in Habu: numbers, then
\ the dictionary search.

require lib/prelude.f
require lib/ieee754.f
require src/habu/layout.f
require src/habu/xref.f
require src/compiler/native/dict.f

\ ---- numbers ----------------------------------------------------------------
\ NUMBER reads a token as the engine's reader does (LNUM, EMIT-NUM in
\ habu1.f): an optional `-`, an optional `$` for radix 16 with digits a-f and
\ A-F, then digits. A radix-10 token may end in one `.` and one or more
\ digits, which makes it a float; its value is then the double's bits.
\
\ A token of that shape can still be out of range, and the interpreter then
\ reports it undefined without looking it up as a name. Its digits may not
\ carry past 2^64 - 1. A decimal integer's magnitude is at most MAX-N, or 2^63
\ when negative; a hex integer keeps all 64 bits and negates modulo 2^64. A
\ float's integer part is at most MAX-N and its fraction at most 18 digits.

package OUTER
private

$7FFFFFFFFFFFFFFF constant MAX-N
$8000000000000000 constant MIN-N


\ Unsigned order: flipping the sign bit maps it onto signed order.
: U> ( n n -- bool )
   MIN-N xor swap MIN-N xor < ;


: BETWEEN? ( n n n -- bool ) {: c:n lo:n hi:n :}
   c lo >= c hi <= and ;


\ The value of byte c as a digit of radix, and whether it is one.
: DIGIT ( n n -- n bool ) {: c:n radix:n :}
   c [char] 0 [char] 9 BETWEEN? if c [char] 0 - true exit then
   radix 16 <> if 0 false exit then
   c [char] a [char] f BETWEEN? if c [char] a - 10 + true exit then
   c [char] A [char] F BETWEEN? if c [char] A - 10 + true exit then
   0 false ;


\ Whether the byte at idx is c.
: AT? ( ptr u8 n n n -- bool ) {: a:ptr u:n idx:n c:n :}
   idx u < if a idx + c@ c = else false then ;


\ The index past the run of radix digits that starts at idx.
: RUN-END ( ptr u8 n n n -- n ) {: a:ptr u:n idx:n radix:n :}
   idx begin
      dup u < if dup a + c@ radix DIGIT nip else false then
   while 1+ repeat ;


\ 2^64 - 1 is limit * radix + top, so int * radix + d carries out of the
\ cell when int is past limit, or at limit with d past top.
: CARRY-BOUND ( n -- n n )
   16 = if $0FFFFFFFFFFFFFFF 15 else 1844674407370955161 5 then ;


: CARRIES? ( n n n -- bool ) {: int:n d:n radix:n :}
   radix CARRY-BOUND {: limit:n top:n :}
   int limit U>  int limit = d top > and  or ;


\ A digit takes int to int * radix + d modulo 2^64; wrapped latches the first
\ carry out of the cell.
: INT-STEP ( n bool n n -- n bool ) {: int:n wrapped:bool d:n radix:n :}
   int radix * d +
   int d radix CARRIES? wrapped or ;


: INTEGER ( ptr u8 n n n -- n bool ) {: a:ptr lo:n hi:n radix:n :}
   0 false
   hi lo ?do a i + c@ radix DIGIT drop radix INT-STEP loop ;


\ A fraction digit takes frac to frac * 10 + d and scale to scale * 10. The
\ fraction stays below the scale, so bounding the scale by MAX-N / 10 (18
\ digits) keeps both in a signed cell; past it the latch refuses the token.
: FRAC-STEP ( n n bool n -- n n bool ) {: frac:n scale:n wrapped:bool d:n :}
   frac 10 * d +
   scale 10 *
   scale MAX-N 10 / > wrapped or ;


: FRACTION ( ptr u8 n n bool -- n n bool ) {: a:ptr lo:n hi:n wrapped:bool :}
   0 1 wrapped
   hi lo ?do a i + c@ 10 DIGIT drop FRAC-STEP loop ;


\ Whether the token from dot on is a float's fraction: a radix-10 `.` and one
\ or more digits that run to the end.
: FRACTION? ( ptr u8 n n n -- bool ) {: a:ptr u:n dot:n radix:n :}
   radix 10 <> if false exit then
   a u dot [char] . AT? 0= if false exit then
   dot 1+ u <  a u dot 1+ 10 RUN-END u =  and ;


: NOT-NUMBER ( -- n bool bool bool )
   0 false false false ;


: OUT-OF-RANGE ( -- n bool bool bool )
   0 false false true ;


\ A carry, or a decimal magnitude past MAX-N (2^63 when negative).
: INT-OUT? ( n bool bool n -- bool ) {: int:n wrapped:bool neg:bool radix:n :}
   wrapped if true exit then
   radix 16 = if false exit then
   neg if int MIN-N U> else int MAX-N U> then ;


: INT-FINISH ( n bool bool n -- n bool bool bool ) {: int:n wrapped:bool neg:bool radix:n :}
   int wrapped neg radix INT-OUT? if OUT-OF-RANGE exit then
   int neg if negate then false true false ;


\ The integer part converts as a signed cell, so it is at most MAX-N. The
\ value is int + frac / scale, negated last, in the engine's order.
: FLOAT-FINISH ( n n n bool bool -- n bool bool bool )
   {: int:n frac:n scale:n wrapped:bool neg:bool :}
   wrapped int MAX-N U> or if OUT-OF-RANGE exit then
   int s>f  frac s>f scale s>f f/  f+
   neg if fnegate then
   IEEE754:F64>BITS true true false ;


\ An optional `-`, then an optional `$` for radix 16: the index after them,
\ whether `-` led, and the radix.
: PREFIX ( ptr u8 n -- n bool n ) {: a:ptr u:n :}
   a u 0 [char] - AT? {: neg:bool :}
   neg if 1 else 0 then {: idx:n :}
   a u idx [char] $ AT? if idx 1+ neg 16 else idx neg 10 then ;

public

\ The token's value (a float's bits), whether it is a float, whether it is a
\ number, and whether its shape is a number's but its value out of range.
\ The value and the float flag are zero unless it is a number.
: NUMBER ( ptr u8 n -- n bool bool bool ) {: a:ptr u:n :}
   a u PREFIX {: lo:n neg:bool radix:n :}
   lo u >= if NOT-NUMBER exit then
   a u lo radix RUN-END {: dot:n :}
   a lo dot radix INTEGER {: int:n wrapped:bool :}
   dot u = if int wrapped neg radix INT-FINISH exit then
   a u dot radix FRACTION? 0= if NOT-NUMBER exit then
   int  a dot 1+ u wrapped FRACTION  neg FLOAT-FINISH ;

;package

\ ---- dictionary search ------------------------------------------------------
\ OUTER:FIND answers the record the engine's LFIND and LFINDUSED resolve for one
\ token (habu1.f EMIT-FIND, habu2.f EMIT-FIND-USED), or XREF-NULL. The caller
\ reads the immediate, wide, internal and min-in facts from the record's
\ XREF-FLAGS, as LFIND's flag output folds them.
\
\ The order. A bare token is asked of the open package's private wordlist, then
\ its public one, then the global one; with no package open, of the global one
\ alone. Only when that chain misses, and only for a token with no colon at all,
\ is every live used public asked: two distinct records there are
\ E-USING-AMBIGUOUS, and one package used twice is one record. NAME:tail asks
\ NAME's public wordlist, found through its namespace row.
\
\ Each wordlist is asked through xref-search-wl (habu1.f WLFIND), which walks the
\ dictionary's hash index under the key LFIND's own probe uses, so one index
\ answers both. The search stops at the first wordlist holding the name and
\ returns that row whatever its flags. `search-wl` would not do: it hides
\ DNAME-INT rows and the engine-helper wordlist, which the interpreter must see
\ in order to refuse them.
\
\ NDICT (src/compiler/native/dict.f) answers a different question: which record
\ a checked program may call. It skips a row the caller may not see and keeps
\ searching, so its chain is not this one; only its open-scope readers are.

package OUTER

private

\ ---- one wordlist -------------------------------------------------------------
\ The record, not an xt: one body can carry several names (EXPORT), so only the
\ record says which wordlist the name resolved in. Trusted-only by the
\ primitive's own row (prims.f).
TRUSTED: FIND-PROBE ( ptr u8 n n -- ptr n ) xref-search-wl ;

\ ---- the token's shape (habu1.f FIND-QSCAN to FIND-QTAILOK) ------------------
-1 constant FIND-BARE
-2 constant FIND-BAD

\ The index of the first colon at or after `from`, or -1.
: FIND-COLON ( ptr u8 n n -- n )
   {: a:ptr u:n from:n :}
   u from ?do
      a i + c@ $3A = if i unloop exit then
   loop
   -1 ;

\ The qualifier is the token's FIRST colon. One at either edge leaves the whole
\ token a bare name, whatever follows it: `:a:b` is bare. A second colon after
\ the qualifier misses. xref.f XREF-QUAL-INDEX calls `:a:b` malformed, so it is
\ not this reader.
: FIND-SPLIT ( ptr u8 n -- n )
   {: a:ptr u:n :}
   a u 0 FIND-COLON {: q:n :}
   q 1 < if FIND-BARE exit then
   q 1+ u >= if FIND-BARE exit then
   a u q 1+ FIND-COLON 0 >= if FIND-BAD exit then
   q ;

\ ---- the open package, then the global wordlist (habu1.f FIND-DONE) ----------
\ A miss in the open package's private wordlist retries its public one, a miss
\ there retries the global one, and any other miss is final. The retry reads
\ the wordlist that missed, not the one the search began in, so P:tail while P
\ is open falls through to a global tail P does not export.
: FIND-OPEN ( ptr u8 n n -- ptr n )
   {: a:ptr u:n wid:n :}
   a u wid FIND-PROBE {: rec:ptr :}
   rec XREF-FOUND? if rec exit then
   NDICT:OPEN-PRI 0= if XREF-NULL exit then
   wid NDICT:OPEN-PRI = if a u NDICT:OPEN-PUB RECURSE exit then
   wid NDICT:OPEN-PUB = if a u 0 RECURSE exit then
   XREF-NULL ;

\ ---- the used publics (habu2.f EMIT-FIND-USED) --------------------------------
: FIND-USE-DEPTH ( -- n )
   data-base USE-DEPTH-CELL + @ ;

: FIND-USE-WID ( n -- n )
   cells data-base USE-WIDS-OFF + + @ ;

\ Every used public is asked, and the answers must agree on one record.
: FIND-USED ( ptr u8 n -- ptr n )
   {: a:ptr u:n :}
   a u 0 FIND-COLON 0 >= if XREF-NULL exit then
   XREF-NULL
   FIND-USE-DEPTH 0 ?do
      a u i FIND-USE-WID FIND-PROBE
      dup XREF-FOUND? if
         over XREF-FOUND? if
            2dup <> if E-USING-AMBIGUOUS throw then
         then
         nip
      else
         drop
      then
   loop ;

: FIND-BARE-REC ( ptr u8 n -- ptr n )
   {: a:ptr u:n :}
   a u NDICT:OPEN-PRI FIND-OPEN {: rec:ptr :}
   rec XREF-FOUND? if rec exit then
   a u FIND-USED ;

\ The namespace row carries its package's public wid where a word keeps its start.
: FIND-QUALIFIED ( ptr u8 n n -- ptr n )
   {: a:ptr u:n q:n :}
   a q XREF-NAMESPACE-WL FIND-PROBE {: ns:ptr :}
   ns XREF-FOUND? 0= if XREF-NULL exit then
   a q 1+ ZPTR+ u q - 1- ns XREF-PKG-PUBLIC FIND-OPEN ;

public

: FIND ( ptr u8 n -- ptr n )
   {: a:ptr u:n :}
   a u FIND-SPLIT {: q:n :}
   q FIND-BAD = if XREF-NULL exit then
   q FIND-BARE = if a u FIND-BARE-REC exit then
   a u q FIND-QUALIFIED ;

;package
