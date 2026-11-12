\ equal.f - VALUE=, whether two JSON texts hold the same value. A file of
\ package JSON (src/lib/json.f).
\
\ Two numbers are equal when their values are, decided on the lexemes the
\ reader admitted, with no conversion to binary64: a nonzero value is the sign,
\ the significant digits d1..dn (the integer and fraction digits from the first
\ nonzero through the last nonzero) and the point exponent x of
\ 0.d1..dn * 10^x. Every zero has no significant digits and equals every other.
\
\ VALUE= compares text a with text b in two passes: an index pass reads b once
\ into rows; the walk then reads a once against them, and reads again only each
\ string of b it compares.

require src/lib/memory.f
require src/lib/span.f
require src/lib/string.f
require src/lib/json/read.f

package JSON

private

: NUMERIC? ( n -- bool )
   dup T-INT = swap T-FLOAT = or ;


\ ---- numbers by value -----------------------------------------------------

MEM-MAX-N negate 1- constant LEAST-N
MEM-MAX-N 10 / constant EXP-CUT              \ m * 10 stays a cell for m >= -EXP-CUT
MEM-MAX-N 10 mod 1+ constant EXP-LAST        \ the largest digit after m = -EXP-CUT

: EXP-MARK? ( n -- bool )
   dup EXP-LOWER = swap EXP-UPPER = or ;

: SIGNIFICANT? ( n -- bool )
   dup STR-DIGIT? swap ZERO <> and ;


\ The first offset below end whose byte answers true to test, or end.
: SCAN-TO ( ptr u8 n [ n -- bool ] -- n )
   {: p:ptr end:n test :}
   0 begin dup end < if dup p + c@ test execute 0= else false then
   while 1+ repeat ;

\ Offset of the exponent marker, or the length without one.
: MANTISSA-END ( ptr u8 n -- n )
   [: EXP-MARK? ;] SCAN-TO ;

\ Offset of the first significant digit, or the mantissa's end for a zero.
: FIRST-SIG ( ptr u8 n -- n )
   {: p:ptr u:n :}
   p  p u MANTISSA-END  [: SIGNIFICANT? ;] SCAN-TO ;

\ Offset of the decimal point, or the mantissa's end without one.
: POINT-AT ( ptr u8 n -- n )
   {: p:ptr u:n :}
   p  p u MANTISSA-END  [: DOT = ;] SCAN-TO ;

: ZERO-NUMBER? ( ptr u8 n -- bool )
   {: p:ptr u:n :}
   p u FIRST-SIG  p u MANTISSA-END = ;

\ Offset of the last significant digit of a nonzero lexeme.
: LAST-SIG ( ptr u8 n -- n )
   {: p:ptr u:n :}
   p u MANTISSA-END 1-
   begin dup p + c@ SIGNIFICANT? 0= while 1- repeat ;


\ Digit d onto m, the negated exponent so far: one past LEAST-N is
\ E-JSON-READ-NUMBER, decided before the product could wrap.
: EXP-DIGIT ( n n -- n )
   {: m:n d:n :}
   m EXP-CUT negate < if E-JSON-READ-NUMBER throw then
   m EXP-CUT negate =  d EXP-LAST >  and if E-JSON-READ-NUMBER throw then
   m 10 * d - ;


\ The exponent the lexeme states, 0 without one; one a signed cell cannot hold
\ is E-JSON-READ-NUMBER.
: STATED-EXP ( ptr u8 n -- n )
   {: p:ptr u:n :}
   p u MANTISSA-END 1+ {: at:n :}
   at u > if 0 exit then
   p at + c@ {: sign:n :}
   sign PLUS =  sign MINUS =  or if at 1+ else at then {: from:n :}
   0 u from ?do p i + c@ ZERO - EXP-DIGIT loop
   sign MINUS = if exit then
   dup LEAST-N = if E-JSON-READ-NUMBER throw then
   negate ;


: EXP+ ( n n -- n )
   {: x:n e:n :}
   e 0 > if x MEM-MAX-N e - > if E-JSON-READ-NUMBER throw then then
   e 0 < if x LEAST-N e - < if E-JSON-READ-NUMBER throw then then
   x e + ;


\ The point exponent x of a nonzero lexeme, 0 for a zero, either after
\ STATED-EXP. With the first significant digit at offset f and the point at
\ offset dot, the digits read as 0.d1..dn * 10^(dot - f) when f is before the
\ point and as 0.d1..dn * 10^(dot - f + 1) after it; the stated exponent adds.
: POINT-EXP ( ptr u8 n -- n )
   {: p:ptr u:n :}
   p u STATED-EXP {: e:n :}
   p u ZERO-NUMBER? if 0 exit then
   p u POINT-AT  p u FIRST-SIG {: dot:n f:n :}
   dot f -  f dot > if 1+ then  e EXP+ ;


\ How many significant digits a nonzero lexeme has, the point not counted.
: DIGIT-COUNT ( ptr u8 n -- n )
   {: p:ptr u:n :}
   p u FIRST-SIG  p u LAST-SIG  p u POINT-AT {: f:n l:n dot:n :}
   l f - 1+  f dot <  dot l <  and if 1- then ;

\ The offset of the digit after the one at offset i, below the last.
: NEXT-DIGIT ( ptr u8 n -- n )
   {: p:ptr i:n :}
   i 1+ dup p + c@ DOT = if 1+ then ;


\ Whether two nonzero lexemes have the same significant digits, byte for byte.
: SAME-DIGITS? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n b:ptr v:n :}
   a u DIGIT-COUNT {: count:n :}
   b v DIGIT-COUNT count <> if false exit then
   a u FIRST-SIG  b v FIRST-SIG
   count 0 ?do
      over a + c@  over b + c@  <> if 2drop false unloop exit then
      i count 1- < if swap a swap NEXT-DIGIT swap b swap NEXT-DIGIT then
   loop
   2drop true ;


: NEGATIVE? ( ptr u8 -- bool )
   c@ MINUS = ;


\ An exponent past a signed cell in either throws, as POINT-EXP does.
: NUMBER= ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n b:ptr v:n :}
   a u POINT-EXP  b v POINT-EXP {: xa:n xb:n :}
   a u ZERO-NUMBER?  b v ZERO-NUMBER? {: za:bool zb:bool :}
   za zb and if true exit then
   za zb or if false exit then
   a NEGATIVE?  b NEGATIVE?  xor if false exit then
   xa xb <> if false exit then
   a u b v SAME-DIGITS? ;


\ ---- the walk --------------------------------------------------------------

: SAME-KIND? ( n n -- bool )
   {: ka:n kb:n :}
   ka kb =  ka NUMERIC? kb NUMERIC? and  or ;


\ One value of b: start..start+len its bytes, a string's quotes included; count a container's
\ children; next the first row past its own; parent -1 at the top; first its first member's place.
STRUCTURE row DERIVE addr
   FIELD kind n
   FIELD start n
   FIELD len n
   FIELD count n
   FIELD next n
   FIELD parent n
   FIELD key-at n
   FIELD key-len n
   FIELD first n
   FIELD matched bool
;STRUCTURE

\ One container open on both sides, kind T-ARR or T-OBJ, against b's row; due
\ is the row an array's next element compares with, count the children
\ matched so far.
STRUCTURE frame DERIVE addr
   FIELD kind n
   FIELD row n
   FIELD due n
   FIELD count n
;STRUCTURE

\ VALUE='s scratch, one mapping laid out by WALK-INIT. A fresh mapping reads 0:
\ every count starts at 0, the member list is empty and no record owns a stack.
STRUCTURE walk DERIVE addr
   FIELD a cursor
   FIELD b cursor
   FIELD s cursor
   FIELD frames ptr u8
   FIELD depth n
   FIELD rows ptr u8
   FIELD used n
   FIELD open n
   FIELD members ptr u8
   FIELD spare ptr u8
   FIELD member-n n
   FIELD keys ptr u8
   FIELD key-cap n
   FIELD key-used n
   FIELD key-at n
   FIELD key-len n
   FIELD due n
   FIELD buf ptr u8
   FIELD cap n
;STRUCTURE

CAST: >ROW ( ptr n -- ptr row )
CAST: >FRAME ( ptr n -- ptr frame )
CAST: >WALK ( ptr n -- ptr walk )

\ The walk's states: a value is due under a's cursor and row due; the
\ innermost frame reads on, past an equal value or into one just opened; the
\ two answers.
0 constant W-VALUE
1 constant W-ON
2 constant W-SAME
3 constant W-DIFFERENT

: ROW ( ptr walk n -- ptr row )
   {: w:ptr i:n :}
   w WALK-ROWS @ i ROW-BYTES * + CELL-VIEW >ROW ;

: FRAME ( ptr walk n -- ptr frame )
   {: w:ptr i:n :}
   w WALK-FRAMES @ i FRAME-BYTES * + CELL-VIEW >FRAME ;

: TOP ( ptr walk -- ptr frame )
   {: w:ptr :}
   w  w WALK-DEPTH @ 1-  FRAME ;


\ ---- the scratch's size -----------------------------------------------------

\ Counts byte c into text's opening brackets, commas and colons so far.
: TALLY ( n n n n -- n n n )
   {: opens:n commas:n colons:n c:n :}
   c LBRACE =  c LBRACK =  or if opens 1+ commas colons exit then
   c COMMA = if opens commas 1+ colons exit then
   opens commas  colons  c COLON = if 1+ then ;

\ A byte inside a string counts too, so each count only bounds what it counts.
: PUNCTUATION ( ptr u8 n -- n n n )
   {: p:ptr u:n :}
   0 0 0  u 0 ?do p i + c@ TALLY loop ;


\ Frames, rows and member cells: each frame is a container open in both texts; each value of b
\ but the top and a container's first follows a comma; a member a colon, a cell in each list.
: WALK-COUNTS ( ptr u8 n ptr u8 n -- n n n )
   {: a:ptr u:n b:ptr v:n :}
   a u PUNCTUATION 2drop  b v PUNCTUATION {: ka:n kb:n commas:n colons:n :}
   ka kb min  commas kb + 1+  colons 2 * ;

: WALK-LEN ( n n n n n -- n )
   {: k:n r:n s:n u:n v:n :}
   WALK-BYTES  k FRAME-BYTES * +  r ROW-BYTES * +  s cells +  u +  v + ;


\ Lays the walk over scratch: k frames, r rows, s member cells (the list, then the
\ spare), v key bytes, then u bytes for a's strings.
: WALK-INIT ( SPAN:span<u8> n n n n n -- ptr walk )
   {: scratch k:n r:n s:n u:n v:n :}
   scratch SPAN:$ drop {: base:ptr :}
   base CELL-VIEW >WALK {: w:ptr :}
   base WALK-BYTES + {: frames:ptr :}
   frames k FRAME-BYTES * + {: rows:ptr :}
   rows r ROW-BYTES * + {: list:ptr :}
   list s cells + {: keys:ptr :}
   frames w WALK-FRAMES !
   rows w WALK-ROWS !
   list w WALK-MEMBERS !
   list s 2 / cells + w WALK-SPARE !
   keys w WALK-KEYS !
   v w WALK-KEY-CAP !
   keys v + w WALK-BUF !
   u w WALK-CAP !
   -1 w WALK-OPEN !
   w ;


: WALK-FREE ( SPAN:span<u8> -- )
   {: scratch :}
   scratch SPAN:$ drop CELL-VIEW >WALK {: w:ptr :}
   w WALK-A MORE-RELEASE
   w WALK-B MORE-RELEASE
   w WALK-S MORE-RELEASE
   scratch MEM:FREE-SPAN ;


\ ---- b's members -------------------------------------------------------------

: LIST-CELL ( ptr u8 n -- ptr n )
   cells + CELL-VIEW ;

: MEMBER@ ( ptr walk n -- n )
   {: w:ptr i:n :}
   w WALK-MEMBERS @ i LIST-CELL @ ;

: MEMBER-KEY$ ( ptr walk n -- ptr u8 n )
   {: w:ptr r:n :}
   w r ROW {: row:ptr :}
   w WALK-KEYS @ row ROW-KEY-AT @ +  row ROW-KEY-LEN @ ;


: MEMBER< ( ptr walk n n -- bool )
   {: w:ptr p:n q:n :}
   w p ROW ROW-PARENT @  w q ROW ROW-PARENT @ {: pp:n qp:n :}
   pp qp <> if pp qp < exit then
   w p MEMBER-KEY$  w q MEMBER-KEY$  STR< ;


\ Whether the merge takes place i of the left run before place j of the right: on a
\ tie the left, so equal keys of one object keep document order.
: LEFT-NEXT? ( ptr walk ptr u8 n n n n -- bool )
   {: w:ptr src:ptr i:n j:n mid:n hi:n :}
   i mid >= if false exit then
   j hi >= if true exit then
   w  src j LIST-CELL @  src i LIST-CELL @  MEMBER< 0= ;


: MERGE-STEP ( n n ptr walk ptr u8 ptr u8 n n n -- n n )
   {: i:n j:n w:ptr src:ptr dst:ptr mid:n hi:n k:n :}
   w src i j mid hi LEFT-NEXT? if
      src i LIST-CELL @  dst k LIST-CELL !  i 1+ j exit
   then
   src j LIST-CELL @  dst k LIST-CELL !  i j 1+ ;


: MERGE-RUNS ( ptr walk ptr u8 ptr u8 n n n -- )
   {: w:ptr src:ptr dst:ptr lo:n mid:n hi:n :}
   lo mid
   hi lo ?do w src dst mid hi i MERGE-STEP loop
   2drop ;


: MERGE-PASS ( ptr walk ptr u8 ptr u8 n n -- )
   {: w:ptr src:ptr dst:ptr n:n width:n :}
   n 0 ?do
      w src dst  i  i width + n min  i width 2 * + n min  MERGE-RUNS
   width 2 * +loop ;


: SORT-ROUND ( ptr walk n -- )
   {: w:ptr width:n :}
   w WALK-MEMBERS @  w WALK-SPARE @ {: src:ptr dst:ptr :}
   w src dst  w WALK-MEMBER-N @  width MERGE-PASS
   dst w WALK-MEMBERS !
   src w WALK-SPARE ! ;


\ Stores place i in its member's object row when the member before it has
\ another parent prev; answers its own parent.
: MARK-FIRST ( n ptr walk n -- n )
   {: prev:n w:ptr i:n :}
   w  w i MEMBER@  ROW ROW-PARENT @ {: obj:n :}
   obj prev <> if i  w obj ROW ROW-FIRST ! then
   obj ;


: SORT-MEMBERS ( ptr walk -- )
   {: w:ptr :}
   1 begin dup w WALK-MEMBER-N @ < while w over SORT-ROUND 2 * repeat drop
   -1  w WALK-MEMBER-N @ 0 ?do w i MARK-FIRST loop drop ;


\ Halves lo..hi toward the lowest place whose key is not below key.
: NARROW ( n n ptr walk ptr u8 n -- n n )
   {: lo:n hi:n w:ptr key:ptr len:n :}
   lo hi + 2 / {: mid:n :}
   w  w mid MEMBER@  MEMBER-KEY$  key len STR< if mid 1+ hi exit then
   lo mid ;


\ Object row obj's member whose key decodes to key, or -1. The first of equal keys is found each
\ time, so a key a repeats meets a member already matched and a key b repeats leaves one unmatched.
: FIND-MEMBER ( ptr walk n ptr u8 n -- n )
   {: w:ptr obj:n key:ptr len:n :}
   w obj ROW {: o:ptr :}
   o ROW-FIRST @  dup o ROW-COUNT @ + {: lo:n hi:n :}
   lo hi begin 2dup < while w key len NARROW repeat
   drop {: at:n :}
   at hi = if -1 exit then
   w at MEMBER@ {: q:n :}
   w q MEMBER-KEY$ key len STR= if q else -1 then ;


\ ---- the index pass ----------------------------------------------------------

\ Decodes the key under record b into the key area as the key read last.
: KEEP-KEY ( ptr walk -- )
   {: w:ptr :}
   w WALK-KEY-USED @ {: at:n :}
   w WALK-B  w WALK-KEYS @ at +  w WALK-KEY-CAP @ at -  STR-INNER {: len:n :}
   at w WALK-KEY-AT !
   len w WALK-KEY-LEN !
   at len + w WALK-KEY-USED ! ;


: ADD-MEMBER ( ptr walk n -- )
   {: w:ptr r:n :}
   w WALK-MEMBER-N @ {: at:n :}
   r  w WALK-MEMBERS @ at LIST-CELL !
   at 1+ w WALK-MEMBER-N ! ;


\ Appends a row for the value of kind under record b, a member's when the
\ innermost open row is an object's, and answers its index.
: ADD-ROW ( ptr walk n -- n )
   {: w:ptr kind:n :}
   w WALK-B {: c:ptr :}
   w WALK-USED @ {: r:n :}
   w WALK-OPEN @ {: obj:n :}
   w r ROW {: row:ptr :}
   kind row ROW-KIND !
   c CURSOR-TOK-AT @  c CURSOR-TOK-LEN @
   kind T-STR = if 2 + swap 1- swap then
   row ROW-LEN !  row ROW-START !
   0 row ROW-COUNT !
   r 1+ row ROW-NEXT !
   obj row ROW-PARENT !
   0 row ROW-KEY-AT !
   0 row ROW-KEY-LEN !
   false row ROW-MATCHED !
   r 1+ w WALK-USED !
   obj 0 < if r exit then
   w obj ROW {: p:ptr :}
   1 p ROW-COUNT +!
   p ROW-KIND @ T-OBJ = if
      w WALK-KEY-AT @ row ROW-KEY-AT !
      w WALK-KEY-LEN @ row ROW-KEY-LEN !
      w r ADD-MEMBER
   then
   r ;


\ b's innermost open container closes: its bytes end past the closer, and
\ the next row is its sibling's.
: CLOSE-ROW ( ptr walk -- )
   {: w:ptr :}
   w  w WALK-OPEN @  ROW {: row:ptr :}
   w WALK-B CURSOR-POS @  row ROW-START @ -  row ROW-LEN !
   w WALK-USED @ row ROW-NEXT !
   row ROW-PARENT @ w WALK-OPEN ! ;


: INDEX-TOKEN ( ptr walk n -- )
   {: w:ptr kind:n :}
   kind T-KEY = if w KEEP-KEY exit then
   kind CLOSER? if w CLOSE-ROW exit then
   kind NUMERIC? if w WALK-B RAW-SPAN$ POINT-EXP drop then
   w kind ADD-ROW {: r:n :}
   kind OPENER? if r w WALK-OPEN ! then ;


\ Reads b whole with record b, each number's exponents checked as NUMBER=
\ reads them, into one row per value.
: INDEX ( ptr walk ptr u8 n -- )
   {: w:ptr b:ptr v:n :}
   w WALK-B {: c:ptr :}
   c b v INIT-STATE
   begin c NEXT-INNER dup T-END <> while w swap INDEX-TOKEN repeat
   drop ;


\ ---- a against b's rows ------------------------------------------------------

\ a's next token, a number's exponents checked as NUMBER= reads them.
: A-NEXT ( ptr walk -- n )
   {: w:ptr :}
   w WALK-A NEXT-INNER
   dup NUMERIC? if w WALK-A RAW-SPAN$ POINT-EXP drop then ;


: OPEN-FRAME ( ptr walk n n -- n )
   {: w:ptr kind:n r:n :}
   w  w WALK-DEPTH @  FRAME {: f:ptr :}
   kind f FRAME-KIND !
   r f FRAME-ROW !
   r 1+ f FRAME-DUE !
   0 f FRAME-COUNT !
   w WALK-DEPTH @ 1+ w WALK-DEPTH !
   W-ON ;

: CLOSE-FRAME ( ptr walk -- )
   {: w:ptr :}
   w WALK-DEPTH @ 1- w WALK-DEPTH ! ;


\ Whether a's scalar equals row r of one kind with it: a string decodes into
\ buf and streams against r's bytes under record s.
: SCALAR= ( ptr walk ptr row -- bool )
   {: w:ptr r:ptr :}
   w WALK-A {: ca:ptr :}
   w WALK-B CURSOR-SRC @ r ROW-START @ +  r ROW-LEN @ {: raw:ptr len:n :}
   ca CURSOR-KIND @ NUMERIC? if ca RAW-SPAN$ raw len NUMBER= exit then
   ca CURSOR-KIND @ T-STR <> if true exit then
   ca w WALK-BUF @ w WALK-CAP @ STR-INNER {: n:n :}
   w WALK-S raw len INIT-STATE
   w WALK-S NEXT-INNER drop
   w WALK-S w WALK-BUF @ n MATCH-INNER ;


: ENTER ( ptr walk -- n )
   {: w:ptr :}
   w WALK-DUE @ {: r:n :}
   w r ROW {: row:ptr :}
   w WALK-A CURSOR-KIND @ {: kind:n :}
   kind row ROW-KIND @ SAME-KIND? 0= if W-DIFFERENT exit then
   kind OPENER? if w kind r OPEN-FRAME exit then
   w row SCALAR= if W-ON else W-DIFFERENT then ;


: ALL-MATCHED? ( ptr walk ptr frame -- bool )
   {: w:ptr f:ptr :}
   f FRAME-COUNT @  w  f FRAME-ROW @  ROW ROW-COUNT @  = ;

: CLOSE-ON ( ptr walk ptr frame -- n )
   {: w:ptr f:ptr :}
   w f ALL-MATCHED? 0= if W-DIFFERENT exit then
   w CLOSE-FRAME W-ON ;


: NEXT-ELEMENT ( ptr walk -- n )
   {: w:ptr :}
   w TOP {: f:ptr :}
   w A-NEXT T-ARR-END = if w f CLOSE-ON exit then
   w f ALL-MATCHED? if W-DIFFERENT exit then
   f FRAME-DUE @ {: r:n :}
   r w WALK-DUE !
   w r ROW ROW-NEXT @ f FRAME-DUE !
   1 f FRAME-COUNT +!
   W-VALUE ;


\ A key whose member has matched already is a repeat in a.
: NEXT-MEMBER ( ptr walk -- n )
   {: w:ptr :}
   w TOP {: f:ptr :}
   w A-NEXT T-KEY <> if w f CLOSE-ON exit then
   w WALK-A w WALK-BUF @ w WALK-CAP @ STR-INNER {: len:n :}
   w  f FRAME-ROW @  w WALK-BUF @ len FIND-MEMBER {: q:n :}
   q 0< if W-DIFFERENT exit then
   w q ROW {: row:ptr :}
   row ROW-MATCHED @ if W-DIFFERENT exit then
   true row ROW-MATCHED !
   1 f FRAME-COUNT +!
   q w WALK-DUE !
   w A-NEXT drop
   W-VALUE ;


: READ-ON ( ptr walk -- n )
   {: w:ptr :}
   w WALK-DEPTH @ 0= if W-SAME exit then
   w TOP FRAME-KIND @ T-ARR = if w NEXT-ELEMENT exit then
   w NEXT-MEMBER ;


\ Whether a's value equals b's top row, one state at a time: the frames live
\ in the scratch, so no nesting reaches the return stack.
: VALUE-EQ? ( ptr walk -- bool )
   {: w:ptr :}
   w A-NEXT drop
   W-VALUE begin dup W-SAME < while
      W-VALUE = if w ENTER else w READ-ON then
   repeat
   W-SAME = ;


\ The answer stands only once record a has read a to its end.
: EQUAL-SLOT ( bool ptr walk ptr u8 n ptr u8 n -- bool ptr walk ptr u8 n ptr u8 n )
   {: w:ptr a:ptr u:n b:ptr v:n :}
   drop
   w b v INDEX
   w SORT-MEMBERS
   w WALK-A a u INIT-STATE
   w VALUE-EQ?
   begin w A-NEXT T-END = until
   w a u b v ;


: EQUAL-RUN ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n b:ptr v:n :}
   a u b v WALK-COUNTS {: k:n r:n s:n :}
   k r s u v WALK-LEN MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: scratch :}
   scratch k r s u v WALK-INIT {: w:ptr :}
   false w a u b v [: EQUAL-SLOT ;] catch
   {: same spare pa ua pb ub code:n :}
   scratch WALK-FREE
   code 0= if same exit then
   code throw ;

public

\ False when either side repeats a key. Both texts are read whole: a malformed one throws whatever
\ the answer, as does an exponent past a cell. Time is at most n log n in the two texts' sizes.
: VALUE= ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n b:ptr v:n :}
   a u REQUIRE-SOURCE
   b v REQUIRE-SOURCE
   a u b v EQUAL-RUN ;

;package
