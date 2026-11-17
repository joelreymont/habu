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
\ string of b it compares. Each number is read once, by the pass that meets it,
\ into what NUMBER= compares. The scratch is one block from MEM:WITH-BYTES,
\ sized from the texts' lengths; b's rows, and the member list and frames laid
\ past them, move to a mapping only when they outgrow the block's tail.

require src/lib/memory.f
require src/lib/sort.f
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


\ One mantissa byte at offset i onto the offsets of the first and the last
\ significant digit and of the point so far, -1 for none yet.
: MANTISSA-BYTE ( n n n ^u8 n -- n n n n )
   {: f:n l:n dot:n p:^ i:n :}
   p i + c@ {: c:n :}
   c DOT = if f l i  i 1+ exit then
   c SIGNIFICANT? 0= if f l dot  i 1+ exit then
   f 0< if i else f then  i dot  i 1+ ;

\ The offsets of the first and the last significant digit and of the point,
\ each -1 without one, then the mantissa's end: the exponent marker or u.
: MANTISSA ( ^u8 n -- n n n n )
   {: p:^ u:n :}
   -1 -1 -1 0
   begin dup u < if dup p + c@ EXP-MARK? 0= else false then
   while p swap MANTISSA-BYTE repeat ;


\ Digit d onto m, the negated exponent so far: one past LEAST-N is
\ E-JSON-READ-NUMBER, decided before the product could wrap.
: EXP-DIGIT ( n n -- n )
   {: m:n d:n :}
   m EXP-CUT negate < if E-JSON-READ-NUMBER throw then
   m EXP-CUT negate =  d EXP-LAST >  and if E-JSON-READ-NUMBER throw then
   m 10 * d - ;


\ The exponent the lexeme states after its mantissa's end m, 0 without one;
\ one a signed cell cannot hold is E-JSON-READ-NUMBER.
: STATED-EXP ( ^u8 n n -- n )
   {: p:^ u:n m:n :}
   m 1+ {: at:n :}
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


\ x for the first significant digit at offset f, the point at offset dot (the
\ mantissa's end without one) and the stated exponent e.
: POINT-EXP ( n n n -- n )
   {: dot:n f:n e:n :}
   dot f -  f dot > if 1+ then  e EXP+ ;

\ How many significant digits lie from offset f through offset l, the point
\ at offset dot not counted.
: DIGIT-COUNT ( n n n -- n )
   {: f:n l:n dot:n :}
   l f - 1+  f dot <  dot l <  and if 1- then ;

\ A lexeme's x, first significant digit's offset and digit count, 0 0 0 for a
\ zero, from one scan; any exponent past a cell is E-JSON-READ-NUMBER.
: DIGITS ( ^u8 n -- n n n )
   {: p:^ u:n :}
   p u MANTISSA {: f:n l:n dot:n m:n :}
   p u m STATED-EXP {: e:n :}
   f 0< if 0 0 0 exit then
   dot 0< if m else dot then {: at:n :}
   at f e POINT-EXP  f  f l at DIGIT-COUNT ;


\ The offset of the digit after the one at offset i, below the last.
: NEXT-DIGIT ( ^u8 n -- n )
   {: p:^ i:n :}
   i 1+ dup p + c@ DOT = if 1+ then ;


\ Whether count significant digits from a and from b are the same, byte for
\ byte, each side stepping over its point.
: SAME-DIGITS? ( ^u8 ^u8 n -- bool )
   {: a:^ b:^ count:n :}
   0 0  count 0 ?do
      over a + c@  over b + c@  <> if 2drop false unloop exit then
      i count 1- < if swap a swap NEXT-DIGIT swap b swap NEXT-DIGIT then
   loop
   2drop true ;


: NEGATIVE? ( ^u8 -- bool )
   c@ MINUS = ;


\ Whether two lexemes have one value, each given by its bytes and what DIGITS
\ answers for it.
: NUMBER= ( ^u8 n n n ^u8 n n n -- bool )
   {: a:^ xa:n fa:n na:n b:^ xb:n fb:n nb:n :}
   na 0=  nb 0= {: za:bool zb:bool :}
   za zb and if true exit then
   za zb or if false exit then
   a NEGATIVE?  b NEGATIVE?  xor if false exit then
   xa xb <>  na nb <>  or if false exit then
   a fa +  b fb +  na SAME-DIGITS? ;


\ A number whose exponents a cell cannot hold is rejected at its token.
: EXP-REJECT ( ^cursor n -- )
   {: state:^ code:n :}
   code READER-CODE? if
      state s" a number whose exponent a signed cell holds"
      s" write the number with an exponent nearer zero" code REJECT-TOKEN
   then
   code throw ;

\ DIGITS's three results replace the slots under the cursor, so the quotation
\ under catch leaves the types it was given.
: DIGITS-SLOT ( n n n ^cursor -- n n n ^cursor )
   {: state:^ :}
   drop drop drop  state RAW-SPAN$ DIGITS  state ;

\ The cursor's number token as DIGITS reads it.
: TOKEN-DIGITS ( ^cursor -- n n n )
   {: state:^ :}
   0 0 0 state [: DIGITS-SLOT ;] catch {: code:n :} drop
   code 0<> if state code EXP-REJECT then ;


\ ---- the walk --------------------------------------------------------------

: SAME-KIND? ( n n -- bool )
   {: ka:n kb:n :}
   ka kb =  ka NUMERIC? kb NUMERIC? and  or ;


\ One value of b: start..start+len its bytes, quotes included; next the first
\ row past its own; parent -1 at the top. count and first: a container's
\ children and an object's first member's place, or a number's digit count and
\ first significant digit's offset, with exp its point exponent (DIGITS).
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
   FIELD exp n
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

\ VALUE='s scratch over a WITH-BYTES block, which reads 0: every count starts
\ at 0 and no record owns a stack. At rows, tail-len bytes (a mapping when
\ mapped): b's rows, then the member list, spare and frames. level and peak:
\ b's containers open as the index pass reads, now and at most; num-exp,
\ num-first and num-count: a's number under record a, as a row holds b's.
STRUCTURE walk DERIVE addr
   FIELD a cursor
   FIELD b cursor
   FIELD s cursor
   FIELD frames ^u8
   FIELD depth n
   FIELD rows ^u8
   FIELD tail-len n
   FIELD mapped bool
   FIELD used n
   FIELD open n
   FIELD level n
   FIELD peak n
   FIELD members ^u8
   FIELD spare ^u8
   FIELD member-n n
   FIELD keys ^u8
   FIELD key-cap n
   FIELD key-used n
   FIELD key-at n
   FIELD key-len n
   FIELD due n
   FIELD buf ^u8
   FIELD cap n
   FIELD num-exp n
   FIELD num-first n
   FIELD num-count n
;STRUCTURE

CAST: >ROW ( ^n -- ^row )
CAST: >FRAME ( ^n -- ^frame )
CAST: >WALK ( ^n -- ^walk )

\ The walk's states: a value is due under a's cursor and row due; the
\ innermost frame reads on, past an equal value or into one just opened; the
\ two answers.
0 constant W-VALUE
1 constant W-ON
2 constant W-SAME
3 constant W-DIFFERENT

: ROW ( ^walk n -- ^row )
   {: w:^ i:n :}
   w WALK-ROWS @ i ROW-BYTES * + CELL-VIEW >ROW ;

: FRAME ( ^walk n -- ^frame )
   {: w:^ i:n :}
   w WALK-FRAMES @ i FRAME-BYTES * + CELL-VIEW >FRAME ;

: TOP ( ^walk -- ^frame )
   {: w:^ :}
   w  w WALK-DEPTH @ 1-  FRAME ;


\ ---- the scratch ------------------------------------------------------------

\ The tail's first length for b's v bytes. n values take at least 2n-1 bytes,
\ and each row adds at most two member-list cells and one frame, so
\ TAIL-BOUND bytes hold any b of v bytes with no move. TAIL-0 is the smaller
\ of that bound and the larger of TAIL-FLOOR bytes and v/10+1 rows: a b whose
\ bound fits the floor never moves; a longer one moves to a mapping only when
\ it needs more than v/10+1 rows' bytes.
2048 constant TAIL-FLOOR

: TAIL-BOUND ( n -- n )
   1+ 2 /  ROW-BYTES 2 cells + FRAME-BYTES +  * ;

: TAIL-0 ( n -- n )
   {: v:n :}
   v TAIL-BOUND  v 10 / 1+ ROW-BYTES *  TAIL-FLOOR max  min ;

\ The block for a's u bytes and b's v: the walk, the tail, v key bytes, then u
\ bytes for a's strings.
: WALK-LEN ( n n -- n )
   {: u:n v:n :}
   WALK-BYTES  v TAIL-0 +  v +  u + ;

: WALK-INIT ( ^u8 n n -- ^walk )
   {: base:^ u:n v:n :}
   base CELL-VIEW >WALK {: w:^ :}
   base WALK-BYTES + {: rows:^ :}
   rows v TAIL-0 + {: keys:^ :}
   rows w WALK-ROWS !
   v TAIL-0 w WALK-TAIL-LEN !
   keys w WALK-KEYS !
   v w WALK-KEY-CAP !
   keys v + w WALK-BUF !
   u w WALK-CAP !
   -1 w WALK-OPEN !
   w ;


: FREE-TAIL ( ^walk -- )
   {: w:^ :}
   w WALK-MAPPED @ 0= if exit then
   w WALK-ROWS @  w WALK-TAIL-LEN @  SPAN:MAKE MEM:FREE-SPAN ;

\ Moves the rows to a mapping of len bytes and frees the mapping they leave,
\ if any.
: MOVE-TAIL ( ^walk n -- )
   {: w:^ len:n :}
   len MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: fresh :}
   w WALK-ROWS @  w WALK-USED @ ROW-BYTES *  fresh SPAN:COPY
   w FREE-TAIL
   fresh SPAN:$  w WALK-TAIL-LEN !  w WALK-ROWS !
   true w WALK-MAPPED ! ;

\ Room for one more row: the rows move to a tail twice as long when they fill
\ theirs.
: ROW-ROOM ( ^walk -- )
   {: w:^ :}
   w WALK-USED @ 1+ ROW-BYTES *  w WALK-TAIL-LEN @ > if
      w  w WALK-TAIL-LEN @ 2 *  MOVE-TAIL
   then ;

\ The rows, the member list and its spare, and a frame for each level of b's
\ deepest container: a frame is a container open in both texts.
: TAIL-NEED ( ^walk -- n )
   {: w:^ :}
   w WALK-USED @ ROW-BYTES *
   w WALK-MEMBER-N @ 2 * cells +
   w WALK-PEAK @ FRAME-BYTES * + ;

\ Lays the member list, its spare and the frames past the rows, moving the
\ tail to a mapping of the length they need when they pass its end.
: LAY-TAIL ( ^walk -- )
   {: w:^ :}
   w TAIL-NEED {: need:n :}
   need w WALK-TAIL-LEN @ > if w need MOVE-TAIL then
   w WALK-ROWS @  w WALK-USED @ ROW-BYTES * + {: list:^ :}
   list w WALK-MEMBERS !
   list w WALK-MEMBER-N @ cells + {: spare:^ :}
   spare w WALK-SPARE !
   spare w WALK-MEMBER-N @ cells + w WALK-FRAMES ! ;


: WALK-FREE ( ^walk -- )
   {: w:^ :}
   w WALK-A MORE-RELEASE
   w WALK-B MORE-RELEASE
   w WALK-S MORE-RELEASE
   w FREE-TAIL ;


\ ---- b's members -------------------------------------------------------------

: LIST-CELL ( ^u8 n -- ^n )
   cells + CELL-VIEW ;

: MEMBER@ ( ^walk n -- n )
   {: w:^ i:n :}
   w WALK-MEMBERS @ i LIST-CELL @ ;

: MEMBER-KEY$ ( ^walk n -- ^u8 n )
   {: w:^ r:n :}
   w r ROW {: row:^ :}
   w WALK-KEYS @ row ROW-KEY-AT @ +  row ROW-KEY-LEN @ ;


: MEMBER< ( ^walk n n -- bool )
   {: w:^ p:n q:n :}
   w p ROW ROW-PARENT @  w q ROW ROW-PARENT @ {: pp:n qp:n :}
   pp qp <> if pp qp < exit then
   w p MEMBER-KEY$  w q MEMBER-KEY$  STR< ;


\ Whether the merge takes place i of the left run before place j of the right: on a
\ tie the left, so equal keys of one object keep document order.
: LEFT-NEXT? ( ^walk ^u8 n n n n -- bool )
   {: w:^ src:^ i:n j:n mid:n hi:n :}
   i mid >= if false exit then
   j hi >= if true exit then
   w  src j LIST-CELL @  src i LIST-CELL @  MEMBER< 0= ;


: MERGE-STEP ( n n ^walk ^u8 ^u8 n n n -- n n )
   {: i:n j:n w:^ src:^ dst:^ mid:n hi:n k:n :}
   w src i j mid hi LEFT-NEXT? if
      src i LIST-CELL @  dst k LIST-CELL !  i 1+ j exit
   then
   src j LIST-CELL @  dst k LIST-CELL !  i j 1+ ;


: MERGE-RUNS ( ^walk ^u8 ^u8 n n n -- )
   {: w:^ src:^ dst:^ lo:n mid:n hi:n :}
   lo mid
   hi lo ?do w src dst mid hi i MERGE-STEP loop
   2drop ;


: MERGE-PASS ( ^walk ^u8 ^u8 n n -- )
   {: w:^ src:^ dst:^ n:n width:n :}
   n 0 ?do
      w src dst  i  i width + n min  i width 2 * + n min  MERGE-RUNS
   width 2 * +loop ;


: SORT-ROUND ( ^walk n -- )
   {: w:^ width:n :}
   w WALK-MEMBERS @  w WALK-SPARE @ {: src:^ dst:^ :}
   w src dst  w WALK-MEMBER-N @  width MERGE-PASS
   dst w WALK-MEMBERS !
   src w WALK-SPARE ! ;


\ Stores place i in its member's object row when the member before it has
\ another parent prev; answers its own parent.
: MARK-FIRST ( n ^walk n -- n )
   {: prev:n w:^ i:n :}
   w  w i MEMBER@  ROW ROW-PARENT @ {: obj:n :}
   obj prev <> if i  w obj ROW ROW-FIRST ! then
   obj ;


: SORT-MEMBERS ( ^walk -- )
   {: w:^ :}
   1 begin dup w WALK-MEMBER-N @ < while w over SORT-ROUND 2 * repeat drop
   -1  w WALK-MEMBER-N @ 0 ?do w i MARK-FIRST loop drop ;


\ Object row obj's member whose key decodes to key, or -1. The first of equal keys is found each
\ time, so a key a repeats meets a member already matched and a key b repeats leaves one unmatched.
: FIND-MEMBER ( ^walk n ^u8 n -- n )
   {: w:^ obj:n key:^ len:n :}
   w obj ROW {: o:^ :}
   o ROW-FIRST @  dup o ROW-COUNT @ + {: lo:n hi:n :}
   lo hi
   [: w swap MEMBER@ w swap MEMBER-KEY$ key len STR< ;]
   SORT:PARTITION-POINT {: at:n :}
   at hi = if -1 exit then
   w at MEMBER@ {: q:n :}
   w q MEMBER-KEY$ key len STR= if q else -1 then ;


\ ---- the index pass ----------------------------------------------------------

\ Decodes the key under record b into the key area as the key read last.
: KEEP-KEY ( ^walk -- )
   {: w:^ :}
   w WALK-KEY-USED @ {: at:n :}
   w WALK-B  w WALK-KEYS @ at +  w WALK-KEY-CAP @ at -  STR-INNER {: len:n :}
   at w WALK-KEY-AT !
   len w WALK-KEY-LEN !
   at len + w WALK-KEY-USED ! ;


\ Appends a row for the value of kind under record b, a member's when the
\ innermost open row is an object's, and answers its index; the rows first
\ make room for it.
: ADD-ROW ( ^walk n -- n )
   {: w:^ kind:n :}
   w ROW-ROOM
   w WALK-B {: c:^ :}
   w WALK-USED @ {: r:n :}
   w WALK-OPEN @ {: obj:n :}
   w r ROW {: row:^ :}
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
   w obj ROW {: p:^ :}
   1 p ROW-COUNT +!
   p ROW-KIND @ T-OBJ = if
      w WALK-KEY-AT @ row ROW-KEY-AT !
      w WALK-KEY-LEN @ row ROW-KEY-LEN !
      1 w WALK-MEMBER-N +!
   then
   r ;


\ b's innermost open container closes: its bytes end past the closer, and
\ the next row is its sibling's.
: CLOSE-ROW ( ^walk -- )
   {: w:^ :}
   w  w WALK-OPEN @  ROW {: row:^ :}
   w WALK-B CURSOR-POS @  row ROW-START @ -  row ROW-LEN !
   w WALK-USED @ row ROW-NEXT !
   row ROW-PARENT @ w WALK-OPEN !
   -1 w WALK-LEVEL +! ;


\ Row r, a container's, is b's innermost open one.
: OPEN-ROW ( ^walk n -- )
   {: w:^ r:n :}
   r w WALK-OPEN !
   1 w WALK-LEVEL +!
   w WALK-LEVEL @  w WALK-PEAK @  max  w WALK-PEAK ! ;


\ Row r's number as DIGITS reads it.
: KEEP-NUMBER ( ^walk n -- )
   {: w:^ r:n :}
   w r ROW {: row:^ :}
   w WALK-B TOKEN-DIGITS
   row ROW-COUNT !  row ROW-FIRST !  row ROW-EXP ! ;


: INDEX-TOKEN ( ^walk n -- )
   {: w:^ kind:n :}
   kind T-KEY = if w KEEP-KEY exit then
   kind CLOSER? if w CLOSE-ROW exit then
   w kind ADD-ROW {: r:n :}
   kind NUMERIC? if w r KEEP-NUMBER then
   kind OPENER? if w r OPEN-ROW then ;


\ Reads b whole with record b into one row per value, each number read once;
\ a key or a closer adds no row and makes no room.
: INDEX ( ^walk ^u8 n -- )
   {: w:^ b:^ v:n :}
   w WALK-B {: c:^ :}
   c b v INIT-STATE
   begin c NEXT-INNER dup T-END <> while w swap INDEX-TOKEN repeat
   drop ;


\ Whether row r is a member: its parent is an object's row.
: MEMBER? ( ^walk n -- bool )
   {: w:^ r:n :}
   w r ROW ROW-PARENT @ {: p:n :}
   p 0< if false exit then
   w p ROW ROW-KIND @ T-OBJ = ;

\ Puts row r at place at of the member list when it is a member; answers the
\ next place.
: LIST-ROW ( n ^walk n -- n )
   {: at:n w:^ r:n :}
   w r MEMBER? 0= if at exit then
   r  w WALK-MEMBERS @ at LIST-CELL !  at 1+ ;

\ The member list, in b's order.
: LIST-MEMBERS ( ^walk -- )
   {: w:^ :}
   0  w WALK-USED @ 0 ?do w i LIST-ROW loop drop ;


\ ---- a against b's rows ------------------------------------------------------

\ a's number under record a as DIGITS reads it.
: KEEP-A-NUMBER ( ^walk -- )
   {: w:^ :}
   w WALK-A TOKEN-DIGITS
   w WALK-NUM-COUNT !  w WALK-NUM-FIRST !  w WALK-NUM-EXP ! ;

\ a's next token, a number read once.
: A-NEXT ( ^walk -- n )
   {: w:^ :}
   w WALK-A NEXT-INNER
   dup NUMERIC? if w KEEP-A-NUMBER then ;


: OPEN-FRAME ( ^walk n n -- n )
   {: w:^ kind:n r:n :}
   w  w WALK-DEPTH @  FRAME {: f:^ :}
   kind f FRAME-KIND !
   r f FRAME-ROW !
   r 1+ f FRAME-DUE !
   0 f FRAME-COUNT !
   w WALK-DEPTH @ 1+ w WALK-DEPTH !
   W-ON ;

: CLOSE-FRAME ( ^walk -- )
   {: w:^ :}
   w WALK-DEPTH @ 1- w WALK-DEPTH ! ;


\ Whether a's number equals row r's, whose bytes start at raw.
: SAME-NUMBER? ( ^walk ^u8 ^row -- bool )
   {: w:^ raw:^ r:^ :}
   w WALK-A RAW-SPAN$ drop
   w WALK-NUM-EXP @  w WALK-NUM-FIRST @  w WALK-NUM-COUNT @
   raw  r ROW-EXP @  r ROW-FIRST @  r ROW-COUNT @  NUMBER= ;


\ Whether a's scalar equals row r of one kind with it: a string decodes into
\ buf and streams against r's bytes under record s.
: SCALAR= ( ^walk ^row -- bool )
   {: w:^ r:^ :}
   w WALK-A {: ca:^ :}
   w WALK-B CURSOR-SRC @ r ROW-START @ +  r ROW-LEN @ {: raw:^ len:n :}
   ca CURSOR-KIND @ NUMERIC? if w raw r SAME-NUMBER? exit then
   ca CURSOR-KIND @ T-STR <> if true exit then
   ca w WALK-BUF @ w WALK-CAP @ STR-INNER {: n:n :}
   w WALK-S raw len INIT-STATE
   w WALK-S NEXT-INNER drop
   w WALK-S w WALK-BUF @ n MATCH-INNER ;


: ENTER ( ^walk -- n )
   {: w:^ :}
   w WALK-DUE @ {: r:n :}
   w r ROW {: row:^ :}
   w WALK-A CURSOR-KIND @ {: kind:n :}
   kind row ROW-KIND @ SAME-KIND? 0= if W-DIFFERENT exit then
   kind OPENER? if w kind r OPEN-FRAME exit then
   w row SCALAR= if W-ON else W-DIFFERENT then ;


: ALL-MATCHED? ( ^walk ^frame -- bool )
   {: w:^ f:^ :}
   f FRAME-COUNT @  w  f FRAME-ROW @  ROW ROW-COUNT @  = ;

: CLOSE-ON ( ^walk ^frame -- n )
   {: w:^ f:^ :}
   w f ALL-MATCHED? 0= if W-DIFFERENT exit then
   w CLOSE-FRAME W-ON ;


: NEXT-ELEMENT ( ^walk -- n )
   {: w:^ :}
   w TOP {: f:^ :}
   w A-NEXT T-ARR-END = if w f CLOSE-ON exit then
   w f ALL-MATCHED? if W-DIFFERENT exit then
   f FRAME-DUE @ {: r:n :}
   r w WALK-DUE !
   w r ROW ROW-NEXT @ f FRAME-DUE !
   1 f FRAME-COUNT +!
   W-VALUE ;


\ A key whose member has matched already is a repeat in a.
: NEXT-MEMBER ( ^walk -- n )
   {: w:^ :}
   w TOP {: f:^ :}
   w A-NEXT T-KEY <> if w f CLOSE-ON exit then
   w WALK-A w WALK-BUF @ w WALK-CAP @ STR-INNER {: len:n :}
   w  f FRAME-ROW @  w WALK-BUF @ len FIND-MEMBER {: q:n :}
   q 0< if W-DIFFERENT exit then
   w q ROW {: row:^ :}
   row ROW-MATCHED @ if W-DIFFERENT exit then
   true row ROW-MATCHED !
   1 f FRAME-COUNT +!
   q w WALK-DUE !
   w A-NEXT drop
   W-VALUE ;


: READ-ON ( ^walk -- n )
   {: w:^ :}
   w WALK-DEPTH @ 0= if W-SAME exit then
   w TOP FRAME-KIND @ T-ARR = if w NEXT-ELEMENT exit then
   w NEXT-MEMBER ;


\ Whether a's value equals b's top row, one state at a time: the frames live
\ in the scratch, so no nesting reaches the return stack.
: VALUE-EQ? ( ^walk -- bool )
   {: w:^ :}
   w A-NEXT drop
   W-VALUE begin dup W-SAME < while
      W-VALUE = if w ENTER else w READ-ON then
   repeat
   W-SAME = ;


\ The answer stands only once record a has read a to its end.
: EQUAL-SLOT ( bool ^walk ^u8 n ^u8 n -- bool ^walk ^u8 n ^u8 n )
   {: w:^ a:^ u:n b:^ v:n :}
   drop
   w b v INDEX
   w LAY-TAIL
   w LIST-MEMBERS
   w SORT-MEMBERS
   w WALK-A a u INIT-STATE
   w VALUE-EQ?
   begin w A-NEXT T-END = until
   w a u b v ;


\ VALUE= over the block at base; a throw leaves no reader stack or tail
\ mapping behind.
: EQUAL-IN ( ^u8 n ^u8 n ^u8 NUM:alloc-byte-len -- bool )
   drop {: a:^ u:n b:^ v:n base:^ :}
   base u v WALK-INIT {: w:^ :}
   false w a u b v [: EQUAL-SLOT ;] catch
   {: same spare pa ua pb ub code:n :}
   w WALK-FREE
   code 0= if same exit then
   code throw ;


: EQUAL-RUN ( ^u8 n ^u8 n -- bool )
   {: a:^ u:n b:^ v:n :}
   a u b v  u v WALK-LEN MEM:BYTES-ALLOC-LEN
   [: EQUAL-IN ;] MEM:WITH-BYTES ;

public

\ False when either side repeats a key. Both texts are read whole: a malformed one throws whatever
\ the answer, as does an exponent past a cell. Time is at most n log n in the two texts' sizes.
: VALUE= ( ^u8 n ^u8 n -- bool )
   {: a:^ u:n b:^ v:n :}
   a u REQUIRE-SOURCE
   b v REQUIRE-SOURCE
   a u b v EQUAL-RUN ;

;package
