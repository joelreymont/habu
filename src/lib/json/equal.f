\ equal.f - VALUE=, whether two JSON texts hold the same value. A file of
\ package JSON (src/lib/json.f).

require src/lib/memory.f
require src/lib/span.f
require src/lib/json/read.f

package JSON

private

: NUMERIC? ( n -- bool )
   dup T-INT = swap T-FLOAT = or ;


\ ---- numbers by value -----------------------------------------------------
\ Two numbers are equal when their values are, decided on the lexemes SCAN-NUMBER
\ admitted, with no conversion to binary64: a nonzero value is the sign, the
\ significant digits d1..dn (the integer and fraction digits from the first
\ nonzero through the last nonzero) and the point exponent x of
\ 0.d1..dn * 10^x. Every zero has no significant digits and equals every other.

MEM-MAX-N negate 1- constant LEAST-N         \ the least signed cell
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


\ x + e, or E-JSON-READ-NUMBER when a signed cell cannot hold the sum.
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


\ Whether two number lexemes have the same value; an exponent past a signed
\ cell in either throws, as POINT-EXP does.
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

\ Reads text as one value with record c, each number's exponents checked as
\ NUMBER= reads them, and answers its deepest container level.
: TEXT-DEPTH ( ptr cursor ptr u8 n -- n )
   {: c:ptr text:ptr u:n :}
   c text u INIT-STATE
   0 begin c NEXT-INNER dup T-END <> while
      NUMERIC? if c RAW-SPAN$ POINT-EXP drop then
      c CURSOR-DEPTH @ max
   repeat drop ;


\ Whether ca's and cb's tokens are one kind, numbers being one kind.
: SAME-KIND? ( ptr cursor ptr cursor -- bool )
   {: ca:ptr cb:ptr :}
   ca CURSOR-KIND @ {: ka:n :}
   cb CURSOR-KIND @ {: kb:n :}
   ka kb =  ka NUMERIC? kb NUMERIC? and  or ;


\ Whether two scalars of one kind hold the same value: a string decodes into
\ buf, which holds any string of ca's text.
: SCALAR= ( ptr cursor ptr cursor ptr u8 n -- bool )
   {: ca:ptr cb:ptr buf:ptr cap:n :}
   ca CURSOR-KIND @ NUMERIC? if ca RAW-SPAN$ cb RAW-SPAN$ NUMBER= exit then
   ca CURSOR-KIND @ T-STR = if cb buf ca buf cap STR-INNER MATCH-INNER exit then
   true ;


\ Opens record r on object text and leaves it past the opener.
: OPEN-OBJECT ( ptr cursor ptr u8 n -- )
   {: r:ptr text:ptr u:n :}
   r text u INIT-STATE
   r NEXT-INNER drop ;


: MEMBER-COUNT ( ptr cursor ptr u8 n -- n )
   {: r:ptr text:ptr u:n :}
   r text u OPEN-OBJECT
   0 begin r NEXT-INNER T-KEY = while
      1+ r NEXT-INNER drop r SKIP-INNER
   repeat ;


\ With ca on a key, finds that key in object text with record r; on true both
\ cursors are on the member's value.
: MEMBER-PAIR? ( ptr cursor ptr cursor ptr u8 n ptr u8 n -- bool )
   {: ca:ptr r:ptr text:ptr u:n buf:ptr cap:n :}
   ca buf cap STR-INNER {: ku:n :}
   r text u OPEN-OBJECT
   r buf ku FIND-INNER 0= if false exit then
   ca NEXT-INNER drop true ;


\ With ca just past the close of the object opened at offset start, whether
\ two of its keys decode the same.
: CLOSED-REPEATS? ( ptr cursor n -- bool )
   {: ca:ptr start:n :}
   ca CURSOR-SRC @ start +  ca CURSOR-POS @ start -  READ-REPEATS? ;


\ One open container of the walk, kind T-ARR or T-OBJ. cur walks b's side: an
\ array's is its parent's, an object's is record, which finds each of a's keys
\ in b's object, the bytes text..text+len. start is the offset of a's object
\ and count the members matched so far.
STRUCTURE frame DERIVE addr
   FIELD kind n
   FIELD cur ptr cursor
   FIELD start n
   FIELD text ptr u8
   FIELD len n
   FIELD count n
   FIELD record cursor
;STRUCTURE

\ VALUE='s scratch: records a and b on the two texts, depth frames open of
\ those at levels, then buf..buf+cap for decoding a's strings.
STRUCTURE walk DERIVE addr
   FIELD a cursor
   FIELD b cursor
   FIELD depth n
   FIELD levels ptr u8
   FIELD buf ptr u8
   FIELD cap n
;STRUCTURE

CAST: >FRAME ( ptr n -- ptr frame )
CAST: >WALK ( ptr n -- ptr walk )

\ The walk's states: a value is due under a's cursor and CB; the innermost frame
\ reads on, past an equal value or into one just opened; the two answers.
0 constant W-VALUE
1 constant W-ON
2 constant W-SAME
3 constant W-DIFFERENT

: FRAME ( ptr walk n -- ptr frame )
   {: w:ptr i:n :}
   w WALK-LEVELS @ i FRAME-BYTES * + CELL-VIEW >FRAME ;

: TOP ( ptr walk -- ptr frame )
   {: w:ptr :}
   w  w WALK-DEPTH @ 1-  FRAME ;

\ The cursor on b's side of the value due.
: CB ( ptr walk -- ptr cursor )
   {: w:ptr :}
   w WALK-DEPTH @ 0= if w WALK-B exit then
   w TOP FRAME-CUR @ ;


: OPEN-FRAME ( ptr walk n ptr cursor -- ptr frame )
   {: w:ptr kind:n cur:ptr :}
   w  w WALK-DEPTH @  FRAME {: f:ptr :}
   kind f FRAME-KIND !
   cur f FRAME-CUR !
   w WALK-DEPTH @ 1+ w WALK-DEPTH !
   f ;

: CLOSE-FRAME ( ptr walk -- )
   {: w:ptr :}
   w WALK-DEPTH @ 1- w WALK-DEPTH ! ;


\ With a's cursor and CB on objects, opens a frame whose record finds a's keys
\ in b's object; CB moves past that object.
: OPEN-OBJECT-FRAME ( ptr walk -- )
   {: w:ptr :}
   w WALK-A CURSOR-TOK-AT @ {: start:n :}
   w CB VALUE-SPAN-INNER {: text:ptr len:n :}
   w  w WALK-DEPTH @  FRAME FRAME-RECORD {: r:ptr :}
   w T-OBJ r OPEN-FRAME {: f:ptr :}
   start f FRAME-START !
   text f FRAME-TEXT !
   len f FRAME-LEN !
   0 f FRAME-COUNT ! ;


\ Compares the value due: a scalar whole, a container by opening its frame.
: ENTER ( ptr walk -- n )
   {: w:ptr :}
   w WALK-A  w CB {: ca:ptr cb:ptr :}
   ca cb SAME-KIND? 0= if W-DIFFERENT exit then
   ca CURSOR-KIND @ {: kind:n :}
   kind T-ARR = if w T-ARR cb OPEN-FRAME drop W-ON exit then
   kind T-OBJ = if w OPEN-OBJECT-FRAME W-ON exit then
   ca cb w WALK-BUF @ w WALK-CAP @ SCALAR= if W-ON else W-DIFFERENT then ;


\ The next elements of both arrays are due, or both closes close the frame.
: NEXT-ELEMENT ( ptr walk -- n )
   {: w:ptr :}
   w WALK-A NEXT-INNER T-ARR-END =  w CB NEXT-INNER T-ARR-END =
   {: ea:bool eb:bool :}
   ea eb and if w CLOSE-FRAME W-ON exit then
   ea eb or if W-DIFFERENT exit then
   W-VALUE ;


\ Every key of a's object found, so as many members in b's and no repeat in
\ a's close the frame: a repeat in b's would be one in a's.
: CLOSE-MEMBERS ( ptr walk -- n )
   {: w:ptr :}
   w TOP {: f:ptr :}
   f FRAME-RECORD f FRAME-TEXT @ f FRAME-LEN @ MEMBER-COUNT
   f FRAME-COUNT @ <> if W-DIFFERENT exit then
   w WALK-A f FRAME-START @ CLOSED-REPEATS? if W-DIFFERENT exit then
   w CLOSE-FRAME W-ON ;


\ a's next key, found in b's object, makes its value due on both sides.
: NEXT-MEMBER ( ptr walk -- n )
   {: w:ptr :}
   w TOP {: f:ptr :}
   w WALK-A NEXT-INNER T-KEY <> if w CLOSE-MEMBERS exit then
   w WALK-A f FRAME-RECORD f FRAME-TEXT @ f FRAME-LEN @
   w WALK-BUF @ w WALK-CAP @ MEMBER-PAIR? 0= if W-DIFFERENT exit then
   f FRAME-COUNT @ 1+ f FRAME-COUNT !
   W-VALUE ;


: READ-ON ( ptr walk -- n )
   {: w:ptr :}
   w WALK-DEPTH @ 0= if W-SAME exit then
   w TOP FRAME-KIND @ T-ARR = if w NEXT-ELEMENT exit then
   w NEXT-MEMBER ;


\ Whether the values under records a and b are equal, one state at a time:
\ the frames live in the scratch, so no nesting reaches the return stack.
: VALUE-EQ? ( ptr walk -- bool )
   {: w:ptr :}
   W-VALUE begin dup W-SAME < while
      W-VALUE = if w ENTER else w READ-ON then
   repeat
   W-SAME = ;


: DEPTH-SLOT ( n SPAN:span<u8> ptr u8 n ptr u8 n -- n SPAN:span<u8> ptr u8 n ptr u8 n )
   {: scratch a:ptr u:n b:ptr v:n :}
   drop scratch SPAN:$ drop RECORD {: c:ptr :}
   c a u TEXT-DEPTH  c b v TEXT-DEPTH  min
   scratch a u b v ;


\ Reads both texts whole, so a malformed one throws whatever the answer, and
\ answers the lesser deepest container level: the walk opens a frame only
\ inside a container on both sides, so no more frames than that.
: CHECKED-DEPTH ( ptr u8 n ptr u8 n -- n )
   {: a:ptr u:n b:ptr v:n :}
   CURSOR-BYTES MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: scratch :}
   0 scratch a u b v [: DEPTH-SLOT ;] catch
   {: depth spare pa ua pb ub code:n :}
   scratch SCRATCH-FREE
   code 0= if depth exit then
   code throw ;


\ The scratch holds the walk, k frames, then u bytes for decoding a's strings.
: WALK-INIT ( SPAN:span<u8> n n -- ptr walk )
   {: scratch k:n u:n :}
   scratch SPAN:$ drop {: base:ptr :}
   base CELL-VIEW >WALK {: w:ptr :}
   base WALK-BYTES + w WALK-LEVELS !
   base WALK-BYTES + k FRAME-BYTES * + w WALK-BUF !
   u w WALK-CAP !
   w ;


\ Releases every record's stack, a frame's whether opened or not, then the
\ scratch.
: WALK-FREE ( SPAN:span<u8> n -- )
   {: scratch k:n :}
   scratch SPAN:$ drop CELL-VIEW >WALK {: w:ptr :}
   w WALK-A MORE-RELEASE
   w WALK-B MORE-RELEASE
   k 0 ?do w i FRAME FRAME-RECORD MORE-RELEASE loop
   scratch MEM:FREE-SPAN ;


: EQUAL-SLOT ( bool ptr walk ptr u8 n ptr u8 n -- bool ptr walk ptr u8 n ptr u8 n )
   {: w:ptr a:ptr u:n b:ptr v:n :}
   drop
   w WALK-A a u INIT-STATE  w WALK-A NEXT-INNER drop
   w WALK-B b v INIT-STATE  w WALK-B NEXT-INNER drop
   w VALUE-EQ?
   w a u b v ;


\ k frames bound the walk's depth (CHECKED-DEPTH).
: EQUAL-RUN ( ptr u8 n ptr u8 n n -- bool )
   {: a:ptr u:n b:ptr v:n k:n :}
   WALK-BYTES k FRAME-BYTES * + u + MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: scratch :}
   scratch k u WALK-INIT {: w:ptr :}
   false w a u b v [: EQUAL-SLOT ;] catch
   {: same spare pa ua pb ub code:n :}
   scratch k WALK-FREE
   code 0= if same exit then
   code throw ;

public

\ Whether two texts, each one JSON value, hold the same value: one kind, all
\ numbers being one; strings that decode to the same bytes; numbers of equal
\ mathematical value, as JSON Schema 2020-12 Core §4.2.2 defines equality,
\ decided on the lexemes with no conversion to binary64, so 1, 1.0, 1e0 and
\ 10e-1 are equal, -0 equals 0, and 0.1 differs from 0.10000000000000001;
\ arrays equal element by element in order; objects with the same keys in any
\ order, each with equal values, and false when either side repeats a key
\ (READ-REPEATS?). Malformed text throws the reader's code, and so does a
\ number whose exponent, as stated or scaled to its digits, a signed cell
\ cannot hold (E-JSON-READ-NUMBER), whatever the answer. Any nesting the
\ reader accepts compares; scratch is freed on every path.
: VALUE= ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n b:ptr v:n :}
   a u REQUIRE-SOURCE
   b v REQUIRE-SOURCE
   a u b v  a u b v CHECKED-DEPTH  EQUAL-RUN ;

;package
