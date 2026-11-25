\ pointer.f - RFC 6901 JSON Pointers: POINTER? and POINTER-SPAN. A file of
\ package JSON (src/lib/json.f).
\
\ SEEK takes the pointer's tokens in order from the value a cursor reads
\ next: on an object FIND-INNER stops on the value of the first member the
\ decoded token names; on an array the cursor skips the elements before the
\ index. The named value's span is taken where the cursor stands.
\ The empty pointer names the value itself. A token, ~1 and ~0 decoded into
\ a vector, names the first member of an object whose decoded name it equals
\ (RFC 6901 leaves repeated names open), or on an array element k for 0 or a
\ digit 1-9 then digits, when k is below its length; - and any other token,
\ and any token on a scalar, name no place. The cursor's end bounds an index.
\ POINTER-SPAN runs SEEK on a reader over the whole text, then READ-FINISH
\ reads the rest, so a text that is not one valid JSON value throws the
\ reader's code whatever the pointer names.
\ STORAGE CLASS. CALLER-OWNED: the caller owns the text and the pointer.
\ POINTER-SPAN holds its cursor in a MEM scope and the decoded token in a heap
\ vector, both released before it answers or throws. No module state.

require src/lib/error.f
require src/lib/memory.f
require src/lib/vector.f
require src/lib/heap.f
require src/lib/span.f
require src/lib/string.f
require src/lib/utf8-scalar.f
require src/lib/json/read.f

\ JSON Pointers: -9870..-9879. The codes stay global names, as the reader's do.
-9870 constant E-JSON-POINTER-FIRST
-9879 constant E-JSON-POINTER-LAST
-9870 s" the text is not an RFC 6901 JSON Pointer" ERROR:CODE E-JSON-POINTER

package JSON

private

47 constant SLASH
49 constant DIGIT-ONE
126 constant TILDE

\ Whether every tilde in the bytes is followed by 0 or 1.
: ESCAPES-OK? ( str -- bool )
   {: s:str :}
   s STR:LEN 0 ?do
      s STR:DATA i + c@ TILDE = if
         i 1+ s STR:LEN >= if false unloop exit then
         s STR:DATA i 1+ + c@ dup ZERO = swap DIGIT-ONE = or
         0= if false unloop exit then
      then
   loop true ;

\ The end of the token that starts at from: the next slash, or u.
: TOKEN-END ( str n -- n )
   {: s:str from:n :}
   from begin
      dup s STR:LEN < if dup s STR:DATA + c@ SLASH <> else false then
   while 1+ repeat ;

\ The byte at in, an escape decoded, and the offset past it. POINTER? passed
\ every tilde, so the byte after one is 0 or 1.
: UNESCAPE-STEP ( ^u8 n -- n n )
   {: p:^ in:n :}
   p in + c@ {: c:n :}
   c TILDE <> if c in 1+ exit then
   p in 1+ + c@ ZERO = if TILDE else SLASH then in 2 + ;

: UNESCAPE-PUT ( n ^u8 ^VEC:vector<u8> -- n )
   {: in:n p:^ out:^ :}
   p in UNESCAPE-STEP {: c:n next:n :}
   c out VEC:PUSH c!
   next ;

\ Empties the vector, decodes token bytes from..end into it and answers them:
\ ~1 is a slash, ~0 a tilde, in one pass, so ~01 is a tilde then 1.
: UNESCAPE ( ^u8 n n ^VEC:vector<u8> -- str )
   {: p:^ from:n end:n out:^ :}
   out 0 VEC:RESIZE
   from begin dup end < while p out UNESCAPE-PUT repeat drop
   out VEC:BYTES ;

\ Index k, digit byte c: the index with c appended, false for a byte that is
\ not a digit or an index at or past cap.
: INDEX-STEP ( n n n -- n bool )
   {: k:n c:n cap:n :}
   c STR:DIGIT? 0= if 0 false exit then
   k 10 * c ZERO - + {: next:n :}
   next cap >= if 0 false exit then
   next true ;

\ The array index a token names: 0, or a digit 1-9 then digits, below cap;
\ false for any other token. An index stays below cap, so it never overflows.
: INDEX? ( str n -- n bool )
   {: s:str cap:n :}
   s STR:LEN 0= if 0 false exit then
   s STR:LEN 1 > s STR:DATA c@ ZERO = and if 0 false exit then
   0 s STR:LEN 0 ?do
      s STR:DATA i + c@ cap INDEX-STEP 0= if false unloop exit then
   loop true ;

\ Skips the array's next element; false at the array's end.
: SKIP-ELEMENT ( ^cursor -- bool ) {: state:^ :}
   state NEXT-INNER T-ARR-END = if false exit then
   state SKIP-INNER true ;

\ Element k of the array the cursor has just opened, the cursor stopping on
\ its first token; false past the last element.
: INTO-ARRAY ( ^cursor n -- bool )
   {: state:^ k:n :}
   k 0 ?do state SKIP-ELEMENT 0= if false unloop exit then loop
   state NEXT-INNER T-ARR-END <> ;

\ Moves the cursor from the value it is on into the place the decoded token
\ names there; false when there is none. cap bounds an index.
: DESCEND ( ^cursor str n -- bool )
   {: state:^ s:str cap:n :}
   state CURSOR-KIND @ {: kind:n :}
   kind T-OBJ = if state s FIND-INNER exit then
   kind T-ARR <> if false exit then
   s cap INDEX? 0= if drop false exit then
   {: k:n :} state k INTO-ARRAY ;

\ Takes the token after the slash at at; answers the offset after the token
\ and whether it names a place.
: TOKEN-STEP ( ^cursor n str ^VEC:vector<u8> n -- n bool )
   {: state:^ at:n s:str out:^ cap:n :}
   s at 1+ TOKEN-END {: end:n :}
   state  s STR:DATA at 1+ end out UNESCAPE  cap DESCEND end swap ;

\ Takes every token of the pointer from the value the cursor is on; false at
\ the first that names no place.
: FOLLOW ( ^cursor str ^VEC:vector<u8> n -- bool )
   {: state:^ s:str out:^ cap:n :}
   0 begin dup s STR:LEN < while
      state swap s out cap TOKEN-STEP 0= if drop false exit then
   repeat drop true ;

\ The offset and length of the value the pointer names from the value the
\ cursor reads next, and true; 0 0 false for no such place.
: SEEK ( ^cursor str ^VEC:vector<u8> -- n n bool )
   {: state:^ s:str out:^ :}
   state NEXT-INNER drop
   state s out state CURSOR-SRC-LEN @ FOLLOW
   0= if 0 0 false exit then
   state ANY-SPAN-INNER {: span:str :}
   span STR:DATA state CURSOR-SRC @ - span STR:LEN true ;

\ The place's offset and length in the text, and whether it is there. The
\ reader reads the whole text on either answer.
: LOCATE ( ^cursor str str ^VEC:vector<u8> -- n n bool )
   {: rec:^ text:str s:str out:^ :}
   rec text READ-INIT
   READER>STATE s out SEEK {: at:n len:n found:bool :}
   READ-FINISH at len found ;

: LOCATE-WITH ( str str ^cursor -- n n bool )
   {: text:str s:str rec:^ :}
   HEAP:ALLOCATOR s STR:LEN [type] u8 VEC:NEW {: out:^ :}
   [: [: rec text s out LOCATE ;] [: rec MORE-RELEASE ;] finally ;]
   [: out VEC:DELETE ;] finally ;

public

\ Whether the bytes are an RFC 6901 pointer: empty, or each reference token
\ after a slash, every tilde followed by 0 or 1, and the bytes UTF-8. False
\ for a negative length or a null address with a positive one.
: POINTER? ( str -- bool )
   {: s:str :}
   s STR:LEN 0= if true exit then
   s STR:LEN 0 < if false exit then
   s STR:DATA 0= if false exit then
   s STR:DATA c@ SLASH <> if false exit then
   s ESCAPES-OK? 0= if false exit then
   s UTF8:VALID? ;

\ The offset and length in text of the value the pointer names, and true; 0 0
\ false for no such place. A pointer POINTER? rejects throws E-JSON-POINTER.
: POINTER-SPAN ( str str -- n n bool )
   {: text:str s:str :}
   s POINTER? 0= if E-JSON-POINTER throw then
   text REQUIRE-SOURCE
   text s [type] cursor [: LOCATE-WITH ;] MEM:WITH-RECORD ;

;package
