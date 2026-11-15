\ pointer.f - RFC 6901 JSON Pointers: POINTER? and POINTER-SPAN. A file of
\ package JSON (src/lib/json.f).
\
\ POINTER-SPAN reads the text once with the pull reader. From the top value it
\ takes the pointer's tokens in order: on an object READ-FIND-KEY stops on the
\ value of the first member the decoded token names; on an array the reader
\ skips the elements before the index. The named value's span is taken where
\ the reader stands, then READ-FINISH reads the rest, so a text that is not one
\ valid JSON value throws the reader's code whatever the pointer names.
\ STORAGE CLASS. CALLER-OWNED: the caller owns the text and the pointer.
\ POINTER-SPAN holds its cursor and the decoded token in a mapping of its own,
\ released before it answers or throws. No module state.

require src/lib/error.f
require src/lib/memory.f
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
: ESCAPES-OK? ( ptr u8 n -- bool )
   {: p:ptr u:n :}
   u 0 ?do
      p i + c@ TILDE = if
         i 1+ u >= if false unloop exit then
         p i 1+ + c@ dup ZERO = swap DIGIT-ONE = or
         0= if false unloop exit then
      then
   loop true ;

\ The end of the token that starts at from: the next slash, or u.
: TOKEN-END ( ptr u8 n n -- n )
   {: p:ptr u:n from:n :}
   from begin
      dup u < if dup p + c@ SLASH <> else false then
   while 1+ repeat ;

\ The byte at in, an escape decoded, and the offset past it. POINTER? passed
\ every tilde, so the byte after one is 0 or 1.
: UNESCAPE-STEP ( ptr u8 n -- n n )
   {: p:ptr in:n :}
   p in + c@ {: c:n :}
   c TILDE <> if c in 1+ exit then
   p in 1+ + c@ ZERO = if TILDE else SLASH then in 2 + ;

: UNESCAPE-PUT ( n n ptr u8 ptr u8 -- n n )
   {: in:n out:n p:ptr room:ptr :}
   p in UNESCAPE-STEP {: c:n next:n :}
   c room out + c!
   next out 1+ ;

\ Decodes token bytes from..end into room and answers their count: ~1 is a
\ slash, ~0 a tilde, in one pass, so ~01 is a tilde then 1.
: UNESCAPE ( ptr u8 n n ptr u8 -- n )
   {: p:ptr from:n end:n room:ptr :}
   from 0 begin over end < while p room UNESCAPE-PUT repeat nip ;

\ Index k, digit byte c: the index with c appended, false for a byte that is
\ not a digit or an index at or past cap.
: INDEX-STEP ( n n n -- n bool )
   {: k:n c:n cap:n :}
   c STR-DIGIT? 0= if 0 false exit then
   k 10 * c ZERO - + {: next:n :}
   next cap >= if 0 false exit then
   next true ;

\ The array index a token names: 0, or a digit 1-9 then digits, below cap;
\ false for any other token. An index stays below cap, so it never overflows.
: INDEX? ( ptr u8 n n -- n bool )
   {: t:ptr len:n cap:n :}
   len 0= if 0 false exit then
   len 1 > t c@ ZERO = and if 0 false exit then
   0 len 0 ?do
      t i + c@ cap INDEX-STEP 0= if false unloop exit then
   loop true ;

\ Skips the array's next element; false at the array's end.
: SKIP-ELEMENT ( JSON:reader -- JSON:reader bool )
   READ-NEXT T-ARR-END = if false exit then
   READ-SKIP-VALUE true ;

\ Element k of the array the reader has just opened, the reader stopping on
\ its first token; false past the last element.
: INTO-ARRAY ( JSON:reader n -- JSON:reader bool )
   {: k:n :}
   k 0 ?do SKIP-ELEMENT 0= if false unloop exit then loop
   READ-NEXT T-ARR-END <> ;

\ Moves the reader from the value it is on into the place the decoded token
\ names there; false when there is none. cap bounds an index.
: DESCEND ( JSON:reader ptr u8 n n -- JSON:reader bool )
   {: t:ptr len:n cap:n :}
   READ-TOKEN {: kind:n :}
   kind T-OBJ = if t len READ-FIND-KEY exit then
   kind T-ARR <> if false exit then
   t len cap INDEX? 0= if drop false exit then
   INTO-ARRAY ;

\ Takes the token after the slash at at; answers the offset after the token
\ and whether it names a place.
: TOKEN-STEP ( JSON:reader n ptr u8 n ptr u8 n -- JSON:reader n bool )
   {: at:n p:ptr u:n room:ptr cap:n :}
   p u at 1+ TOKEN-END {: end:n :}
   p at 1+ end room UNESCAPE {: len:n :}
   room len cap DESCEND end swap ;

\ Takes every token of the pointer from the value the reader is on; false at
\ the first that names no place.
: FOLLOW ( JSON:reader ptr u8 n ptr u8 n -- JSON:reader bool )
   {: p:ptr u:n room:ptr cap:n :}
   0 begin dup u < while
      p u room cap TOKEN-STEP 0= if drop false exit then
   repeat drop true ;

\ The place's offset and length in the text, and whether it is there. The
\ reader reads the whole text on either answer.
: LOCATE ( ptr cursor ptr u8 n ptr u8 n ptr u8 -- n n bool )
   {: rec:ptr text:ptr tu:n p:ptr u:n room:ptr :}
   rec text tu READ-INIT
   READ-NEXT drop
   p u room tu FOLLOW if
      READ-ANY-SPAN$ {: at:ptr len:n :}
      READ-FINISH at text - len true exit
   then
   READ-FINISH 0 0 false ;

\ The cursor record, then room for the decoded token, which is never longer
\ than the pointer.
: SPAN-SLOT ( n n bool SPAN:span<u8> ptr u8 n ptr u8 n -- n n bool SPAN:span<u8> ptr u8 n ptr u8 n )
   {: scratch text:ptr tu:n p:ptr u:n :}
   drop drop drop
   scratch SPAN:$ drop {: base:ptr :}
   base RECORD text tu p u base CURSOR-BYTES + LOCATE
   scratch text tu p u ;

: POINTER-FREE ( SPAN:span<u8> -- )
   {: scratch :}
   scratch SPAN:$ drop RECORD MORE-RELEASE
   scratch MEM:FREE-SPAN ;

public

\ Whether the bytes are an RFC 6901 pointer: empty, or each reference token
\ after a slash, every tilde followed by 0 or 1, and the bytes UTF-8. False
\ for a negative length or a null address with a positive one.
: POINTER? ( ptr u8 n -- bool )
   {: p:ptr u:n :}
   u 0= if true exit then
   u 0 < if false exit then
   p 0= if false exit then
   p c@ SLASH <> if false exit then
   p u ESCAPES-OK? 0= if false exit then
   p u UTF8:VALID? ;

\ The offset and length in text, one JSON value, of the value the pointer
\ names, as written (READ-ANY-SPAN$), and true; 0 0 false for no such place.
\ The empty pointer names the whole value. A token, ~1 and ~0 decoded, names
\ the first member of an object whose decoded name it equals (RFC 6901 leaves
\ repeated names open), or on an array element k for 0 or a digit 1-9 then
\ digits, when k is below its length; - and any other token, and any token on
\ a scalar, name no place. A pointer POINTER? rejects throws E-JSON-POINTER
\ before the text is read; a text that is not one valid JSON value throws the
\ reader's code.
: POINTER-SPAN ( ptr u8 n ptr u8 n -- n n bool )
   {: text:ptr tu:n p:ptr u:n :}
   p u POINTER? 0= if E-JSON-POINTER throw then
   text tu REQUIRE-SOURCE
   CURSOR-BYTES u + MEM:BYTES-ALLOC-LEN MEM:ALLOC-SPAN {: scratch :}
   0 0 false scratch text tu p u [: SPAN-SLOT ;] catch
   {: at len found spare st su sp pu code:n :}
   scratch POINTER-FREE
   code 0= if at len found exit then
   code throw ;

;package
