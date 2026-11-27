\ line-break-test.f - UNICODE-LINE-BREAK:ONE-LINE+ appends after the bytes a
\ vector holds, each mandatory line break one space and every other byte kept.

require src/lib/test/assert.f
require src/lib/heap.f
require src/lib/vector.f
require src/lib/unicode/line-break.f

package UNICODE-LINE-BREAK-TEST

\ Appends the text to a vector holding x; the result is want.
: ASSERT-LINE ( str str -- ) {: in:str want:str :}
   HEAP:ALLOCATOR 0 [type] u8 VEC:NEW {: v:^ :}
   s" x" v VEC:APPEND
   in v UNICODE-LINE-BREAK:ONE-LINE+
   v VEC:BYTES want T$=
   v VEC:DELETE ;

: CR-LF ( -- )
   S\" a\x0D\x0Ab" s" xa b" ASSERT-LINE
   S\" a\x0D\x0D\x0Ab" s" xa  b" ASSERT-LINE
   S\" a\x0Db\x0Ac" s" xa b c" ASSERT-LINE ;

: VT-FF ( -- )
   S\" a\x0Bb\x0Cc" s" xa b c" ASSERT-LINE ;

: SEPARATORS ( -- )
   S\" a\xC2\x85b" s" xa b" ASSERT-LINE
   S\" a\xE2\x80\xA8b" s" xa b" ASSERT-LINE
   S\" a\xE2\x80\xA9b" s" xa b" ASSERT-LINE ;

\ Bytes that start no break are kept, invalid UTF-8 included.
: KEPT ( -- )
   S\" a\x85b" S\" xa\x85b" ASSERT-LINE
   S\" a\xE2\x80" S\" xa\xE2\x80" ASSERT-LINE
   s" " s" x" ASSERT-LINE
   S\" a\x09\x0E\xC2\xA0\xE2\x80\x99\xE2\x80\xAAb"
   S\" xa\x09\x0E\xC2\xA0\xE2\x80\x99\xE2\x80\xAAb" ASSERT-LINE ;

public

: RUN ( -- )
   T-RESET
   CR-LF VT-FF SEPARATORS KEPT ;

;package

UNICODE-LINE-BREAK-TEST:RUN
T-REPORT
