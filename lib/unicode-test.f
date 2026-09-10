require lib/test.f
require lib/unicode.f

package UNICODE-TEST
using UNICODE

: EQUAL ( ptr u8 n ptr u8 n -- )
   2over 2over CASEFOLD= TTRUE
   2swap CASEFOLD= TTRUE ;

: DIFFERENT ( ptr u8 n ptr u8 n -- )
   2over 2over CASEFOLD= TFALSE
   2swap CASEFOLD= TFALSE ;

: INVALID ( ptr u8 n -- )
   [: 2dup s" " CASEFOLD= drop ;] catch E-UTF8 T=
   [: 2dup s" " 2swap CASEFOLD= drop ;] catch E-UTF8 T=
   2drop ;

: BAD-UTF8 ( -- )
   s" x" drop -1 INVALID
   s\" \x80" INVALID
   s\" \xC0\xAF" INVALID
   s\" \xC2" INVALID
   s\" \xE2\x82" INVALID
   s\" \xF0\x9F\x98" INVALID
   s\" \xED\xA0\x80" INVALID
   s\" \xF4\x90\x80\x80" INVALID
   s\" x\xFF" INVALID ;

T-RESET
s" " s" " EQUAL
s" AbCd" s" aBcD" EQUAL
s" УКРАЇНА Ґ Є І Ї" s" україна ґ є і ї" EQUAL
s" Straße" s" STRASSE" EQUAL
s" ẞ" s" ss" EQUAL
s" Σσς" s" σσσ" EQUAL
s" ﬃ" s" FFI" EQUAL
s" İ" s" i̇" EQUAL
s" I" s" i" EQUAL
s" K" s" k" EQUAL
s" 😀名" s" 😀名" EQUAL
s\" A\zB" s\" a\zb" EQUAL
s" I" s" ı" DIFFERENT
s" İ" s" i" DIFFERENT
s" é" s" é" DIFFERENT
s" ab" s" abc" DIFFERENT
s" " s" x" DIFFERENT
s\" a\zB" s\" a\zC" DIFFERENT
$20 WHITE-SPACE? TTRUE
$A0 WHITE-SPACE? TTRUE
$2009 WHITE-SPACE? TTRUE
$202F WHITE-SPACE? TTRUE
$3000 WHITE-SPACE? TTRUE
$200B WHITE-SPACE? TFALSE
$FEFF WHITE-SPACE? TFALSE
$D800 WHITE-SPACE? TFALSE
BAD-UTF8
T-REPORT
;using
;package
