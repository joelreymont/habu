require lib/test.f
require lib/property.f
require lib/unicode.f

package UNICODE-TEST
using UNICODE
using PROP
256 constant CAPACITY
create OUTPUT CAPACITY allot
create SECOND-BUF CAPACITY allot

: FOLDED= ( ptr u8 n ptr u8 n -- )
   {: expected:ptr expected-size:n :}
   2dup FOLDED-BYTES expected-size T=
   OUTPUT CAPACITY UNICODE:FOLD
   OUTPUT swap expected expected-size T$= ;

: CANARY! ( -- )
   CAPACITY 0 ?do $55 OUTPUT i + c! loop ;

: CANARY= ( -- )
   CAPACITY 0 ?do OUTPUT i + c@ $55 T= loop ;

: FOLD-PREFLIGHT ( -- )
   CANARY!
   [: s" İ" OUTPUT 2 UNICODE:FOLD drop ;] E-CAPACITY TTHROWSQ CANARY=
   [: s" x" OUTPUT -1 UNICODE:FOLD drop ;] E-CAPACITY TTHROWSQ CANARY=
   [: s\" x\xFF" OUTPUT CAPACITY UNICODE:FOLD drop ;] E-UTF8 TTHROWSQ CANARY=
   [: s\" \xC0\xAF" FOLDED-BYTES drop ;] E-UTF8 TTHROWSQ
   s" Straße" s" strasse" FOLDED= ;

: CHUNK$ ( -- ptr u8 n )
   8 RND% case
      0 of s" ẞ" endof
      1 of s" İ" endof
      2 of s" УКРАЇНА" endof
      3 of s" ﬃ" endof
      4 of s" Σςσ" endof
      5 of s" é😀" endof
      6 of s\" A\zB" endof
      s" abc 019" rot
   endcase ;

: RANDOM-FOLDS ( -- )
   913 2048 RUN-RESET
   COUNT@ 0 ?do
      PROP:BUF-RESET 16 RND% 0 ?do CHUNK$ BUF+ loop
      BUF$ OUTPUT CAPACITY UNICODE:FOLD {: size:n :}
      BUF$ FOLDED-BYTES size T=
      BUF$ OUTPUT size CASEFOLD= TTRUE
      OUTPUT size SECOND-BUF CAPACITY UNICODE:FOLD {: repeated:n :}
      OUTPUT size SECOND-BUF repeated T$=
   loop ;

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

: IMAGE-REBIND ( -- )
   2 0 ?do
      IMAGE-LIFECYCLE:PREPARE
      s" Straße" s" STRASSE" EQUAL
      s" İ" s" i̇" FOLDED=
   loop ;

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
s" " s" " FOLDED=
s" STRAẞE" s" strasse" FOLDED=
s" İ" s" i̇" FOLDED=
s" ẞ" s" ss" FOLDED=
s" УКРАЇНА" s" україна" FOLDED=
s\" A\zB" s\" a\zb" FOLDED=
FOLD-PREFLIGHT
RANDOM-FOLDS
IMAGE-REBIND
T-REPORT
;using
;using
;package
