require src/lib/test/assert.f
require src/lib/property.f
require src/lib/unicode.f

package UNICODE-TEST
256 constant CAPACITY
create OUTPUT CAPACITY allot
create SECOND-BUF CAPACITY allot

: FOLDED= ( str str -- )
   {: expected:str :}
   dup UNICODE:FOLDED-BYTES expected STR:LEN T=
   OUTPUT CAPACITY UNICODE:FOLD
   OUTPUT swap >str expected T$= ;

: LOWERED= ( str str -- )
   {: expected:str :}
   dup expected UNICODE:LOWERS-TO? TTRUE
   dup UNICODE:LOWERED-BYTES expected STR:LEN T=
   OUTPUT CAPACITY UNICODE:LOWER
   OUTPUT swap >str expected T$= ;

: CANARY! ( -- )
   CAPACITY 0 ?do $55 OUTPUT i + c! loop ;

: CANARY= ( -- )
   CAPACITY 0 ?do OUTPUT i + c@ $55 T= loop ;

: FOLD-PREFLIGHT ( -- )
   CANARY!
   [: s" İ" OUTPUT 2 UNICODE:FOLD drop ;] UNICODE:E-CAPACITY TTHROWSQ CANARY=
   [: s" x" OUTPUT -1 UNICODE:FOLD drop ;] UNICODE:E-CAPACITY TTHROWSQ CANARY=
   [: s\" x\xFF" OUTPUT CAPACITY UNICODE:FOLD drop ;] UNICODE:E-UTF8 TTHROWSQ CANARY=
   [: s\" \xC0\xAF" UNICODE:FOLDED-BYTES drop ;] UNICODE:E-UTF8 TTHROWSQ
   s" Straße" s" strasse" FOLDED= ;

: LOWER-PREFLIGHT ( -- )
   CANARY!
   [: s" İ" OUTPUT 2 UNICODE:LOWER drop ;] UNICODE:E-CAPACITY TTHROWSQ CANARY=
   [: s\" x\xFF" OUTPUT CAPACITY UNICODE:LOWER drop ;] UNICODE:E-UTF8 TTHROWSQ CANARY=
   [: s\" \xC0\xAF" UNICODE:LOWERED-BYTES drop ;] UNICODE:E-UTF8 TTHROWSQ
   [: s" xy" OUTPUT 1 UNICODE:LOWER drop ;] UNICODE:E-CAPACITY TTHROWSQ CANARY=
   [: s" x" STR:DATA -1 >str UNICODE:LOWERED-BYTES drop ;] UNICODE:E-UTF8 TTHROWSQ
   s" Az" s" Az" UNICODE:LOWERS-TO? TFALSE
   s" STRASSE" s" straße" UNICODE:LOWERS-TO? TFALSE
   s" Straße" s" Straße" UNICODE:LOWERS-TO? TFALSE
   [: s\" x\xFF" s" x" UNICODE:LOWERS-TO? drop ;] UNICODE:E-UTF8 TTHROWSQ
   [: s" x" s\" x\xFF" UNICODE:LOWERS-TO? drop ;] UNICODE:E-UTF8 TTHROWSQ ;

: SAME-LOWER ( -- )
   s" Ꭰ.txt" s" ꭰ.txt" UNICODE:LOWER= TTRUE
   s" Straße" s" strasse" UNICODE:LOWER= TFALSE
   s" ABC" s" abc" UNICODE:LOWER= TTRUE
   s" ΟΔΟΣ" s" οδος" UNICODE:LOWER= TTRUE
   [: s\" x\xFF" s" x" UNICODE:LOWER= drop ;] UNICODE:E-UTF8 TTHROWSQ
   [: s" x" s\" x\xFF" UNICODE:LOWER= drop ;] UNICODE:E-UTF8 TTHROWSQ ;

: CHUNK$ ( -- str )
   8 PROP:RND% case
      0 of s" ẞ" endof
      1 of s" İ" endof
      2 of s" УКРАЇНА" endof
      3 of s" ﬃ" endof
      4 of s" Σςσ" endof
      5 of s" é😀" endof
      6 of s\" A\zB" endof
      s" abc 019" swap
   endcase ;

: RANDOM-FOLDS ( -- )
   913 2048 PROP:RUN-RESET
   PROP:COUNT@ 0 ?do
      PROP:BUF-RESET 16 PROP:RND% 0 ?do CHUNK$ PROP:BUF+ loop
      PROP:BUF$ OUTPUT CAPACITY UNICODE:FOLD {: size:n :}
      PROP:BUF$ UNICODE:FOLDED-BYTES size T=
      PROP:BUF$ OUTPUT size >str UNICODE:CASEFOLD= TTRUE
      OUTPUT size >str SECOND-BUF CAPACITY UNICODE:FOLD {: repeated:n :}
      OUTPUT size >str SECOND-BUF repeated >str T$=
   loop ;

: EQUAL ( str str -- )
   over over UNICODE:CASEFOLD= TTRUE
   swap UNICODE:CASEFOLD= TTRUE ;

: DIFFERENT ( str str -- )
   over over UNICODE:CASEFOLD= TFALSE
   swap UNICODE:CASEFOLD= TFALSE ;

: INVALID ( str -- )
   [: dup s" " UNICODE:CASEFOLD= drop ;] catch UNICODE:E-UTF8 T=
   [: dup s" " swap UNICODE:CASEFOLD= drop ;] catch UNICODE:E-UTF8 T=
   drop ;

: BAD-UTF8 ( -- )
   s" x" STR:DATA -1 >str INVALID
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
s" @AZ[`az{ MiXeD" s" @az[`AZ{ mIxEd" EQUAL
s" I" s" ı" DIFFERENT
s" İ" s" i" DIFFERENT
s" é" s" é" DIFFERENT
s" ab" s" abc" DIFFERENT
s" " s" x" DIFFERENT
s\" a\zB" s\" a\zC" DIFFERENT
s" @" s" `" DIFFERENT
s" [" s" {" DIFFERENT
$20 UNICODE:WHITE-SPACE? TTRUE
$A0 UNICODE:WHITE-SPACE? TTRUE
$2009 UNICODE:WHITE-SPACE? TTRUE
$202F UNICODE:WHITE-SPACE? TTRUE
$3000 UNICODE:WHITE-SPACE? TTRUE
$200B UNICODE:WHITE-SPACE? TFALSE
$FEFF UNICODE:WHITE-SPACE? TFALSE
$D800 UNICODE:WHITE-SPACE? TFALSE
$301 UNICODE:MARK? TTRUE
$93E UNICODE:MARK? TTRUE
$20DD UNICODE:MARK? TTRUE
$E48 UNICODE:MARK? TTRUE
$41 UNICODE:MARK? TFALSE
$915 UNICODE:MARK? TFALSE
$30 UNICODE:MARK? TFALSE
$20 UNICODE:MARK? TFALSE
$DC00 UNICODE:MARK? TFALSE
$110000 UNICODE:MARK? TFALSE
BAD-UTF8
s" " s" " FOLDED=
s" STRAẞE" s" strasse" FOLDED=
s" İ" s" i̇" FOLDED=
s" ẞ" s" ss" FOLDED=
s" УКРАЇНА" s" україна" FOLDED=
s\" A\zB" s\" a\zb" FOLDED=
s" @AZ[`az{ MiXeD" s" @az[`az{ mixed" FOLDED=
FOLD-PREFLIGHT
s" " s" " LOWERED=
s" Straße" s" straße" LOWERED=
s" İ" s" i̇" LOWERED=
s" ΟΔΟΣ" s" οδος" LOWERED=
s" ΓΙΑΝΝΗΣ'S.doc" s" γιαννησ's.doc" LOWERED=
s" Α'Σ" s" α'ς" LOWERED=
s" ʰΣ" s" ʰσ" LOWERED=
s" ΑΣʰ" s" αςʰ" LOWERED=
s" ΑΣ" s" ας" LOWERED=
s" Σ" s" σ" LOWERED=
s" ΑΣΑ" s" ασα" LOWERED=
s\" A\zB" s\" a\zb" LOWERED=
s" @AZ[`az{ MiXeD" s" @az[`az{ mixed" LOWERED=
LOWER-PREFLIGHT
SAME-LOWER
RANDOM-FOLDS
T-REPORT
;package
