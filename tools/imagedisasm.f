\ imagedisasm.f - disassemble a raw executable slice with Habu's ARM64 decoder.
\ Run: bin/hb --load tools/imagedisasm.f -- image offset count

require src/lib/string.f
require src/lib/adt/option.f
require src/arch/arm64/disasm.f
require src/lib/fs.f
require src/core/exit-status.f

package IMAGE-DISASM
private

$7FFFFFFFFFFFFFF constant IMGD-HEX-MAX-PRE

TYPED-VARIABLE IMGD-IMG ^u8
variable IMGD-LEN
variable IMGD-FD
variable IMGD-OFF
variable IMGD-N
variable IMGD-HEX-I
variable IMGD-HEX-ACC
variable IMGD-HEX-DIG
variable IMGD-LOWER

: IMGD-IMG@ ( -- ^u8 )
   IMGD-IMG @ ;

: IMGD-IMG! ( ^u8 -- )
   IMGD-IMG ! ;

: IMGD-USAGE ( -- )
   s" usage: imagedisasm image offset count" EXIT-STATUS:USAGE die ;

: IMGD-RANGE-FAIL ( -- )
   s" imagedisasm: range outside image" EXIT-STATUS:USAGE die ;

\ mmap answers the mapping's address as an integer; a failed map dies first.
CAST: MAP>BYTES ( n -- ^u8 )
: IMGD-MMAP-PTR ( n -- ^u8 )
   dup 0 < IF IMGD-FD @ close s" imagedisasm: mmap failed" EXIT-STATUS:SYSTEM die THEN MAP>BYTES ;

: IMGD-READ ( str -- )
   FS:OPEN-SIZED 0= IF 2drop s" imagedisasm: open failed" EXIT-STATUS:IOERR die THEN
   IMGD-LEN !  FD>N IMGD-FD !
   IMGD-LEN @ 0 > 0= IF IMGD-FD @ close s" imagedisasm: empty image" EXIT-STATUS:DATAERR die THEN
   0 IMGD-LEN @ 1 2 IMGD-FD @ 0 mmap IMGD-MMAP-PTR IMGD-IMG!
   IMGD-FD @ close ;

: IMGD-HEX-DIGIT ( n -- option<n> )
   {: c:n :}
   c STR:ZERO >= c 57 <= and IF c STR:ZERO - OPTION:SOME EXIT THEN
   c STR:ASCII-LOWER IMGD-LOWER !
   IMGD-LOWER @ 97 >= IMGD-LOWER @ 102 <= and IF IMGD-LOWER @ 87 - OPTION:SOME EXIT THEN
   OPTION:NONE ;

: IMGD-HEX-STEP ( n n -- option<n> )   \ NONE on overflow
   {: acc:n digit:n :}
   acc IMGD-HEX-MAX-PRE > IF OPTION:NONE EXIT THEN
   acc 16 * digit + OPTION:SOME ;

: IMGD-PARSE-HEX ( str -- option<n> )
   {: s:str :}
   s STR:LEN 0= IF OPTION:NONE EXIT THEN
   0 IMGD-HEX-I !
   0 IMGD-HEX-ACC !
   BEGIN IMGD-HEX-I @ s STR:LEN < WHILE
      s STR:DATA IMGD-HEX-I @ + c@ IMGD-HEX-DIGIT MATCH option
        none OF OPTION:NONE EXIT ENDOF
        some OF IMGD-HEX-DIG ! ENDOF
      ;MATCH
      IMGD-HEX-ACC @ IMGD-HEX-DIG @ IMGD-HEX-STEP MATCH option
        none OF OPTION:NONE EXIT ENDOF
        some OF IMGD-HEX-ACC ! ENDOF
      ;MATCH
      IMGD-HEX-I @ 1 + IMGD-HEX-I !
   REPEAT
   IMGD-HEX-ACC @ OPTION:SOME ;

: IMGD-HEX-BODY ( str -- str bool )
   {: s:str :}
   s STR:LEN 1 > IF
      s STR:DATA c@ 36 = IF
         s STR:DATA 1 + s STR:LEN 1 - >str STR:TRUE EXIT
      THEN
   THEN
   s STR:LEN 2 > IF
      s STR:DATA c@ STR:ZERO = s STR:DATA 1 + c@ STR:ASCII-LOWER 120 = and IF
         s STR:DATA 2 + s STR:LEN 2 - >str STR:TRUE EXIT
      THEN
   THEN
   s STR:FALSE ;

public

: IMGD>NUMBER? ( str -- option<n> )   \ accepts $hex, 0xhex and decimal
   IMGD-HEX-BODY IF IMGD-PARSE-HEX EXIT THEN
   STR:>NUMBER? ;

private

: IMGD-POS-NUM ( str -- n )
   IMGD>NUMBER? MATCH option
     none OF IMGD-USAGE ENDOF
     some OF ENDOF
   ;MATCH
   dup 0 < IF IMGD-USAGE THEN ;

: IMGD-PARSE-ARGS ( -- )
   SCRIPT-ARGC 3 <> IF IMGD-USAGE THEN
   0 SCRIPT-ARGV$ IMGD-READ
   1 SCRIPT-ARGV$ IMGD-POS-NUM IMGD-OFF !
   2 SCRIPT-ARGV$ IMGD-POS-NUM IMGD-N !
   IMGD-N @ 0 <= IF IMGD-USAGE THEN ;

: IMGD-CHECK-RANGE ( -- )
   IMGD-OFF @ 0 < IF IMGD-RANGE-FAIL THEN
   IMGD-OFF @ IMGD-LEN @ > IF IMGD-RANGE-FAIL THEN
   IMGD-N @ IMGD-LEN @ 4 / > IF IMGD-RANGE-FAIL THEN
   IMGD-N @ 4 * IMGD-LEN @ IMGD-OFF @ - > IF IMGD-RANGE-FAIL THEN ;

: IMGD-MAIN ( -- )
   IMGD-PARSE-ARGS
   IMGD-CHECK-RANGE
   IMGD-IMG@ IMGD-OFF @ + IMGD-N @ 4 * >str
   A64DIS:DISASM ;


: IMGD-RUN? ( -- )
   SCRIPT-ARGC 0 > IF IMGD-MAIN THEN ;

IMGD-RUN?

;package
