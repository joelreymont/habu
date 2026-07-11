\ imagedisasm.f - disassemble a raw executable slice with Habu's ARM64 decoder.
\ Run: bin/hb --load lib/errors.f lib/string.f src/arch/arm64/disasm.f tools/imagedisasm.f -- image offset count

require lib/adt/option.f                 \ option<n> for the number parsers (switchover wave A)

1024 constant IMGD-PATH-CAP
144 constant IMGD-STAT-U
$7FFFFFFFFFFFFFF constant IMGD-HEX-MAX-PRE

create IMGD-PATH IMGD-PATH-CAP 1 + allot
create IMGD-STAT IMGD-STAT-U allot

variable IMGD-IMG
variable IMGD-LEN
variable IMGD-FD
variable IMGD-OFF
variable IMGD-N
variable IMGD-HEX-I
variable IMGD-HEX-ACC
variable IMGD-HEX-DIG
variable IMGD-LOWER

: IMGD-IMG-FIELD ( -- ptr ptr u8 )
   IMGD-IMG 0 ptr-field ;

: IMGD-IMG@ ( -- ptr u8 )
   IMGD-IMG-FIELD @ ;

: IMGD-IMG! ( ptr u8 -- )
   IMGD-IMG-FIELD ! ;

: IMGD-USAGE ( -- )
   s" usage: imagedisasm image offset count" 64 die ;

: IMGD-RANGE-FAIL ( -- )
   s" imagedisasm: range outside image" 74 die ;

: IMGD-ZPATH ( ptr u8 n -- )
   {: a:ptr u :}
   u IMGD-PATH-CAP > IF s" imagedisasm: path too long" 74 die THEN
   0 BEGIN dup u < WHILE
      dup a + c@ over IMGD-PATH + c!
      1 +
   REPEAT drop
   0 IMGD-PATH u + c! ;

TRUSTED: IMGD-MMAP-PTR ( n -- ptr u8 )
   dup 0 < IF IMGD-FD @ close s" imagedisasm: mmap failed" 74 die THEN ;

: IMGD-READ ( ptr u8 n -- )
   IMGD-ZPATH
   IMGD-PATH IMGD-STAT stat64 0 < IF s" imagedisasm: stat failed" 74 die THEN
   IMGD-STAT 96 + @ IMGD-LEN !
   IMGD-LEN @ 0 > 0= IF s" imagedisasm: empty image" 74 die THEN
   IMGD-PATH 0 0 open IMGD-FD !
   IMGD-FD @ 0 < IF s" imagedisasm: open failed" 74 die THEN
   0 IMGD-LEN @ 1 2 IMGD-FD @ 0 mmap IMGD-MMAP-PTR IMGD-IMG!
   IMGD-FD @ close ;

: IMGD-HEX-DIGIT ( n -- option<n> )   \ SOME hex digit value, else NONE
   {: c:n :}
   c STR-ZERO >= c 57 <= and IF c STR-ZERO - OPTION:SOME EXIT THEN
   c ASCII-LOWER IMGD-LOWER !
   IMGD-LOWER @ 97 >= IMGD-LOWER @ 102 <= and IF IMGD-LOWER @ 87 - OPTION:SOME EXIT THEN
   OPTION:NONE ;

: IMGD-HEX-STEP ( n n -- option<n> )   \ SOME accumulated value, NONE on overflow
   {: acc:n digit:n :}
   acc IMGD-HEX-MAX-PRE > IF OPTION:NONE EXIT THEN
   acc 16 * digit + OPTION:SOME ;

: IMGD-PARSE-HEX ( ptr u8 n -- option<n> )   \ SOME parsed hex value, else NONE
   {: a:ptr u:n :}
   u 0= IF OPTION:NONE EXIT THEN
   0 IMGD-HEX-I !
   0 IMGD-HEX-ACC !
   BEGIN IMGD-HEX-I @ u < WHILE
      a IMGD-HEX-I @ + c@ IMGD-HEX-DIGIT MATCH option
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

: IMGD-HEX-BODY ( ptr u8 n -- ptr u8 n bool )
   {: a:ptr u :}
   u 1 > IF
      a c@ 36 = IF a 1 + u 1 - STR-TRUE EXIT THEN
   THEN
   u 2 > IF
      a c@ STR-ZERO = a 1 + c@ ASCII-LOWER 120 = and IF
         a 2 + u 2 - STR-TRUE EXIT
      THEN
   THEN
   a u STR-FALSE ;

: IMGD>NUMBER? ( ptr u8 n -- option<n> )   \ SOME parsed $hex/0xhex/decimal, else NONE
   IMGD-HEX-BODY IF IMGD-PARSE-HEX EXIT THEN
   STR>NUMBER? ;

: IMGD-POS-NUM ( ptr u8 n -- n )
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
   IMGD-IMG@ IMGD-OFF @ + IMGD-N @ DISASM
   cr ;

: IMGD-RUN? ( -- )
   SCRIPT-ARGC 0 > IF IMGD-MAIN THEN ;

IMGD-RUN?
