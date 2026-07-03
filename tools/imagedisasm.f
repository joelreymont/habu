\ imagedisasm.f - disassemble a raw executable slice with Habu's ARM64 decoder.
\ Run: bin/hb --load lib/errors.f lib/string.f src/arch/arm64/disasm.f tools/imagedisasm.f -- image offset count

s" src/core/result.f" required

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

: IMGD-MMAP-RESULT ( -- result<ptr u8,n> )
   0 IMGD-LEN @ 1 2 IMGD-FD @ 0 mmap RESULT:MMAP>BYTES ;

: IMGD-MMAP-ERR ( n -- )
   drop IMGD-FD @ close s" imagedisasm: mmap failed" 74 die ;

: IMGD-MMAP ( -- ptr u8 )
   IMGD-MMAP-RESULT
   [: ;] [: IMGD-MMAP-ERR ;] RESULT:CASE ;

: IMGD-READ ( ptr u8 n -- )
   IMGD-ZPATH
   IMGD-PATH IMGD-STAT stat64 0 < IF s" imagedisasm: stat failed" 74 die THEN
   IMGD-STAT 96 + @ IMGD-LEN !
   IMGD-LEN @ 0 > 0= IF s" imagedisasm: empty image" 74 die THEN
   IMGD-PATH 0 0 open IMGD-FD !
   IMGD-FD @ 0 < IF s" imagedisasm: open failed" 74 die THEN
   IMGD-MMAP IMGD-IMG!
   IMGD-FD @ close ;

: IMGD-HEX-DIGIT ( n -- n bool )
   {: c :}
   c STR-ZERO >= c 57 <= and IF c STR-ZERO - STR-TRUE EXIT THEN
   c ASCII-LOWER IMGD-LOWER !
   IMGD-LOWER @ 97 >= IMGD-LOWER @ 102 <= and IF IMGD-LOWER @ 87 - STR-TRUE EXIT THEN
   0 STR-FALSE ;

: IMGD-HEX-STEP ( n n -- n bool )
   {: acc digit :}
   acc IMGD-HEX-MAX-PRE > IF 0 STR-FALSE EXIT THEN
   acc 16 * digit + STR-TRUE ;

: IMGD-PARSE-HEX ( ptr u8 n -- n bool )
   {: a:ptr u :}
   u 0= IF 0 STR-FALSE EXIT THEN
   0 IMGD-HEX-I !
   0 IMGD-HEX-ACC !
   BEGIN IMGD-HEX-I @ u < WHILE
      a IMGD-HEX-I @ + c@ IMGD-HEX-DIGIT 0= IF drop 0 STR-FALSE EXIT THEN
      IMGD-HEX-DIG !
      IMGD-HEX-ACC @ IMGD-HEX-DIG @ IMGD-HEX-STEP 0= IF drop 0 STR-FALSE EXIT THEN
      IMGD-HEX-ACC !
      IMGD-HEX-I @ 1 + IMGD-HEX-I !
   REPEAT
   IMGD-HEX-ACC @ STR-TRUE ;

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

: IMGD>NUMBER? ( ptr u8 n -- n bool )
   IMGD-HEX-BODY IF IMGD-PARSE-HEX EXIT THEN
   STR>NUMBER? ;

: IMGD-POS-NUM ( ptr u8 n -- n )
   IMGD>NUMBER? 0= IF drop IMGD-USAGE THEN
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
