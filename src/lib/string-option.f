\ string-option.f - the STR words that answer option<a> and the string builders.
\ They load after the kernel: option<a> is an ENUM, and kernel files build ENUM
\ with STR's folded comparison; the builders throw error.f's E-STR-CAPACITY
\ instead of truncating, and the SB builder keeps its text in task data.
\ STORAGE CLASS: the SB builder is TASK-LOCAL (SB-BUF); the BUF-* family is
\ CALLER-OWNED, the builder for one shared by two tasks; the rest is pure.

s" src/lib/error.f" required
s" src/lib/adt/option.f" required
s" src/lib/string.f" required

STRING-ABI:SB-BUF-BYTES constant SB-CAP

package STR
private EXPORT BOOT:data-base
EXPORT STRING-ABI:SB-BUF-OFF
EXPORT STRING-ABI:SB-LEN-OFF
public
\ Here a bare = or < is STR's string word, so integer tests avoid both.

: INDEX-OF ( str n -- option<idx> ) {: s:str c:n :}   \ SOME first index of byte c, else NONE
   0 begin s STR:LEN over > while
      dup s STR:DATA + c@ c <> 0= if >IDX OPTION:SOME exit then
      1+
   repeat drop OPTION:NONE ;

: PARSE-POS ( str -- option<n> ) {: s:str :}   \ SOME parsed nonnegative bounded i64, else NONE
   s DIGITS? 0= if OPTION:NONE exit then
   s MAX-I64$ I64-DIGITS >str DIGITS<= 0= if OPTION:NONE exit then
   0 0 begin s STR:LEN over > while
      swap BASE * over s STR:DATA + c@ DIGIT-VALUE + swap
      1+
   repeat drop OPTION:SOME ;

: PARSE-NEG ( str -- option<n> ) {: s:str :}   \ SOME parsed negative bounded i64, else NONE
   s DIGITS? 0= if OPTION:NONE exit then
   s MIN-I64$ I64-DIGITS >str DIGITS<= 0= if OPTION:NONE exit then
   0 0 begin s STR:LEN over > while
      swap BASE * over s STR:DATA + c@ DIGIT-VALUE - swap
      1+
   repeat drop OPTION:SOME ;

: >NUMBER? ( str -- option<n> ) {: s:str :}   \ SOME optional-sign bounded i64, else NONE
   s STR:LEN 0= if OPTION:NONE exit then
   s STR:DATA c@ MINUS <> 0= if
      s STR:LEN 1 <> 0= if OPTION:NONE exit then
      s STR:DATA 1+ s STR:LEN 1- >str PARSE-NEG exit
   then
   s STR:DATA c@ PLUS <> 0= if
      s STR:LEN 1 <> 0= if OPTION:NONE exit then
      s STR:DATA 1+ s STR:LEN 1- >str PARSE-POS exit
   then
   s PARSE-POS ;

\ Sixteen digits fill the cell, so the top digit sets the sign bit and
\ FFFFFFFFFFFFFFFF is -1: the bit pattern, not a bounded magnitude.
: HEX>NUMBER? ( str -- option<n> )   \ SOME the cell the 1..16 hex digits spell, else NONE
   {: s:str :}
   s STR:LEN 0= if OPTION:NONE exit then
   s STR:LEN HEX-MAX-DIGITS > if OPTION:NONE exit then
   0 0 begin s STR:LEN over > while
      dup s STR:DATA + c@ dup HEX-DIGIT? 0= if drop 2drop OPTION:NONE exit then
      HEX-DIGIT-VALUE rot HEX-BASE * + swap
      1+
   repeat drop OPTION:SOME ;

\ An offset into the whole text; NONE for a start outside [0, length].
: FIND-SUB-FROM ( str str n -- option<idx> )   \ SOME first offset of b in a at or after start, else NONE
   {: s:str t:str start:n :}
   start 0< start s STR:LEN > or if OPTION:NONE exit then
   t STR:LEN 0= if start >IDX OPTION:SOME exit then
   start begin dup s STR:LEN t STR:LEN - <= while
      dup s STR:DATA + t STR:LEN >str t = if >IDX OPTION:SOME exit then
      1+
   repeat drop OPTION:NONE ;

: FIND-SUB ( str str -- option<idx> )   \ SOME first offset of b in a (empty needle -> SOME 0), else NONE
   0 FIND-SUB-FROM ;

: CONTAINS? ( str str -- bool )
   FIND-SUB MATCH option
     none OF FALSE ENDOF
     some OF drop TRUE ENDOF
   ;MATCH ;

: CONTAINS-CI? ( str str -- bool )   \ whether a holds b, letters compared as =CI compares them
   {: s:str t:str :}
   0 begin dup s STR:LEN t STR:LEN - <= while
      dup s STR:DATA + t STR:LEN >str t =CI if drop TRUE exit then
      1+
   repeat drop FALSE ;

global

\ TASK-LOCAL: `data-base` is the running task's DATA region, at STRING-ABI's
\ offsets (src/habu/layout.f). A fresh region is zeroed, so a new task's cursor
\ starts at zero.
: SB-BUF ( -- ^u8 )
   data-base SB-BUF-OFF + BYTE-VIEW ;

: SB-LEN ( -- ^n )
   data-base SB-LEN-OFF + ;
;package

: STR-LEN ( n -- len )
   >LEN ;

: STR-OFF ( n -- off )
   >OFF ;

: STR-COUNT ( n -- count )
   >COUNT ;

: SB-CHECK-LEN-ROOM ( len -- ) {: add :}
   add LEN>N SB-CAP SB-LEN @ - > if E-STR-CAPACITY throw then ;

: SB-CHECK-ROOM ( n -- )
   STR-LEN SB-CHECK-LEN-ROOM ;

: SB-RESET ( -- )
   0 SB-LEN ! ;

: SB-APPEND ( str -- ) {: s:str :}
   s STR:LEN SB-CHECK-ROOM
   s STR:DATA SB-BUF SB-LEN @ + s STR:LEN BYTE-COPY
   SB-LEN @ s STR:LEN + SB-LEN ! ;

: SB-APPEND-C ( n -- ) {: c :}
   1 SB-CHECK-ROOM
   c SB-BUF SB-LEN @ + c!
   SB-LEN @ 1+ SB-LEN ! ;

: SB$ ( -- str )
   SB-BUF SB-LEN @ >str ;

: BUF-CHECK-LEN ( len len ^len -- )
   {: add cap lenp:^ :}
   lenp @ LEN>N cap LEN>N > if E-STR-CAPACITY throw then
   add LEN>N cap LEN>N lenp @ LEN>N - > if E-STR-CAPACITY throw then ;

: BUF-RESET ( ^len -- )
   0 >LEN swap ! ;

: BUF-LEN@ ( ^len -- n )
   @ LEN>N ;

: BUF-APPEND-LEN ( str ^u8 len ^len -- )
   {: src:str dst:^ cap lenp:^ :}
   src STR:LEN STR-LEN cap lenp BUF-CHECK-LEN
   src STR:DATA dst lenp @ LEN>N + src STR:LEN BYTE-COPY
   lenp @ LEN>N src STR:LEN + >LEN lenp ! ;

: BUF-APPEND ( str ^u8 n ^len -- )
   {: src:str dst:^ cap lenp:^ :}
   src dst cap STR-LEN lenp BUF-APPEND-LEN ;

: BUF-APPEND-C ( n ^u8 n ^len -- )
   {: c dst:^ cap lenp:^ :}
   1 >LEN cap STR-LEN lenp BUF-CHECK-LEN
   c dst lenp @ LEN>N + c!
   lenp @ LEN>N 1+ >LEN lenp ! ;
