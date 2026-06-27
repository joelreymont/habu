\ script-argv.f - bin/hb source-list script argument convention.

: SCRIPT-LOAD-Z? ( ptr u8 -- bool ) {: z:ptr :}
   z c@ ENV-DASH <> if ENV-FALSE exit then
   z 1 + c@ ENV-DASH <> if ENV-FALSE exit then
   z 2 + c@ 108 <> if ENV-FALSE exit then
   z 3 + c@ 111 <> if ENV-FALSE exit then
   z 4 + c@ 97 <> if ENV-FALSE exit then
   z 5 + c@ 100 <> if ENV-FALSE exit then
   z 6 + c@ 0 = ;
s" SCRIPT-LOAD-Z?" s" ptr u8 -- bool" TRUST

: SCRIPT-LOAD? ( -- bool )
   ARGC 1 <= if ENV-FALSE exit then
   1 ARGV SCRIPT-LOAD-Z? ;
s" SCRIPT-LOAD?" s" -- bool" TRUST

: SCRIPT-SEP? ( n -- bool ) {: idx :}
   idx ARGV {: z:ptr :}
   z c@ ENV-DASH <> if ENV-FALSE exit then
   z 1 + c@ ENV-DASH <> if ENV-FALSE exit then
   z 2 + c@ 0 = ;
s" SCRIPT-SEP?" s" n -- bool" TRUST

: SCRIPT-ARG-START ( -- n )
   SCRIPT-LOAD? 0= if 2 exit then
   2 begin dup ARGC < while
      dup SCRIPT-SEP? if 1 + exit then
      1 +
   repeat
   drop ARGC ;
s" SCRIPT-ARG-START" s" -- n" TRUST

: SCRIPT-ARGC ( -- n )
   ARGC SCRIPT-ARG-START - dup 0 < if drop 0 then ;
s" SCRIPT-ARGC" s" -- n" TRUST

: SCRIPT-ARGV ( i -- z )
   SCRIPT-ARG-START + ARGV ;
s" SCRIPT-ARGV" s" n -- ptr u8" TRUST

: SCRIPT-ARGV$ ( i -- a u )
   SCRIPT-ARGV dup ZLEN ;
s" SCRIPT-ARGV$" s" n -- ptr u8 n" TRUST
