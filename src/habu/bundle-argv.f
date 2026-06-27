\ bundle-argv.f - standalone bundle script argument convention.

: SCRIPT-ARG-START ( -- n )
   1 ;
s" SCRIPT-ARG-START" s" -- n" TRUST

: SCRIPT-ARGC ( -- n )
   ARGC 1 - dup 0 < if drop 0 then ;
s" SCRIPT-ARGC" s" -- n" TRUST

: SCRIPT-ARGV ( i -- z )
   1 + ARGV ;
s" SCRIPT-ARGV" s" n -- ptr u8" TRUST

: SCRIPT-ARGV$ ( i -- a u )
   SCRIPT-ARGV dup ZLEN ;
s" SCRIPT-ARGV$" s" n -- ptr u8 n" TRUST
