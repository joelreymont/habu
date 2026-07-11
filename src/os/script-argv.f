\ script-argv.f - bin/hb source-list script argument convention.

: SCRIPT-LOAD-Z? ( ptr u8 -- bool )
   dup 0 ZBYTE@ ENV-DASH <> if drop ENV-FALSE exit then
   dup 1 ZBYTE@ ENV-DASH <> if drop ENV-FALSE exit then
   dup 2 ZBYTE@ $6C <> if drop ENV-FALSE exit then
   dup 3 ZBYTE@ $6F <> if drop ENV-FALSE exit then
   dup 4 ZBYTE@ $61 <> if drop ENV-FALSE exit then
   dup 5 ZBYTE@ $64 <> if drop ENV-FALSE exit then
   6 ZBYTE@ 0 = ;
s" SCRIPT-LOAD-Z?" s" ptr u8 -- bool" TRUST

: SCRIPT-BUILD-Z? ( ptr u8 -- bool )
   dup 0 ZBYTE@ ENV-DASH <> if drop ENV-FALSE exit then
   dup 1 ZBYTE@ ENV-DASH <> if drop ENV-FALSE exit then
   dup 2 ZBYTE@ $62 <> if drop ENV-FALSE exit then
   dup 3 ZBYTE@ $75 <> if drop ENV-FALSE exit then
   dup 4 ZBYTE@ $69 <> if drop ENV-FALSE exit then
   dup 5 ZBYTE@ $6C <> if drop ENV-FALSE exit then
   dup 6 ZBYTE@ $64 <> if drop ENV-FALSE exit then
   7 ZBYTE@ 0 = ;
s" SCRIPT-BUILD-Z?" s" ptr u8 -- bool" TRUST

: SCRIPT-SOURCE-Z? ( ptr u8 -- bool )
   dup SCRIPT-LOAD-Z? if drop ENV-FALSE 0= exit then
   SCRIPT-BUILD-Z? ;
s" SCRIPT-SOURCE-Z?" s" ptr u8 -- bool" TRUST

: SCRIPT-SOURCE? ( -- bool )
   ARGC 1 <= if ENV-FALSE exit then
   1 ARGV SCRIPT-SOURCE-Z? ;
s" SCRIPT-SOURCE?" s" -- bool" TRUST

: SCRIPT-SEP? ( n -- bool )
   ARGV
   dup 0 ZBYTE@ ENV-DASH <> if drop ENV-FALSE exit then
   dup 1 ZBYTE@ ENV-DASH <> if drop ENV-FALSE exit then
   2 ZBYTE@ 0 = ;
s" SCRIPT-SEP?" s" n -- bool" TRUST

: SCRIPT-ARG-START ( -- n )
   SCRIPT-SOURCE? 0= if 2 exit then
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
