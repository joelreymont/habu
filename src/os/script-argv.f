\ script-argv.f - bin/hb source-list script argument convention.
\ Checked C-string readers locate the source-list separator and argument slice.

: SCRIPT-LOAD-Z? ( ptr u8 -- bool )
   dup 0 ZBYTE@ ENV-DASH <> if drop ENV-FALSE exit then
   dup 1 ZBYTE@ ENV-DASH <> if drop ENV-FALSE exit then
   dup 2 ZBYTE@ $6C <> if drop ENV-FALSE exit then
   dup 3 ZBYTE@ $6F <> if drop ENV-FALSE exit then
   dup 4 ZBYTE@ $61 <> if drop ENV-FALSE exit then
   dup 5 ZBYTE@ $64 <> if drop ENV-FALSE exit then
   6 ZBYTE@ 0 = ;

: SCRIPT-BUILD-Z? ( ptr u8 -- bool )
   dup 0 ZBYTE@ ENV-DASH <> if drop ENV-FALSE exit then
   dup 1 ZBYTE@ ENV-DASH <> if drop ENV-FALSE exit then
   dup 2 ZBYTE@ $62 <> if drop ENV-FALSE exit then
   dup 3 ZBYTE@ $75 <> if drop ENV-FALSE exit then
   dup 4 ZBYTE@ $69 <> if drop ENV-FALSE exit then
   dup 5 ZBYTE@ $6C <> if drop ENV-FALSE exit then
   dup 6 ZBYTE@ $64 <> if drop ENV-FALSE exit then
   7 ZBYTE@ 0 = ;

: SCRIPT-SOURCE-Z? ( ptr u8 -- bool )
   dup SCRIPT-LOAD-Z? if drop ENV-FALSE 0= exit then
   SCRIPT-BUILD-Z? ;

: SCRIPT-SOURCE? ( -- bool )
   ARGC 1 <= if ENV-FALSE exit then
   1 ARGV SCRIPT-SOURCE-Z? ;

: SCRIPT-SEP? ( n -- bool )
   ARGV
   dup 0 ZBYTE@ ENV-DASH <> if drop ENV-FALSE exit then
   dup 1 ZBYTE@ ENV-DASH <> if drop ENV-FALSE exit then
   2 ZBYTE@ 0 = ;

: SCRIPT-ARG-START ( -- n )
   data-base APP-ENTRY:XT-CELL + @ 0 <> if
      ARGC 1 > if 1 SCRIPT-SEP? if 2 exit then then
      1 exit
   then
   SCRIPT-SOURCE? 0= if 2 exit then
   2 begin dup ARGC < while
      dup SCRIPT-SEP? if 1 + exit then
      1 +
   repeat
   drop ARGC ;

: SCRIPT-ARGC ( -- n )
   ARGC SCRIPT-ARG-START - dup 0 < if drop 0 then ;

: SCRIPT-ARGV ( n -- ptr u8 )
   SCRIPT-ARG-START + ARGV ;

: SCRIPT-ARGV$ ( n -- ptr u8 n )
   SCRIPT-ARGV dup ZLEN ;
