\ file-map.f - checked stdlib filesystem and map usage example.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f tools/examples-test.f

8 constant FM-CAP

create FM-MAP FM-CAP >COUNT MAP-CELLS COUNT>N cells allot

: FM-INIT ( -- )
   FM-MAP FM-CAP >COUNT MAP-INIT ;

: FM-INC ( ptr u8 n -- ) {: key:ptr len :}
   FM-MAP FM-CAP >COUNT key len >LEN MAP-GET MATCH option
     none OF 1 ENDOF                                    \ not present: count starts at 1
     some OF 1+ ENDOF                                   \ present: increment the value
   ;MATCH
   FM-MAP FM-CAP >COUNT key len >LEN MAP-SET ;

: FM-CLASSIFY ( ptr u8 n -- ) {: a:ptr u :}
   a u s" .f" ENDS-WITH? if s" forth" FM-INC exit then
   a u s" .txt" ENDS-WITH? if s" text" FM-INC exit then
   s" other" FM-INC ;

: FM-COUNT ( ptr u8 n -- n ) {: key:ptr len :}
   FM-MAP FM-CAP >COUNT key len >LEN MAP-GET MATCH option
     none OF 0 ENDOF                                    \ not present: 0
     some OF ENDOF                                      \ present: the stored count
   ;MATCH ;

: FM-ASSERT-COUNT ( ptr u8 n n -- ) {: key:ptr len want :}
   key len FM-COUNT want T= ;

: FM-MAIN ( -- )
   T-RESET
   SCRIPT-ARGC 1 T=
   FM-INIT
   0 SCRIPT-ARGV$ [: FM-CLASSIFY ;] WALK-FILES
   s" forth" 2 FM-ASSERT-COUNT
   s" text" 1 FM-ASSERT-COUNT
   s" other" 1 FM-ASSERT-COUNT
   s" missing" 0 FM-ASSERT-COUNT
   T-REPORT ;

FM-MAIN
