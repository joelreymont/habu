\ A maker that compiled the original runtime installs that runtime when the
\ runtime source changes after the maker ran. The next build takes the edit.
\ Run via hb-build-producer-cache-test.f in its private copy of the tree, after
\ that copy's builds.

require src/lib/string.f
require src/lib/test.f
require tools/hb-build-lib.f

package HB-BUILD-CLI

65536 constant HBT-RACE-TEXT-CAP
262144 constant HBT-RACE-IMAGE-CAP
create HBT-RACE-TEXT HBT-RACE-TEXT-CAP allot
create HBT-RACE-IMAGE HBT-RACE-IMAGE-CAP allot

: HBT-RACE-CHANGE-RT ( -- )
   s" src/habu/rt.f" HBT-RACE-TEXT HBT-RACE-TEXT-CAP FS:READ-ALL {: u:n :}
   HBT-RACE-TEXT u s" hb: cannot map guarded VM stack" FIND-SUB MATCH option
      none OF s" rt.f has no guarded-stack diagnostic" T-EX-FAIL die ENDOF
      some OF
         IDX>N HBT-RACE-TEXT +
         s" hb: REVIEW map guarded VM stack" rot swap BYTE-COPY
      ENDOF
   ;MATCH
   s" src/habu/rt.f" HBT-RACE-TEXT u FS:WRITE-ALL ;

: HBT-RACE-IMAGE-HAS ( ptr u8 n ptr u8 n -- )
   {: yes:ptr yesu:n no:ptr nou:n :}
   s" app" HBT-RACE-IMAGE HBT-RACE-IMAGE-CAP FS:READ-ALL {: u:n :}
   HBT-RACE-IMAGE u yes yesu CONTAINS? TTRUE
   HBT-RACE-IMAGE u no nou CONTAINS? TFALSE ;

public

: HBT-RACE-RUN ( -- )
   T-RESET
   HBB-RESET-OPTIONS
   s" app.f" s" app" HBB-PATHS!
   HBB-PREPARE-TMP
   HBB-BUILD-BEGIN
   HBB-PREPARE-AOT
   HBB-RUN-MAKER
   HBT-RACE-CHANGE-RT
   HBB-INSTALL-OUT
   s" hb: cannot map guarded VM stack" s" hb: REVIEW map guarded VM stack" HBT-RACE-IMAGE-HAS
   HBB-BUILD
   HBB-MAKER-RUN @ 0<> TTRUE
   s" hb: REVIEW map guarded VM stack" s" hb: cannot map guarded VM stack" HBT-RACE-IMAGE-HAS
   HBB-CLEANUP
   T-REPORT
   s" hb-build-producer-race-test: ok" type cr ;

;package

HB-BUILD-CLI:HBT-RACE-RUN
