\ A saved record-table wrapper and its fresh source consumers at both tiers.
\ The printed directory retains the saved image, source copies and child logs.
require src/lib/test.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require test/e2e-tree.f

using E2E-TREE
package C2-RECORDS-E2E
private

: SETUP ( -- )
   s" c2-records-e2e" TREE
   s" test/c2-records-wrapper.f" COPY-TEST
   s" test/c2-records-saved.f" COPY-TEST ;

: SAVED-CASES ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   s" hb-saved" AT-A >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$
   S\" require src/habu/app-image.f\nrequire test/c2-records-wrapper.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   RUN-INPUT
   s" save.out" s" save.err" SAVE-LOG
   s" a table wrapper saves with its complete scheme" T-LABEL
   RC@ 0 T=
   RC@ 0<> if exit then
   s" hb-saved" AT-A FS:EXECUTABLE? TTRUE
   s" hb-saved" AT-A s" test/c2-records-saved.f" RUN-IMAGE
   s" saved-source.out" s" saved-source.err" SAVE-LOG
   s" a fresh saved-image source consumer opens a table" T-LABEL
   s" c2-records-saved: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved" AT-A
   S\" 1 set-tier\nrequire test/c2-records-saved.f\ns\q c2-records-saved-tier1: ok\q type cr\n"
   RUN-INPUT
   s" saved-tier1.out" s" saved-tier1.err" SAVE-LOG
   s" a fresh saved-image tier one consumer opens a table" T-LABEL
   s" c2-records-saved-tier1: ok" EXPECT-OK ;

public

: RUN ( -- )
   T-RESET
   SETUP SAVED-CASES
   s" c2-records-e2e tree: " type ROOT$ type cr
   T-REPORT ;

;package
;using

C2-RECORDS-E2E:RUN
