\ Capture borrowed record declarations in a saved image, then use their
\ constructors from the fresh saved image in a relocated source tree.
require src/lib/test.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require test/e2e-tree.f

using E2E-TREE
package C2-VIEW-RECORD-E2E
private

: SETUP ( -- )
   s" c2-view-record-e2e" TREE
   s" test/c2-view-record-defs.f" COPY-TEST
   s" test/c2-view-record-use-refusals.f" COPY-TEST
   s" test/c2-retained-contract-wide.f" COPY-TEST
   s" test/c2-view-record-saved.f" COPY-TEST ;

: SAVED-CASE ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   s" hb-saved" AT-A >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$
   S\" 1 set-tier\nrequire test/c2-view-record-defs.f\nrequire src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   RUN-INPUT
   s" save.out" s" save.err" SAVE-LOG
   s" borrowed field schemas survive native capture" T-LABEL NEED-OK
   s" hb-saved" AT-A FS:EXECUTABLE? TTRUE
   s" hb-saved" AT-A s" test/c2-view-record-saved.f" RUN-IMAGE
   s" saved.out" s" saved.err" SAVE-LOG
   s" a fresh saved image constructs and consumes the records" T-LABEL
   s" c2-view-record-saved: ok" EXPECT-OK ;

public

: RUN ( -- )
   T-RESET
   SETUP SAVED-CASE
   s" c2-view-record-e2e tree: " type ROOT$ type cr
   T-REPORT ;

;package
;using

C2-VIEW-RECORD-E2E:RUN
