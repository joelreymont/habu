\ Use the initial C2 image, run borrowed records, capture declarations, then
\ use their constructors from a fresh saved image in a relocated source tree.
require src/lib/test.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/process-cwd.f
require test/c2-image-pair.f
require test/e2e-tree.f

using E2E-TREE
package C2-VIEW-RECORD-E2E
private

: SETUP ( -- )
   s" c2-view-record-e2e" TREE
   s" test/c2-view-record-defs.f" COPY-TEST
   s" test/c2-view-record-source.f" COPY-TEST
   s" test/c2-view-record-refusals.f" COPY-TEST
   s" test/c2-view-record-use-refusals.f" COPY-TEST
   s" test/c2-retained-contract-wide.f" COPY-TEST
   s" test/c2-view-record-saved.f" COPY-TEST ;

: IMAGES ( -- )
   ROOT$ C2-IMAGE-PAIR:PROVIDE
   s" rooted image is supplied" T-LABEL
   s" hb-root" AT-A FILE? TTRUE ;

: SOURCE-CASES ( -- )
   s" hb-root" s" test/c2-view-record-source.f" RUN-IMAGE
   s" source.out" s" source.err" SAVE-LOG
   s" borrowed records round trip through nested records and a sum" T-LABEL
   s" c2-view-record-source: ok" EXPECT-OK
   s" hb-root" s" test/c2-view-record-refusals.f" RUN-IMAGE
   s" refusals.out" s" refusals.err" SAVE-LOG
   s" borrowed records keep escape and pointer fences" T-LABEL
   s" c2-view-record-refusals: ok" EXPECT-OK ;

: SAVED-CASE ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   s" hb-saved" AT-A >LEN PROC-ARGV+
   s" hb-root"
   S\" 1 set-tier\nrequire test/c2-view-record-defs.f\nrequire src/habu/app-image.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   RUN-INPUT
   s" save.out" s" save.err" SAVE-LOG
   s" borrowed field schemas survive native capture" T-LABEL NEED-OK
   s" hb-saved" AT-A EXECUTABLE? TTRUE
   s" hb-saved" s" test/c2-view-record-saved.f" RUN-IMAGE
   s" saved.out" s" saved.err" SAVE-LOG
   s" a fresh saved image constructs and consumes the records" T-LABEL
   s" c2-view-record-saved: ok" EXPECT-OK ;

public

: RUN ( -- )
   T-RESET
   SETUP IMAGES SOURCE-CASES SAVED-CASE
   s" c2-view-record-e2e tree: " type ROOT$ type cr
   T-REPORT ;

;package
;using

C2-VIEW-RECORD-E2E:RUN
