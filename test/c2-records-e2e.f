\ Native record tables, fresh source consumers and a saved wrapper at both tiers.
\ The printed directory retains the images, source copies and child logs.
require src/lib/test.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/process-cwd.f
require test/c2-image-pair.f
require test/e2e-tree.f

using E2E-TREE
package C2-RECORDS-E2E
private

: SETUP ( -- )
   s" c2-records-e2e" TREE
   s" test/c2-records-types.f" COPY-TEST
   s" test/c2-records-program.f" COPY-TEST
   s" test/c2-records-refusals.f" COPY-TEST
   s" test/c2-records-lifecycle.f" COPY-TEST
   s" test/c2-records-scopes.f" COPY-TEST
   s" test/c2-records-wrapper.f" COPY-TEST
   s" test/c2-records-saved.f" COPY-TEST ;

: IMAGES ( -- )
   ROOT$ C2-IMAGE-PAIR:PROVIDE
   s" native C2 record image is supplied" T-LABEL
   s" hb-root" AT-A EXECUTABLE? TTRUE ;

: SOURCE-CASES ( -- )
   s" hb-root" s" test/c2-records-program.f" RUN-IMAGE
   s" source.out" s" source.err" SAVE-LOG
   s" a fresh source program edits and closes a record table" T-LABEL
   s" c2-records-program: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" 1 set-tier\nrequire test/c2-records-program.f\ns\q c2-records-tier1: ok\q type cr\n"
   RUN-INPUT
   s" tier1.out" s" tier1.err" SAVE-LOG
   s" fresh tier one code edits and closes a record table" T-LABEL
   s" c2-records-tier1: ok" EXPECT-OK
   s" hb-root" s" test/c2-records-refusals.f" RUN-IMAGE
   s" refusals.out" s" refusals.err" SAVE-LOG
   s" table and element authority refusals hold" T-LABEL
   s" c2-records-refusals: ok" EXPECT-OK
   s" hb-root" s" test/c2-records-lifecycle.f" RUN-IMAGE
   s" lifecycle.out" s" lifecycle.err" SAVE-LOG
   s" throw and task halt close the complete table scope" T-LABEL
   s" c2-records-lifecycle: ok" EXPECT-OK
   s" hb-root" s" test/c2-records-scopes.f" RUN-IMAGE
   s" scopes.out" s" scopes.err" SAVE-LOG
   s" source and decoded field views survive cursor close" T-LABEL
   s" c2-records-scopes: ok" EXPECT-OK ;

: SAVED-CASES ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   s" hb-saved" AT-A >LEN PROC-ARGV+
   s" hb-root"
   S\" require src/habu/app-image.f\nrequire test/c2-records-wrapper.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   RUN-INPUT
   s" save.out" s" save.err" SAVE-LOG
   s" a table wrapper saves with its complete scheme" T-LABEL
   RC@ 0 T=
   RC@ 0<> if exit then
   s" hb-saved" AT-A EXECUTABLE? TTRUE
   s" hb-saved" s" test/c2-records-saved.f" RUN-IMAGE
   s" saved-source.out" s" saved-source.err" SAVE-LOG
   s" a fresh saved-image source consumer opens a table" T-LABEL
   s" c2-records-saved: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved"
   S\" 1 set-tier\nrequire test/c2-records-saved.f\ns\q c2-records-saved-tier1: ok\q type cr\n"
   RUN-INPUT
   s" saved-tier1.out" s" saved-tier1.err" SAVE-LOG
   s" a fresh saved-image tier one consumer opens a table" T-LABEL
   s" c2-records-saved-tier1: ok" EXPECT-OK ;

public

: RUN ( -- )
   T-RESET
   SETUP IMAGES SOURCE-CASES SAVED-CASES
   s" c2-records-e2e tree: " type ROOT$ type cr
   T-REPORT ;

;package
;using

C2-RECORDS-E2E:RUN
