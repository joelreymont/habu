\ Native embedded field loans, fresh source consumers and a saved wrapper at both tiers.
\ The printed directory retains the images, source copies and child logs.
require src/lib/test.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/process-cwd.f
require test/c2-image-pair.f
require test/e2e-tree.f

using E2E-TREE
package C2-FIELD-LOAN-E2E
private

: SETUP ( -- )
   s" c2-field-loan-e2e" TREE
   s" test/c2-field-loan-program.f" COPY-TEST
   s" test/c2-field-loan-refusals.f" COPY-TEST
   s" test/c2-field-loan-lifecycle.f" COPY-TEST
   s" test/c2-field-loan-kind.f" COPY-TEST
   s" test/c2-field-loan-ordinary.f" COPY-TEST
   s" test/c2-field-loan-wrapper.f" COPY-TEST
   s" test/c2-field-loan-saved.f" COPY-TEST ;

: IMAGES ( -- )
   ROOT$ C2-IMAGE-PAIR:PROVIDE
   s" native C2 field loan image is supplied" T-LABEL
   s" hb-root" AT-A EXECUTABLE? TTRUE ;

: SOURCE-CASES ( -- )
   s" hb-root" s" test/c2-field-loan-kind.f" RUN-IMAGE
   s" kind.out" s" kind.err" SAVE-LOG
   s" the exact field entry has scope kind eight" T-LABEL
   s" c2-field-loan-kind: ok" EXPECT-OK
   s" hb-ordinary" s" test/c2-field-loan-ordinary.f" RUN-IMAGE
   s" ordinary.out" s" ordinary.err" SAVE-LOG
   s" the ordinary image carries no field authority" T-LABEL
   s" c2-field-loan-ordinary: ok" EXPECT-OK
   s" hb-root" s" test/c2-field-loan-program.f" RUN-IMAGE
   s" source.out" s" source.err" SAVE-LOG
   s" a fresh source program edits an embedded field" T-LABEL
   s" c2-field-loan-program: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" 1 set-tier\nrequire test/c2-field-loan-program.f\ns\q c2-field-loan-tier1: ok\q type cr\n"
   RUN-INPUT
   s" tier1.out" s" tier1.err" SAVE-LOG
   s" fresh tier one code edits an embedded field" T-LABEL
   s" c2-field-loan-tier1: ok" EXPECT-OK
   s" hb-root" s" test/c2-field-loan-refusals.f" RUN-IMAGE
   s" refusals.out" s" refusals.err" SAVE-LOG
   s" field-loan authority refusals hold" T-LABEL
   s" c2-field-loan-refusals: ok" EXPECT-OK
   s" hb-root" s" test/c2-field-loan-lifecycle.f" RUN-IMAGE
   s" lifecycle.out" s" lifecycle.err" SAVE-LOG
   s" throw and task halt close the complete field-loan scope" T-LABEL
   s" c2-field-loan-lifecycle: ok" EXPECT-OK ;

: SAVED-CASES ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   s" hb-saved" AT-A >LEN PROC-ARGV+
   s" hb-root"
   S\" require src/habu/app-image.f\nrequire test/c2-field-loan-wrapper.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   RUN-INPUT
   s" save.out" s" save.err" SAVE-LOG
   s" a field-loan wrapper saves with its complete scheme" T-LABEL
   RC@ 0 T=
   RC@ 0<> if exit then
   s" hb-saved" AT-A EXECUTABLE? TTRUE
   s" hb-saved" s" test/c2-field-loan-saved.f" RUN-IMAGE
   s" saved-source.out" s" saved-source.err" SAVE-LOG
   s" a fresh saved-image source consumer opens a field loan" T-LABEL
   s" c2-field-loan-saved: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved"
   S\" 1 set-tier\nrequire test/c2-field-loan-saved.f\ns\q c2-field-loan-saved-tier1: ok\q type cr\n"
   RUN-INPUT
   s" saved-tier1.out" s" saved-tier1.err" SAVE-LOG
   s" a fresh saved-image tier one consumer opens a field loan" T-LABEL
   s" c2-field-loan-saved-tier1: ok" EXPECT-OK ;

public

: RUN ( -- )
   T-RESET
   SETUP IMAGES SOURCE-CASES SAVED-CASES
   s" c2-field-loan-e2e tree: " type ROOT$ type cr
   T-REPORT ;

;package
;using

C2-FIELD-LOAN-E2E:RUN
