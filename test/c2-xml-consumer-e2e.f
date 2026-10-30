\ Use the engine under test, save XML-C2 and compile a fresh consumer.
\ The printed tree keeps the saved image and child logs for replay.
require src/lib/test.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require test/e2e-tree.f

using E2E-TREE
package C2-XML-CONSUMER-E2E
private

: SETUP ( -- )
   s" c2-xml-consumer-e2e" TREE
   s" test/c2-xml-consumer-program.f" COPY-TEST
   s" test/c2-xml-consumer-refusals.f" COPY-TEST
   s" test/c2-xml-consumer-saved.f" COPY-TEST ;

: SOURCE-CASES ( -- )
   ENGINE-CANDIDATE:PATH$ s" test/c2-xml-consumer-program.f" RUN-IMAGE
   s" source.out" s" source.err" SAVE-LOG
   s" source-loaded XML-C2 borrows names across cursor cleanup" T-LABEL
   s" c2-xml-consumer-program: ok" EXPECT-OK
   ENGINE-CANDIDATE:PATH$ s" test/c2-xml-consumer-refusals.f" RUN-IMAGE
   s" refusals.out" s" refusals.err" SAVE-LOG
   s" checked source rejects cursor and source authority escapes" T-LABEL
   s" c2-xml-consumer-refusals: ok" EXPECT-OK ;

: SAVE-XML ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   s" hb-saved" AT-A >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$
   S\" require src/habu/app-image.f\nrequire src/lib/xml/c2.f\nrequire src/lib/c2-bytes.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   RUN-INPUT
   s" save.out" s" save.err" SAVE-LOG
   s" the typed XML-C2 module saves into a new image" T-LABEL NEED-OK
   RC@ 0<> if exit then
   s" hb-saved" AT-A FS:EXECUTABLE? TTRUE ;

: SAVED-CASES ( -- )
   s" hb-saved" AT-A s" test/c2-xml-consumer-saved.f" RUN-IMAGE
   s" saved.out" s" saved.err" SAVE-LOG
   s" a fresh saved-image consumer decodes into separate destinations" T-LABEL
   s" c2-xml-consumer-saved: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved" AT-A
   S\" 1 set-tier\nrequire test/c2-xml-consumer-saved.f\n"
   RUN-INPUT
   s" saved-tier1.out" s" saved-tier1.err" SAVE-LOG
   s" a fresh saved-image consumer compiles at tier 1" T-LABEL
   s" c2-xml-consumer-saved: ok" EXPECT-OK
   s" hb-saved" AT-A s" test/c2-xml-consumer-refusals.f" RUN-IMAGE
   s" saved-refusals.out" s" saved-refusals.err" SAVE-LOG
   s" saved-image authority checks remain closed" T-LABEL
   s" c2-xml-consumer-refusals: ok" EXPECT-OK ;

public
: RUN ( -- )
   T-RESET
   SETUP SOURCE-CASES SAVE-XML RC@ 0= if SAVED-CASES then
   s" c2-xml-consumer-e2e tree: " type ROOT$ type cr
   T-REPORT ;
;package
;using

C2-XML-CONSUMER-E2E:RUN
