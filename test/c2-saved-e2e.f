\ C2 wrappers saved in one app image, then fresh tier-0 consumers and live capture.
\ The printed tree retains both products and every child log for replay.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require test/e2e-tree.f

using E2E-TREE
package C2-SAVED-E2E
private

: SETUP ( -- )
   s" c2-saved-e2e" TREE
   s" test/c2-memory-wrapper.f" COPY-TEST
   s" test/c2-memory-saved-consumer.f" COPY-TEST
   s" test/c2-memory-live-save.f" COPY-TEST
   s" test/c2-init-wrapper.f" COPY-TEST
   s" test/c2-init-saved-consumer.f" COPY-TEST
   s" test/c2-init-live-save.f" COPY-TEST
   s" test/c2-owner-producer-program.f" COPY-TEST
   s" test/c2-owner-dispose.f" COPY-TEST
   s" test/c2-forall-input.f" COPY-TEST
   s" test/c2-forall-input-refusals.f" COPY-TEST
   s" test/c2-field-loan-wrapper.f" COPY-TEST
   s" test/c2-field-loan-saved.f" COPY-TEST
   s" test/c2-records-wrapper.f" COPY-TEST
   s" test/c2-records-saved.f" COPY-TEST
   s" test/c2-view-record-defs.f" COPY-TEST
   s" test/c2-view-record-use-refusals.f" COPY-TEST
   s" test/c2-retained-contract-wide.f" COPY-TEST
   s" test/c2-view-record-saved.f" COPY-TEST
   s" test/c2-xml-consumer-program.f" COPY-TEST
   s" test/c2-xml-consumer-refusals.f" COPY-TEST
   s" test/c2-xml-consumer-saved.f" COPY-TEST ;

\ Each shared child session prints every program's own ok marker.
: MARKED ( ptr u8 n ptr u8 n -- )
   {: text:ptr textu:n mark:ptr marku:n :}
   text textu T-LABEL
   OUT$ mark marku CONTAINS? TTRUE ;

: SAVE ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   s" hb-saved" AT-A >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$
   S\" require src/habu/app-image.f\nrequire test/c2-memory-wrapper.f\nrequire test/c2-init-wrapper.f\nrequire test/c2-field-loan-wrapper.f\nrequire test/c2-records-wrapper.f\nrequire test/c2-view-record-defs.f\nrequire src/lib/xml/c2.f\nrequire src/lib/c2-bytes.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   RUN-INPUT
   s" save.out" s" save.err" SAVE-LOG
   s" the engine under test saves every C2 wrapper" T-LABEL NEED-OK
   RC@ 0<> if exit then
   s" hb-saved" AT-A FS:EXECUTABLE? TTRUE ;

\ One session runs every consumer; each refusal case inside it is its own
\ SUBJECT child because a refused program ends its process.
: SAVED-CASES ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved" AT-A
   S\" require test/c2-memory-saved-consumer.f\nrequire test/c2-init-saved-consumer.f\nrequire test/c2-owner-producer-program.f\nrequire test/c2-owner-dispose.f\nrequire test/c2-forall-input.f\nrequire test/c2-field-loan-saved.f\nrequire test/c2-records-saved.f\nrequire test/c2-view-record-saved.f\nrequire test/c2-xml-consumer-saved.f\nrequire test/c2-forall-input-refusals.f\nrequire test/c2-xml-consumer-refusals.f\n"
   RUN-INPUT
   s" saved-tier0.out" s" saved-tier0.err" SAVE-LOG
   s" the shared tier-0 saved session exits cleanly" T-LABEL NEED-OK
   s" a fresh saved consumer invokes new owner and loan scopes"
   s" c2-memory-saved-consumer: ok" MARKED
   s" a fresh saved consumer opens the initialized record bounds"
   s" c2-init-saved-consumer: ok" MARKED
   s" a fresh saved consumer appends through the owner"
   s" c2-owner-producer-program: ok" MARKED
   s" a fresh saved consumer disposes foreign resources through its owner"
   s" c2-owner-dispose: ok" MARKED
   s" a fresh saved image specializes supplied callbacks"
   s" c2-forall-input: ok" MARKED
   s" a fresh saved-image source consumer opens a field loan"
   s" c2-field-loan-saved: ok" MARKED
   s" a fresh saved-image source consumer opens a table"
   s" c2-records-saved: ok" MARKED
   s" a fresh saved image constructs and consumes the records"
   s" c2-view-record-saved: ok" MARKED
   s" a fresh saved-image consumer decodes into separate destinations"
   s" c2-xml-consumer-saved: ok" MARKED
   s" saved callbacks retain their input identities and bounds"
   s" c2-forall-input-refusals: ok" MARKED
   s" saved-image authority checks remain closed"
   s" c2-xml-consumer-refusals: ok" MARKED ;

\ Live capture runs in its own engine, and hb-after is a second image, so each
\ is a separate child.
: LIVE-SAVE ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-CANDIDATE:PATH$
   S\" require test/c2-memory-live-save.f\nC2-MEMORY-LIVE-SAVE:RUN\n"
   RUN-INPUT
   s" live-save.out" s" live-save.err" SAVE-LOG
   s" live capture fails and capture after close succeeds" T-LABEL
   s" c2-memory-live-save: closed" EXPECT-OK
   s" hb-live-loan" AT-A FS:EXISTS? TFALSE
   s" hb-live-owner" AT-A FS:EXISTS? TFALSE
   s" hb-live-append" AT-A FS:EXISTS? TFALSE
   s" hb-after" AT-A FS:EXECUTABLE? TTRUE
   s" hb-after" AT-A s" test/c2-owner-producer-program.f" RUN-IMAGE
   s" after-append.out" s" after-append.err" SAVE-LOG
   s" a saved image appends through a fresh owner after refused live capture" T-LABEL
   s" c2-owner-producer-program: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-CANDIDATE:PATH$
   S\" require test/c2-init-live-save.f\nC2-INIT-LIVE-SAVE:RUN\n"
   RUN-INPUT
   s" live-init.out" s" live-init.err" SAVE-LOG
   s" live initialized storage refuses capture" T-LABEL
   s" c2-init-live-save: closed" EXPECT-OK
   s" hb-live-init" AT-A FS:EXISTS? TFALSE ;

public

: RUN ( -- )
   T-RESET
   SETUP SAVE RC@ 0= if SAVED-CASES then LIVE-SAVE
   s" c2-saved-e2e tree: " type ROOT$ type cr
   T-REPORT ;

;package
;using

C2-SAVED-E2E:RUN
