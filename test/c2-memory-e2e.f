\ End-to-end C2 unique storage through source, native and saved images.
\ The printed tree retains both products and every child log for replay.
require src/lib/test.f
require src/lib/string.f
require src/lib/fs.f
require src/lib/process-argv.f
require src/lib/process-cwd.f
require src/lib/engine-candidate.f
require test/e2e-tree.f

using E2E-TREE
package C2-MEMORY-E2E
private

: SETUP ( -- )
   s" c2-memory-e2e" TREE
   s" test/c2-memory-program.f" COPY-TEST
   s" test/c2-memory-refusals.f" COPY-TEST
   s" test/c2-memory-scope-refusals.f" COPY-TEST
   s" test/c2-memory-wrapper.f" COPY-TEST
   s" test/c2-memory-saved-consumer.f" COPY-TEST
   s" test/c2-memory-native-boundary.f" COPY-TEST
   s" test/c2-memory-live-save.f" COPY-TEST
   s" test/c2-init-program.f" COPY-TEST
   s" test/c2-init-refusals.f" COPY-TEST
   s" test/c2-init-wrapper.f" COPY-TEST
   s" test/c2-init-saved-consumer.f" COPY-TEST
   s" test/c2-init-live-save.f" COPY-TEST
   s" test/c2-owner-producer-program.f" COPY-TEST
   s" test/c2-owner-producer-refusals.f" COPY-TEST
   s" test/c2-owner-dispose.f" COPY-TEST
   s" test/c2-forall-input.f" COPY-TEST
   s" test/c2-forall-input-refusals.f" COPY-TEST ;

\ Each shared child session prints every program's own ok marker.
: MARKED ( ptr u8 n ptr u8 n -- )
   {: text:ptr textu:n mark:ptr marku:n :}
   text textu T-LABEL
   OUT$ mark marku CONTAINS? TTRUE ;

: SOURCE-CASES ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-CANDIDATE:PATH$
   S\" require test/c2-memory-native-boundary.f\nrequire test/c2-memory-program.f\nrequire test/c2-init-program.f\nrequire test/c2-memory-refusals.f\nrequire test/c2-memory-scope-refusals.f\nrequire test/c2-init-refusals.f\nrequire test/c2-owner-producer-program.f\nrequire test/c2-owner-producer-refusals.f\nrequire test/c2-owner-dispose.f\nrequire test/c2-forall-input.f\nrequire test/c2-forall-input-refusals.f\n"
   RUN-INPUT
   s" source-tier0.out" s" source-tier0.err" SAVE-LOG
   s" the shared tier-0 native session exits cleanly" T-LABEL NEED-OK
   s" real, shadow, and alias entries retain expected native scope kinds"
   s" c2-memory-native-boundary: ok" MARKED
   s" unique byte and loan behavior works in the native image"
   s" c2-memory-program: ok" MARKED
   s" a live owner initializes and clears fixed records"
   s" c2-init-program: ok" MARKED
   s" unique copy and escape refusals remain enforced"
   s" c2-memory-refusals: ok" MARKED
   s" scope schemes and shared loans retain their boundaries"
   s" c2-memory-scope-refusals: ok" MARKED
   s" initialization rejects invalid records and views"
   s" c2-init-refusals: ok" MARKED
   s" an owner appends and publishes scoped allocations"
   s" c2-owner-producer-program: ok" MARKED
   s" producer capabilities reject copying and mutation after publish"
   s" c2-owner-producer-refusals: ok" MARKED
   s" explicit owners release registered foreign resources"
   s" c2-owner-dispose: ok" MARKED
   s" supplied callbacks specialize from typed inputs"
   s" c2-forall-input: ok" MARKED
   s" callbacks retain their input identities and bounds"
   s" c2-forall-input-refusals: ok" MARKED
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-CANDIDATE:PATH$
   S\" 1 set-tier\nrequire test/c2-init-program.f\nrequire test/c2-memory-program.f\nrequire test/c2-owner-producer-program.f\nrequire test/c2-owner-dispose.f\nrequire test/c2-forall-input.f\n"
   RUN-INPUT
   s" source-tier1.out" s" source-tier1.err" SAVE-LOG
   s" the shared tier-1 native session exits cleanly" T-LABEL NEED-OK
   s" fixed records initialize and clear at tier 1"
   s" c2-init-program: ok" MARKED
   s" unique byte and loan behavior works at tier 1"
   s" c2-memory-program: ok" MARKED
   s" appended allocations work through the native compiler"
   s" c2-owner-producer-program: ok" MARKED
   s" owner disposal works through the native compiler"
   s" c2-owner-dispose: ok" MARKED
   s" callback specialization also compiles at tier 1"
   s" c2-forall-input: ok" MARKED ;

: EXPECT-ENTRY-REFUSAL ( ptr u8 n ptr u8 n -- )
   {: reason:ptr reasonu:n name:ptr nameu:n :}
   s" a scoped entry rejects a prompt callback before it runs" T-LABEL
   RC@ 70 T=
   ERR$ reason reasonu CONTAINS? TTRUE
   ERR$ name nameu CONTAINS? TTRUE
   OUT$ s" accepted" CONTAINS? TFALSE ;

: ENTRY-CASES ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-CANDIDATE:PATH$
   S\" package C2-ENTRY-TEST public TRUSTED: LEAKER ( ptr u8 n -- ptr u8 n ) ; ;package\n8 MEM:BYTES-ALLOC-LEN ' C2-ENTRY-TEST:LEAKER C2-MEM:WITH-MUT\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-mut.out" s" entry-mut.err" SAVE-LOG
   s" hb: internal engine word: " s" C2-MEM:WITH-MUT" EXPECT-ENTRY-REFUSAL
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-CANDIDATE:PATH$
   S\" package C2-ENTRY-ALIAS public EXPORT C2-MEM:WITH-MUT ;package\npackage C2-ENTRY-TEST public TRUSTED: LEAKER ( ptr u8 n -- ptr u8 n ) ; ;package\n8 MEM:BYTES-ALLOC-LEN ' C2-ENTRY-TEST:LEAKER C2-ENTRY-ALIAS:WITH-MUT\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-alias.out" s" entry-alias.err" SAVE-LOG
   s" hb: internal engine word: " s" C2-ENTRY-ALIAS:WITH-MUT" EXPECT-ENTRY-REFUSAL
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-CANDIDATE:PATH$
   S\" package C2-ENTRY-TEST public TRUSTED: LEAKER ( ptr u8 n -- ptr u8 n ) ; ;package\ns\q abc\q ' C2-ENTRY-TEST:LEAKER C2-MEM:WITH-READ\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-read.out" s" entry-read.err" SAVE-LOG
   s" hb: internal engine word: " s" C2-MEM:WITH-READ" EXPECT-ENTRY-REFUSAL
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-CANDIDATE:PATH$
   S\" package C2-ENTRY-TEST public TRUSTED: LEAKER ( ptr u8 n -- ptr u8 n ) ; ;package\ns\q abc\q ' C2-ENTRY-TEST:LEAKER C2-MEM:WITH-MUT-LOAN\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-mut-loan.out" s" entry-mut-loan.err" SAVE-LOG
   s" hb: internal engine word: " s" C2-MEM:WITH-MUT-LOAN" EXPECT-ENTRY-REFUSAL
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-CANDIDATE:PATH$
   S\" require src/habu/interpret.f\ns\q 8 MEM:BYTES-ALLOC-LEN 0 C2-MEM:WITH-MUT\q OUTER:INTERPRET\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-outer.out" s" entry-outer.err" SAVE-LOG
   s" hb: internal engine word: " s" C2-MEM:WITH-MUT" EXPECT-ENTRY-REFUSAL
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-CANDIDATE:PATH$
   S\" require test/c2-memory-wrapper.f\n: C2-ENTRY-RAW ( n ptr u8 n -- n ptr u8 n ) s\q raw callback entered\q type cr ;\n0 ' C2-ENTRY-RAW C2-MEMORY-WRAPPER:TWICE .\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-forall.out" s" entry-forall.err" SAVE-LOG
   s" hb: interpret-mode layout value: " s" C2-MEMORY-WRAPPER:TWICE" EXPECT-ENTRY-REFUSAL
   OUT$ s" raw callback entered" CONTAINS? TFALSE
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-CANDIDATE:PATH$
   S\" require test/c2-memory-wrapper.f\npackage C2-ENTRY-ALIAS public EXPORT C2-MEMORY-WRAPPER:TWICE ;package\n: C2-ENTRY-RAW ( n ptr u8 n -- n ptr u8 n ) s\q raw callback entered\q type cr ;\n0 ' C2-ENTRY-RAW C2-ENTRY-ALIAS:TWICE .\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-forall-alias.out" s" entry-forall-alias.err" SAVE-LOG
   s" hb: interpret-mode layout value: " s" C2-ENTRY-ALIAS:TWICE" EXPECT-ENTRY-REFUSAL
   OUT$ s" raw callback entered" CONTAINS? TFALSE
   PROC-CWD:ARGV-ENV-CWD-RESET
   ENGINE-CANDIDATE:PATH$
   S\" : C2-ENTRY-INC ( n -- n ) 1 + ;\n: C2-ENTRY-APPLY ( n [ n -- n ] -- n ) execute ;\ns\q ordinary prompt: \q type 41 ' C2-ENTRY-INC C2-ENTRY-APPLY . cr\n"
   RUN-INPUT
   s" entry-ordinary.out" s" entry-ordinary.err" SAVE-LOG
   s" an ordinary callback remains callable at the prompt" T-LABEL
   s" ordinary prompt: 42" EXPECT-OK ;

: SAVED-CASES ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   s" hb-saved" AT-A >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$
   S\" require src/habu/app-image.f\nrequire test/c2-memory-wrapper.f\nrequire test/c2-init-wrapper.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   RUN-INPUT
   s" save.out" s" save.err" SAVE-LOG
   s" the engine under test saves the unique wrapper" T-LABEL NEED-OK
   RC@ 0<> if exit then
   s" hb-saved" AT-A FS:EXECUTABLE? TTRUE
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved" AT-A
   S\" : C2-ENTRY-RAW ( n ptr u8 n -- n ptr u8 n ) s\q raw callback entered\q type cr ;\n0 ' C2-ENTRY-RAW C2-MEMORY-WRAPPER:TWICE .\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" saved-entry-forall.out" s" saved-entry-forall.err" SAVE-LOG
   s" hb: interpret-mode layout value: " s" C2-MEMORY-WRAPPER:TWICE" EXPECT-ENTRY-REFUSAL
   OUT$ s" raw callback entered" CONTAINS? TFALSE
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved" AT-A
   S\" require test/c2-memory-saved-consumer.f\nrequire test/c2-init-saved-consumer.f\nrequire test/c2-owner-producer-program.f\nrequire test/c2-owner-dispose.f\nrequire test/c2-forall-input.f\nrequire test/c2-forall-input-refusals.f\n"
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
   s" saved callbacks retain their input identities and bounds"
   s" c2-forall-input-refusals: ok" MARKED
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved" AT-A
   S\" 1 set-tier\nrequire test/c2-owner-dispose.f\nrequire test/c2-owner-producer-program.f\nrequire test/c2-init-saved-consumer.f\nrequire test/c2-memory-saved-consumer.f\nrequire test/c2-forall-input.f\n"
   RUN-INPUT
   s" saved-tier1.out" s" saved-tier1.err" SAVE-LOG
   s" the shared tier-1 saved session exits cleanly" T-LABEL NEED-OK
   s" a fresh saved tier 1 consumer disposes through its owner"
   s" c2-owner-dispose: ok" MARKED
   s" a fresh saved producer also compiles at tier 1"
   s" c2-owner-producer-program: ok" MARKED
   s" saved initialized accessors work at tier 1"
   s" c2-init-saved-consumer: ok" MARKED
   s" a fresh saved tier 1 consumer invokes new scopes"
   s" c2-memory-saved-consumer: ok" MARKED
   s" saved callback specialization also compiles at tier 1"
   s" c2-forall-input: ok" MARKED ;

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

: PARTIAL-CASE ( -- )
   s" test/c2-memory-partial-write.f" ARGS
   s" --" >LEN PROC-ARGV+
   s" hb-partial" AT-A >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ SOURCE-ROOT:CWD$ RUN-ON
   s" partial.out" s" partial.err" SAVE-LOG
   s" partial capture cannot bind public C2 entries" T-LABEL
   RC@ 74 T=
   OUT$ s" c2-partial: code bytes " CONTAINS? TTRUE
   s" hb-partial" AT-A FS:EXISTS? TFALSE ;

public

: RUN ( -- )
   T-RESET
   SETUP SOURCE-CASES ENTRY-CASES SAVED-CASES LIVE-SAVE PARTIAL-CASE
   s" c2-memory-e2e tree: " type ROOT$ type cr
   T-REPORT ;

;package
;using

C2-MEMORY-E2E:RUN
