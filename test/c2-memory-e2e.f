\ End-to-end C2 unique storage through source, native and saved images.
\ The printed tree retains both products and every child log for replay.
require lib/test.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process-argv.f
require lib/process-env.f
require lib/process-cwd.f
require lib/engine-candidate.f

package C2-MEMORY-E2E
private

$8000 constant IO-CAP
600000 constant BUILD-TIMEOUT-MS
30000 constant CHILD-TIMEOUT-MS

create ROOT FS-PATH-CAP allot variable ROOT-U
create PATH FS-PATH-CAP allot
create TARGET FS-PATH-CAP allot variable TARGET-U
create OUT IO-CAP allot variable OUT-U
create ERR IO-CAP allot variable ERR-U
variable RC

: ROOT$ ( -- ptr u8 n ) ROOT ROOT-U @ ;

: AT-A ( ptr u8 n -- ptr u8 n ) {: rel:ptr relu:n :}
   ROOT$ rel relu PATH JOIN-PATH PATH swap ;

: LINK ( ptr u8 n -- ) {: rel:ptr relu:n :}
   SOURCE-ROOT:CWD$ rel relu TARGET JOIN-PATH TARGET-U !
   TARGET TARGET-U @ rel relu AT-A MAKE-SYMLINK ;

: COPY-TEST ( ptr u8 n -- ) {: rel:ptr relu:n :}
   SOURCE-ROOT:CWD$ rel relu TARGET JOIN-PATH TARGET-U !
   TARGET TARGET-U @ rel relu AT-A COPY-FILE-STREAM ;

: SETUP ( -- )
   s" c2-memory-e2e" HB-TMP-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY u ROOT-U !
   s" src" LINK s" lib" LINK s" tools" LINK
   s" test" AT-A MAKE-DIRS
   s" test/c2-memory-program.f" COPY-TEST
   s" test/c2-memory-refusals.f" COPY-TEST
   s" test/c2-memory-scope-refusals.f" COPY-TEST
   s" test/c2-memory-wrapper.f" COPY-TEST
   s" test/c2-memory-saved-consumer.f" COPY-TEST
   s" test/c2-memory-ordinary.f" COPY-TEST
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

: CAPTURE-RESULT ( result<pcap:captured,pcap:failed> -- )
   MATCH result
      ok OF PCAP-CAPTURED:UNMAKE {: outu:len erru:len :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! 0 RC ! ENDOF
      err OF PCAP-FAILED:UNMAKE {: outu:len erru:len code:rc :}
         outu LEN>N OUT-U ! erru LEN>N ERR-U ! code RC>N RC ! ENDOF
   ;MATCH ;

: ARGS ( ptr u8 n -- ) {: source:ptr size:n :}
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --load" >LEN PROC-ARGV+
   source size >LEN PROC-ARGV+ ;

: RUN-ON ( ptr u8 n ptr u8 n n -- )
   {: engine:ptr engineu:n cwd:ptr cwdu:n timeout:n :}
   PROC-ENV-INHERIT-MISSING
   engine engineu >LEN cwd cwdu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN timeout >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-CAPTURE CAPTURE-RESULT ;

: SAVE-LOG ( ptr u8 n ptr u8 n -- )
   {: outpath:ptr outu:n errpath:ptr erru:n :}
   outpath outu AT-A OUT OUT-U @ WRITE-ALL
   errpath erru AT-A ERR ERR-U @ WRITE-ALL ;

: NEED-OK ( -- )
   RC @ 0<> if OUT OUT-U @ type ERR ERR-U @ type then
   RC @ 0 T= ;

: EXPECT-OK ( ptr u8 n -- ) {: marker:ptr markeru:n :}
   NEED-OK
   OUT OUT-U @ marker markeru CONTAINS? TTRUE ;

: BUILD ( -- )
   s" test/c2-memory-image-build.f" ARGS
   s" --" >LEN PROC-ARGV+
   s" hb-root" AT-A >LEN PROC-ARGV+
   s" hb-ordinary" AT-A >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ SOURCE-ROOT:CWD$ BUILD-TIMEOUT-MS RUN-ON
   s" build.out" s" build.err" SAVE-LOG
   s" unique owner native image build completes" T-LABEL NEED-OK
   RC @ 0<> if exit then
   s" hb-root" AT-A EXECUTABLE? TTRUE
   s" hb-ordinary" AT-A CHMOD-X
   s" hb-ordinary" AT-A EXECUTABLE? TTRUE
   s" hb-root.names" AT-A FILE? TTRUE
   s" hb-root.names" AT-A {: names:ptr namesu:n :}
   names TARGET namesu BYTE-COPY namesu TARGET-U !
   TARGET TARGET-U @ s" hb-ordinary.names" AT-A COPY-FILE-STREAM ;

: RUN-IMAGE ( ptr u8 n ptr u8 n -- )
   {: image:ptr imageu:n source:ptr sourceu:n :}
   source sourceu ARGS
   image imageu AT-A ROOT$ CHILD-TIMEOUT-MS RUN-ON ;

: RUN-INPUT ( ptr u8 n ptr u8 n -- )
   {: image:ptr imageu:n input:ptr inputu:n :}
   PROC-ENV-INHERIT-MISSING
   image imageu AT-A >LEN ROOT$ >LEN input inputu >LEN
   OUT IO-CAP >LEN ERR IO-CAP >LEN CHILD-TIMEOUT-MS >MS
   PROC-CWD:RUN-ARGV-ENV-CWD-STDIN-CAPTURE CAPTURE-RESULT ;

: ROOT-CASES ( -- )
   s" hb-root" s" test/c2-memory-native-boundary.f" RUN-IMAGE
   s" root-native.out" s" root-native.err" SAVE-LOG
   s" real, shadow, and alias entries retain expected native scope kinds" T-LABEL
   s" c2-memory-native-boundary: ok" EXPECT-OK
   s" hb-root" s" test/c2-memory-program.f" RUN-IMAGE
   s" root-program.out" s" root-program.err" SAVE-LOG
   s" unique byte and loan behavior works in the native image" T-LABEL
   s" c2-memory-program: ok" EXPECT-OK
   s" hb-root" s" test/c2-init-program.f" RUN-IMAGE
   s" root-init.out" s" root-init.err" SAVE-LOG
   s" a live owner initializes and clears fixed records" T-LABEL
   s" c2-init-program: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" 1 set-tier\nrequire test/c2-init-program.f\ns\q c2-init-program-tier1: ok\q type cr\n"
   RUN-INPUT
   s" root-init-tier1.out" s" root-init-tier1.err" SAVE-LOG
   s" fixed records initialize and clear at tier 1" T-LABEL
   s" c2-init-program-tier1: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" 1 set-tier\nrequire test/c2-memory-program.f\ns\q c2-memory-program-tier1: ok\q type cr\n"
   RUN-INPUT
   s" root-program-tier1.out" s" root-program-tier1.err" SAVE-LOG
   s" unique byte and loan behavior works at tier 1" T-LABEL
   s" c2-memory-program-tier1: ok" EXPECT-OK
   s" hb-root" s" test/c2-memory-refusals.f" RUN-IMAGE
   s" root-refusals.out" s" root-refusals.err" SAVE-LOG
   s" unique copy and escape refusals remain enforced" T-LABEL
   s" c2-memory-refusals: ok" EXPECT-OK
   s" hb-root" s" test/c2-memory-scope-refusals.f" RUN-IMAGE
   s" root-scope-refusals.out" s" root-scope-refusals.err" SAVE-LOG
   s" scope schemes and shared loans retain their boundaries" T-LABEL
   s" c2-memory-scope-refusals: ok" EXPECT-OK
   s" hb-root" s" test/c2-init-refusals.f" RUN-IMAGE
   s" root-init-refusals.out" s" root-init-refusals.err" SAVE-LOG
   s" initialization rejects invalid records and views" T-LABEL
   s" c2-init-refusals: ok" EXPECT-OK
   s" hb-root" s" test/c2-owner-producer-program.f" RUN-IMAGE
   s" root-producer.out" s" root-producer.err" SAVE-LOG
   s" an owner appends and publishes scoped allocations" T-LABEL
   s" c2-owner-producer-program: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" 1 set-tier\nrequire test/c2-owner-producer-program.f\n"
   RUN-INPUT
   s" root-producer-tier1.out" s" root-producer-tier1.err" SAVE-LOG
   s" appended allocations work through the native compiler" T-LABEL
   s" c2-owner-producer-program: ok" EXPECT-OK
   s" hb-root" s" test/c2-owner-producer-refusals.f" RUN-IMAGE
   s" root-producer-refusals.out" s" root-producer-refusals.err" SAVE-LOG
   s" producer capabilities reject copying and mutation after publish" T-LABEL
   s" c2-owner-producer-refusals: ok" EXPECT-OK
   s" hb-root" s" test/c2-owner-dispose.f" RUN-IMAGE
   s" root-dispose.out" s" root-dispose.err" SAVE-LOG
   s" explicit owners release registered foreign resources" T-LABEL
   s" c2-owner-dispose: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" 1 set-tier\nrequire test/c2-owner-dispose.f\n"
   RUN-INPUT
   s" root-dispose-tier1.out" s" root-dispose-tier1.err" SAVE-LOG
   s" owner disposal works through the native compiler" T-LABEL
   s" c2-owner-dispose: ok" EXPECT-OK
   s" hb-root" s" test/c2-forall-input.f" RUN-IMAGE
   s" root-forall.out" s" root-forall.err" SAVE-LOG
   s" supplied callbacks specialize from typed inputs" T-LABEL
   s" c2-forall-input: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" 1 set-tier\nrequire test/c2-forall-input.f\n"
   RUN-INPUT
   s" root-forall-tier1.out" s" root-forall-tier1.err" SAVE-LOG
   s" callback specialization also compiles at tier 1" T-LABEL
   s" c2-forall-input: ok" EXPECT-OK
   s" hb-root" s" test/c2-forall-input-refusals.f" RUN-IMAGE
   s" root-forall-refusals.out" s" root-forall-refusals.err" SAVE-LOG
   s" callbacks retain their input identities and bounds" T-LABEL
   s" c2-forall-input-refusals: ok" EXPECT-OK ;

: EXPECT-ENTRY-REFUSAL ( ptr u8 n ptr u8 n -- )
   {: reason:ptr reasonu:n name:ptr nameu:n :}
   s" a scoped entry rejects a prompt callback before it runs" T-LABEL
   RC @ 70 T=
   ERR ERR-U @ reason reasonu CONTAINS? TTRUE
   ERR ERR-U @ name nameu CONTAINS? TTRUE
   OUT OUT-U @ s" accepted" CONTAINS? TFALSE ;

: ENTRY-CASES ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" package C2-ENTRY-TEST public TRUSTED: LEAKER ( ptr u8 n -- ptr u8 n ) ; ;package\n8 MEM:BYTES-ALLOC-LEN ' C2-ENTRY-TEST:LEAKER C2-MEM:WITH-MUT\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-mut.out" s" entry-mut.err" SAVE-LOG
   s" hb: internal engine word: " s" C2-MEM:WITH-MUT" EXPECT-ENTRY-REFUSAL
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" package C2-ENTRY-TEST public TRUSTED: LEAKER ( ptr u8 n -- ptr u8 n ) ; ;package\nusing C2-MEM\n8 MEM:BYTES-ALLOC-LEN ' C2-ENTRY-TEST:LEAKER WITH-MUT\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-bare.out" s" entry-bare.err" SAVE-LOG
   s" hb: internal engine word: " s" WITH-MUT" EXPECT-ENTRY-REFUSAL
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" package C2-ENTRY-ALIAS public EXPORT C2-MEM:WITH-MUT ;package\npackage C2-ENTRY-TEST public TRUSTED: LEAKER ( ptr u8 n -- ptr u8 n ) ; ;package\n8 MEM:BYTES-ALLOC-LEN ' C2-ENTRY-TEST:LEAKER C2-ENTRY-ALIAS:WITH-MUT\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-alias.out" s" entry-alias.err" SAVE-LOG
   s" hb: internal engine word: " s" C2-ENTRY-ALIAS:WITH-MUT" EXPECT-ENTRY-REFUSAL
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" package C2-ENTRY-TEST public TRUSTED: LEAKER ( ptr u8 n -- ptr u8 n ) ; ;package\ns\q abc\q ' C2-ENTRY-TEST:LEAKER C2-MEM:WITH-READ\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-read.out" s" entry-read.err" SAVE-LOG
   s" hb: internal engine word: " s" C2-MEM:WITH-READ" EXPECT-ENTRY-REFUSAL
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" package C2-ENTRY-TEST public TRUSTED: LEAKER ( ptr u8 n -- ptr u8 n ) ; ;package\ns\q abc\q ' C2-ENTRY-TEST:LEAKER C2-MEM:WITH-MUT-LOAN\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-mut-loan.out" s" entry-mut-loan.err" SAVE-LOG
   s" hb: internal engine word: " s" C2-MEM:WITH-MUT-LOAN" EXPECT-ENTRY-REFUSAL
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" require src/habu/interpret.f\ns\q 8 MEM:BYTES-ALLOC-LEN 0 C2-MEM:WITH-MUT\q OUTER:INTERPRET\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-outer.out" s" entry-outer.err" SAVE-LOG
   s" hb: internal engine word: " s" C2-MEM:WITH-MUT" EXPECT-ENTRY-REFUSAL
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" require test/c2-memory-wrapper.f\n: C2-ENTRY-RAW ( n ptr u8 n -- n ptr u8 n ) s\q raw callback entered\q type cr ;\n0 ' C2-ENTRY-RAW C2-MEMORY-WRAPPER:TWICE .\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-forall.out" s" entry-forall.err" SAVE-LOG
   s" hb: interpret-mode layout value: " s" C2-MEMORY-WRAPPER:TWICE" EXPECT-ENTRY-REFUSAL
   OUT OUT-U @ s" raw callback entered" CONTAINS? TFALSE
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" require test/c2-memory-wrapper.f\n: C2-ENTRY-RAW ( n ptr u8 n -- n ptr u8 n ) s\q raw callback entered\q type cr ;\nusing C2-MEMORY-WRAPPER\n0 ' C2-ENTRY-RAW TWICE .\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-forall-bare.out" s" entry-forall-bare.err" SAVE-LOG
   s" hb: interpret-mode layout value: " s" TWICE" EXPECT-ENTRY-REFUSAL
   OUT OUT-U @ s" raw callback entered" CONTAINS? TFALSE
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" require test/c2-memory-wrapper.f\npackage C2-ENTRY-ALIAS public EXPORT C2-MEMORY-WRAPPER:TWICE ;package\n: C2-ENTRY-RAW ( n ptr u8 n -- n ptr u8 n ) s\q raw callback entered\q type cr ;\n0 ' C2-ENTRY-RAW C2-ENTRY-ALIAS:TWICE .\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" entry-forall-alias.out" s" entry-forall-alias.err" SAVE-LOG
   s" hb: interpret-mode layout value: " s" C2-ENTRY-ALIAS:TWICE" EXPECT-ENTRY-REFUSAL
   OUT OUT-U @ s" raw callback entered" CONTAINS? TFALSE
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" : C2-ENTRY-INC ( n -- n ) 1 + ;\n: C2-ENTRY-APPLY ( n [ n -- n ] -- n ) execute ;\ns\q ordinary prompt: \q type 41 ' C2-ENTRY-INC C2-ENTRY-APPLY . cr\n"
   RUN-INPUT
   s" entry-ordinary.out" s" entry-ordinary.err" SAVE-LOG
   s" an ordinary callback remains callable at the prompt" T-LABEL
   s" ordinary prompt: 42" EXPECT-OK ;

: ORDINARY-CASES ( -- )
   s" hb-ordinary" s" test/c2-memory-program.f" RUN-IMAGE
   s" ordinary-program.out" s" ordinary-program.err" SAVE-LOG
   s" ordinary image refuses the real unique program" T-LABEL
   RC @ 70 T=
   ERR ERR-U @ s" C2-MEM:WITH-MUT" CONTAINS? TTRUE
   s" hb-ordinary" s" test/c2-memory-ordinary.f" RUN-IMAGE
   s" ordinary.out" s" ordinary.err" SAVE-LOG
   s" ordinary image grants no unique scope authority" T-LABEL
   s" c2-memory-ordinary: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-ordinary"
   S\" 1 set-tier\nrequire test/c2-memory-ordinary.f\ns\q c2-memory-ordinary-tier1: ok\q type cr\n"
   RUN-INPUT
   s" ordinary-tier1.out" s" ordinary-tier1.err" SAVE-LOG
   s" ordinary tier 1 grants no unique scope authority" T-LABEL
   s" c2-memory-ordinary-tier1: ok" EXPECT-OK ;

: SAVED-CASES ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" --" >LEN PROC-ARGV+
   s" hb-saved" AT-A >LEN PROC-ARGV+
   s" hb-root"
   S\" require src/habu/app-image.f\nrequire test/c2-memory-wrapper.f\nrequire test/c2-init-wrapper.f\n0 SCRIPT-ARGV$ APP-IMAGE:SAVE\n"
   RUN-INPUT
   s" save.out" s" save.err" SAVE-LOG
   s" rooted image saves the unique wrapper" T-LABEL NEED-OK
   RC @ 0<> if exit then
   s" hb-saved" AT-A EXECUTABLE? TTRUE
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved"
   S\" : C2-ENTRY-RAW ( n ptr u8 n -- n ptr u8 n ) s\q raw callback entered\q type cr ;\n0 ' C2-ENTRY-RAW C2-MEMORY-WRAPPER:TWICE .\ns\q accepted\q type cr\n"
   RUN-INPUT
   s" saved-entry-forall.out" s" saved-entry-forall.err" SAVE-LOG
   s" hb: interpret-mode layout value: " s" C2-MEMORY-WRAPPER:TWICE" EXPECT-ENTRY-REFUSAL
   OUT OUT-U @ s" raw callback entered" CONTAINS? TFALSE
   s" hb-saved" s" test/c2-memory-saved-consumer.f" RUN-IMAGE
   s" saved-consumer.out" s" saved-consumer.err" SAVE-LOG
   s" a fresh saved consumer invokes new owner and loan scopes" T-LABEL
   s" c2-memory-saved-consumer: ok" EXPECT-OK
   s" hb-saved" s" test/c2-init-saved-consumer.f" RUN-IMAGE
   s" saved-init.out" s" saved-init.err" SAVE-LOG
   s" a fresh saved consumer opens the initialized record bounds" T-LABEL
   s" c2-init-saved-consumer: ok" EXPECT-OK
   s" hb-saved" s" test/c2-owner-producer-program.f" RUN-IMAGE
   s" saved-producer.out" s" saved-producer.err" SAVE-LOG
   s" a fresh saved consumer appends through the owner" T-LABEL
   s" c2-owner-producer-program: ok" EXPECT-OK
   s" hb-saved" s" test/c2-owner-dispose.f" RUN-IMAGE
   s" saved-dispose.out" s" saved-dispose.err" SAVE-LOG
   s" a fresh saved consumer disposes foreign resources through its owner" T-LABEL
   s" c2-owner-dispose: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved"
   S\" 1 set-tier\nrequire test/c2-owner-dispose.f\n"
   RUN-INPUT
   s" saved-dispose-tier1.out" s" saved-dispose-tier1.err" SAVE-LOG
   s" a fresh saved tier 1 consumer disposes through its owner" T-LABEL
   s" c2-owner-dispose: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved"
   S\" 1 set-tier\nrequire test/c2-owner-producer-program.f\n"
   RUN-INPUT
   s" saved-producer-tier1.out" s" saved-producer-tier1.err" SAVE-LOG
   s" a fresh saved producer also compiles at tier 1" T-LABEL
   s" c2-owner-producer-program: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved"
   S\" 1 set-tier\nrequire test/c2-init-saved-consumer.f\ns\q c2-init-saved-tier1: ok\q type cr\n"
   RUN-INPUT
   s" saved-init-tier1.out" s" saved-init-tier1.err" SAVE-LOG
   s" saved initialized accessors work at tier 1" T-LABEL
   s" c2-init-saved-tier1: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved"
   S\" 1 set-tier\nrequire test/c2-memory-saved-consumer.f\ns\q c2-memory-saved-tier1: ok\q type cr\n"
   RUN-INPUT
   s" saved-consumer-tier1.out" s" saved-consumer-tier1.err" SAVE-LOG
   s" a fresh saved tier 1 consumer invokes new scopes" T-LABEL
   s" c2-memory-saved-tier1: ok" EXPECT-OK
   s" hb-saved" s" test/c2-forall-input.f" RUN-IMAGE
   s" saved-forall.out" s" saved-forall.err" SAVE-LOG
   s" a fresh saved image specializes supplied callbacks" T-LABEL
   s" c2-forall-input: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-saved"
   S\" 1 set-tier\nrequire test/c2-forall-input.f\n"
   RUN-INPUT
   s" saved-forall-tier1.out" s" saved-forall-tier1.err" SAVE-LOG
   s" saved callback specialization also compiles at tier 1" T-LABEL
   s" c2-forall-input: ok" EXPECT-OK
   s" hb-saved" s" test/c2-forall-input-refusals.f" RUN-IMAGE
   s" saved-forall-refusals.out" s" saved-forall-refusals.err" SAVE-LOG
   s" saved callbacks retain their input identities and bounds" T-LABEL
   s" c2-forall-input-refusals: ok" EXPECT-OK ;

: LIVE-SAVE ( -- )
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" require test/c2-memory-live-save.f\nC2-MEMORY-LIVE-SAVE:RUN\n"
   RUN-INPUT
   s" live-save.out" s" live-save.err" SAVE-LOG
   s" live capture fails and capture after close succeeds" T-LABEL
   s" c2-memory-live-save: closed" EXPECT-OK
   s" hb-live-loan" AT-A EXISTS? TFALSE
   s" hb-live-owner" AT-A EXISTS? TFALSE
   s" hb-live-append" AT-A EXISTS? TFALSE
   s" hb-after" AT-A EXECUTABLE? TTRUE
   s" hb-after" s" test/c2-owner-producer-program.f" RUN-IMAGE
   s" after-append.out" s" after-append.err" SAVE-LOG
   s" a saved image appends through a fresh owner after refused live capture" T-LABEL
   s" c2-owner-producer-program: ok" EXPECT-OK
   PROC-CWD:ARGV-ENV-CWD-RESET
   s" hb-root"
   S\" require test/c2-init-live-save.f\nC2-INIT-LIVE-SAVE:RUN\n"
   RUN-INPUT
   s" live-init.out" s" live-init.err" SAVE-LOG
   s" live initialized storage refuses capture" T-LABEL
   s" c2-init-live-save: closed" EXPECT-OK
   s" hb-live-init" AT-A EXISTS? TFALSE ;

: PARTIAL-CASE ( -- )
   s" test/c2-memory-partial-write.f" ARGS
   s" --" >LEN PROC-ARGV+
   s" hb-partial" AT-A >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ SOURCE-ROOT:CWD$ BUILD-TIMEOUT-MS RUN-ON
   s" partial.out" s" partial.err" SAVE-LOG
   s" partial capture cannot bind public C2 entries" T-LABEL
   RC @ 74 T=
   OUT OUT-U @ s" c2-partial: code bytes " CONTAINS? TTRUE
   s" hb-partial" AT-A EXISTS? TFALSE ;

public

: RUN ( -- )
   T-RESET
   SETUP BUILD
   RC @ 0= if
      ROOT-CASES ENTRY-CASES ORDINARY-CASES SAVED-CASES LIVE-SAVE PARTIAL-CASE
   then
   s" c2-memory-e2e tree: " type ROOT$ type cr
   T-REPORT ;

;package

C2-MEMORY-E2E:RUN
