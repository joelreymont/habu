\ load-reject-diag-test.f - a rejecting `bin/hb --load` must identify itself.
\
\ Invariant (dot habu-silent-exit-70-215b2da7): every load leg that rejects a
\ definition exits 70 WITH a named diagnostic on stderr — a silent fail-closed
\ exit is the harness gap this campaign keeps paying for (cf. the seal-exit
\ labeling and the check.f E-FS-CAPACITY raw-67 fix). Pins the direct --load
\ leg, the require-chain leg, and the checked-body reject leg by spawning the
\ engine on generated rejecting fixtures and asserting stderr names the error.
\ The child engine is HABU_UNDER_TEST when the gate sets it, else bin/hb.
\
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f lib/fs.f
\   lib/fs-mutate.f lib/process.f lib/process-argv.f lib/process-env.f
\   test/load-reject-diag-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/fs.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/engine-candidate.f

package LRD
private

$1000 constant CAP
60000 constant TIMEOUT-MS
70 constant REJECT-RC            \ the checked-reject exit class
3 constant TEST-OUT-U
5 constant TEST-ERR-U
$22 constant QUOTE
$20 constant SPACE
$0A constant LF

create OUT CAP allot
create ERR CAP allot
create EMPTY 1 allot             \ zero-length stdin
create ROOT FS-PATH-CAP allot
create UNDEF-PATH FS-PATH-CAP allot
create BODY-PATH FS-PATH-CAP allot
create OUTER-PATH FS-PATH-CAP allot
create INCLUDE-FRAG-PATH FS-PATH-CAP allot
create REQUIRE-FRAG-PATH FS-PATH-CAP allot
create INCLUDE-PATH FS-PATH-CAP allot
create REQUIRE-PATH FS-PATH-CAP allot
create INCLUDE-MARK-PATH FS-PATH-CAP allot
create REQUIRE-MARK-PATH FS-PATH-CAP allot
create POSITIVE-PATH FS-PATH-CAP allot
create POS-INCLUDE-PATH FS-PATH-CAP allot
create POS-REQUIRE-PATH FS-PATH-CAP allot
create PROVIDED-PATH FS-PATH-CAP allot
create MISSING-PATH FS-PATH-CAP allot
create SHADOW-PATH FS-PATH-CAP allot
create INPUT-PATH FS-PATH-CAP allot
create MODELED-INPUT-PATH FS-PATH-CAP allot
create TRUSTED-PATH FS-PATH-CAP allot
create RECOVER-PATH FS-PATH-CAP allot
create REUSE-PATH FS-PATH-CAP allot
create CHURN-PATH FS-PATH-CAP allot

variable ROOT-U
variable UNDEF-U
variable BODY-U
variable OUTER-U
variable INCLUDE-FRAG-U
variable REQUIRE-FRAG-U
variable INCLUDE-U
variable REQUIRE-U
variable INCLUDE-MARK-U
variable REQUIRE-MARK-U
variable POSITIVE-U
variable POS-INCLUDE-U
variable POS-REQUIRE-U
variable PROVIDED-U
variable MISSING-U
variable SHADOW-U
variable INPUT-U
variable MODELED-INPUT-U
variable TRUSTED-U
variable RECOVER-U
variable REUSE-U
variable CHURN-U
variable OUT-U
variable ERR-U
variable EXITED
variable RC

: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;

: UNDEF$ ( -- ptr u8 n )
   UNDEF-PATH UNDEF-U @ ;

: BODY$ ( -- ptr u8 n )
   BODY-PATH BODY-U @ ;

: OUTER$ ( -- ptr u8 n )
   OUTER-PATH OUTER-U @ ;

: INCLUDE-FRAG$ ( -- ptr u8 n )
   INCLUDE-FRAG-PATH INCLUDE-FRAG-U @ ;

: REQUIRE-FRAG$ ( -- ptr u8 n )
   REQUIRE-FRAG-PATH REQUIRE-FRAG-U @ ;

: INCLUDE$ ( -- ptr u8 n )
   INCLUDE-PATH INCLUDE-U @ ;

: REQUIRE$ ( -- ptr u8 n )
   REQUIRE-PATH REQUIRE-U @ ;

: INCLUDE-MARK$ ( -- ptr u8 n )
   INCLUDE-MARK-PATH INCLUDE-MARK-U @ ;

: REQUIRE-MARK$ ( -- ptr u8 n )
   REQUIRE-MARK-PATH REQUIRE-MARK-U @ ;

: POSITIVE$ ( -- ptr u8 n )
   POSITIVE-PATH POSITIVE-U @ ;

: POS-INCLUDE$ ( -- ptr u8 n )
   POS-INCLUDE-PATH POS-INCLUDE-U @ ;

: POS-REQUIRE$ ( -- ptr u8 n )
   POS-REQUIRE-PATH POS-REQUIRE-U @ ;

: PROVIDED$ ( -- ptr u8 n )
   PROVIDED-PATH PROVIDED-U @ ;

: MISSING$ ( -- ptr u8 n )
   MISSING-PATH MISSING-U @ ;

: SHADOW$ ( -- ptr u8 n )
   SHADOW-PATH SHADOW-U @ ;

: INPUT$ ( -- ptr u8 n )
   INPUT-PATH INPUT-U @ ;

: MODELED-INPUT$ ( -- ptr u8 n )
   MODELED-INPUT-PATH MODELED-INPUT-U @ ;

: TRUSTED$ ( -- ptr u8 n )
   TRUSTED-PATH TRUSTED-U @ ;

: RECOVER$ ( -- ptr u8 n )
   RECOVER-PATH RECOVER-U @ ;

: REUSE$ ( -- ptr u8 n )
   REUSE-PATH REUSE-U @ ;

: CHURN$ ( -- ptr u8 n )
   CHURN-PATH CHURN-U @ ;

: OPEN-STRING ( -- )
   s" s" SB-APPEND  QUOTE SB-APPEND-C  SPACE SB-APPEND-C ;

: CLOSE-STRING ( -- )
   QUOTE SB-APPEND-C ;

: LOADER-SOURCE! ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: word:ptr wordu:n frag:ptr fragu:n mark:ptr marku:n path:ptr pathu:n :}
   SB-RESET
   s" require lib/errors.f" SB-APPEND  LF SB-APPEND-C
   s" require lib/string.f" SB-APPEND  LF SB-APPEND-C
   s" require lib/fs.f" SB-APPEND  LF SB-APPEND-C
   s" package LRD-FIXTURE private" SB-APPEND  LF SB-APPEND-C
   s" TRUSTED: TOUCH ( -- ) " SB-APPEND  OPEN-STRING
   mark marku SB-APPEND  CLOSE-STRING
   s"  " SB-APPEND  OPEN-STRING
   s" touched" SB-APPEND  CLOSE-STRING
   s"  WRITE-ALL ; immediate" SB-APPEND  LF SB-APPEND-C
   s" : IMM ( -- ) 73 " SB-APPEND
   word wordu SB-APPEND
   s"  " SB-APPEND
   frag fragu SB-APPEND
   s"  . ; ;package" SB-APPEND
   path pathu SB$ WRITE-ALL ;

: POSITIVE-SOURCE! ( -- )
   SB-RESET
   s" require lib/errors.f" SB-APPEND  LF SB-APPEND-C
   s" require lib/string.f" SB-APPEND  LF SB-APPEND-C
   s" require lib/fs.f" SB-APPEND  LF SB-APPEND-C
   s" package LRD-POS public" SB-APPEND  LF SB-APPEND-C
   s" : DO-INCLUDED ( ptr u8 n -- ) included ;" SB-APPEND  LF SB-APPEND-C
   s" : DO-REQUIRED ( ptr u8 n -- ) required ;" SB-APPEND  LF SB-APPEND-C
   s" : DO-PROVIDED ( ptr u8 n -- ) provided ;" SB-APPEND  LF SB-APPEND-C
   s" ;package" SB-APPEND  LF SB-APPEND-C
   OPEN-STRING  POS-INCLUDE$ SB-APPEND  CLOSE-STRING
   s"  LRD-POS:DO-INCLUDED" SB-APPEND  LF SB-APPEND-C
   OPEN-STRING  POS-REQUIRE$ SB-APPEND  CLOSE-STRING
   s"  LRD-POS:DO-REQUIRED" SB-APPEND  LF SB-APPEND-C
   OPEN-STRING  POS-REQUIRE$ SB-APPEND  CLOSE-STRING
   s"  LRD-POS:DO-REQUIRED" SB-APPEND  LF SB-APPEND-C
   OPEN-STRING  PROVIDED$ SB-APPEND  CLOSE-STRING
   s"  LRD-POS:DO-PROVIDED" SB-APPEND  LF SB-APPEND-C
   OPEN-STRING  PROVIDED$ SB-APPEND  CLOSE-STRING
   s"  LRD-POS:DO-REQUIRED" SB-APPEND  LF SB-APPEND-C
   S\" s\" PROVIDED-OK\" type cr\n" SB-APPEND
   POSITIVE$ SB$ WRITE-ALL ;

: SETUP ( -- )
   CLEANUP-RESET
   s" habu-load-reject-diag" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT u BYTE-COPY
   u ROOT-U !
   ROOT$ CLEANUP-TREE+
   ROOT$ s" undef.f" UNDEF-PATH JOIN-PATH UNDEF-U !
   ROOT$ s" body.f" BODY-PATH JOIN-PATH BODY-U !
   ROOT$ s" outer.f" OUTER-PATH JOIN-PATH OUTER-U !
   ROOT$ s" include-fragment.f" INCLUDE-FRAG-PATH JOIN-PATH INCLUDE-FRAG-U !
   ROOT$ s" require-fragment.f" REQUIRE-FRAG-PATH JOIN-PATH REQUIRE-FRAG-U !
   ROOT$ s" include-body.f" INCLUDE-PATH JOIN-PATH INCLUDE-U !
   ROOT$ s" require-body.f" REQUIRE-PATH JOIN-PATH REQUIRE-U !
   ROOT$ s" include-mark" INCLUDE-MARK-PATH JOIN-PATH INCLUDE-MARK-U !
   ROOT$ s" require-mark" REQUIRE-MARK-PATH JOIN-PATH REQUIRE-MARK-U !
   ROOT$ s" positive.f" POSITIVE-PATH JOIN-PATH POSITIVE-U !
   ROOT$ s" positive-include.f" POS-INCLUDE-PATH JOIN-PATH POS-INCLUDE-U !
   ROOT$ s" positive-require.f" POS-REQUIRE-PATH JOIN-PATH POS-REQUIRE-U !
   ROOT$ s" provided-missing.f" PROVIDED-PATH JOIN-PATH PROVIDED-U !
   ROOT$ s" missing-hook.f" MISSING-PATH JOIN-PATH MISSING-U !
   ROOT$ s" shadow.f" SHADOW-PATH JOIN-PATH SHADOW-U !
   ROOT$ s" input.f" INPUT-PATH JOIN-PATH INPUT-U !
   ROOT$ s" modeled-input.f" MODELED-INPUT-PATH JOIN-PATH MODELED-INPUT-U !
   ROOT$ s" trusted.f" TRUSTED-PATH JOIN-PATH TRUSTED-U !
   ROOT$ s" recover.f" RECOVER-PATH JOIN-PATH RECOVER-U !
   ROOT$ s" reuse.f" REUSE-PATH JOIN-PATH REUSE-U !
   ROOT$ s" churn.f" CHURN-PATH JOIN-PATH CHURN-U !
   UNDEF$ s" package LRD-FIXTURE private : BAD ( -- ) NO-SUCH-WORD-XYZ ; ;package" WRITE-ALL
   BODY$ s" package LRD-FIXTURE private : BAD ( n -- n ) drop ; ;package" WRITE-ALL
   INCLUDE-FRAG$ s" TOUCH" WRITE-ALL
   REQUIRE-FRAG$ s" TOUCH" WRITE-ALL
   s" include" INCLUDE-FRAG$ INCLUDE-MARK$ INCLUDE$ LOADER-SOURCE!
   s" require" REQUIRE-FRAG$ REQUIRE-MARK$ REQUIRE$ LOADER-SOURCE!
   POS-INCLUDE$ S\" s\" INCLUDED-OK\" type cr\n" WRITE-ALL
   POS-REQUIRE$ S\" s\" REQUIRED-OK\" type cr\n" WRITE-ALL
   POSITIVE-SOURCE!
   MISSING$ S\" package LRD-FIXTURE private\n0 0 set-checks\n: IMM ( -- ) ; immediate\n: DEF-HOOK ( ptr u8 n -- n ) 2drop -1 ;\n' DEF-HOOK set-check\n: BAD ( -- ) IMM ;\n;package\n" WRITE-ALL
   SHADOW$ S\" package LRD-OLD public\n: MODELED ( -- ) ; immediate\n;package\ns\" LRD-OLD:MODELED\" 0 PARSE-IMM\npackage LRD-FIXTURE private\n: MODELED ( -- ) ; immediate\n: BAD ( -- ) MODELED ;\n;package\n" WRITE-ALL
   INPUT$ S\" package LRD-FIXTURE private\n: IMM ( n -- ) drop ; immediate\n: BAD ( -- ) IMM ;\n;package\n" WRITE-ALL
   MODELED-INPUT$ S\" package LRD-FIXTURE private\n: IM ( n -- n n ) dup ; immediate\ns\" IM\" 0 PARSE-IMM\n: BAD ( -- ) IM ;\n;package\n" WRITE-ALL
   TRUSTED$ S\" package LRD-FIXTURE private\n: IMM ( -- ) ; immediate\nTRUSTED: MAIN ( -- ) IMM ;\nMAIN\n;package\n" WRITE-ALL
   RECOVER$ S\" package LRD-FIXTURE private\n: IMM ( -- ) ; immediate\nTRUSTED: EVAL-BAD ( -- ) s\" : BAD ( -- ) IMM ;\" evaluate ;\n' EVAL-BAD catch . cr\n: AFTER ( -- n ) 17 ;\nAFTER . cr\n;package\n" WRITE-ALL
   REUSE$ S\" package LRD-FIXTURE private\n70 constant UNDEFINED-RC\n91 constant REUSE-RC\nvariable OLD-XT\n: ASSERT-RC ( n n -- ) 2dup = if 2drop exit then drop throw ;\nTRUSTED: MODEL-TEMP ( -- ) s\" TEMP\" 0 PARSE-IMM ;\nTRUSTED: ROLLBACK ( -- ) s\" : TEMP ( -- ) ; immediate ' TEMP OLD-XT ! MODEL-TEMP NO-SUCH-ROLLBACK\" evaluate ;\n' ROLLBACK catch UNDEFINED-RC ASSERT-RC\n: REUSED ( -- ) ; immediate\nTRUSTED: ASSERT-REUSE ( -- ) ['] REUSED OLD-XT @ <> if REUSE-RC throw then ;\nASSERT-REUSE\n: BAD ( -- ) REUSED ;\n;package\n" WRITE-ALL
   CHURN$ S\" package LRD-FIXTURE private\n91 constant ROLLBACK-RC\n: ASSERT-RC ( n n -- ) 2dup = if 2drop exit then drop throw ;\nTRUSTED: FAIL ( -- ) s\" : TEMP ( -- ) ; 91 throw\" evaluate ;\nTRUSTED: CHURN ( -- ) 32769 0 ?do ['] FAIL catch ROLLBACK-RC ASSERT-RC loop ;\nCHURN\n: AFTER ( -- n ) 17 ;\nAFTER . cr\n;package\n" WRITE-ALL
   SB-RESET
   s" include " SB-APPEND
   UNDEF$ SB-APPEND
   OUTER$ SB$ WRITE-ALL ;

: STORE! ( len len outcome -- )
   MATCH outcome
     exited OF RC ! 0 0= EXITED ! ENDOF
     signaled OF RC ! 0 0= 0= EXITED ! ENDOF
     timeout OF 0 RC ! 0 0= 0= EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U !  LEN>N OUT-U ! ;

\ Spawn `<hb> --load <fixture>` with empty stdin, capture the exit outcome.
: RUN ( ptr u8 n -- ) {: path:ptr pathu:n :}
   PROC-ARGV-RESET
   s" --load" >LEN PROC-ARGV+
   path pathu >LEN PROC-ARGV+
   ENGINE-CANDIDATE:PATH$ >LEN  EMPTY 0 >LEN  OUT CAP >LEN
   ERR CAP >LEN  TIMEOUT-MS >MS  RUN-ARGV-STDIN-CAPTURE-OUTCOME
   STORE! ;

: ERR$ ( -- ptr u8 n )
   ERR ERR-U @ ;

\ Every rejecting load: exit-kind EXIT, rc 70, EMPTY stdout, NON-EMPTY stderr
\ that names the failing token — the silent-exit-70 red/green discriminator.
: ASSERT-NAMED ( ptr u8 n -- ) {: name:ptr nameu:n :}
   EXITED @ TTRUE
   RC @ REJECT-RC T=
   OUT-U @ 0 T=
   ERR-U @ 0 > TTRUE
   ERR$ name nameu CONTAINS? TTRUE ;

: TEST-STORE-SIGNALED ( -- )
   s" signaled outcome remains distinguishable from exit" T-LABEL
   TEST-OUT-U >LEN TEST-ERR-U >LEN SIGKILL OUTCOME:SIGNALED STORE!
   EXITED @ TFALSE
   RC @ SIGKILL T=
   OUT-U @ TEST-OUT-U T=
   ERR-U @ TEST-ERR-U T= ;

: TEST-STORE-TIMEOUT ( -- )
   s" timeout outcome remains distinguishable from exit" T-LABEL
   TEST-OUT-U >LEN TEST-ERR-U >LEN OUTCOME:TIMEOUT STORE!
   EXITED @ TFALSE
   RC @ 0 T=
   OUT-U @ TEST-OUT-U T=
   ERR-U @ TEST-ERR-U T= ;

: TEST-UNDEF ( -- )
   s" direct --load reject names the undefined word" T-LABEL
   UNDEF$ RUN
   s" E-UNDEFINED" ASSERT-NAMED
   ERR$ s" NO-SUCH-WORD-XYZ" CONTAINS? TTRUE ;

: TEST-BODY ( -- )
   s" checked-body reject names word and token" T-LABEL
   BODY$ RUN
   s" in bad" ASSERT-NAMED
   ERR$ s" at 'drop'" CONTAINS? TTRUE ;

: TEST-REQUIRE-CHAIN ( -- )
   s" require-chain reject names the undefined word" T-LABEL
   OUTER$ RUN
   s" E-UNDEFINED" ASSERT-NAMED
   ERR$ s" NO-SUCH-WORD-XYZ" CONTAINS? TTRUE ;

: TEST-IMMEDIATE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu:n word:ptr wordu:n :}
   path pathu RUN
   s" E-UNMODELED-IMMEDIATE" ASSERT-NAMED
   ERR$ word wordu CONTAINS? TTRUE ;

: TEST-IMMEDIATES ( -- )
   s" include is rejected before its nested evaluate" T-LABEL
   INCLUDE$ s" include" TEST-IMMEDIATE
   s" rejected include produced no external marker" T-LABEL
   INCLUDE-MARK$ FILE? TFALSE
   s" require is rejected before its nested evaluate" T-LABEL
   REQUIRE$ s" require" TEST-IMMEDIATE
   s" rejected require produced no external marker" T-LABEL
   REQUIRE-MARK$ FILE? TFALSE ;

: TEST-RUNTIME-LOADERS ( -- )
   POSITIVE$ RUN
   s" checked runtime loader process exited" T-LABEL
   EXITED @ TTRUE
   s" checked runtime loader process succeeded" T-LABEL
   RC @ 0 T=
   s" checked runtime loader output exact" T-LABEL
   OUT OUT-U @ S\" INCLUDED-OK\nREQUIRED-OK\nPROVIDED-OK\n" T$=
   s" checked runtime loader stderr empty" T-LABEL
   ERR ERR-U @ s" " T$= ;

: TEST-MISSING-HOOK ( -- )
   s" armed checker without immediate hook fails closed" T-LABEL
   MISSING$ RUN
   s" immediate preflight unavailable" ASSERT-NAMED
   ERR$ s" IMM" CONTAINS? TTRUE ;

: TEST-IDENTITY ( -- )
   s" parse model is bound to exact immediate identity" T-LABEL
   SHADOW$ s" MODELED" TEST-IMMEDIATE ;

: TEST-ROLLBACK-REUSE ( -- )
   s" parse model cannot survive exact xt reuse after rollback" T-LABEL
   REUSE$ s" REUSED" TEST-IMMEDIATE ;

: TEST-HIDX-CHURN ( -- )
   s" rolled-back dictionary indices remain bounded and reusable" T-LABEL
   CHURN$ RUN
   EXITED @ TTRUE
   RC @ 0 T=
   OUT OUT-U @ s" 17" CONTAINS? TTRUE
   ERR-U @ 0 T= ;

: TEST-INPUT-ORDER ( -- )
   s" immediate authorization precedes input-depth diagnostics" T-LABEL
   INPUT$ s" IMM" TEST-IMMEDIATE
   ERR$ s" interpret stack underdepth" CONTAINS? TFALSE ;

: TEST-MODELED-INPUT ( -- )
   s" modeled immediate retains compile-time input guard" T-LABEL
   MODELED-INPUT$ RUN
   s" interpret stack underdepth" ASSERT-NAMED
   ERR$ s" IM" CONTAINS? TTRUE
   ERR$ s" E-UNMODELED-IMMEDIATE" CONTAINS? TFALSE ;

: TEST-TRUSTED ( -- )
   s" TRUSTED body may execute an audited immediate" T-LABEL
   TRUSTED$ RUN
   EXITED @ TTRUE
   RC @ 0 T=
   OUT-U @ 0 T=
   ERR-U @ 0 T= ;

: TEST-RECOVER ( -- )
   s" caught preflight rejection restores compiler state" T-LABEL
   RECOVER$ RUN
   EXITED @ TTRUE
   RC @ 0 T=
   OUT OUT-U @ s" 70" CONTAINS? TTRUE
   OUT OUT-U @ s" 17" CONTAINS? TTRUE
   ERR$ s" E-UNMODELED-IMMEDIATE" CONTAINS? TTRUE ;

: MAIN ( -- )
   T-RESET
   SETUP
   TEST-STORE-SIGNALED
   TEST-STORE-TIMEOUT
   TEST-UNDEF
   TEST-BODY
   TEST-REQUIRE-CHAIN
   TEST-IMMEDIATES
   TEST-RUNTIME-LOADERS
   TEST-MISSING-HOOK
   TEST-IDENTITY
   TEST-ROLLBACK-REUSE
   TEST-HIDX-CHURN
   TEST-INPUT-ORDER
   TEST-MODELED-INPUT
   TEST-TRUSTED
   TEST-RECOVER
   CLEANUP-RUN
   T-REPORT
   s" load-reject-diag-test: ok" type cr ;

MAIN

;package
