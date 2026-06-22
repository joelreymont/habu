\ drive-build-lib.f - native stdlib build benchmark driver library.
\
\ Load after bench/llm/drive-stdlib-lib.f, bench/llm/driver-token-helpers.f,
\ and bench/llm/driver-fixture-helpers.f.

10 constant DB-LF

create DB-PRELUDE-PATH FS-PATH-CAP allot
create DB-SOURCE-PATH FS-PATH-CAP allot
create DB-RUN-PATH FS-PATH-CAP allot
create DB-NOART-PATH FS-PATH-CAP allot
create DB-ARTIFACT-PATH FS-PATH-CAP allot
create DB-MISSING-PATH FS-PATH-CAP allot

variable DB-PRELUDE-U
variable DB-SOURCE-U
variable DB-RUN-U
variable DB-NOART-U
variable DB-ARTIFACT-U
variable DB-MISSING-U

: DB-PRELUDE$ ( -- ptr u8 n )
   DB-PRELUDE-PATH DB-PRELUDE-U @ ;

: DB-SOURCE$ ( -- ptr u8 n )
   DB-SOURCE-PATH DB-SOURCE-U @ ;

: DB-RUN$ ( -- ptr u8 n )
   DB-RUN-PATH DB-RUN-U @ ;

: DB-NOART$ ( -- ptr u8 n )
   DB-NOART-PATH DB-NOART-U @ ;

: DB-ARTIFACT$ ( -- ptr u8 n )
   DB-ARTIFACT-PATH DB-ARTIFACT-U @ ;

: DB-MISSING$ ( -- ptr u8 n )
   DB-MISSING-PATH DB-MISSING-U @ ;

: DB-PATHS! ( -- )
   s" prelude.f" DB-PRELUDE-PATH DB-PRELUDE-U DS-JOIN!
   s" build-source-ok.f" DB-SOURCE-PATH DB-SOURCE-U DS-JOIN!
   s" build-make-artifact" DB-RUN-PATH DB-RUN-U DS-JOIN!
   s" build-no-artifact" DB-NOART-PATH DB-NOART-U DS-JOIN!
   s" build-artifact.out" DB-ARTIFACT-PATH DB-ARTIFACT-U DS-JOIN!
   s" build-missing.out" DB-MISSING-PATH DB-MISSING-U DS-JOIN! ;

: DB-CHECK-TASK? ( -- bool )
   DS-NAME$ s" BUILD-CHECK-SOURCE-OK?" STR= ;

: DB-ARTIFACT-TASK? ( -- bool )
   DS-NAME$ s" BUILD-ARTIFACT-OK?" STR= ;

: DB-STEP-STATUS-TASK? ( -- bool )
   DS-NAME$ s" BUILD-STEP-STATUS" STR= ;

: DB-RUN-ARTIFACT-TASK? ( -- bool )
   DS-NAME$ s" BUILD-RUN-ARTIFACT-OK?" STR= ;

: DB-MISSING-ARTIFACT-TASK? ( -- bool )
   DS-NAME$ s" BUILD-MISSING-ARTIFACT" STR= ;

: DB-NEGATIVE-TASK? ( -- bool )
   DB-STEP-STATUS-TASK? if DS-TRUE exit then
   DB-MISSING-ARTIFACT-TASK? ;

: DB-SB-LF ( -- )
   DB-LF SB-APPEND-C ;

: DB-SHEBANG ( -- )
   s" #!/usr/bin/env bin/hb" SB-APPEND
   DB-SB-LF ;

: DB-SB-C, ( n -- )
   FS-MUT-SB-U
   s"  c, " SB-APPEND ;

: DB-SB-BYTES ( ptr u8 n -- ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ DB-SB-C,
      1+
   repeat drop ;

: DB-SB-ZPATH ( ptr u8 n -- )
   s" create P " SB-APPEND
   DB-SB-BYTES
   0 DB-SB-C, ;

: DB-SB-ARTIFACT-DATA ( -- )
   s" create A 97 c, 114 c, 116 c, 105 c, 102 c, 97 c, 99 c, 116 c, " SB-APPEND ;

: DB-RUN-SCRIPT$ ( -- ptr u8 n )
   SB-RESET
   DB-SHEBANG
   DB-ARTIFACT$ DB-SB-ZPATH
   DB-SB-ARTIFACT-DATA
   s" P 1537 420 open dup A 8 write drop close" SB-APPEND
   DB-SB-LF
   SB$ ;

: DB-NOART-SCRIPT$ ( -- ptr u8 n )
   SB-RESET
   DB-SHEBANG
   s" 0 0 0 die" SB-APPEND
   DB-SB-LF
   SB$ ;

: DB-WRITE-RUN-SCRIPT ( -- )
   DB-RUN$ DB-RUN-SCRIPT$ WRITE-ALL
   DB-RUN$ CHMOD-X ;

: DB-WRITE-NOART-SCRIPT ( -- )
   DB-NOART$ DB-NOART-SCRIPT$ WRITE-ALL
   DB-NOART$ CHMOD-X ;

: DB-PREPARE-FIXTURE ( -- )
   DB-PATHS!
   DB-SOURCE$ s" 0 0 1 die : MAIN ( -- i64 ) 42 ; : INC ( i64 -- i64 ) 1 + ;" WRITE-ALL
   DB-WRITE-RUN-SCRIPT
   DB-WRITE-NOART-SCRIPT ;

: DB-BUILD-PRELUDE ( -- )
   DS-TEST-RESET
   s" create BUILD-FIX-PATH-BUF FS-PATH-CAP allot" DS-TEST-LN
   DS-ROOT$ s" BUILD-FIX-ROOT-BUF" s" BUILD-FIX-ROOT$" DFH-STRING
   DB-SOURCE$ s" BUILD-FIX-SOURCE-PATH-BUF" s" BUILD-FIX-SOURCE-PATH$" DFH-STRING
   DB-RUN$ s" BUILD-FIX-RUN-PATH-BUF" s" BUILD-FIX-RUN-PATH$" DFH-STRING
   DB-NOART$ s" BUILD-FIX-NOART-PATH-BUF" s" BUILD-FIX-NOART-PATH$" DFH-STRING
   DB-ARTIFACT$ s" BUILD-FIX-ARTIFACT-PATH-BUF" s" BUILD-FIX-ARTIFACT-PATH$" DFH-STRING
   DB-MISSING$ s" BUILD-FIX-MISSING-PATH-BUF" s" BUILD-FIX-MISSING-PATH$" DFH-STRING
   s" build-artifact.out" s" BUILD-FIX-ARTIFACT-NAME-BUF" s" BUILD-FIX-ARTIFACT-NAME$" DFH-STRING
   s" : BUILD-FIX-BAD-STEP ( -- n ) 7 ;" DS-TEST-LN
   DB-PRELUDE$ DS-TEST$ WRITE-ALL ;

: DB-BUILD-PROMPT ( -- )
   DS-BUILD-PROMPT
   s" " DS-PROMPT-LN
   s" The driver preloads a checked fixture vocabulary for deterministic build tests." DS-PROMPT-LN
   s" Use BUILD-FIX-SOURCE-PATH$, BUILD-FIX-ROOT$, BUILD-FIX-ARTIFACT-NAME$, and BUILD-FIX-PATH-BUF for source/path tasks." DS-PROMPT-LN
   s" BUILD-FIX-RUN-PATH$ is an executable Habu build script that creates BUILD-FIX-ARTIFACT-PATH$." DS-PROMPT-LN
   s" BUILD-FIX-NOART-PATH$ exits 0 without creating BUILD-FIX-MISSING-PATH$." DS-PROMPT-LN
   s" BUILD-FIX-BAD-STEP returns a nonzero build-step status." DS-PROMPT-LN
   s" Do not use SCRIPT-ARGV$, raw RUN-* process helpers, evaluate, or define/shadow BUILD-FIX-* words." DS-PROMPT-LN ;

: DB-CAND-SHADOW-FIXTURE? ( -- bool )
   s" BUILD-FIX-" DS-CAND-DEFINES-PREFIX? ;

: DB-CAND-FORBIDDEN? ( -- bool )
   DS-CAND-FORBIDDEN? if DS-TRUE exit then
   DS-CAND$ s" SCRIPT-ARGV$" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" RUN-RC" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" RUN-CAPTURE" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" RUN-ARGV" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" evaluate" CONTAINS? if DS-TRUE exit then
   DS-CAND$ s" EVALUATE" CONTAINS? if DS-TRUE exit then
   DB-CAND-SHADOW-FIXTURE? ;

: DB-CAND-USES-REQUIRED? ( -- bool )
   DB-CHECK-TASK? if
      s" BUILD-CHECK" DS-CAND-HAS-WORD?
      s" BUILD-FIX-SOURCE-PATH$" DS-CAND-HAS-WORD? and
      s" BUILD-TRUE" DS-CAND-HAS-WORD? and exit
   then
   DB-ARTIFACT-TASK? if
      s" BUILD-ARTIFACT" DS-CAND-HAS-WORD?
      s" BUILD-FIX-ROOT$" DS-CAND-HAS-WORD? and
      s" BUILD-FIX-ARTIFACT-NAME$" DS-CAND-HAS-WORD? and
      s" BUILD-FIX-ARTIFACT-PATH$" DS-CAND-HAS-WORD? and
      s" STR=" DS-CAND-HAS-WORD? and exit
   then
   DB-STEP-STATUS-TASK? if
      s" BUILD-STEP" DS-CAND-HAS-WORD?
      s" BUILD-FIX-BAD-STEP" DS-CAND-HAS-WORD? and exit
   then
   DB-RUN-ARTIFACT-TASK? if
      s" BUILD-RUN" DS-CAND-HAS-WORD?
      s" BUILD-FIX-RUN-PATH$" DS-CAND-HAS-WORD? and
      s" BUILD-FIX-ARTIFACT-PATH$" DS-CAND-HAS-WORD? and exit
   then
   DB-MISSING-ARTIFACT-TASK? if
      s" BUILD-RUN" DS-CAND-HAS-WORD?
      s" BUILD-FIX-NOART-PATH$" DS-CAND-HAS-WORD? and
      s" BUILD-FIX-MISSING-PATH$" DS-CAND-HAS-WORD? and exit
   then
   DS-FALSE ;

: DB-CAND-VALID? ( -- bool )
   DS-CAND-HAS-PUBLIC? 0= if DS-FALSE exit then
   DB-CAND-FORBIDDEN? if DS-FALSE exit then
   DS-CAND-COMPLETE? 0= if DS-FALSE exit then
   DB-CAND-USES-REQUIRED? ;

: DB-INVALID-CANDIDATE ( -- )
   DS-CAND-HAS-PUBLIC? 0= if
      s" missing public task definition" DS-WRITE-INVALID-DIAG
      s" reject" DS-LR-REJECT
      s" habu-stdlib-build" LR-ARM!
      exit
   then
   DB-CAND-FORBIDDEN? if
      s" forbidden build fixture boundary" DS-WRITE-INVALID-DIAG
      s" reject" DS-LR-REJECT
      s" habu-stdlib-build" LR-ARM!
      exit
   then
   DS-CAND-COMPLETE? if
      s" required stdlib word missing" DS-WRITE-INVALID-DIAG
      s" reject" DS-LR-REJECT
      s" habu-stdlib-build" LR-ARM!
      exit
   then
   s" incomplete Forth definition" DS-WRITE-INVALID-DIAG
   s" reject" DS-LR-REJECT
   s" habu-stdlib-build" LR-ARM! ;

: DB-LR-REJECT ( ptr u8 n -- )
   DS-LR-REJECT
   s" habu-stdlib-build" LR-ARM! ;

: DB-LR-PASS ( -- )
   DS-LR-PASS
   s" habu-stdlib-build" LR-ARM! ;

: DB-LR-FAIL ( -- )
   DS-LR-FAIL
   s" habu-stdlib-build" LR-ARM! ;

: DB-ADD-LIBS ( -- )
   DS-ADD-LIBS
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/build.f"  >LEN PROC-ARGV+ ;

: DB-RUN-CHECK ( -- )
   PROC-ARGV-ENV-RESET
   DB-ADD-LIBS
   DB-PRELUDE$  >LEN PROC-ARGV+
   DS-CAND-PATH$  >LEN PROC-ARGV+
   DS-HB-CAPTURE
   DS-CHECK-CLEAN? if
      DS-DIAG-PATH$ s" " WRITE-ALL
      0 DS-DIAG-COUNT !
      exit
   then
   DS-DIAG-PATH$ DS-WRITE-CAPTURE
   1 DS-DIAG-COUNT ! ;

: DB-RUN-TESTS ( -- )
   PROC-ARGV-ENV-RESET
   DB-ADD-LIBS
   DB-PRELUDE$  >LEN PROC-ARGV+
   DS-CAND-PATH$  >LEN PROC-ARGV+
   DS-BUNDLE-PATH$  >LEN PROC-ARGV+
   DS-HB-CAPTURE
   DS-TEST-PATH$ DS-WRITE-CAPTURE ;

: DB-BUILD-POSITIVE-TESTS ( -- )
   DS-TEST-RESET
   DS-STACK-DSL
   s" T{  " DS-TEST+
   DS-NAME$ DS-TEST+
   s"  -> -1 }T" DS-TEST-LN
   s" DST-REPORT" DS-TEST-LN ;

: DB-EXPECTED-CODE$ ( -- ptr u8 n )
   DB-STEP-STATUS-TASK? if s" code E-BUILD-STATUS" exit then
   s" code E-BUILD-PATH" ;

: DB-EXPECTED-CONST$ ( -- ptr u8 n )
   DB-STEP-STATUS-TASK? if s" E-BUILD-STATUS" exit then
   s" E-BUILD-PATH" ;

: DB-BUILD-NEGATIVE-TESTS ( -- )
   DS-TEST-RESET
   s" : DB-NEG-MAIN ( -- )" DS-TEST-LN
   s"    ['] " DS-TEST+
   DS-NAME$ DS-TEST+
   s"  catch" DS-TEST-LN
   s"    dup " DS-TEST+
   DB-EXPECTED-CONST$ DS-TEST+
   s"  = if drop " DS-TEST+
   DB-EXPECTED-CODE$ DFH-SOURCE-S"
   s"  type cr exit then" DS-TEST-LN
   s"    0= if " DS-TEST+
   s" silent success" DFH-SOURCE-S"
   s"  type cr 1 die then" DS-TEST-LN
   s"    " DS-TEST+
   s" wrong error code" DFH-SOURCE-S"
   s"  type cr 1 die ;" DS-TEST-LN
   s" DB-NEG-MAIN" DS-TEST-LN ;

: DB-WRITE-BUNDLE ( -- )
   DS-BUNDLE-PATH$ DS-TEST$ WRITE-ALL ;

: DB-FINISH-NEGATIVE ( -- )
   DB-EXPECTED-CODE$ DS-LR-NEGATIVE ;

: DB-BUILD-TESTS ( -- )
   DB-NEGATIVE-TASK? if DB-BUILD-NEGATIVE-TESTS exit then
   DB-BUILD-POSITIVE-TESTS ;

: DB-EVALUATE-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   text textu DS-EXTRACT-CANDIDATE
   DS-CAND-PATH$ DS-CAND$ WRITE-ALL
   DB-BUILD-TESTS
   DB-WRITE-BUNDLE
   DB-CAND-VALID? 0= if DB-INVALID-CANDIDATE exit then
   DB-RUN-CHECK
   DS-RC @ 0 <> if s" reject" DB-LR-REJECT exit then
   DB-RUN-TESTS
   DB-NEGATIVE-TASK? if DB-FINISH-NEGATIVE exit then
   DS-TEST-PASS? if DB-LR-PASS else DB-LR-FAIL then ;

: DB-PREPARE ( -- )
   CLEANUP-RESET
   DS-TEMP
   DB-PREPARE-FIXTURE
   DB-BUILD-PRELUDE
   DB-BUILD-PROMPT
   DS-PROMPT-PATH$ DS-PROMPT$ WRITE-ALL
   DS-WRITE-EMPTY-ARTIFACTS ;

: DB-RUN-TEXT ( ptr u8 n -- ) {: text:ptr textu :}
   textu DS-OUT-CAP > if E-DS-CAPACITY throw then
   text DS-OUT-BUF textu BYTE-COPY
   textu DS-OUT-U !
   DB-PREPARE
   0 DS-TOKENS !
   DS-RAW-PATH$ DS-OUT-BUF DS-OUT-U @ WRITE-ALL
   DS-OUT-BUF DS-OUT-U @ DB-EVALUATE-TEXT ;
