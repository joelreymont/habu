\ build-test.f - focused tests for checked build helpers.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f lib/fs-mutate.f lib/process.f lib/build.f lib/build-test.f

create BUILD-TEST-PATH FS-PATH-CAP allot
create BUILD-TEST-STEP BUILD-STEP-CELLS cells allot

variable BT-ROOT-U
variable BT-SRC-U
variable BT-MISSING-U
variable BT-CMD-OK-U
variable BT-CMD-NOART-U
variable BT-CMD-FAIL-U
variable BT-BAD-SRC-U
variable BT-UNCHECKABLE-SRC-U
variable BT-TOP-DIE-SRC-U

create BT-ROOT-BUF FS-PATH-CAP allot
create BT-SRC-BUF FS-PATH-CAP allot
create BT-MISSING-BUF FS-PATH-CAP allot
create BT-CMD-OK-BUF FS-PATH-CAP allot
create BT-CMD-NOART-BUF FS-PATH-CAP allot
create BT-CMD-FAIL-BUF FS-PATH-CAP allot
create BT-BAD-SRC-BUF FS-PATH-CAP allot
create BT-UNCHECKABLE-SRC-BUF FS-PATH-CAP allot
create BT-TOP-DIE-SRC-BUF FS-PATH-CAP allot

: BT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: BT-PATH! ( ptr u8 n ptr u8 n ptr u8 ptr n -- ) {: pa:ptr pu na:ptr nu dst:ptr lenp:ptr :}
   pa pu na nu dst JOIN-PATH lenp ! ;

: BT-ROOT ( -- ptr u8 n )
   BT-ROOT-BUF BT-ROOT-U @ ;

: BT-SRC ( -- ptr u8 n )
   BT-SRC-BUF BT-SRC-U @ ;

: BT-MISSING ( -- ptr u8 n )
   BT-MISSING-BUF BT-MISSING-U @ ;

: BT-CMD-OK ( -- ptr u8 n )
   BT-CMD-OK-BUF BT-CMD-OK-U @ ;

: BT-CMD-NOART ( -- ptr u8 n )
   BT-CMD-NOART-BUF BT-CMD-NOART-U @ ;

: BT-CMD-FAIL ( -- ptr u8 n )
   BT-CMD-FAIL-BUF BT-CMD-FAIL-U @ ;

: BT-BAD-SRC ( -- ptr u8 n )
   BT-BAD-SRC-BUF BT-BAD-SRC-U @ ;

: BT-UNCHECKABLE-SRC ( -- ptr u8 n )
   BT-UNCHECKABLE-SRC-BUF BT-UNCHECKABLE-SRC-U @ ;

: BT-TOP-DIE-SRC ( -- ptr u8 n )
   BT-TOP-DIE-SRC-BUF BT-TOP-DIE-SRC-U @ ;

: BT-ROOT! ( -- )
   s" habu-build" TMPDIR-MKDIR {: a:ptr u :}
   a u BT-ROOT-BUF BT-ROOT-U BT-COPY! ;

: BT-PATHS! ( -- )
   BT-ROOT s" source.f" BT-SRC-BUF BT-SRC-U BT-PATH!
   BT-ROOT s" missing.f" BT-MISSING-BUF BT-MISSING-U BT-PATH!
   BT-ROOT s" cmd-ok.f" BT-CMD-OK-BUF BT-CMD-OK-U BT-PATH!
   BT-ROOT s" cmd-noart.f" BT-CMD-NOART-BUF BT-CMD-NOART-U BT-PATH!
   BT-ROOT s" cmd-fail.f" BT-CMD-FAIL-BUF BT-CMD-FAIL-U BT-PATH!
   BT-ROOT s" bad.f" BT-BAD-SRC-BUF BT-BAD-SRC-U BT-PATH!
   BT-ROOT s" uncheckable.f" BT-UNCHECKABLE-SRC-BUF BT-UNCHECKABLE-SRC-U BT-PATH!
   BT-ROOT s" top-die.f" BT-TOP-DIE-SRC-BUF BT-TOP-DIE-SRC-U BT-PATH! ;

: BT-ART ( -- ptr u8 n )
   BT-ROOT s" artifact.bin" BUILD-ARTIFACT ;

: BT-NOART ( -- ptr u8 n )
   BT-ROOT s" noart.bin" BUILD-ARTIFACT ;

: BT-SHEBANG ( -- )
   s" #!/usr/bin/env bin/hb" SB-APPEND
   10 SB-APPEND-C ;

: BT-SB-C, ( n -- )
   FS-MUT-SB-U
   s"  c, " SB-APPEND ;

: BT-SB-BYTES ( ptr u8 n -- ) {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ BT-SB-C,
      1+
   repeat drop ;

: BT-SB-PATH ( ptr u8 n -- )
   s" create P " SB-APPEND
   BT-SB-BYTES
   0 BT-SB-C, ;

: BT-SB-ARTIFACT-DATA ( -- )
   s" create A 97 c, 114 c, 116 c, 105 c, 102 c, 97 c, 99 c, 116 c, " SB-APPEND ;

: BT-SCRIPT$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u :}
   SB-RESET
   BT-SHEBANG
   a u SB-APPEND
   SB$ ;

: BT-OK-SCRIPT$ ( -- ptr u8 n )
   SB-RESET
   BT-SHEBANG
   BT-ART BT-SB-PATH
   BT-SB-ARTIFACT-DATA
   s" P 1537 420 open dup A 8 write drop close" SB-APPEND
   SB$ ;

: BT-WRITE-FILE ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu src:ptr srcu :}
   path pathu src srcu WRITE-ALL
   path pathu CLEANUP+ ;

: BT-WRITE-CMD ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu src:ptr srcu :}
   src srcu BT-SCRIPT$ {: code:ptr codeu :}
   path pathu code codeu WRITE-ALL
   path pathu CHMOD-X
   path pathu CLEANUP+ ;

: BT-WRITE-CMD-OK ( -- )
   BT-CMD-OK BT-OK-SCRIPT$ WRITE-ALL
   BT-CMD-OK CHMOD-X
   BT-CMD-OK CLEANUP+ ;

: BT-WRITE-FIXTURES ( -- )
   BT-SRC s" : MAIN ( -- ) ; : INC ( i64 -- i64 ) 1 + ;" BT-WRITE-FILE
   BT-BAD-SRC s" : BAD ( i64 -- i64 ) 0= ;" BT-WRITE-FILE
   BT-UNCHECKABLE-SRC s" : UNCHECKABLE ( i64 -- i64 ) evaluate ;" BT-WRITE-FILE
   BT-TOP-DIE-SRC s" 0 0 1 die : SAFE ( i64 -- i64 ) 1 + ;" BT-WRITE-FILE
   BT-WRITE-CMD-OK
   BT-CMD-NOART s" : NOART ( -- ) ; NOART" BT-WRITE-CMD
   BT-CMD-FAIL s" 0 0 7 die" BT-WRITE-CMD
   BT-ART CLEANUP+
   BT-NOART CLEANUP+ ;

: BT-PREPARE ( -- )
   CLEANUP-RESET
   BT-ROOT!
   BT-ROOT CLEANUP-DIR+
   BT-PATHS!
   BT-WRITE-FIXTURES ;

: BT-CLEANUP ( -- )
   CLEANUP-RUN
   BT-ROOT EXISTS? TFALSE ;

: BT-CHECK-ARTIFACT-PATH ( -- )
   BT-ROOT s" artifact.bin" BUILD-ARTIFACT {: got:ptr gotu :}
   BT-ROOT s" artifact.bin" BUILD-TEST-PATH JOIN-PATH {: wantu :}
   got gotu BUILD-TEST-PATH wantu T$= ;

: BT-FILL-STEP ( -- )
   BUILD-TEST-STEP BUILD-STEP-CLEAR
   s" make-artifact" BUILD-TEST-STEP BUILD-STEP-NAME!
   BT-CMD-OK BUILD-TEST-STEP BUILD-STEP-COMMAND!
   s" --emit-artifact" BUILD-TEST-STEP BUILD-STEP-ARGV!
   BT-ROOT BUILD-TEST-STEP BUILD-STEP-TMP!
   BT-ART BUILD-TEST-STEP BUILD-STEP-ARTIFACT! ;

: BT-FILL-STEP-NOART ( -- )
   BUILD-TEST-STEP BUILD-STEP-CLEAR
   s" no-artifact" BUILD-TEST-STEP BUILD-STEP-NAME!
   BT-CMD-NOART BUILD-TEST-STEP BUILD-STEP-COMMAND!
   s" --missing-artifact" BUILD-TEST-STEP BUILD-STEP-ARGV!
   BT-ROOT BUILD-TEST-STEP BUILD-STEP-TMP!
   BT-NOART BUILD-TEST-STEP BUILD-STEP-ARTIFACT! ;

: BT-OK-STEP ( -- n )
   0 ;

: BT-BAD-STEP ( -- n )
   7 ;

: BT-MISSING-SOURCE ( -- )
   BT-MISSING BUILD-CHECK ;

: BT-BAD-SOURCE ( -- )
   BT-BAD-SRC BUILD-CHECK ;

: BT-UNCHECKABLE-SOURCE ( -- )
   BT-UNCHECKABLE-SRC BUILD-CHECK ;

: BT-MISSING-EXPECT ( -- )
   BT-NOART BUILD-EXPECT ;

: BT-EMPTY-ARTIFACT ( -- )
   BT-ROOT s" " BUILD-ARTIFACT 2drop ;

: BT-BAD-STEP-RUN ( -- )
   s" bad-step" [: BT-BAD-STEP ;] BUILD-STEP ;

: BT-EMPTY-STEP ( -- )
   s" " [: BT-OK-STEP ;] BUILD-STEP ;

: BT-MISSING-COMMAND ( -- )
   BT-MISSING BT-ART BUILD-RUN drop ;

: BT-NO-ARTIFACT ( -- )
   BT-CMD-NOART BT-NOART BUILD-RUN drop ;

: BT-FAIL-COMMAND ( -- )
   BT-CMD-FAIL BT-ART BUILD-RUN drop ;

: BT-BAD-STEP-FIELD ( -- )
   BUILD-TEST-STEP BUILD-STEP-CELLS BUILD-STEP-FIELD drop ;

: BT-MISSING-STEP-COMMAND ( -- )
   BUILD-TEST-STEP BUILD-STEP-CLEAR
   s" bad" BUILD-TEST-STEP BUILD-STEP-NAME!
   s" --none" BUILD-TEST-STEP BUILD-STEP-ARGV!
   BT-ROOT BUILD-TEST-STEP BUILD-STEP-TMP!
   BT-ART BUILD-TEST-STEP BUILD-STEP-ARTIFACT!
   BUILD-TEST-STEP BUILD-STEP-RUN drop ;

: BT-EMPTY-STEP-TMP ( -- )
   s" " BUILD-TEST-STEP BUILD-STEP-TMP! ;

: BT-NOART-STEP-RUN ( -- )
   BT-FILL-STEP-NOART
   BUILD-TEST-STEP BUILD-STEP-RUN drop ;

: BUILD-TEST-PATHS ( -- )
   BT-SRC BUILD-CHECK
   BT-TOP-DIE-SRC BUILD-CHECK
   ['] BT-MISSING-SOURCE E-BUILD-SOURCE TTHROWS
   ['] BT-BAD-SOURCE E-BUILD-SOURCE TTHROWS
   ['] BT-UNCHECKABLE-SOURCE E-BUILD-SOURCE TTHROWS
   BT-CHECK-ARTIFACT-PATH
   ['] BT-MISSING-EXPECT E-BUILD-PATH TTHROWS
   ['] BT-EMPTY-ARTIFACT E-BUILD-PATH TTHROWS ;

: BUILD-TEST-STEPS ( -- )
   s" ok-step" [: BT-OK-STEP ;] BUILD-STEP
   ['] BT-BAD-STEP-RUN E-BUILD-STATUS TTHROWS
   ['] BT-EMPTY-STEP E-BUILD-COMMAND TTHROWS ;

: BUILD-TEST-RUNS ( -- )
   BT-CMD-OK BT-ART BUILD-RUN 0 T=
   BT-ART FILE? TTRUE
   ['] BT-MISSING-COMMAND E-BUILD-COMMAND TTHROWS
   ['] BT-NO-ARTIFACT E-BUILD-PATH TTHROWS
   ['] BT-FAIL-COMMAND E-BUILD-STATUS TTHROWS ;

: BUILD-TEST-RECORDS ( -- )
   BT-FILL-STEP
   BUILD-TEST-STEP BUILD-STEP-NAME$ s" make-artifact" T$=
   BUILD-TEST-STEP BUILD-STEP-COMMAND$ BT-CMD-OK T$=
   BUILD-TEST-STEP BUILD-STEP-ARGV$ s" --emit-artifact" T$=
   BUILD-TEST-STEP BUILD-STEP-TMP$ BT-ROOT T$=
   BUILD-TEST-STEP BUILD-STEP-ARTIFACT$ BT-ART T$=
   BUILD-TEST-STEP BUILD-STEP-VALIDATE
   BUILD-TEST-STEP BUILD-STEP-RUN 0 T=
   BUILD-TEST-STEP BUILD-STEP-RC@ 0 T=
   BT-ART FILE? TTRUE
   ['] BT-BAD-STEP-FIELD E-BUILD-PATH TTHROWS
   ['] BT-MISSING-STEP-COMMAND E-BUILD-COMMAND TTHROWS
   ['] BT-EMPTY-STEP-TMP E-BUILD-PATH TTHROWS
   ['] BT-NOART-STEP-RUN E-BUILD-PATH TTHROWS ;

: BUILD-TEST-MAIN ( -- )
   T-RESET
   BT-PREPARE
   BUILD-TEST-PATHS
   BUILD-TEST-STEPS
   BUILD-TEST-RUNS
   BUILD-TEST-RECORDS
   BT-CLEANUP
   T-REPORT
   s" build-test: ok" type cr ;

BUILD-TEST-MAIN
