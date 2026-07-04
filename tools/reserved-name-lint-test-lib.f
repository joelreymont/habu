\ reserved-name-lint-test.f - checked fixtures for reserved-name-lint.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f lib/process-argv.f
\ tools/lint/text.f tools/lint/token.f tools/lint/lib.f
\ tools/lint/json-writer.f tools/lint/source-lex.f tools/argv.f tools/cli-run.f
\ tools/reserved-name-lint-core.f tools/reserved-name-lint-test.f

$1000 constant RNLT-BUF-CAP

create RNLT-ROOT FS-PATH-CAP allot
create RNLT-GOOD FS-PATH-CAP allot
create RNLT-BAD FS-PATH-CAP allot
create RNLT-CASE FS-PATH-CAP allot
create RNLT-LOADER FS-PATH-CAP allot
create RNLT-OUT RNLT-BUF-CAP allot

variable RNLT-ROOT-U
variable RNLT-GOOD-U
variable RNLT-BAD-U
variable RNLT-CASE-U
variable RNLT-LOADER-U

: RNLT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: RNLT-ROOT$ ( -- ptr u8 n )
   RNLT-ROOT RNLT-ROOT-U @ ;

: RNLT-GOOD$ ( -- ptr u8 n )
   RNLT-GOOD RNLT-GOOD-U @ ;

: RNLT-BAD$ ( -- ptr u8 n )
   RNLT-BAD RNLT-BAD-U @ ;

: RNLT-CASE$ ( -- ptr u8 n )
   RNLT-CASE RNLT-CASE-U @ ;

: RNLT-LOADER$ ( -- ptr u8 n )
   RNLT-LOADER RNLT-LOADER-U @ ;

: RNLT-LF ( -- )
   $0A SB-APPEND-C ;

: RNLT-GOOD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : OK ( -- n ) 1 ;" SB-APPEND RNLT-LF
   s" : LOCAL-IJ ( n n -- n ) {: i:n j:n :} i j + ;" SB-APPEND RNLT-LF
   s" variable IX" SB-APPEND RNLT-LF
   s" variable JX" SB-APPEND RNLT-LF
   SB$ ;

: RNLT-BAD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" variable I" SB-APPEND RNLT-LF
   s" 1 constant j" SB-APPEND RNLT-LF
   s" : LOOP ( -- ) ;" SB-APPEND RNLT-LF
   s" : CASE ( -- ) ;" SB-APPEND RNLT-LF
   s" : undefine ( -- ) ;" SB-APPEND RNLT-LF
   SB$ ;

: RNLT-CASE-SRC$ ( -- ptr u8 n )
   s" : i ( -- n ) 1 ;" ;

: RNLT-LOADER-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : include ( -- ) ;" SB-APPEND RNLT-LF
   s" : included ( ptr u8 n -- ) ;" SB-APPEND RNLT-LF
   s" : require ( -- ) ;" SB-APPEND RNLT-LF
   s" : required ( ptr u8 n -- ) ;" SB-APPEND RNLT-LF
   s" : provided ( ptr u8 n -- ) ;" SB-APPEND RNLT-LF
   SB$ ;

: RNLT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-reserved-name-lint" TMPDIR-MKDIR RNLT-ROOT RNLT-ROOT-U RNLT-COPY!
   RNLT-ROOT$ CLEANUP-TREE+
   RNLT-ROOT$ s" good.f" RNLT-GOOD JOIN-PATH RNLT-GOOD-U !
   RNLT-ROOT$ s" bad.f" RNLT-BAD JOIN-PATH RNLT-BAD-U !
   RNLT-ROOT$ s" case.f" RNLT-CASE JOIN-PATH RNLT-CASE-U !
   RNLT-ROOT$ s" loader.f" RNLT-LOADER JOIN-PATH RNLT-LOADER-U !
   RNLT-GOOD$ RNLT-GOOD-SRC$ WRITE-ALL
   RNLT-BAD$ RNLT-BAD-SRC$ WRITE-ALL
   RNLT-CASE$ RNLT-CASE-SRC$ WRITE-ALL
   RNLT-LOADER$ RNLT-LOADER-SRC$ WRITE-ALL ;

: RNLT-CORE-SETUP ( bool -- ) {: json:bool :}
   RESERVED-NAME-LINT-RESET
   RNLT-OUT RNLT-BUF-CAP LINT-OUT-BUFFER!
   json RNL-JSON! ;

: RNLT-CORE-FINISH ( -- n n n n )
   [: RESERVED-NAME-LINT-FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 PROC-OUTCOME-EXIT rc ;

: RNLT-RUN-CORE ( ptr u8 n -- n n n n )
   LINT-FALSE RNLT-CORE-SETUP
   RESERVED-NAME-LINT-FILE
   RNLT-CORE-FINISH ;

: RNLT-RUN-CORE-JSON ( -- n n n n )
   LINT-TRUE RNLT-CORE-SETUP
   RNLT-CASE$ s" <converted>" RESERVED-NAME-LINT-FILE-AS
   RNLT-CORE-FINISH ;

: RNLT-JSON-WORD-I$ ( -- ptr u8 n )
   SB-RESET
   $22 SB-APPEND-C
   s" word" SB-APPEND
   $22 SB-APPEND-C
   $3A SB-APPEND-C
   $22 SB-APPEND-C
   s" i" SB-APPEND
   $22 SB-APPEND-C
   SB$ ;

: RNLT-EXPECT-EXIT ( n n n n n -- n n ) {: outu:n erru:n kind:n code:n want:n :}
   kind PROC-OUTCOME-EXIT T=
   code want T=
   outu erru ;

: RNLT-TEST-GOOD ( -- )
   RNLT-GOOD$ RNLT-RUN-CORE 0 RNLT-EXPECT-EXIT {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: RNLT-TEST-BAD ( -- )
   RNLT-BAD$ RNLT-RUN-CORE 1 RNLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   RNLT-OUT outu s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   RNLT-OUT outu s" `I`" CONTAINS? TTRUE
   RNLT-OUT outu s" `j`" CONTAINS? TTRUE
   RNLT-OUT outu s" `LOOP`" CONTAINS? TTRUE
   RNLT-OUT outu s" `CASE`" CONTAINS? TTRUE
   RNLT-OUT outu s" `undefine`" CONTAINS? TTRUE ;

: RNLT-TEST-JSON ( -- )
   RNLT-RUN-CORE-JSON 1 RNLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   RNLT-OUT outu s" schema_version" CONTAINS? TTRUE
   RNLT-OUT outu s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   RNLT-OUT outu s" <converted>" CONTAINS? TTRUE
   RNLT-OUT outu RNLT-JSON-WORD-I$ CONTAINS? TTRUE ;

: RNLT-TEST-LOADER ( -- )
   RNLT-LOADER$ RNLT-RUN-CORE 1 RNLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   RNLT-OUT outu s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   RNLT-OUT outu s" `include`" CONTAINS? TTRUE
   RNLT-OUT outu s" `included`" CONTAINS? TTRUE
   RNLT-OUT outu s" `require`" CONTAINS? TTRUE
   RNLT-OUT outu s" `required`" CONTAINS? TTRUE
   RNLT-OUT outu s" `provided`" CONTAINS? TTRUE ;

: RNLT-MAIN ( -- )
   T-RESET
   RNLT-PREPARE
   RNLT-TEST-GOOD
   RNLT-TEST-BAD
   RNLT-TEST-JSON
   RNLT-TEST-LOADER
   CLEANUP-RUN
   RNLT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" reserved-name-lint-test: ok" type cr ;
