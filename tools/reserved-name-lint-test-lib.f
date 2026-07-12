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
create RNLT-TFAM FS-PATH-CAP allot
create RNLT-CONTROL FS-PATH-CAP allot
create RNLT-NUM FS-PATH-CAP allot
create RNLT-OUT RNLT-BUF-CAP allot

variable RNLT-ROOT-U
variable RNLT-GOOD-U
variable RNLT-BAD-U
variable RNLT-CASE-U
variable RNLT-LOADER-U
variable RNLT-TFAM-U
variable RNLT-CONTROL-U
variable RNLT-NUM-U

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

: RNLT-TFAM$ ( -- ptr u8 n )
   RNLT-TFAM RNLT-TFAM-U @ ;

: RNLT-CONTROL$ ( -- ptr u8 n )
   RNLT-CONTROL RNLT-CONTROL-U @ ;

: RNLT-NUM$ ( -- ptr u8 n )
   RNLT-NUM RNLT-NUM-U @ ;

: RNLT-LF ( -- )
   $0A SB-APPEND-C ;

: RNLT-GOOD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : OK ( -- n ) 1 ;" SB-APPEND RNLT-LF
   s" : LOCAL-IJ ( n n -- n ) {: i:n j:n :} i j + ;" SB-APPEND RNLT-LF
   s" variable IX" SB-APPEND RNLT-LF
   s" variable JX" SB-APPEND RNLT-LF
   s" : .U ( n -- ) drop ;" SB-APPEND RNLT-LF        \ dot-letter: not a number
   s" : .INT ( n -- ) drop ;" SB-APPEND RNLT-LF
   s" : F.N ( r n -- ) drop fdrop ;" SB-APPEND RNLT-LF
   s" : 1STNZ ( -- n ) 1 ;" SB-APPEND RNLT-LF        \ digit-leading, not number-shaped
   s" : 0<> ( n -- bool ) 0 <> ;" SB-APPEND RNLT-LF
   s" : 2UNIFY-OR ( -- ) ;" SB-APPEND RNLT-LF
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

: RNLT-TFAM-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : typefamily ( -- ) ;" SB-APPEND RNLT-LF
   s" : SUMTYPE ( -- ) ;" SB-APPEND RNLT-LF
   s" : variant ( -- ) ;" SB-APPEND RNLT-LF
   s" : ;VARIANT ( -- ) ;" SB-APPEND RNLT-LF
   s" : ;sumtype ( -- ) ;" SB-APPEND RNLT-LF
   s" : Export ( -- ) ;" SB-APPEND RNLT-LF
   SB$ ;

\ TFAM 9: construct/MATCH/;MATCH are reserved control forms — a definition may
\ not take those names (case-folded), exactly like case/of/endof.
: RNLT-CONTROL-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : match ( -- ) ;" SB-APPEND RNLT-LF
   s" : ;match ( -- ) ;" SB-APPEND RNLT-LF
   s" : CONSTRUCT ( -- ) ;" SB-APPEND RNLT-LF
   SB$ ;

\ Number-shaped names: the numeric parser wins over the dictionary
\ (test/gate-dictionary-lib.f GD-LITERAL-FIRST), so these definitions are
\ unreachable or float-confusable and must be rejected (the lib/fmt.f .0/U.0
\ incident: a generator read `60 .0` as a float spelling).
: RNLT-NUM-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : .0 ( n -- ) drop ;" SB-APPEND RNLT-LF
   s" : U.0 ( n -- ) drop ;" SB-APPEND RNLT-LF
   s" : 42 ( -- ) ;" SB-APPEND RNLT-LF
   s" : -7 ( -- ) ;" SB-APPEND RNLT-LF
   s" variable 1.5" SB-APPEND RNLT-LF
   s" 1 constant $FF" SB-APPEND RNLT-LF
   SB$ ;

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
   RNLT-ROOT$ s" tfam.f" RNLT-TFAM JOIN-PATH RNLT-TFAM-U !
   RNLT-ROOT$ s" control.f" RNLT-CONTROL JOIN-PATH RNLT-CONTROL-U !
   RNLT-ROOT$ s" numeric.f" RNLT-NUM JOIN-PATH RNLT-NUM-U !
   RNLT-GOOD$ RNLT-GOOD-SRC$ WRITE-ALL
   RNLT-BAD$ RNLT-BAD-SRC$ WRITE-ALL
   RNLT-CASE$ RNLT-CASE-SRC$ WRITE-ALL
   RNLT-LOADER$ RNLT-LOADER-SRC$ WRITE-ALL
   RNLT-TFAM$ RNLT-TFAM-SRC$ WRITE-ALL
   RNLT-CONTROL$ RNLT-CONTROL-SRC$ WRITE-ALL
   RNLT-NUM$ RNLT-NUM-SRC$ WRITE-ALL ;

: RNLT-CORE-SETUP ( bool -- ) {: json:bool :}
   RESERVED-NAME-LINT-RESET
   RNLT-OUT RNLT-BUF-CAP LINT-OUT-BUFFER!
   json RNL-JSON! ;

: RNLT-CORE-FINISH ( -- n n n )
   [: RESERVED-NAME-LINT-FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 rc ;

: RNLT-RUN-CORE ( ptr u8 n -- n n n )
   LINT-FALSE RNLT-CORE-SETUP
   RESERVED-NAME-LINT-FILE
   RNLT-CORE-FINISH ;

: RNLT-RUN-CORE-JSON ( -- n n n )
   LINT-TRUE RNLT-CORE-SETUP
   RNLT-CASE$ s" <converted>" RESERVED-NAME-LINT-FILE-AS
   RNLT-CORE-FINISH ;

: RNLT-RUN-NUM-JSON ( -- n n n )
   LINT-TRUE RNLT-CORE-SETUP
   RNLT-NUM$ RESERVED-NAME-LINT-FILE
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

: RNLT-EXPECT-EXIT ( n n n n -- n n ) {: outu:n erru:n code:n want:n :}
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

: RNLT-TEST-TFAM ( -- )
   RNLT-TFAM$ RNLT-RUN-CORE 1 RNLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   RNLT-OUT outu s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   RNLT-OUT outu s" `typefamily`" CONTAINS? TTRUE
   RNLT-OUT outu s" `SUMTYPE`" CONTAINS? TTRUE
   RNLT-OUT outu s" `variant`" CONTAINS? TTRUE
   RNLT-OUT outu s" `;VARIANT`" CONTAINS? TTRUE
   RNLT-OUT outu s" `;sumtype`" CONTAINS? TTRUE
   RNLT-OUT outu s" `Export`" CONTAINS? TTRUE ;

: RNLT-TEST-LOADER ( -- )
   RNLT-LOADER$ RNLT-RUN-CORE 1 RNLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   RNLT-OUT outu s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   RNLT-OUT outu s" `include`" CONTAINS? TTRUE
   RNLT-OUT outu s" `included`" CONTAINS? TTRUE
   RNLT-OUT outu s" `require`" CONTAINS? TTRUE
   RNLT-OUT outu s" `required`" CONTAINS? TTRUE
   RNLT-OUT outu s" `provided`" CONTAINS? TTRUE ;

: RNLT-TEST-CONTROL ( -- )
   RNLT-CONTROL$ RNLT-RUN-CORE 1 RNLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   RNLT-OUT outu s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   RNLT-OUT outu s" `match`" CONTAINS? TTRUE
   RNLT-OUT outu s" `;match`" CONTAINS? TTRUE
   RNLT-OUT outu s" `CONSTRUCT`" CONTAINS? TTRUE ;

: RNLT-TEST-NUMERIC ( -- )
   RNLT-NUM$ RNLT-RUN-CORE 1 RNLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   RNLT-OUT outu s" E-NUMERIC-DEFINITION" CONTAINS? TTRUE
   RNLT-OUT outu s" `.0`" CONTAINS? TTRUE
   RNLT-OUT outu s" `U.0`" CONTAINS? TTRUE
   RNLT-OUT outu s" `42`" CONTAINS? TTRUE
   RNLT-OUT outu s" `-7`" CONTAINS? TTRUE
   RNLT-OUT outu s" `1.5`" CONTAINS? TTRUE
   RNLT-OUT outu s" `$FF`" CONTAINS? TTRUE ;

: RNLT-TEST-NUMERIC-JSON ( -- )
   RNLT-RUN-NUM-JSON 1 RNLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   RNLT-OUT outu s" schema_version" CONTAINS? TTRUE
   RNLT-OUT outu s" E-NUMERIC-DEFINITION" CONTAINS? TTRUE
   RNLT-OUT outu s" U.0" CONTAINS? TTRUE ;

: RNLT-MAIN ( -- )
   T-RESET
   RNLT-PREPARE
   RNLT-TEST-GOOD
   RNLT-TEST-BAD
   RNLT-TEST-JSON
   RNLT-TEST-LOADER
   RNLT-TEST-TFAM
   RNLT-TEST-CONTROL
   RNLT-TEST-NUMERIC
   RNLT-TEST-NUMERIC-JSON
   CLEANUP-RUN
   RNLT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" reserved-name-lint-test: ok" type cr ;
