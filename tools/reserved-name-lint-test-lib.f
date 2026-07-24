\ reserved-name-lint-test-lib.f - checked fixtures for tools/reserved-name-lint.f.
\ Load-only fixture library; the thin entry tools/reserved-name-lint-test.f
\ requires this file and its dependencies, then calls RESERVED-NAME-LINT-TEST:RUN.

package RESERVED-NAME-LINT-TEST

$1000 constant BUF-CAP

create ROOT FS-PATH-CAP allot
create GOOD FS-PATH-CAP allot
create BAD FS-PATH-CAP allot
create CASEFOLD FS-PATH-CAP allot
create LOADER FS-PATH-CAP allot
create TFAM FS-PATH-CAP allot
create CONTROL FS-PATH-CAP allot
create NUM FS-PATH-CAP allot
create OUT BUF-CAP allot

variable ROOT-U
variable GOOD-U
variable BAD-U
variable CASEFOLD-U
variable LOADER-U
variable TFAM-U
variable CONTROL-U
variable NUM-U

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;

: GOOD$ ( -- ptr u8 n )
   GOOD GOOD-U @ ;

: BAD$ ( -- ptr u8 n )
   BAD BAD-U @ ;

: CASEFOLD$ ( -- ptr u8 n )
   CASEFOLD CASEFOLD-U @ ;

: LOADER$ ( -- ptr u8 n )
   LOADER LOADER-U @ ;

: TFAM$ ( -- ptr u8 n )
   TFAM TFAM-U @ ;

: CONTROL$ ( -- ptr u8 n )
   CONTROL CONTROL-U @ ;

: NUM$ ( -- ptr u8 n )
   NUM NUM-U @ ;

: LF ( -- )
   $0A SB-APPEND-C ;

: GOOD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : OK ( -- n ) 1 ;" SB-APPEND LF
   s" : LOCAL-IJ ( n n -- n ) {: i:n j:n :} i j + ;" SB-APPEND LF
   s" variable IX" SB-APPEND LF
   s" variable JX" SB-APPEND LF
   s" : .U ( n -- ) drop ;" SB-APPEND LF        \ dot-letter: not a number
   s" : .INT ( n -- ) drop ;" SB-APPEND LF
   s" : F.N ( r n -- ) drop fdrop ;" SB-APPEND LF
   s" : 1STNZ ( -- n ) 1 ;" SB-APPEND LF        \ digit-leading, not number-shaped
   s" : 0<> ( n -- bool ) 0 <> ;" SB-APPEND LF
   s" : 2UNIFY-OR ( -- ) ;" SB-APPEND LF
   SB$ ;

: BAD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" variable I" SB-APPEND LF
   s" 1 constant j" SB-APPEND LF
   s" : LOOP ( -- ) ;" SB-APPEND LF
   s" : CASE ( -- ) ;" SB-APPEND LF
   s" : undefine ( -- ) ;" SB-APPEND LF
   s" 2 LAYOUT-BUFFER IF sample" SB-APPEND LF
   SB$ ;

: CASEFOLD-SRC$ ( -- ptr u8 n )
   s" : i ( -- n ) 1 ;" ;

: TFAM-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : typefamily ( -- ) ;" SB-APPEND LF
   s" : SUMTYPE ( -- ) ;" SB-APPEND LF
   s" : variant ( -- ) ;" SB-APPEND LF
   s" : ;VARIANT ( -- ) ;" SB-APPEND LF
   s" : ;sumtype ( -- ) ;" SB-APPEND LF
   s" : ;package ( -- ) ;" SB-APPEND LF
   s" : Export ( -- ) ;" SB-APPEND LF
   s" : LAYOUT-BUFFER ( -- ) ;" SB-APPEND LF
   SB$ ;

\ TFAM 9: construct/MATCH/;MATCH are reserved control forms — a definition may
\ not take those names (case-folded), exactly like case/of/endof.
: CONTROL-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : match ( -- ) ;" SB-APPEND LF
   s" : ;match ( -- ) ;" SB-APPEND LF
   s" : CONSTRUCT ( -- ) ;" SB-APPEND LF
   SB$ ;

\ Number-shaped names: the numeric parser wins over the dictionary
\ (test/gate-dictionary-lib.f GD-LITERAL-FIRST), so these definitions are
\ unreachable or float-confusable and must be rejected (the lib/fmt.f .0/U.0
\ incident: a generator read `60 .0` as a float spelling).
: NUM-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : .0 ( n -- ) drop ;" SB-APPEND LF
   s" : U.0 ( n -- ) drop ;" SB-APPEND LF
   s" : 42 ( -- ) ;" SB-APPEND LF
   s" : -7 ( -- ) ;" SB-APPEND LF
   s" variable 1.5" SB-APPEND LF
   s" 1 constant $FF" SB-APPEND LF
   SB$ ;

: LOADER-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : include ( -- ) ;" SB-APPEND LF
   s" : included ( ptr u8 n -- ) ;" SB-APPEND LF
   s" : require ( -- ) ;" SB-APPEND LF
   s" : required ( ptr u8 n -- ) ;" SB-APPEND LF
   s" : provided ( ptr u8 n -- ) ;" SB-APPEND LF
   SB$ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-reserved-name-lint" TMPDIR-MKDIR ROOT ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   ROOT$ s" good.f" GOOD JOIN-PATH GOOD-U !
   ROOT$ s" bad.f" BAD JOIN-PATH BAD-U !
   ROOT$ s" case.f" CASEFOLD JOIN-PATH CASEFOLD-U !
   ROOT$ s" loader.f" LOADER JOIN-PATH LOADER-U !
   ROOT$ s" tfam.f" TFAM JOIN-PATH TFAM-U !
   ROOT$ s" control.f" CONTROL JOIN-PATH CONTROL-U !
   ROOT$ s" numeric.f" NUM JOIN-PATH NUM-U !
   GOOD$ GOOD-SRC$ WRITE-ALL
   BAD$ BAD-SRC$ WRITE-ALL
   CASEFOLD$ CASEFOLD-SRC$ WRITE-ALL
   LOADER$ LOADER-SRC$ WRITE-ALL
   TFAM$ TFAM-SRC$ WRITE-ALL
   CONTROL$ CONTROL-SRC$ WRITE-ALL
   NUM$ NUM-SRC$ WRITE-ALL ;

: CORE-SETUP ( bool -- ) {: json:bool :}
   RESERVED-NAME-LINT:RESET
   OUT BUF-CAP LINT-OUT-BUFFER!
   json RESERVED-NAME-LINT:JSON! ;

: CORE-FINISH ( -- n n n )
   [: RESERVED-NAME-LINT:FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 rc ;

: RUN-CORE ( ptr u8 n -- n n n )
   LINT-FALSE CORE-SETUP
   RESERVED-NAME-LINT:FILE
   CORE-FINISH ;

: RUN-CORE-JSON ( -- n n n )
   LINT-TRUE CORE-SETUP
   CASEFOLD$ s" <converted>" RESERVED-NAME-LINT:FILE-AS
   CORE-FINISH ;

: RUN-NUM-JSON ( -- n n n )
   LINT-TRUE CORE-SETUP
   NUM$ RESERVED-NAME-LINT:FILE
   CORE-FINISH ;

: JSON-WORD-I$ ( -- ptr u8 n )
   SB-RESET
   $22 SB-APPEND-C
   s" word" SB-APPEND
   $22 SB-APPEND-C
   $3A SB-APPEND-C
   $22 SB-APPEND-C
   s" i" SB-APPEND
   $22 SB-APPEND-C
   SB$ ;

: EXPECT-EXIT ( n n n n -- n n ) {: outu:n erru:n code:n want:n :}
   code want T=
   outu erru ;

: TEST-GOOD ( -- )
   GOOD$ RUN-CORE 0 EXPECT-EXIT {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-BAD ( -- )
   BAD$ RUN-CORE 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   OUT outu s" `I`" CONTAINS? TTRUE
   OUT outu s" `j`" CONTAINS? TTRUE
   OUT outu s" `LOOP`" CONTAINS? TTRUE
   OUT outu s" `CASE`" CONTAINS? TTRUE
   OUT outu s" `undefine`" CONTAINS? TTRUE
   OUT outu s" `IF`" CONTAINS? TTRUE ;

: TEST-JSON ( -- )
   RUN-CORE-JSON 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu s" schema_version" CONTAINS? TTRUE
   OUT outu s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   OUT outu s" <converted>" CONTAINS? TTRUE
   OUT outu JSON-WORD-I$ CONTAINS? TTRUE ;

: TEST-TFAM ( -- )
   TFAM$ RUN-CORE 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   OUT outu s" `typefamily`" CONTAINS? TTRUE
   OUT outu s" `SUMTYPE`" CONTAINS? TTRUE
   OUT outu s" `variant`" CONTAINS? TTRUE
   OUT outu s" `;VARIANT`" CONTAINS? TTRUE
   OUT outu s" `;sumtype`" CONTAINS? TTRUE
   OUT outu s" `;package`" CONTAINS? TTRUE
   OUT outu s" `Export`" CONTAINS? TTRUE
   OUT outu s" `LAYOUT-BUFFER`" CONTAINS? TTRUE ;

: TEST-LOADER ( -- )
   LOADER$ RUN-CORE 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   OUT outu s" `include`" CONTAINS? TTRUE
   OUT outu s" `included`" CONTAINS? TTRUE
   OUT outu s" `require`" CONTAINS? TTRUE
   OUT outu s" `required`" CONTAINS? TTRUE
   OUT outu s" `provided`" CONTAINS? TTRUE ;

: TEST-CONTROL ( -- )
   CONTROL$ RUN-CORE 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu s" E-RESERVED-DEFINITION" CONTAINS? TTRUE
   OUT outu s" `match`" CONTAINS? TTRUE
   OUT outu s" `;match`" CONTAINS? TTRUE
   OUT outu s" `CONSTRUCT`" CONTAINS? TTRUE ;

: TEST-NUMERIC ( -- )
   NUM$ RUN-CORE 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu s" E-NUMERIC-DEFINITION" CONTAINS? TTRUE
   OUT outu s" `.0`" CONTAINS? TTRUE
   OUT outu s" `U.0`" CONTAINS? TTRUE
   OUT outu s" `42`" CONTAINS? TTRUE
   OUT outu s" `-7`" CONTAINS? TTRUE
   OUT outu s" `1.5`" CONTAINS? TTRUE
   OUT outu s" `$FF`" CONTAINS? TTRUE ;

: TEST-NUMERIC-JSON ( -- )
   RUN-NUM-JSON 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu s" schema_version" CONTAINS? TTRUE
   OUT outu s" E-NUMERIC-DEFINITION" CONTAINS? TTRUE
   OUT outu s" U.0" CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   PREPARE
   TEST-GOOD
   TEST-BAD
   TEST-JSON
   TEST-LOADER
   TEST-TFAM
   TEST-CONTROL
   TEST-NUMERIC
   TEST-NUMERIC-JSON
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE
   T-REPORT
   s" reserved-name-lint-test: ok" type cr ;

;package
