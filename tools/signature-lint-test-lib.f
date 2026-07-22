\ signature-lint-test-lib.f - checked fixtures for tools/signature-lint.f.
\ Load-only fixture library; the thin entry tools/signature-lint-test.f
\ requires this file and its dependencies, then calls SIGNATURE-LINT-TEST:RUN.

package SIGNATURE-LINT-TEST

4096 constant BUF-CAP

variable ROOT-U
variable GOOD-U
variable MISSING-U
variable OPTOUT-U
variable NAME-U

create ROOT-BUF FS-PATH-CAP allot
create GOOD-BUF FS-PATH-CAP allot
create MISSING-BUF FS-PATH-CAP allot
create OPTOUT-BUF FS-PATH-CAP allot
create NAME-BUF FS-PATH-CAP allot
create OUT BUF-CAP allot

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: ROOT ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: GOOD ( -- ptr u8 n )
   GOOD-BUF GOOD-U @ ;

: MISSING ( -- ptr u8 n )
   MISSING-BUF MISSING-U @ ;

: OPTOUT ( -- ptr u8 n )
   OPTOUT-BUF OPTOUT-U @ ;

: NAME ( -- ptr u8 n )
   NAME-BUF NAME-U @ ;

: LF ( -- )
   10 SB-APPEND-C ;

: DQ ( -- )
   34 SB-APPEND-C ;

: GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" : OK ( n -- n ) dup ;" SB-APPEND LF
   s" \\ : COMMENTED dup ;" SB-APPEND LF
   115 SB-APPEND-C DQ s"  : STRING ;" SB-APPEND DQ LF
   s" ( : PAREN dup ; )" SB-APPEND LF
   SB$ ;

: MISSING$ ( -- ptr u8 n )
   SB-RESET
   s" : NOSIG dup ;" SB-APPEND LF
   SB$ ;

: OPTOUT$ ( -- ptr u8 n )
   SB-RESET
   s" : X ( infer ) dup ;" SB-APPEND LF
   SB$ ;

: NAME$ ( -- ptr u8 n )
   SB-RESET
   s" : ( n -- n ) dup ;" SB-APPEND LF
   SB$ ;

: EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: MISSING-CODE$ ( -- ptr u8 n )
   s" E-MISSING-SIGNATURE" ;

: UNVERIFIED-CODE$ ( -- ptr u8 n )
   s" E-UNVERIFIED-SIGNATURE" ;

: NAME-CODE$ ( -- ptr u8 n )
   s" E-MISSING-NAME" ;

: JSON-MISSING$ ( -- ptr u8 n )
   SB-RESET
   DQ s" code" SB-APPEND DQ
   58 SB-APPEND-C
   DQ MISSING-CODE$ SB-APPEND DQ
   SB$ ;

: JSON-UNVERIFIED$ ( -- ptr u8 n )
   SB-RESET
   DQ s" code" SB-APPEND DQ
   58 SB-APPEND-C
   DQ UNVERIFIED-CODE$ SB-APPEND DQ
   SB$ ;

: JSON-LABEL$ ( -- ptr u8 n )
   SB-RESET
   DQ s" file" SB-APPEND DQ
   58 SB-APPEND-C
   DQ s" <stdin>" SB-APPEND DQ
   SB$ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-signature-lint" TMPDIR-MKDIR {: a:ptr u:n :}
   a u ROOT-BUF ROOT-U COPY!
   ROOT CLEANUP-DIR+
   ROOT s" good.f" GOOD-BUF JOIN-PATH GOOD-U !
   ROOT s" missing.f" MISSING-BUF JOIN-PATH MISSING-U !
   ROOT s" optout.f" OPTOUT-BUF JOIN-PATH OPTOUT-U !
   ROOT s" missing-name.f" NAME-BUF JOIN-PATH NAME-U !
   GOOD CLEANUP+
   MISSING CLEANUP+
   OPTOUT CLEANUP+
   NAME CLEANUP+
   GOOD GOOD$ WRITE-ALL
   MISSING MISSING$ WRITE-ALL
   OPTOUT OPTOUT$ WRITE-ALL
   NAME NAME$ WRITE-ALL ;

: CORE-SETUP ( bool -- ) {: json:bool :}
   SIGNATURE-LINT-RESET
   OUT BUF-CAP LINT-OUT-BUFFER!
   json SL-JSON! ;

: CORE-FINISH ( -- n n n )
   [: SIGNATURE-LINT-FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 rc ;

: RUN-CORE ( ptr u8 n -- n n n )
   LINT-FALSE CORE-SETUP
   SIGNATURE-LINT-FILE
   CORE-FINISH ;

: RUN-CORE-JSON ( ptr u8 n -- n n n )
   LINT-TRUE CORE-SETUP
   SIGNATURE-LINT-FILE
   CORE-FINISH ;

: RUN-CORE-JSON-LABEL ( ptr u8 n -- n n n )
   {: a:ptr u:n :}
   LINT-TRUE CORE-SETUP
   a u s" <stdin>" SIGNATURE-LINT-FILE-AS
   CORE-FINISH ;

: EXPECT-EXIT ( n n n n -- n n ) {: outu:n erru:n code:n expect:n :}
   code expect T=
   outu erru ;

: TEST-GOOD ( -- )
   GOOD RUN-CORE 0 EXPECT-EXIT {: outu:n erru:n :}
   OUT outu EMPTY$ T$=
   erru 0 T= ;

: TEST-MISSING ( -- )
   MISSING RUN-CORE 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu MISSING-CODE$ CONTAINS? TTRUE ;

: TEST-MISSING-JSON ( -- )
   MISSING RUN-CORE-JSON-LABEL 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu JSON-MISSING$ CONTAINS? TTRUE
   OUT outu JSON-LABEL$ CONTAINS? TTRUE ;

: TEST-GOOD-JSON-LABEL ( -- )
   GOOD RUN-CORE-JSON-LABEL 0 EXPECT-EXIT {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-OPTOUT-JSON ( -- )
   OPTOUT RUN-CORE-JSON 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu JSON-UNVERIFIED$ CONTAINS? TTRUE ;

: TEST-MISSING-NAME ( -- )
   NAME RUN-CORE 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu NAME-CODE$ CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   PREPARE
   TEST-GOOD
   TEST-MISSING
   TEST-MISSING-JSON
   TEST-GOOD-JSON-LABEL
   TEST-OPTOUT-JSON
   TEST-MISSING-NAME
   CLEANUP-RUN
   ROOT EXISTS? TFALSE
   T-REPORT
   s" signature-lint-test: ok" type cr ;

;package
