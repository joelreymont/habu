\ duplicate-definition-lint-test-lib.f - checked fixtures for tools/duplicate-definition-lint.f.
\ Load-only fixture library; the thin entry tools/duplicate-definition-lint-test.f
\ requires this file and its dependencies, then calls DUPLICATE-DEFINITION-LINT-TEST:RUN.

package DUPLICATE-DEFINITION-LINT-TEST

$1000 constant BUF-CAP

create ROOT FS-PATH-CAP allot
create GOOD FS-PATH-CAP allot
create BAD-A FS-PATH-CAP allot
create BAD-B FS-PATH-CAP allot
create CASEFOLD FS-PATH-CAP allot
create OUT BUF-CAP allot

variable ROOT-U
variable GOOD-U
variable BAD-A-U
variable BAD-B-U
variable CASEFOLD-U

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;

: GOOD$ ( -- ptr u8 n )
   GOOD GOOD-U @ ;

: BAD-A$ ( -- ptr u8 n )
   BAD-A BAD-A-U @ ;

: BAD-B$ ( -- ptr u8 n )
   BAD-B BAD-B-U @ ;

: CASEFOLD$ ( -- ptr u8 n )
   CASEFOLD CASEFOLD-U @ ;

: LF ( -- )
   $0A SB-APPEND-C ;

: GOOD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : OK ( -- n ) 1 ;" SB-APPEND LF
   s" : (OK) ( -- n ) 2 ;" SB-APPEND LF
   s" : REDO ( -- n ) 1 ;" SB-APPEND LF
   s" undefine REDO" SB-APPEND LF
   s" : redo ( -- n ) 2 ;" SB-APPEND LF
   s" variable STATE" SB-APPEND LF
   s" 7 constant LIMIT" SB-APPEND LF
   s" 2 LAYOUT-BUFFER ARENA sample" SB-APPEND LF
   SB$ ;

: BAD-A-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : RESET ( -- n ) 1 ;" SB-APPEND LF
   s" 2 LAYOUT-BUFFER LCH sample" SB-APPEND LF
   SB$ ;

: BAD-B-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : OTHER ( -- n ) 2 ;" SB-APPEND LF
   s" : LCH ( -- n ) 3 ;" SB-APPEND LF
   SB$ ;

: CASEFOLD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : RESET ( -- n ) 1 ;" SB-APPEND LF
   s" : reset ( -- n ) 2 ;" SB-APPEND LF
   SB$ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-duplicate-definition-lint" TMPDIR-MKDIR ROOT ROOT-U COPY!
   ROOT$ CLEANUP-TREE+
   ROOT$ s" good.f" GOOD JOIN-PATH GOOD-U !
   ROOT$ s" bad-a.f" BAD-A JOIN-PATH BAD-A-U !
   ROOT$ s" bad-b.f" BAD-B JOIN-PATH BAD-B-U !
   ROOT$ s" case.f" CASEFOLD JOIN-PATH CASEFOLD-U !
   GOOD$ GOOD-SRC$ WRITE-ALL
   BAD-A$ BAD-A-SRC$ WRITE-ALL
   BAD-B$ BAD-B-SRC$ WRITE-ALL
   CASEFOLD$ CASEFOLD-SRC$ WRITE-ALL ;

: CORE-SETUP ( bool -- ) {: json:bool :}
   DUPLICATE-DEFINITION-LINT-RESET
   OUT BUF-CAP LINT-OUT-BUFFER!
   json DDL-JSON! ;

: CORE-FINISH ( -- n n n )
   [: DUPLICATE-DEFINITION-LINT-FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 rc ;

: RUN-CORE-GOOD ( -- n n n )
   LINT-FALSE CORE-SETUP
   GOOD$ DUPLICATE-DEFINITION-LINT-FILE
   CORE-FINISH ;

: RUN-CORE-CROSS ( -- n n n )
   LINT-FALSE CORE-SETUP
   BAD-A$ DUPLICATE-DEFINITION-LINT-FILE
   BAD-B$ DUPLICATE-DEFINITION-LINT-FILE
   CORE-FINISH ;

: RUN-CORE-JSON ( -- n n n )
   LINT-TRUE CORE-SETUP
   CASEFOLD$ s" <stage2-src>" DUPLICATE-DEFINITION-LINT-FILE-AS
   CORE-FINISH ;

: JSON-WORD-RESET$ ( -- ptr u8 n )
   SB-RESET
   $22 SB-APPEND-C
   s" word" SB-APPEND
   $22 SB-APPEND-C
   $3A SB-APPEND-C
   $22 SB-APPEND-C
   s" reset" SB-APPEND
   $22 SB-APPEND-C
   SB$ ;

: EXPECT-EXIT ( n n n n -- n n ) {: outu:n erru:n code:n want:n :}
   code want T=
   outu erru ;

: TEST-GOOD ( -- )
   RUN-CORE-GOOD 0 EXPECT-EXIT {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: TEST-CROSS ( -- )
   RUN-CORE-CROSS 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu s" E-DUPLICATE-DEFINITION" CONTAINS? TTRUE
   OUT outu s" `LCH`" CONTAINS? TTRUE
   OUT outu s" bad-a.f" CONTAINS? TTRUE
   OUT outu s" bad-b.f" CONTAINS? TTRUE ;

: TEST-JSON ( -- )
   RUN-CORE-JSON 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu s" schema_version" CONTAINS? TTRUE
   OUT outu s" E-DUPLICATE-DEFINITION" CONTAINS? TTRUE
   OUT outu s" <stage2-src>" CONTAINS? TTRUE
   OUT outu JSON-WORD-RESET$ CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   PREPARE
   TEST-GOOD
   TEST-CROSS
   TEST-JSON
   CLEANUP-RUN
   ROOT$ EXISTS? TFALSE
   T-REPORT
   s" duplicate-definition-lint-test: ok" type cr ;

;package
