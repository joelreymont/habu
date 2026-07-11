\ duplicate-definition-lint-test.f - checked fixtures for duplicate-definition-lint.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f tools/lint/text.f
\ tools/lint/token.f tools/lint/lib.f tools/lint/json-writer.f
\ tools/duplicate-definition-lint-core.f tools/duplicate-definition-lint-test.f

$1000 constant DDLT-BUF-CAP

create DDLT-ROOT FS-PATH-CAP allot
create DDLT-GOOD FS-PATH-CAP allot
create DDLT-BAD-A FS-PATH-CAP allot
create DDLT-BAD-B FS-PATH-CAP allot
create DDLT-CASE FS-PATH-CAP allot
create DDLT-OUT DDLT-BUF-CAP allot

variable DDLT-ROOT-U
variable DDLT-GOOD-U
variable DDLT-BAD-A-U
variable DDLT-BAD-B-U
variable DDLT-CASE-U

: DDLT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: DDLT-ROOT$ ( -- ptr u8 n )
   DDLT-ROOT DDLT-ROOT-U @ ;

: DDLT-GOOD$ ( -- ptr u8 n )
   DDLT-GOOD DDLT-GOOD-U @ ;

: DDLT-BAD-A$ ( -- ptr u8 n )
   DDLT-BAD-A DDLT-BAD-A-U @ ;

: DDLT-BAD-B$ ( -- ptr u8 n )
   DDLT-BAD-B DDLT-BAD-B-U @ ;

: DDLT-CASE$ ( -- ptr u8 n )
   DDLT-CASE DDLT-CASE-U @ ;

: DDLT-LF ( -- )
   $0A SB-APPEND-C ;

: DDLT-GOOD-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : OK ( -- n ) 1 ;" SB-APPEND DDLT-LF
   s" : (OK) ( -- n ) 2 ;" SB-APPEND DDLT-LF
   s" : REDO ( -- n ) 1 ;" SB-APPEND DDLT-LF
   s" undefine REDO" SB-APPEND DDLT-LF
   s" : redo ( -- n ) 2 ;" SB-APPEND DDLT-LF
   s" variable STATE" SB-APPEND DDLT-LF
   s" 7 constant LIMIT" SB-APPEND DDLT-LF
   s" LAYOUT-BUFFER ARENA sample 2" SB-APPEND DDLT-LF
   SB$ ;

: DDLT-BAD-A-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : RESET ( -- n ) 1 ;" SB-APPEND DDLT-LF
   s" LAYOUT-BUFFER LCH sample 2" SB-APPEND DDLT-LF
   SB$ ;

: DDLT-BAD-B-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : OTHER ( -- n ) 2 ;" SB-APPEND DDLT-LF
   s" : LCH ( -- n ) 3 ;" SB-APPEND DDLT-LF
   SB$ ;

: DDLT-CASE-SRC$ ( -- ptr u8 n )
   SB-RESET
   s" : RESET ( -- n ) 1 ;" SB-APPEND DDLT-LF
   s" : reset ( -- n ) 2 ;" SB-APPEND DDLT-LF
   SB$ ;

: DDLT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-duplicate-definition-lint" TMPDIR-MKDIR DDLT-ROOT DDLT-ROOT-U DDLT-COPY!
   DDLT-ROOT$ CLEANUP-TREE+
   DDLT-ROOT$ s" good.f" DDLT-GOOD JOIN-PATH DDLT-GOOD-U !
   DDLT-ROOT$ s" bad-a.f" DDLT-BAD-A JOIN-PATH DDLT-BAD-A-U !
   DDLT-ROOT$ s" bad-b.f" DDLT-BAD-B JOIN-PATH DDLT-BAD-B-U !
   DDLT-ROOT$ s" case.f" DDLT-CASE JOIN-PATH DDLT-CASE-U !
   DDLT-GOOD$ DDLT-GOOD-SRC$ WRITE-ALL
   DDLT-BAD-A$ DDLT-BAD-A-SRC$ WRITE-ALL
   DDLT-BAD-B$ DDLT-BAD-B-SRC$ WRITE-ALL
   DDLT-CASE$ DDLT-CASE-SRC$ WRITE-ALL ;

: DDLT-CORE-SETUP ( bool -- ) {: json:bool :}
   DUPLICATE-DEFINITION-LINT-RESET
   DDLT-OUT DDLT-BUF-CAP LINT-OUT-BUFFER!
   json DDL-JSON! ;

: DDLT-CORE-FINISH ( -- n n n )
   [: DUPLICATE-DEFINITION-LINT-FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 rc ;

: DDLT-RUN-CORE-GOOD ( -- n n n )
   LINT-FALSE DDLT-CORE-SETUP
   DDLT-GOOD$ DUPLICATE-DEFINITION-LINT-FILE
   DDLT-CORE-FINISH ;

: DDLT-RUN-CORE-CROSS ( -- n n n )
   LINT-FALSE DDLT-CORE-SETUP
   DDLT-BAD-A$ DUPLICATE-DEFINITION-LINT-FILE
   DDLT-BAD-B$ DUPLICATE-DEFINITION-LINT-FILE
   DDLT-CORE-FINISH ;

: DDLT-RUN-CORE-JSON ( -- n n n )
   LINT-TRUE DDLT-CORE-SETUP
   DDLT-CASE$ s" <stage2-src>" DUPLICATE-DEFINITION-LINT-FILE-AS
   DDLT-CORE-FINISH ;

: DDLT-JSON-WORD-RESET$ ( -- ptr u8 n )
   SB-RESET
   $22 SB-APPEND-C
   s" word" SB-APPEND
   $22 SB-APPEND-C
   $3A SB-APPEND-C
   $22 SB-APPEND-C
   s" reset" SB-APPEND
   $22 SB-APPEND-C
   SB$ ;

: DDLT-EXPECT-EXIT ( n n n n -- n n ) {: outu:n erru:n code:n want:n :}
   code want T=
   outu erru ;

: DDLT-TEST-GOOD ( -- )
   DDLT-RUN-CORE-GOOD 0 DDLT-EXPECT-EXIT {: outu:n erru:n :}
   outu 0 T=
   erru 0 T= ;

: DDLT-TEST-CROSS ( -- )
   DDLT-RUN-CORE-CROSS 1 DDLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   DDLT-OUT outu s" E-DUPLICATE-DEFINITION" CONTAINS? TTRUE
   DDLT-OUT outu s" `LCH`" CONTAINS? TTRUE
   DDLT-OUT outu s" bad-a.f" CONTAINS? TTRUE
   DDLT-OUT outu s" bad-b.f" CONTAINS? TTRUE ;

: DDLT-TEST-JSON ( -- )
   DDLT-RUN-CORE-JSON 1 DDLT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   DDLT-OUT outu s" schema_version" CONTAINS? TTRUE
   DDLT-OUT outu s" E-DUPLICATE-DEFINITION" CONTAINS? TTRUE
   DDLT-OUT outu s" <stage2-src>" CONTAINS? TTRUE
   DDLT-OUT outu DDLT-JSON-WORD-RESET$ CONTAINS? TTRUE ;

: DDLT-MAIN ( -- )
   T-RESET
   DDLT-PREPARE
   DDLT-TEST-GOOD
   DDLT-TEST-CROSS
   DDLT-TEST-JSON
   CLEANUP-RUN
   DDLT-ROOT$ EXISTS? TFALSE
   T-REPORT
   s" duplicate-definition-lint-test: ok" type cr ;
