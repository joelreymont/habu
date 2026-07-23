\ aot-lint-test.f - checked fixtures for tools/aot-lint.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f tools/lint/text.f
\ tools/lint/token.f tools/lint/lib.f tools/lint/json-writer.f
\ tools/lint/source-lex.f tools/aot-lint-core.f tools/aot-lint-test.f

package AOT-LINT-TEST
private

4096 constant BUF-CAP

variable ROOT-U
variable GOOD-U
variable BAD-U

create ROOT-BUF FS-PATH-CAP allot
create GOOD-BUF FS-PATH-CAP allot
create BAD-BUF FS-PATH-CAP allot
create OUT BUF-CAP allot

: COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: ROOT ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: GOOD ( -- ptr u8 n )
   GOOD-BUF GOOD-U @ ;

: BAD ( -- ptr u8 n )
   BAD-BUF BAD-U @ ;

: LF ( -- )
   10 SB-APPEND-C ;

: DQ ( -- )
   34 SB-APPEND-C ;

: GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" \\ here in comment" SB-APPEND LF
   115 SB-APPEND-C DQ s"  here in string" SB-APPEND DQ LF
   s" : MAIN ( -- ) 42 . CR ;" SB-APPEND LF
   SB$ ;

: BAD$ ( -- ptr u8 n )
   SB-RESET
   s" : MAIN ( -- ) 0 0 patch32 ;" SB-APPEND LF
   SB$ ;

: EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: CODE$ ( -- ptr u8 n )
   s" E-AOT-UNSUPPORTED" ;

: JSON-CODE$ ( -- ptr u8 n )
   SB-RESET
   DQ s" code" SB-APPEND DQ
   58 SB-APPEND-C
   DQ CODE$ SB-APPEND DQ
   SB$ ;

: JSON-LABEL$ ( -- ptr u8 n )
   SB-RESET
   DQ s" file" SB-APPEND DQ
   58 SB-APPEND-C
   DQ s" <stdin>" SB-APPEND DQ
   SB$ ;

: JSON-STR-FIELD$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: key:ptr keyu:n val:ptr valu:n :}
   SB-RESET
   DQ key keyu SB-APPEND DQ
   58 SB-APPEND-C
   DQ val valu SB-APPEND DQ
   SB$ ;

: JSON-TOKEN$ ( -- ptr u8 n )
   s" token" s" patch32" JSON-STR-FIELD$ ;

: JSON-WORD$ ( -- ptr u8 n )
   s" word" s" MAIN" JSON-STR-FIELD$ ;

: JSON-REASON$ ( -- ptr u8 n )
   s" reason" s" stripped AOT has no runtime compiler or writable code" JSON-STR-FIELD$ ;

: PREPARE ( -- )
   CLEANUP-RESET
   s" habu-aot-lint" TMPDIR-MKDIR {: a:ptr u:n :}
   a u ROOT-BUF ROOT-U COPY!
   ROOT CLEANUP-DIR+
   ROOT s" good.f" GOOD-BUF JOIN-PATH GOOD-U !
   ROOT s" bad.f" BAD-BUF JOIN-PATH BAD-U !
   GOOD CLEANUP+
   BAD CLEANUP+
   GOOD GOOD$ WRITE-ALL
   BAD BAD$ WRITE-ALL ;

: CORE-SETUP ( bool -- ) {: json:bool :}
   AOT-LINT:RESET
   OUT BUF-CAP LINT-OUT-BUFFER!
   json AOT-LINT:JSON! ;

: CORE-FINISH ( -- n n outcome )
   [: AOT-LINT:FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 rc OUTCOME:EXITED ;

: RUN-CORE ( ptr u8 n -- n n outcome )
   LINT-FALSE CORE-SETUP
   AOT-LINT:FILE
   CORE-FINISH ;

: RUN-JSON-LABEL ( ptr u8 n -- n n outcome ) {: a:ptr u:n :}
   LINT-TRUE CORE-SETUP
   a u s" <stdin>" AOT-LINT:FILE-AS
   CORE-FINISH ;

: EXPECT-EXIT ( n n outcome n -- n n ) {: expect:n :}
   expect T-OUTCOME-EXITED= ;

: TEST-GOOD ( -- )
   GOOD RUN-CORE 0 EXPECT-EXIT {: outu:n erru:n :}
   OUT outu EMPTY$ T$=
   erru 0 T= ;

: TEST-BAD ( -- )
   BAD RUN-CORE 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu CODE$ CONTAINS? TTRUE ;

: TEST-BAD-JSON ( -- )
   BAD RUN-JSON-LABEL 1 EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   OUT outu JSON-CODE$ CONTAINS? TTRUE
   OUT outu JSON-LABEL$ CONTAINS? TTRUE
   OUT outu JSON-TOKEN$ CONTAINS? TTRUE
   OUT outu JSON-WORD$ CONTAINS? TTRUE
   OUT outu JSON-REASON$ CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   PREPARE
   TEST-GOOD
   TEST-BAD
   TEST-BAD-JSON
   CLEANUP-RUN
   ROOT EXISTS? TFALSE
   T-REPORT
   s" aot-lint-test: ok" type cr ;

;package
