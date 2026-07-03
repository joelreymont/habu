\ aot-lint-test.f - checked fixtures for tools/aot-lint.f.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/memory.f
\ lib/vector.f lib/fs.f lib/fs-mutate.f lib/process.f tools/lint/text.f
\ tools/lint/token.f tools/lint/lib.f tools/lint/json-writer.f
\ tools/lint/source-lex.f tools/aot-lint-core.f tools/aot-lint-test.f

4096 constant ALT-BUF-CAP

variable ALT-ROOT-U
variable ALT-GOOD-U
variable ALT-BAD-U

create ALT-ROOT-BUF FS-PATH-CAP allot
create ALT-GOOD-BUF FS-PATH-CAP allot
create ALT-BAD-BUF FS-PATH-CAP allot
create ALT-OUT ALT-BUF-CAP allot

: ALT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr lenp:ptr :}
   a dst u BYTE-COPY
   u lenp ! ;

: ALT-ROOT ( -- ptr u8 n )
   ALT-ROOT-BUF ALT-ROOT-U @ ;

: ALT-GOOD ( -- ptr u8 n )
   ALT-GOOD-BUF ALT-GOOD-U @ ;

: ALT-BAD ( -- ptr u8 n )
   ALT-BAD-BUF ALT-BAD-U @ ;

: ALT-LF ( -- )
   10 SB-APPEND-C ;

: ALT-DQ ( -- )
   34 SB-APPEND-C ;

: ALT-GOOD$ ( -- ptr u8 n )
   SB-RESET
   s" \\ here in comment" SB-APPEND ALT-LF
   115 SB-APPEND-C ALT-DQ s"  here in string" SB-APPEND ALT-DQ ALT-LF
   s" : MAIN ( -- ) 42 . CR ;" SB-APPEND ALT-LF
   SB$ ;

: ALT-BAD$ ( -- ptr u8 n )
   SB-RESET
   s" : MAIN ( -- ) 0 0 patch32 ;" SB-APPEND ALT-LF
   SB$ ;

: ALT-EMPTY$ ( -- ptr u8 n )
   SB-RESET
   SB$ ;

: ALT-CODE$ ( -- ptr u8 n )
   s" E-AOT-UNSUPPORTED" ;

: ALT-JSON-CODE$ ( -- ptr u8 n )
   SB-RESET
   ALT-DQ s" code" SB-APPEND ALT-DQ
   58 SB-APPEND-C
   ALT-DQ ALT-CODE$ SB-APPEND ALT-DQ
   SB$ ;

: ALT-JSON-LABEL$ ( -- ptr u8 n )
   SB-RESET
   ALT-DQ s" file" SB-APPEND ALT-DQ
   58 SB-APPEND-C
   ALT-DQ s" <stdin>" SB-APPEND ALT-DQ
   SB$ ;

: ALT-JSON-STR-FIELD$ ( ptr u8 n ptr u8 n -- ptr u8 n )
   {: key:ptr keyu:n val:ptr valu:n :}
   SB-RESET
   ALT-DQ key keyu SB-APPEND ALT-DQ
   58 SB-APPEND-C
   ALT-DQ val valu SB-APPEND ALT-DQ
   SB$ ;

: ALT-JSON-TOKEN$ ( -- ptr u8 n )
   s" token" s" patch32" ALT-JSON-STR-FIELD$ ;

: ALT-JSON-WORD$ ( -- ptr u8 n )
   s" word" s" MAIN" ALT-JSON-STR-FIELD$ ;

: ALT-JSON-REASON$ ( -- ptr u8 n )
   s" reason" s" stripped AOT has no runtime compiler or writable code" ALT-JSON-STR-FIELD$ ;

: ALT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-aot-lint" TMPDIR-MKDIR {: a:ptr u:n :}
   a u ALT-ROOT-BUF ALT-ROOT-U ALT-COPY!
   ALT-ROOT CLEANUP-DIR+
   ALT-ROOT s" good.f" ALT-GOOD-BUF JOIN-PATH ALT-GOOD-U !
   ALT-ROOT s" bad.f" ALT-BAD-BUF JOIN-PATH ALT-BAD-U !
   ALT-GOOD CLEANUP+
   ALT-BAD CLEANUP+
   ALT-GOOD ALT-GOOD$ WRITE-ALL
   ALT-BAD ALT-BAD$ WRITE-ALL ;

: ALT-CORE-SETUP ( bool -- ) {: json:bool :}
   AOT-LINT-RESET
   ALT-OUT ALT-BUF-CAP LINT-OUT-BUFFER!
   json AL-JSON! ;

: ALT-CORE-FINISH ( -- n n n n )
   [: AOT-LINT-FINISH ;] catch {: rc:n :}
   LINT-OUT$ nip LINT-OUT-BUFFER-OFF
   0 PROC-OUTCOME-EXIT rc ;

: ALT-RUN-CORE ( ptr u8 n -- n n n n )
   LINT-FALSE ALT-CORE-SETUP
   AOT-LINT-FILE
   ALT-CORE-FINISH ;

: ALT-RUN-CORE-JSON-LABEL ( ptr u8 n -- n n n n ) {: a:ptr u:n :}
   LINT-TRUE ALT-CORE-SETUP
   a u s" <stdin>" AOT-LINT-FILE-AS
   ALT-CORE-FINISH ;

: ALT-EXPECT-EXIT ( n n n n n -- n n ) {: outu:n erru:n kind:n code:n expect:n :}
   kind PROC-OUTCOME-EXIT T=
   code expect T=
   outu erru ;

: ALT-TEST-GOOD ( -- )
   ALT-GOOD ALT-RUN-CORE 0 ALT-EXPECT-EXIT {: outu:n erru:n :}
   ALT-OUT outu ALT-EMPTY$ T$=
   erru 0 T= ;

: ALT-TEST-BAD ( -- )
   ALT-BAD ALT-RUN-CORE 1 ALT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   ALT-OUT outu ALT-CODE$ CONTAINS? TTRUE ;

: ALT-TEST-BAD-JSON ( -- )
   ALT-BAD ALT-RUN-CORE-JSON-LABEL 1 ALT-EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   ALT-OUT outu ALT-JSON-CODE$ CONTAINS? TTRUE
   ALT-OUT outu ALT-JSON-LABEL$ CONTAINS? TTRUE
   ALT-OUT outu ALT-JSON-TOKEN$ CONTAINS? TTRUE
   ALT-OUT outu ALT-JSON-WORD$ CONTAINS? TTRUE
   ALT-OUT outu ALT-JSON-REASON$ CONTAINS? TTRUE ;

: ALT-MAIN ( -- )
   T-RESET
   ALT-PREPARE
   ALT-TEST-GOOD
   ALT-TEST-BAD
   ALT-TEST-BAD-JSON
   CLEANUP-RUN
   ALT-ROOT EXISTS? TFALSE
   T-REPORT
   s" aot-lint-test: ok" type cr ;
