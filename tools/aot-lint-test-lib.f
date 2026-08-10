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
variable CASE-U

create ROOT-BUF FS-PATH-CAP allot
create GOOD-BUF FS-PATH-CAP allot
create BAD-BUF FS-PATH-CAP allot
create CASE-BUF FS-PATH-CAP allot
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

: CASE-PATH ( -- ptr u8 n )
   CASE-BUF CASE-U @ ;

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
   ROOT s" case.f" CASE-BUF JOIN-PATH CASE-U !
   GOOD CLEANUP+
   BAD CLEANUP+
   CASE-PATH CLEANUP+
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

\ ---- every unsafe token, and every way of not being one ----------------------
\ WHY THE ROWS NEED THEIR OWN CASES. tools/aot-lint-core.f UNSAFE? is a list of
\ six spellings, and until now one of them - `patch32` - carried the whole suite.
\ A row added to that list, or a row deleted from it, changed nothing any test
\ could see: a stripped AOT binary has no writable code region, so a word reaching
\ any of the six at runtime cannot run there, and a missing row means the lint
\ passes a program that will fault.
\
\ AND WHY THE NEGATIVE CASES ARE THE HALF THAT MATTERS. The lint reads TOKENS,
\ not text, so the claim is about where a spelling stands and not about whether
\ the file contains it. Each unsafe spelling is therefore also presented in a
\ comment, inside a string literal, and as the tail of a longer word - three
\ places a text search finds it and a lexer does not. A lint that regressed to
\ substring matching passes every positive case below and fails every negative
\ one.
: CASE-RUN ( ptr u8 n n -- n ) {: src:ptr u:n want:n :}
   CASE-PATH src u WRITE-ALL
   CASE-PATH RUN-CORE want EXPECT-EXIT {: outu:n erru:n :}
   erru 0 T=
   outu ;

\ A flagged source reports the unsupported token BY NAME, so a case cannot pass
\ on some other finding the same file happens to earn - and the token assertion
\ is what separates "this row exists" from "this file is rejected for any reason".
: FLAGS ( ptr u8 n ptr u8 n -- ) {: src:ptr u:n t:ptr tu:n :}
   src u 1 CASE-RUN {: outu:n :}
   OUT outu CODE$ CONTAINS? TTRUE
   OUT outu t tu CONTAINS? TTRUE ;

\ A passing source produces NO output at all. Asserting emptiness rather than
\ "no finding for this token" is what catches a lint that flagged the line for a
\ different reason and still exited zero.
: PASSES ( ptr u8 n -- ) {: src:ptr u:n :}
   src u 0 CASE-RUN {: outu:n :}
   outu 0 T= ;

\ A body that calls the token, built around whatever spelling it is handed, so
\ the six positive cases are one statement and not six.
: CALLS$ ( ptr u8 n -- ptr u8 n ) {: t:ptr tu:n :}
   SB-RESET
   s" : MAIN ( -- ) " SB-APPEND  t tu SB-APPEND  s"  ;" SB-APPEND LF
   SB$ ;

: IN-COMMENT$ ( ptr u8 n -- ptr u8 n ) {: t:ptr tu:n :}
   SB-RESET
   s" \\ " SB-APPEND  t tu SB-APPEND  s"  is named here and not called" SB-APPEND LF
   s" : MAIN ( -- ) 42 . CR ;" SB-APPEND LF
   SB$ ;

: IN-STRING$ ( ptr u8 n -- ptr u8 n ) {: t:ptr tu:n :}
   SB-RESET
   s" : MAIN ( -- ) " SB-APPEND
   115 SB-APPEND-C DQ  t tu SB-APPEND  DQ
   s"  TYPE ;" SB-APPEND LF
   SB$ ;

\ The spelling as the TAIL of a longer word. `MY-patch32` is a different token,
\ and a lint comparing the end of a token - or searching the line - flags it.
: AS-TAIL$ ( ptr u8 n -- ptr u8 n ) {: t:ptr tu:n :}
   SB-RESET
   s" : MY-" SB-APPEND  t tu SB-APPEND  s"  ( -- ) 42 . CR ;" SB-APPEND LF
   s" : MAIN ( -- ) MY-" SB-APPEND  t tu SB-APPEND  s"  ;" SB-APPEND LF
   SB$ ;

: TOKEN-CASE ( ptr u8 n -- ) {: t:ptr tu:n :}
   t tu CALLS$ t tu FLAGS
   t tu IN-COMMENT$ PASSES
   t tu IN-STRING$ PASSES
   t tu AS-TAIL$ PASSES ;

: TEST-TOKENS ( -- )
   s" a runtime compile is unsupported" T-LABEL
   s" compile," TOKEN-CASE
   s" a single-word code poke is unsupported" T-LABEL
   s" patch32" TOKEN-CASE
   s" the bulk publication window is unsupported" T-LABEL
   s" code-publish" TOKEN-CASE
   s" the call relocation map is unsupported" T-LABEL
   s" callmap-set" TOKEN-CASE
   s" the address-literal relocation map is unsupported" T-LABEL
   s" addrmap-set" TOKEN-CASE
   s" retargeting a live record is unsupported" T-LABEL
   s" xref-retarget" TOKEN-CASE ;

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
   TEST-TOKENS
   TEST-BAD-JSON
   CLEANUP-RUN
   ROOT EXISTS? TFALSE
   T-REPORT
   s" aot-lint-test: ok" type cr ;

;package
