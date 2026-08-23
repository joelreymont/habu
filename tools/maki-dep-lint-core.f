\ maki-dep-lint-core.f - one-way habu<-maki dependency guard.
\
\ Enforces the PLAN guardrail (maki/README.md): imports flow maki -> habu, NEVER
\ habu -> maki. The Habu core (src/), libraries (lib/), and the native gate (test/)
\ must never reference a maki/ path; maki/ is application Forth fenced outside the
\ trust root. Until this lint, the seam was held only by review + the gate's explicit
\ allow-lists.
\
\ ONE sanctioned exception (dot habu-route-the-maki-e61d8a1b): the native gate
\ (test/run-lib.f) names the maki suite entry maki/test.f so it can SPAWN it as a
\ black-box subprocess. That is not an import - habu never loads maki into its image
\ - so the guarded dependency direction is intact. GATE-ROUTE? allows exactly
\ that one token in exactly that one file; every other maki/ token still throws.
\
\ WHICH BYTES ARE CODE is decided by the one shared source lexer, package LINT-LEX
\ in tools/lint/source-lex.f, exactly as tools/error-code-lint-core.f decides it.
\ The lexer consumes `\` line comments, `( ... )` and `.( ... )` comment bodies, and
\ every string literal body, and it hands a literal's payload back as CONTENT. So a
\ path written in a comment is a COMMENT token this scan steps over, and a path
\ written in a literal is read out of the payload rather than guessed at from raw
\ bytes.
\
\ This scan used to run on tools/lint/token.f's whitespace splitter and decide by
\ substring, and it was wrong in both directions. `.( loading maki/array.f )` is a
\ printing comment the engine never executes, and a `(` that opens a comment before
\ a NEWLINE rather than a space is still a comment - the splitter read both as code
\ and reported the path inside them. A `\` written INSIDE a string body made the
\ splitter strip that string's closing quote and skip to end of line, so
\ `s" a \ b" --load maki/x.f` hid a real load behind a string. And `LINT-CONTAINS?`
\ over a whole token called `premaki/thing` a maki reference, because `maki/`
\ occurs in it as bytes.
\
\ WHAT COUNTS AS A REFERENCE. A candidate is either a whole WORD token or one
\ whitespace-delimited word of a string literal's payload - payload WORDS, not the
\ whole payload, the rule tools/lint/schedule-lint.f reads paths by, because
\ `s" --load maki/x.f"` carries the path inside a longer literal. A candidate is a
\ reference when `maki` is a PATH COMPONENT of it: the anchored `maki/` prefix, or
\ an interior component delimited on both sides. Both arms are decided by the `/`
\ delimiters, so a component that merely ends in `maki` is not one.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, tools/lint/text.f,
\ tools/lint/token.f, tools/lint/lib.f, and tools/lint/source-lex.f.
\
\ The module lives in `package MAKI-DEP-LINT`, the shape its sibling lints use
\ (tools/error-code-lint-core.f, tools/signature-lint-core.f). Its five public
\ words are the whole surface the CLI and the suite need - the walk, one string
\ scanned in isolation, and the two the suite drives a scan's path with - and
\ every helper below them is private.

package MAKI-DEP-LINT

public

\ ---- source-defect codes ----------------------------------------------------
\ A lexer diagnostic truncates the token table at the defect, so every reference
\ after it in that source is invisible while the walk still prints its count.
\ Each defect gets its own name, the way tools/error-code-lint-core.f splits
\ E-QUOTE from E-ROW, and they are public so a fixture can pin which one refused.
\ -4821..-4823 continue the unclaimed lint-tool gap that holds E-SHADOW-UNTERM
\ (-4800) through E-AOT-REACH-LEX (-4820); the gap ends before lib/errors.f's
\ reserved E-REPORT block at -4900.
-4821 constant E-MAKIDEP-QUOTE  \ a string literal ran past end of input
-4822 constant E-MAKIDEP-ROW    \ a `PRIM:`/`PPRIM:` axiom row lacked a header or its closer
\ The residual arm: a diagnostic or token kind added to LINT-LEX after this
\ consumer was written. It must reach a named refusal rather than borrow one of
\ the two labels above, and it must never pass in silence.
-4823 constant E-MAKIDEP-LEX    \ a lexer diagnostic or token kind this lint was never taught

private

32 constant NCAP
10 constant LF
48 constant ZERO

\ One file at a time, in a slab sized from the file. It was a fixed arena that
\ had been doubled for src/core/checker.f, the largest source in the tree, and a
\ whole-file reader over an input that grows is what tools/lint/text.f LINT-SLAB
\ exists for: the arena's next overrun would stop the lint on a file's length
\ rather than on anything the lint is about.
create SLAB LINT-SLAB:CELLS cells allot
create NBUF NCAP allot
create PATH 512 allot

variable PATHU
variable BAD
variable FILES
variable NL#
variable REPORT?
variable PW-I                      \ payload cursor: the word scan is not nested
variable PW-S                      \ start of the payload word being read

: NL ( -- ) LF emit ;

: REPORT! ( bool -- )
   REPORT? ! ;

: REPORT-ON ( -- )
   LINT-TRUE REPORT! ;

: REPORT-OFF ( -- )
   LINT-FALSE REPORT! ;

: U. ( n -- )
   0 NL# !
   dup 0= IF drop ZERO emit exit THEN
   begin dup 0 > while
      dup 10 mod ZERO + NBUF NL# @ + c!
      10 / NL# @ 1+ NL# !
   repeat drop
   begin NL# @ 0 > while
      NL# @ 1- NL# !
      NBUF NL# @ + c@ emit
   repeat ;

: BAD+ ( -- ) BAD @ 1+ BAD ! ;

: SRC? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" .f" HAS-EXT?  a u s" .fs" HAS-EXT? or ;

: PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   a PATH u LINT-BMOVE  u PATHU ! ;

: WHERE ( -- ) PATH PATHU @ type ;

: HIT ( ptr u8 n -- ) {: t:ptr tu:n :}
   REPORT? @ if
      s" MAKI-DEP " type
      WHERE
      s" : forbidden maki/ reference in token '" type
      t tu type
      s" '" type NL
   then
   BAD+ ;

\ `maki` as a PATH COMPONENT of the candidate. The first arm is anchored at the
\ start; the second is delimited on both sides, so it names a directory called
\ exactly `maki` and nothing that merely ends in those four bytes.
: MAKI-PATH? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" maki/" LINT-PREFIX? if LINT-TRUE exit then
   a u s" /maki/" LINT-CONTAINS? ;

\ The sole sanctioned habu->maki reference: test/run-lib.f names the maki suite
\ entries to SPAWN them (dots habu-route-the-maki-e61d8a1b,
\ habu-split-monolithic-maki-fccca4ea). Allowed in exactly that file are the
\ full-suite entry maki/test.f and its parallel slice loaders maki/test-<slice>.f
\ (each ending .f). The candidate arrives as a whole word - a bare token, or one
\ word of a literal's payload - so this compares the path itself. It used to
\ compare the token with one trailing `"` chopped off, because the old splitter
\ left an s" body's closing quote riding on the last token; the lexer hands the
\ payload back without it, and that arithmetic is gone with the splitter.
\ A near-miss (maki/test.fs, maki/test-core.fs) or the path in any other file is
\ still a finding.
: GATE-ROUTE? ( ptr u8 n -- bool ) {: t:ptr tu:n :}
   PATH PATHU @ s" test/run-lib.f" LINT-STR= 0= if LINT-FALSE exit then
   t tu s" maki/test.f" LINT-STR= if LINT-TRUE exit then
   t tu s" maki/test-" LINT-STARTS-WITH?
   t tu s" .f" LINT-ENDS-WITH? and ;

: CANDIDATE ( ptr u8 n -- )
   2dup MAKI-PATH? 0= IF 2drop exit THEN
   2dup GATE-ROUTE? IF 2drop exit THEN
   HIT ;

: PW-SKIP-WS ( ptr u8 n -- ) {: a:ptr u:n :}
   begin PW-I @ u < a PW-I @ + c@ LINT-WS? and while PW-I @ 1+ PW-I ! repeat ;

: PW-SKIP-WORD ( ptr u8 n -- ) {: a:ptr u:n :}
   begin PW-I @ u < a PW-I @ + c@ LINT-WS? 0= and while PW-I @ 1+ PW-I ! repeat ;

\ Every whitespace-delimited word of a string literal's payload. A load path is
\ one word of a literal that may hold several (`s" --load maki/x.f"`), so the
\ payload is read word by word rather than as one candidate.
: PAYLOAD-WORDS ( ptr u8 n -- ) {: a:ptr u:n :}
   0 PW-I !
   begin PW-I @ u < while
      a u PW-SKIP-WS
      PW-I @ u < IF
         PW-I @ PW-S !
         a u PW-SKIP-WORD
         a PW-S @ +  PW-I @ PW-S @ -  CANDIDATE
      THEN
   repeat ;

: WORD? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

\ Kinds this scan understands. A WORD is code. A `( ... )` or `.( ... )` comment
\ and a complete `PRIM:`/`PPRIM:` axiom row are inert spans that import nothing,
\ so they are stepped over whole. Any other kind is one this lint was never
\ taught, and skipping it in silence is how a scanner goes blind: the token would
\ span source the scan never reads while the walk still reports zero findings.
: KNOWN-KIND? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ {: kind:n :}
   kind LINT-LEX:WORD = if LINT-TRUE exit then
   kind LINT-LEX:COMMENT = if LINT-TRUE exit then
   kind LINT-LEX:REGISTRY = ;

: STRING-TOKEN? ( n -- bool ) {: k:n :}
   k LINT-LEX:TOKEN LINT-NORMAL-STRING-OPENER? if LINT-TRUE exit then
   k LINT-LEX:TOKEN LINT-ESC-STRING-OPENER? ;

: SCAN-TOKEN ( n -- ) {: k:n :}
   k STRING-TOKEN? IF k LINT-LEX:CONTENT PAYLOAD-WORDS exit THEN
   k LINT-LEX:TOKEN CANDIDATE ;

: UNKNOWN-KIND ( n -- ) {: k:n :}
   s" maki-dep-lint: " type WHERE
   s"  token " type k U.
   s" : unknown lexer token kind " type k LINT-LEX:KIND@ U. NL
   E-MAKIDEP-LEX throw ;

: SCAN-TOKENS ( -- )
   0 begin dup LINT-LEX:COUNT < while
      dup KNOWN-KIND? 0= IF dup UNKNOWN-KIND THEN
      dup WORD? IF dup SCAN-TOKEN THEN
      1+
   repeat drop ;

: DEFECT-SITE ( -- )
   s" maki-dep-lint: " type WHERE
   s" :" type LINT-LEX:ERROR-LINE@ U.
   s" :" type LINT-LEX:ERROR-COL@ U.
   s" : " type ;

\ Fail-closed: a lexer diagnostic stops the scan at the defect, so every path
\ after it in that source is unreadable. Name the file, the site and the defect,
\ then throw a catchable code rather than certify a file the scan half read.
: LEX-DEFECT ( -- )
   DEFECT-SITE
   LINT-LEX:ERROR-KIND@ {: kind:n :}
   kind LINT-LEX:UNTERMINATED-QUOTE = IF
      s" unterminated string literal" type NL  E-MAKIDEP-QUOTE throw
   THEN
   kind LINT-LEX:MALFORMED-REGISTRY = IF
      s" malformed primitive-axiom row" type NL  E-MAKIDEP-ROW throw
   THEN
   s" unknown lexer diagnostic" type NL  E-MAKIDEP-LEX throw ;

\ scan an arbitrary source string (used by both the file walk and the tests)
: SCAN-STR ( ptr u8 n -- )
   LINT-LEX:SOURCE
   LINT-LEX:ERROR? IF LEX-DEFECT THEN
   SCAN-TOKENS ;

\ findings produced by scanning one string in isolation (reset -> scan -> count)
: COUNT ( ptr u8 n -- n )
   REPORT? @ {: report:bool :}
   REPORT-OFF
   0 BAD !
   SCAN-STR
   report REPORT!
   BAD @ ;

: SCAN-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u SRC? 0= IF exit THEN
   a u PATH!
   FILES @ 1+ FILES !
   a u SLAB LINT-SLAB:LOAD
   SLAB LINT-SLAB:TEXT SCAN-STR ;

: RUN ( -- )
   REPORT-ON
   0 BAD !  0 FILES !
   s" src/"  [: SCAN-FILE ;] WALK-FILES
   s" lib/"  [: SCAN-FILE ;] WALK-FILES
   s" test/" [: SCAN-FILE ;] WALK-FILES
   s" maki-dep-lint: " type
   FILES @ U. s"  file(s), " type
   BAD @ U.   s"  finding(s)" type NL
   BAD @ 0 > IF 1 throw THEN ;

public

\ The whole surface outside this file. RUN is the CLI's walk; COUNT is one
\ string scanned in isolation, which is how the suite drives the token rules;
\ SRC? and PATH! are the two the suite sets a scan's file context with, and
\ PATHU is the cell it clears afterwards so later scans are file-agnostic.
EXPORT RUN
EXPORT COUNT
EXPORT SRC?
EXPORT PATH!
EXPORT PATHU

;package
