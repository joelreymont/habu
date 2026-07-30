\ render-parse-lint-core.f - reject compiler code that could read rendered IR text.
\
\ WHAT RULE THIS ENFORCES. docs/compiler-ir-design.md section 6.6 says of the
\ renderer: "IR:RENDER is diagnostic text. It is not parsed by the compiler."
\ Section 5.6 says the same thing from the other side, listing "deriving schedule
\ facts from rendered text" among the prohibited moves and adding that "all such
\ facts must exist before rendering". A rule stated only in a document is a rule
\ the next lane can break by accident, so this lint makes it a gate.
\
\ WHY IT CHECKS REACHABILITY RATHER THAN LOOKING FOR PARSING. Deciding whether a
\ given piece of code "parses" text is not a question a scan can answer, and any
\ attempt would be a pile of guesses about what a tokenizer looks like. What CAN
\ be decided exactly is whether compiler code is able to OBTAIN rendered text at
\ all, and it cannot obtain any without naming the two packages that produce it.
\ So the rule is an existence check on the reference, not a judgement about a
\ value: no source under src/compiler/ other than the renderer stage's own files
\ may
\
\   name a word of package IR-RENDER or package IR-DIFF (`IR-RENDER:RENDER`,
\   `IR-DIFF:DIFF`, and any other qualified name in either package);
\
\   open or reopen either package, which is how a caller would reach those words
\   under their bare package-local names instead;
\
\   require src/compiler/ir/render.f or src/compiler/ir/diff.f, which is how the
\   words would be brought into scope in the first place.
\
\ A compiler source that does none of those three cannot hold a single byte of
\ rendered text, so it cannot parse one. That is the whole argument, and it does
\ not depend on recognising anything.
\
\ WHICH FILES ARE ALLOWED TO REFER TO THE STAGE. Exactly the two files the stage
\ is built from: src/compiler/ir/render.f, which produces the text, and
\ src/compiler/ir/diff.f, which asks the renderer to spell the rows it found a
\ difference in. The ledger is deliberately a short list of exact paths rather
\ than a pattern: the design's publication facade will one day re-export
\ IR:RENDER and IR:DIFF, and when it does, the lane that writes it has to add
\ itself here and say in review why forwarding a caller's byte span is not
\ reading it. Growing the ledger is a decision, which is the point.
\
\ WHY THE SCAN IS STRUCTURAL. Which bytes are code is decided by the one shared
\ source lexer, package LINT-LEX in tools/lint/source-lex.f. It consumes `\` line
\ comments, `( ... )` and `.( ... )` comment bodies, and every string literal
\ body - the plain `s" c" ."` openers and the escaped `s\" c\" .\"` openers alike
\ - so a package name mentioned in a comment or inside a string never reaches
\ this scan as a token, and a `"` written in the middle of a file cannot blind the
\ rest of it. A lexer diagnostic stops the scan and is reported, because
\ continuing would certify a ledger built from half a file. Reordering the
\ offending line, writing it twice, or hiding it in a docstring therefore changes
\ nothing about the verdict, which tools/render-parse-lint-test.f measures with a
\ fixture per evasion.
\
\ RUN prints the ledger without throwing; STRICT throws on any finding and is the
\ gate entry. COUNT scans one string in isolation for the fixtures.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/vector.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f and
\ tools/lint/source-lex.f.

require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/lint/source-lex.f

package RENDER-PARSE-LINT
public

\ ---- source-defect codes -----------------------------------------------------
\ A lexer diagnostic truncates the token table at the defect, so every later
\ token in that source is invisible. Continuing would certify a verdict built
\ from a partial file: exactly the blindness this lint exists to prevent. They
\ are public so a caller can name what it caught.
-4812 constant E-RPL-QUOTE   \ a string literal ran past end of input
-4813 constant E-RPL-ROW     \ a `PRIM:`/`PPRIM:` axiom row lacked a header or its closer
-4814 constant E-RPL-LEX     \ a lexer diagnostic or token kind this lint was never taught

private

$40000 constant SRC-CAP
512 constant PATH-CAP
48 constant ZERO-C

create BUF SRC-CAP allot
create PATH PATH-CAP allot
create DIGITS 32 allot

variable PATH-U
variable BAD
variable FILE#
variable SHOW?
variable ND#

: NL ( -- ) 10 emit ;

: SHOW! ( bool -- )  SHOW? ! ;
: SHOW-ON  ( -- )  LINT-TRUE  SHOW! ;
: SHOW-OFF ( -- )  LINT-FALSE SHOW! ;

: EMIT-U ( n -- )
   0 ND# !
   dup 0= if drop ZERO-C emit exit then
   begin dup 0 > while
      dup 10 mod ZERO-C + DIGITS ND# @ + c!
      10 / ND# @ 1+ ND# !
   repeat drop
   begin ND# @ 0 > while
      ND# @ 1- ND# !
      DIGITS ND# @ + c@ emit
   repeat ;

\ A path longer than the diagnostic buffer is kept to its first PATH-CAP bytes:
\ the copy is only ever printed, never compared, so a truncated name in a message
\ is better than a refusal that hides the finding it was about to report.
: PATH! ( ptr u8 n -- )
   {: a:ptr u:n :}
   u PATH-CAP min {: k:n :}
   a PATH k LINT-BMOVE
   k PATH-U ! ;

: PATH$ ( -- ptr u8 n )
   PATH PATH-U @ ;

\ ---- case-insensitive prefix -------------------------------------------------
\ tools/lint/text.f publishes an exact prefix test and a case-insensitive whole
\ comparison, and a qualified name needs both at once.
: PREFIX-CI? ( ptr u8 n ptr u8 n -- bool )
   {: a:ptr u:n b:ptr v:n :}
   u v < if LINT-FALSE exit then
   a v b v LINT-STR=CI ;

\ ---- the two packages this lint fences ---------------------------------------
\ A qualified name is `PACKAGE:TAIL`, so a token that starts with the package
\ name and a colon names a word of that package however it is spelled. The
\ dictionary is case-insensitive, so the comparison is too: `ir-render:render`
\ reaches the same word as `IR-RENDER:RENDER`, and a case-sensitive scan would be
\ an evasion vector.
: RENDER-QUALIFIED? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" IR-RENDER:" PREFIX-CI? ;

: DIFF-QUALIFIED? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" IR-DIFF:" PREFIX-CI? ;

: STAGE-QUALIFIED? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u RENDER-QUALIFIED? if LINT-TRUE exit then
   a u DIFF-QUALIFIED? ;

: STAGE-PACKAGE? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" IR-RENDER" LINT-STR=CI if LINT-TRUE exit then
   a u s" IR-DIFF" LINT-STR=CI ;

: STAGE-FILE-REF? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" src/compiler/ir/render.f" LINT-ENDS-WITH? if LINT-TRUE exit then
   a u s" src/compiler/ir/diff.f" LINT-ENDS-WITH? ;

: PACKAGE-OPENER? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" package" LINT-STR=CI ;

: REQUIRE-WORD? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" require" LINT-STR=CI if LINT-TRUE exit then
   a u s" include" LINT-STR=CI ;

\ ---- which sources are fenced ------------------------------------------------
: COMPILER-SRC? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" src/compiler/" LINT-CONTAINS? 0= if LINT-FALSE exit then
   a u s" .f" HAS-EXT? if LINT-TRUE exit then
   a u s" .fs" HAS-EXT? ;

\ The renderer stage's own two files. Any other compiler source that names the
\ stage is a finding; adding a file here is a reviewed decision, not a shortcut.
: STAGE-OWN-FILE? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u s" src/compiler/ir/render.f" LINT-ENDS-WITH? if LINT-TRUE exit then
   a u s" src/compiler/ir/diff.f" LINT-ENDS-WITH? ;

\ ---- findings ----------------------------------------------------------------
: WHERE ( -- )
   PATH$ type ;

: HIT ( ptr u8 n ptr u8 n -- )
   {: w:ptr wu:n d:ptr du:n :}
   SHOW? @ if
      s" RENDER-PARSE " type WHERE
      s" : " type d du type
      s"  '" type w wu type
      s" ' - renderer output must never reach compiler code" type NL
   then
   BAD @ 1+ BAD ! ;

\ ---- token walk --------------------------------------------------------------
: WORD? ( n -- bool )
   {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

\ Kinds this scan understands. A WORD is code. A `( ... )` or `.( ... )` comment
\ and a complete `PRIM:`/`PPRIM:` axiom row are inert spans that name no word, so
\ they are stepped over whole. Any other kind is one this lint was never taught,
\ and skipping it in silence is how a scanner goes blind.
: KNOWN-KIND? ( n -- bool )
   {: k:n :}
   k LINT-LEX:KIND@ {: kind:n :}
   kind LINT-LEX:WORD = if LINT-TRUE exit then
   kind LINT-LEX:COMMENT = if LINT-TRUE exit then
   kind LINT-LEX:REGISTRY = ;

\ Index of the next WORD token at or after k, or the token count when the source
\ has no word left. A comment sits between words without being code, so
\ `package ( note ) IR-RENDER` is still an opener naming the package.
: NEXT-WORD ( n -- n )
   {: k:n :}
   k begin dup LINT-LEX:COUNT < while
      dup WORD? if exit then
      1+
   repeat ;

: QUALIFIED-AT ( n -- )
   {: k:n :}
   k LINT-LEX:TOKEN {: a:ptr u:n :}
   a u STAGE-QUALIFIED? 0= if exit then
   a u s" names a renderer-stage word" HIT ;

: OPENER-AT ( n -- )
   {: k:n :}
   k LINT-LEX:TOKEN PACKAGE-OPENER? 0= if exit then
   k 1+ NEXT-WORD {: ni:n :}
   ni LINT-LEX:COUNT >= if exit then
   ni LINT-LEX:TOKEN {: a:ptr u:n :}
   a u STAGE-PACKAGE? 0= if exit then
   a u s" opens a renderer-stage package" HIT ;

: REQUIRE-AT ( n -- )
   {: k:n :}
   k LINT-LEX:TOKEN REQUIRE-WORD? 0= if exit then
   k 1+ NEXT-WORD {: ni:n :}
   ni LINT-LEX:COUNT >= if exit then
   ni LINT-LEX:TOKEN {: a:ptr u:n :}
   a u STAGE-FILE-REF? 0= if exit then
   a u s" loads a renderer-stage source" HIT ;

: UNKNOWN-KIND ( n -- )
   {: k:n :}
   s" render-parse-lint: " type WHERE
   s"  token " type k EMIT-U
   s" : unknown lexer token kind " type k LINT-LEX:KIND@ EMIT-U NL
   E-RPL-LEX throw ;

: SCAN-TOKENS ( -- )
   0 begin dup LINT-LEX:COUNT < while
      dup KNOWN-KIND? 0= if dup UNKNOWN-KIND then
      dup WORD? if
         dup QUALIFIED-AT
         dup OPENER-AT
         dup REQUIRE-AT
      then
      1+
   repeat drop ;

: DEFECT-SITE ( -- )
   s" render-parse-lint: " type WHERE
   s" :" type LINT-LEX:ERROR-LINE@ EMIT-U
   s" :" type LINT-LEX:ERROR-COL@ EMIT-U
   s" : " type ;

\ Fail-closed: a lexer diagnostic stops the scan at the defect, so every token
\ after it in that source is unreadable. Name the file, the site and the defect,
\ then throw a catchable code rather than certify a partial file.
: LEX-DEFECT ( -- )
   DEFECT-SITE
   LINT-LEX:ERROR-KIND@ {: kind:n :}
   kind LINT-LEX:UNTERMINATED-QUOTE = if
      s" unterminated string literal" type NL  E-RPL-QUOTE throw
   then
   kind LINT-LEX:MALFORMED-REGISTRY = if
      s" malformed primitive-axiom row" type NL  E-RPL-ROW throw
   then
   s" unknown lexer diagnostic" type NL  E-RPL-LEX throw ;

: SCAN-TEXT ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u LINT-LEX:SOURCE
   LINT-LEX:ERROR? if LEX-DEFECT then
   SCAN-TOKENS ;

\ ---- the file walk -----------------------------------------------------------
: SCAN-FILE ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u COMPILER-SRC? 0= if exit then
   a u STAGE-OWN-FILE? if exit then
   a u PATH!
   FILE# @ 1+ FILE# !
   a u BUF SRC-CAP READ-FILE SCAN-TEXT ;

: WALK ( -- )
   0 BAD !  0 FILE# !
   s" src/compiler/" [: SCAN-FILE ;] WALK-FILES ;

: SUMMARY ( -- )
   s" render-parse-lint: " type
   FILE# @ EMIT-U s"  compiler file(s), " type
   BAD   @ EMIT-U s"  finding(s)" type NL ;

public

\ findings from scanning one string in isolation; the fixtures' entry point
: COUNT ( ptr u8 n -- n )
   {: a:ptr u:n :}
   SHOW? @ {: show:bool :}
   SHOW-OFF
   0 BAD !
   s" <test>" PATH!
   a u SCAN-TEXT
   show SHOW!
   BAD @ ;

\ findings from scanning one string as if it were a named file, so a fixture can
\ ask whether the renderer stage's own files are exempt
: COUNT-AS ( ptr u8 n ptr u8 n -- n )
   {: p:ptr pu:n a:ptr u:n :}
   SHOW? @ {: show:bool :}
   SHOW-OFF
   0 BAD !
   p pu STAGE-OWN-FILE? if show SHOW! 0 exit then
   p pu PATH!
   a u SCAN-TEXT
   show SHOW!
   BAD @ ;

\ true when this lint scans a source at that path at all
: FENCED? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   a u COMPILER-SRC? 0= if LINT-FALSE exit then
   a u STAGE-OWN-FILE? 0= ;

\ report view: prints the ledger without throwing
: RUN ( -- )
   SHOW-ON  WALK  SUMMARY ;

\ gate entry (enforcing): any compiler source that can reach rendered text fails
: STRICT ( -- )
   RUN
   BAD @ 0 > if 1 throw then ;

;package
