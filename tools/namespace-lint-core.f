\ namespace-lint-core.f - flag maki definitions that live outside a subsystem
\ package (the dot habu-maki-subsystem-pkgs ledger tool).
\
\ The dot replaces stem-prefix namespacing with real per-subsystem packages
\ (TENSOR, LOSS, OPTIM, ONNX, ...). A definition written at GLOBAL scope inside a
\ maki file (no `package` block open) leaks into the shared dictionary and is the
\ opposite of that goal, so this lint reports each one as a TODO-ledger entry.
\
\ WHICH BYTES ARE CODE is decided by the one shared source lexer, package LINT-LEX
\ in tools/lint/source-lex.f, exactly as tools/error-code-lint-core.f decides it.
\ The lexer consumes `\` line comments, `( ... )` and `.( ... )` comment bodies and
\ every string-literal body, so none of that text reaches this scan as a token and
\ the scan has no opinion of its own about where a string starts.
\
\ It used to have one, and that is the defect this file was rewritten to remove.
\ The scan counted `"` bytes per token and toggled an in-string flag on an odd
\ count - a value heuristic standing in for a structural fact - and it failed in
\ both directions. One bare `[char] "` set the flag and blinded the REST OF THE
\ FILE while the summary still printed `0 finding(s)`. A `\` written INSIDE a
\ string body made the old splitter strip that string's closing quote and skip to
\ end of line, inverting the flag for everything after it. And in the other
\ direction it over-reported: a `.( ... )` printing comment, and a `( ` comment
\ opened before a NEWLINE rather than a space, both read as code, so a definition
\ written inside either one was reported as a global. The same heuristic in
\ tools/error-code-lint-core.f is recorded there as having failed the same way.
\
\ Scan: each maki/*.f source is lexed, then the token stream is walked while
\ tracking `package`/`;package` depth. Every defining word (`:`, `constant`,
\ `variable`, `create`, `KERNEL:`, ...) seen at depth 0 names a global maki def;
\ the next WORD token is its name, and that name is a finding unless whitelisted.
\ A token the engine PARSES rather than executes - the word after `'`, `[']`,
\ `postpone`, `char` or `[char]` - is an operand, not code, so `[char] :` declares
\ nothing. Definer and package tokens match case-INSENSITIVELY: the dictionary is
\ case-insensitive (docs/forth.md § Naming), so `CREATE BUF` defines a global and
\ `;package` closes a package exactly like their lower-case spellings; a
\ case-sensitive scan is an evasion vector (this subsumed the retired
\ tools/maki-ns-lint.f, whose one-package-per-file marker model could not express
\ the multi-package subsystem files - dot habu-maki-ns-lint-reconcile).
\
\ Whitelist (documented substrate / cross-cutting, NOT flagged):
\   - E-*  names: cross-cutting error-code constants, top-level by convention.
\   - maki/array.f: the documented ARRAY value-array substrate (T-GET/T-SET/...),
\     used bare across lib/ptx tests; packaging it is churn for negative gain.
\   - BEGIN-*/END-* names: legacy scope pairs (owed a FOO/;FOO rename by their own
\     dot); reported in a separate legacy-pair tally, not the primary finding set.
\   - *-test.f files: test scaffolding legitimately defines global test locals.
\   - maki/device-smoke.f: the gate's device-FFI canary - a smoke-test suite run by
\     maki/test.f under lib/test.f (T-RESET/TTRUE/T-REPORT). It is gate scaffolding,
\     the same category as *-test.f, and just lacks the -test suffix by history.
\
\ LEDGER prints the ledger and the count without throwing (a report view).
\ STRICT throws on any finding and is the gate entrypoint now that the eval/gpu
\ GLOBAL clusters have landed in per-subsystem packages: the ledger is clean,
\ enforcement is on, and any NEW global maki def outside a package fails the gate.
\
\ The module lives in `package NAMESPACE-LINT`, the shape its sibling lints use
\ (tools/error-code-lint-core.f, tools/maki-dep-lint-core.f). It carried no
\ package at all while its body was being edited here, and the package lint will
\ not let a definition outside one be touched, so it is sealed in the same change
\ - the route tools/lint/intern.f took. Consumers import it with `using` or name
\ it qualified; nothing here is global.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/vector.f, lib/fs.f,
\ tools/lint/text.f, tools/lint/token.f, tools/lint/lib.f and
\ tools/lint/source-lex.f.

package NAMESPACE-LINT

public

\ ---- source-defect codes ----------------------------------------------------
\ A lexer diagnostic truncates the token table at the defect, so every definition
\ after it in that source is invisible while the summary still prints its count -
\ which is precisely the blindness this rewrite exists to end, so it must not
\ come back through the lexer's own diagnostics. Each defect gets its own name,
\ the way tools/error-code-lint-core.f splits E-QUOTE from E-ROW, and they are
\ public so a fixture can pin which one refused. -4824..-4827 continue the
\ unclaimed lint-tool gap that holds E-SHADOW-UNTERM (-4800) through
\ E-MAKIDEP-LEX (-4823); the gap ends before lib/errors.f's reserved E-REPORT
\ block at -4900.
-4824 constant E-NS-QUOTE    \ a string literal ran past end of input
-4825 constant E-NS-ROW      \ a `PRIM:`/`PPRIM:` axiom row lacked a header or its closer
\ The residual arm: a diagnostic or token kind added to LINT-LEX after this
\ consumer was written. It must reach a named refusal rather than borrow one of
\ the two labels above, and it must never pass in silence.
-4826 constant E-NS-LEX      \ a lexer diagnostic or token kind this lint was never taught
\ A definer whose name never arrives - the definer stands at the end of the scan
\ with no WORD after it. That is a defect of the FILE, and a scan that shrugged
\ at it would drop the one definition most likely to be malformed.
-4827 constant E-NS-NONAME   \ a defining word has no name token after it

private

512 constant PCAP
48 constant ZERO
10 constant LF

\ One file at a time, in a slab sized from the file. It was a fixed $40000 arena,
\ and a whole-file reader over an input that grows is what tools/lint/text.f
\ LINT-SLAB exists for: the arena's next overrun would stop the lint on a maki
\ file's length rather than on anything the lint is about.
create SLAB LINT-SLAB:CELLS cells allot
create PATH PCAP allot
create NBUF 32 allot

variable PATHU
variable DEPTH                          \ current package nesting depth
variable BAD                            \ primary findings (global maki defs)
variable LEGACY                         \ legacy BEGIN-/END- pair tally
variable FILES
variable REPORT?
variable ND#

: NL ( -- ) LF emit ;

: REPORT! ( bool -- )  REPORT? ! ;
: REPORT-ON  ( -- )  LINT-TRUE  REPORT! ;
: REPORT-OFF ( -- )  LINT-FALSE REPORT! ;

\ unsigned decimal (no <# number ring in this Habu; digit-buffer like maki-dep)
: U. ( n -- )
   0 ND# !
   dup 0= if drop ZERO emit exit then
   begin dup 0 > while
      dup 10 mod ZERO + NBUF ND# @ + c!
      10 / ND# @ 1+ ND# !
   repeat drop
   begin ND# @ 0 > while
      ND# @ 1- ND# !
      NBUF ND# @ + c@ emit
   repeat ;

\ ---- token classification ---------------------------------------------------
\ true when the token names the next token as a new global word (CI: the
\ dictionary is case-insensitive, so `CREATE`/`Constant` define like `create`)
: DEF-WORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" :"         LINT-STR=   if LINT-TRUE exit then
   a u s" +:"        LINT-STR=   if LINT-TRUE exit then
   a u s" constant"  LINT-STR=CI if LINT-TRUE exit then
   a u s" 2constant" LINT-STR=CI if LINT-TRUE exit then
   a u s" fconstant" LINT-STR=CI if LINT-TRUE exit then
   a u s" variable"  LINT-STR=CI if LINT-TRUE exit then
   a u s" 2variable" LINT-STR=CI if LINT-TRUE exit then
   a u s" fvariable" LINT-STR=CI if LINT-TRUE exit then
   a u s" create"    LINT-STR=CI if LINT-TRUE exit then
   a u s" LAYOUT-BUFFER" LINT-STR=CI if LINT-TRUE exit then
   a u s" value"     LINT-STR=CI if LINT-TRUE exit then
   a u s" defer"     LINT-STR=CI if LINT-TRUE exit then
   a u s" CHECKED:"  LINT-STR=CI if LINT-TRUE exit then
   a u s" TRUSTED:"  LINT-STR=CI if LINT-TRUE exit then
   a u s" KERNEL:"   LINT-STR=CI if LINT-TRUE exit then
   LINT-FALSE ;

: LEGACY-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" BEGIN-" LINT-PREFIX? a u s" END-" LINT-PREFIX? or ;

\ E-* error constants are documented cross-cutting exceptions
: WHITELISTED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" E-" LINT-PREFIX? ;

\ ---- file selection ---------------------------------------------------------
\ The walk is rooted at `maki/`, so a source of this lint is a path UNDER that
\ root: an anchored prefix, not the four bytes occurring anywhere in the path.
: MAKI-SRC? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" maki/" LINT-PREFIX?  a u s" .f" HAS-EXT?  and ;

\ documented ARRAY substrate + test/canary scaffolding are exempt from the scan
: SKIP-FILE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" -test.f"             HAS-EXT?        if LINT-TRUE exit then
   a u s" maki/device-smoke.f" LINT-ENDS-WITH? if LINT-TRUE exit then
   a u s" maki/array.f"        LINT-ENDS-WITH? ;

: PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   a PATH u LINT-BMOVE  u PATHU ! ;

: WHERE ( -- )  PATH PATHU @ type ;

: HIT ( ptr u8 n -- ) {: nptr:ptr nu:n :}
   REPORT? @ if
      s" NAMESPACE " type WHERE
      s" : global maki def '" type  nptr nu type  s" ' outside a package" type
      NL
   then
   BAD @ 1+ BAD ! ;

: LEGACY-HIT ( ptr u8 n -- ) {: nptr:ptr nu:n :}
   REPORT? @ if
      s" NAMESPACE-LEGACY " type WHERE
      s" : legacy scope-pair name '" type  nptr nu type  s" '" type NL
   then
   LEGACY @ 1+ LEGACY ! ;

\ ---- token walk with package-scope tracking ---------------------------------
: WORD? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ LINT-LEX:WORD = ;

\ Kinds this scan understands. A WORD is code. A `( ... )` or `.( ... )` comment
\ and a complete `PRIM:`/`PPRIM:` axiom row are inert spans that publish no
\ dictionary word (docs/forth.md § Packages), so they are stepped over whole. Any
\ other kind is one this lint was never taught, and skipping it in silence is how
\ a scanner goes blind: the token would span source the scan never reads while
\ the ledger still reports zero findings.
: KNOWN-KIND? ( n -- bool ) {: k:n :}
   k LINT-LEX:KIND@ {: kind:n :}
   kind LINT-LEX:WORD = if LINT-TRUE exit then
   kind LINT-LEX:COMMENT = if LINT-TRUE exit then
   kind LINT-LEX:REGISTRY = ;

\ Index of the next WORD token at or after k, or the token count when the source
\ has no word left. A comment sits between words without being code, so
\ `create ( note ) BUF` still names BUF.
: NEXT-WORD ( n -- n ) {: k:n :}
   k begin dup LINT-LEX:COUNT < while
      dup WORD? if exit then
      1+
   repeat ;

\ After one of these the engine PARSES the next token as raw text and never
\ executes it, so the operand is not code: `[char] :` is the colon BYTE, not a
\ definition, and `' create` ticks a word rather than declaring one. The rule is
\ tools/bootstrap-mirror-lint.f ESCAPED?, and it is deliberately the immediately
\ preceding token: an inert span between the two is not this shape.
: ESCAPED? ( n -- bool ) {: k:n :}
   k 0 <= if LINT-FALSE exit then
   k 1- WORD? 0= if LINT-FALSE exit then
   k 1- LINT-LEX:TOKEN s" '" LINT-STR= if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" [']" LINT-STR= if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" postpone" LINT-STR=CI if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" char" LINT-STR=CI if LINT-TRUE exit then
   k 1- LINT-LEX:TOKEN s" [char]" LINT-STR=CI ;

: NO-NAME ( n -- ) {: k:n :}
   s" namespace-lint: " type WHERE
   s" :" type k LINT-LEX:LINE@ U.
   s" : defining word '" type k LINT-LEX:TOKEN type
   s" ' has no name token after it" type NL
   E-NS-NONAME throw ;

\ classify one definition at depth 0: primary finding, legacy tally, or exempt
: DEF-AT ( n -- ) {: i:n :}
   i 1+ NEXT-WORD {: ni:n :}
   ni LINT-LEX:COUNT >= if i NO-NAME then
   ni LINT-LEX:TOKEN {: nptr:ptr nu:n :}
   nptr nu LEGACY-NAME? if nptr nu LEGACY-HIT exit then
   nptr nu WHITELISTED? if exit then
   nptr nu HIT ;

: PACKAGE-OPEN? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" package" LINT-STR=CI ;

: PACKAGE-CLOSE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" ;package" LINT-STR=CI ;

: STEP ( n -- n ) {: i:n :}             \ handle WORD token i, return next index
   i ESCAPED? if i 1+ exit then
   i LINT-LEX:TOKEN {: tp:ptr tu:n :}
   tp tu PACKAGE-OPEN? if
      DEPTH @ 1+ DEPTH !
      i 1+ NEXT-WORD 1+ exit then       \ step past the package NAME token
   tp tu PACKAGE-CLOSE? if
      DEPTH @ 1- DEPTH !  i 1+ exit then
   DEPTH @ 0= tp tu DEF-WORD? and if
      i DEF-AT
      i 1+ NEXT-WORD 1+ exit then       \ step past the defined name
   i 1+ ;

: UNKNOWN-KIND ( n -- ) {: k:n :}
   s" namespace-lint: " type WHERE
   s"  token " type k U.
   s" : unknown lexer token kind " type k LINT-LEX:KIND@ U. NL
   E-NS-LEX throw ;

: SCAN-TOKENS ( -- )
   0 DEPTH !
   0 begin dup LINT-LEX:COUNT < while
      dup KNOWN-KIND? 0= if dup UNKNOWN-KIND then
      dup WORD? if STEP else 1+ then
   repeat drop ;

: DEFECT-SITE ( -- )
   s" namespace-lint: " type WHERE
   s" :" type LINT-LEX:ERROR-LINE@ U.
   s" :" type LINT-LEX:ERROR-COL@ U.
   s" : " type ;

\ Fail-closed: a lexer diagnostic stops the scan at the defect, so every
\ definition after it in that source is unreadable. Name the file, the site and
\ the defect, then throw a catchable code rather than certify a partial file.
: LEX-DEFECT ( -- )
   DEFECT-SITE
   LINT-LEX:ERROR-KIND@ {: kind:n :}
   kind LINT-LEX:UNTERMINATED-QUOTE = if
      s" unterminated string literal" type NL  E-NS-QUOTE throw
   then
   kind LINT-LEX:MALFORMED-REGISTRY = if
      s" malformed primitive-axiom row" type NL  E-NS-ROW throw
   then
   s" unknown lexer diagnostic" type NL  E-NS-LEX throw ;

: SCAN-STR ( ptr u8 n -- )
   LINT-LEX:SOURCE
   LINT-LEX:ERROR? if LEX-DEFECT then
   SCAN-TOKENS ;

\ findings from scanning one string in isolation (reset -> scan -> count); tests
: COUNT ( ptr u8 n -- n )
   REPORT? @ {: report:bool :}
   REPORT-OFF
   0 BAD !  0 LEGACY !
   SCAN-STR
   report REPORT!
   BAD @ ;

: LEGACY-COUNT ( ptr u8 n -- n )
   REPORT? @ {: report:bool :}
   REPORT-OFF
   0 BAD !  0 LEGACY !
   SCAN-STR
   report REPORT!
   LEGACY @ ;

: SCAN-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u MAKI-SRC? 0= if exit then
   a u SKIP-FILE? if exit then
   a u PATH!
   FILES @ 1+ FILES !
   a u SLAB LINT-SLAB:LOAD
   SLAB LINT-SLAB:TEXT SCAN-STR ;

: WALK ( -- )
   0 BAD !  0 LEGACY !  0 FILES !
   s" maki/" [: SCAN-FILE ;] WALK-FILES ;

: SUMMARY ( -- )
   s" namespace-lint: " type
   FILES  @ U. s"  maki file(s), " type
   BAD    @ U. s"  global-def finding(s), " type
   LEGACY @ U. s"  legacy-pair(s)" type NL ;

public

\ report view: prints the ledger without throwing (STRICT enforces)
: LEDGER ( -- )
   REPORT-ON  WALK  SUMMARY ;

\ gate entry (enforcing): any global maki def outside a package fails the gate
: STRICT ( -- )
   LEDGER
   BAD @ 0 > if 1 throw then ;

EXPORT COUNT
EXPORT LEGACY-COUNT
EXPORT MAKI-SRC?
EXPORT SKIP-FILE?

;package
