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
\ Scan: each src/, lib/, test/ Forth file is TOKENIZEd (which strips `\` line comments
\ and `( )` stack-effect comments and keeps `s"` string bodies as tokens), then every
\ whole token is matched for `maki/` - not a raw-file substring match, so prose in
\ comments cannot false-positive while a real load token (`maki/x.f`) or a string-
\ literal load path (`s" maki/x.f"`) is caught (docs/forth.md "Source-use guards match
\ tokens"). Any hit throws a named finding.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, tools/lint/text.f,
\ and tools/lint/token.f.
\
\ The module lives in `package MAKI-DEP-LINT`, the shape its sibling lints use
\ (tools/error-code-lint-core.f, tools/signature-lint-core.f). Its five public
\ words are the whole surface the CLI and the suite need - the walk, one string
\ scanned in isolation, and the two the suite drives a scan's path with - and
\ every helper below them is private.

package MAKI-DEP-LINT

private

32 constant NCAP
10 constant LF
48 constant ZERO
34 constant DQUOTE   \ TOKENIZE keeps an s" body's closing quote on the token

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

: HIT ( ptr u8 n -- ) {: t:ptr tu:n :}
   REPORT? @ if
      s" MAKI-DEP " type
      PATH PATHU @ type
      s" : forbidden maki/ reference in token '" type
      t tu type
      s" '" type NL
   then
   BAD+ ;

\ The sole sanctioned habu->maki reference: test/run-lib.f names the maki suite
\ entries to SPAWN them (dots habu-route-the-maki-e61d8a1b,
\ habu-split-monolithic-maki-fccca4ea). Allowed in exactly that file are the
\ full-suite entry maki/test.f and its parallel slice loaders maki/test-<slice>.f
\ (each ending .f), as a bare token or an s" body (the closing quote rides on the
\ token). A near-miss (maki/test.fs, maki/test-core.fs) or the token in any other
\ file is still a finding.
: GATE-EFFECTIVE ( ptr u8 n -- ptr u8 n ) {: t:ptr tu:n :}
   tu 0 > if t tu 1- + c@ DQUOTE = if t tu 1- exit then then
   t tu ;

: GATE-ROUTE? ( ptr u8 n -- bool ) {: t:ptr tu:n :}
   PATH PATHU @ s" test/run-lib.f" LINT-STR= 0= if LINT-FALSE exit then
   t tu GATE-EFFECTIVE {: e:ptr eu:n :}
   e eu s" maki/test.f" LINT-STR= if LINT-TRUE exit then
   e eu s" maki/test-" LINT-STARTS-WITH?
   e eu s" .f" LINT-ENDS-WITH? and ;

: SCAN-TOKENS ( -- )
   0 begin dup TN# @ < while
      dup TOK 2dup s" maki/" LINT-CONTAINS? IF
         2dup GATE-ROUTE? IF 2drop ELSE HIT THEN
      ELSE 2drop THEN
      1+
   repeat drop ;

\ scan an arbitrary source string (used by both the file walk and the tests)
: SCAN-STR ( ptr u8 n -- )
   LINT-TRUE PARENS? !
   TOKENIZE
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
