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
\ - so the guarded dependency direction is intact. MDL-GATE-ROUTE? allows exactly
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

$80000 constant MDL-CAP   \ >= largest scanned source (checker.f grew past $40000)
32 constant MDL-NCAP
10 constant MDL-LF
48 constant MDL-ZERO
34 constant MDL-DQUOTE   \ TOKENIZE keeps an s" body's closing quote on the token

create MDL-BUF  MDL-CAP allot
create MDL-NBUF MDL-NCAP allot
create MDL-PATH 512 allot

variable MDL-PATHU
variable MDL-BAD
variable MDL-FILES
variable MDL-NL#
variable MDL-REPORT?

: MDL-NL ( -- ) MDL-LF emit ;

: MDL-TRUE ( -- bool )
   0 0= ;

: MDL-FALSE ( -- bool )
   0 0= 0= ;

: MDL-REPORT! ( bool -- )
   MDL-REPORT? ! ;

: MDL-REPORT-ON ( -- )
   MDL-TRUE MDL-REPORT! ;

: MDL-REPORT-OFF ( -- )
   MDL-FALSE MDL-REPORT! ;

: MDL-U. ( n -- )
   0 MDL-NL# !
   dup 0= IF drop MDL-ZERO emit exit THEN
   begin dup 0 > while
      dup 10 mod MDL-ZERO + MDL-NBUF MDL-NL# @ + c!
      10 / MDL-NL# @ 1+ MDL-NL# !
   repeat drop
   begin MDL-NL# @ 0 > while
      MDL-NL# @ 1- MDL-NL# !
      MDL-NBUF MDL-NL# @ + c@ emit
   repeat ;

: MDL-BAD+ ( -- ) MDL-BAD @ 1+ MDL-BAD ! ;

: MDL-SRC? ( ptr u8 n -- bool ) {: a:ptr u :}
   a u s" .f" HAS-EXT?  a u s" .fs" HAS-EXT? or ;

: MDL-PATH! ( ptr u8 n -- ) {: a:ptr u :}
   a MDL-PATH u LINT-BMOVE  u MDL-PATHU ! ;

: MDL-HIT ( ptr u8 n -- ) {: t:ptr tu :}
   MDL-REPORT? @ if
      s" MAKI-DEP " type
      MDL-PATH MDL-PATHU @ type
      s" : forbidden maki/ reference in token '" type
      t tu type
      s" '" type MDL-NL
   then
   MDL-BAD+ ;

\ The sole sanctioned habu->maki reference: test/run-lib.f names the maki suite
\ entry maki/test.f to SPAWN it (dot habu-route-the-maki-e61d8a1b). Allow exactly
\ that token - the bare load path maki/test.f or its s" body maki/test.f" (the
\ closing quote rides on the token) - in exactly that file. A longer near-miss
\ (maki/test.fs) or the token in any other file is still a finding.
: MDL-GATE-ROUTE? ( ptr u8 n -- bool ) {: t:ptr tu :}
   MDL-PATH MDL-PATHU @ s" test/run-lib.f" LINT-STR= 0= if MDL-FALSE exit then
   t tu s" maki/test.f" LINT-STARTS-WITH? 0= if MDL-FALSE exit then
   s" maki/test.f" nip {: pu :}
   tu pu = if MDL-TRUE exit then
   tu pu 1+ = if t tu 1- + c@ MDL-DQUOTE = exit then
   MDL-FALSE ;

: MDL-SCAN-TOKENS ( -- )
   0 begin dup TN# @ < while
      dup TOK 2dup s" maki/" LINT-CONTAINS? IF
         2dup MDL-GATE-ROUTE? IF 2drop ELSE MDL-HIT THEN
      ELSE 2drop THEN
      1+
   repeat drop ;

\ scan an arbitrary source string (used by both the file walk and the tests)
: MDL-SCAN-STR ( ptr u8 n -- )
   LINT-TRUE PARENS? !
   TOKENIZE
   MDL-SCAN-TOKENS ;

\ findings produced by scanning one string in isolation (reset -> scan -> count)
: MDL-COUNT ( ptr u8 n -- n )
   MDL-REPORT? @ {: report:bool :}
   MDL-REPORT-OFF
   0 MDL-BAD !
   MDL-SCAN-STR
   report MDL-REPORT!
   MDL-BAD @ ;

: MDL-SCAN-FILE ( ptr u8 n -- ) {: a:ptr u :}
   a u MDL-SRC? 0= IF exit THEN
   a u MDL-PATH!
   MDL-FILES @ 1+ MDL-FILES !
   a u MDL-BUF MDL-CAP READ-FILE MDL-SCAN-STR ;

: MAKI-DEP-LINT ( -- )
   MDL-REPORT-ON
   0 MDL-BAD !  0 MDL-FILES !
   s" src/"  [: MDL-SCAN-FILE ;] WALK-FILES
   s" lib/"  [: MDL-SCAN-FILE ;] WALK-FILES
   s" test/" [: MDL-SCAN-FILE ;] WALK-FILES
   s" maki-dep-lint: " type
   MDL-FILES @ MDL-U. s"  file(s), " type
   MDL-BAD @ MDL-U.   s"  finding(s)" type MDL-NL
   MDL-BAD @ 0 > IF 1 throw THEN ;
