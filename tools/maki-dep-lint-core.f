\ maki-dep-lint-core.f - one-way habu<-maki dependency guard.
\
\ Enforces the PLAN guardrail (maki/README.md): imports flow maki -> habu, NEVER
\ habu -> maki. The Habu core (src/), libraries (lib/), and the native gate (test/)
\ must never reference a maki/ path; maki/ is application Forth fenced outside the
\ trust root. Until this lint, the seam was held only by review + the gate's explicit
\ allow-lists.
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

$40000 constant MDL-CAP
32 constant MDL-NCAP
10 constant MDL-LF
48 constant MDL-ZERO

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

: MDL-SCAN-TOKENS ( -- )
   0 begin dup TN# @ < while
      dup TOK 2dup s" maki/" LINT-CONTAINS? IF MDL-HIT ELSE 2drop THEN
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
