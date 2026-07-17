\ namespace-lint-core.f - flag governed definitions outside their packages.
\
\ The maki ledger rejects definitions outside subsystem packages. The governed
\ tool list extends that fail-closed rule to modules that have completed their
\ package migration; side-content errors therefore cannot return to global scope.
\
\ Scan: each governed source is TOKENIZEd (PARENS? on, so `\` line comments and
\ `( )` stack comments are stripped and `s"` bodies stay single tokens), then
\ the token stream is walked while tracking `package`/`;package` depth. Every
\ defining word (`:`, `constant`, `variable`, `create`, `DEFTYPE`, `KERNEL:`, ...)
\ seen at depth 0 names a global definition; its name token is a finding unless
\ whitelisted. Definer and package tokens match case-INSENSITIVELY: the dictionary
\ is case-insensitive (docs/forth.md § Naming), so `CREATE BUF` defines a global
\ and `;package` closes a package exactly like their lower-case spellings; a
\ case-sensitive scan is an evasion vector (this subsumed the retired
\ tools/maki-ns-lint.f, whose one-package-per-file marker model could not express
\ the multi-package subsystem files - dot habu-maki-ns-lint-reconcile).
\
\ Whitelist (documented substrate / cross-cutting, NOT flagged):
\   - E-* names in maki/: cross-cutting error-code constants, top-level by
\     convention. Package-governed tool modules receive no such exemption.
\   - maki/array.f: the documented ARRAY value-array substrate (T-GET/T-SET/...),
\     used bare across lib/ptx tests; packaging it is churn for negative gain.
\   - BEGIN-*/END-* names: legacy scope pairs (owed a FOO/;FOO rename by their own
\     dot); reported in a separate legacy-pair tally, not the primary finding set.
\   - *-test.f files: test scaffolding legitimately defines global test locals.
\   - maki/device-smoke.f: the gate's device-FFI canary - a smoke-test suite run by
\     maki/test.f under lib/test.f (T-RESET/TTRUE/T-REPORT). It is gate scaffolding,
\     the same category as *-test.f, and just lacks the -test suffix by history.
\
\ NAMESPACE-LINT prints the ledger and the count without throwing (a report view).
\ NAMESPACE-LINT-STRICT throws on any finding and is the gate entrypoint now that the
\ eval/gpu GLOBAL clusters have landed in per-subsystem packages: the ledger is clean,
\ enforcement is on, and any new governed global definition fails the gate.
\
\ Load after lib/errors.f, lib/string.f, lib/memory.f, lib/fs.f, tools/lint/text.f,
\ and tools/lint/token.f.

$40000 constant NL-CAP
512 constant NL-PCAP
48 constant NL-ZERO

create NL-BUF  NL-CAP allot
create NL-PATH NL-PCAP allot
create NL-NBUF 32 allot

variable NL-PATHU
variable NL-INSTR                       \ inside an s" ... " string literal body
variable NL-DEPTH                       \ current package nesting depth
variable NL-BAD                         \ primary findings (governed global defs)
variable NL-LEGACY                      \ legacy BEGIN-/END- pair tally
variable NL-FILES
variable NL-REPORT?
variable NL-ND#
variable NL-QI
variable NL-ALLOW-E

: NL-NL ( -- ) 10 emit ;

: NL-REPORT! ( bool -- )  NL-REPORT? ! ;
: NL-REPORT-ON  ( -- )  LINT-TRUE  NL-REPORT! ;
: NL-REPORT-OFF ( -- )  LINT-FALSE NL-REPORT! ;

\ unsigned decimal (no <# number ring in this Habu; digit-buffer like maki-dep)
: NL-U. ( n -- )
   0 NL-ND# !
   dup 0= if drop NL-ZERO emit exit then
   begin dup 0 > while
      dup 10 mod NL-ZERO + NL-NBUF NL-ND# @ + c!
      10 / NL-ND# @ 1+ NL-ND# !
   repeat drop
   begin NL-ND# @ 0 > while
      NL-ND# @ 1- NL-ND# !
      NL-NBUF NL-ND# @ + c@ emit
   repeat ;

\ ---- token classification ---------------------------------------------------
\ true when the token names the next token as a new global word (CI: the
\ dictionary is case-insensitive, so `CREATE`/`Constant` define like `create`)
: NL-DEF-WORD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
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
   a u s" DEFTYPE"   LINT-STR=CI if LINT-TRUE exit then
   a u s" value"     LINT-STR=CI if LINT-TRUE exit then
   a u s" defer"     LINT-STR=CI if LINT-TRUE exit then
   a u s" CHECKED:"  LINT-STR=CI if LINT-TRUE exit then
   a u s" TRUSTED:"  LINT-STR=CI if LINT-TRUE exit then
   a u s" KERNEL:"   LINT-STR=CI if LINT-TRUE exit then
   LINT-FALSE ;

: NL-LEGACY-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" BEGIN-" LINT-PREFIX? a u s" END-" LINT-PREFIX? or ;

\ odd number of `"` bytes -> this token flips the in-string state (an opener like
\ s"/."/c"/s\" or a closer ...") so string bodies never read as definitions
: NL-QUOTES-ODD? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   0  0 NL-QI !
   begin NL-QI @ u < while
      a NL-QI @ BYTE@ 34 = if 1+ then
      NL-QI @ 1+ NL-QI !
   repeat
   1 and 0= 0= ;

\ E-* constants are cross-cutting exceptions only in the legacy maki ledger
: NL-WHITELISTED? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   NL-ALLOW-E @ 0= 0= a u s" E-" LINT-PREFIX? and ;

\ ---- file selection ---------------------------------------------------------
: NL-MAKI-SRC? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" maki/" LINT-CONTAINS?  a u s" .f" HAS-EXT?  and ;

: NL-OWNED-TOOL? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" tools/diff-side-content.f" LINT-STR= if
      LINT-TRUE exit
   then
   a u s" tools/diff-side-content-read.f" LINT-STR= ;

\ documented ARRAY substrate + test/canary scaffolding are exempt from the scan
: NL-SKIP-FILE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" -test.f"             HAS-EXT?        if LINT-TRUE exit then
   a u s" maki/device-smoke.f" LINT-ENDS-WITH? if LINT-TRUE exit then
   a u s" maki/array.f"        LINT-ENDS-WITH? ;

: NL-PATH! ( ptr u8 n -- ) {: a:ptr u:n :}
   a NL-PATH u LINT-BMOVE  u NL-PATHU ! ;

: NL-WHERE ( -- )  NL-PATH NL-PATHU @ type ;

: NL-HIT ( ptr u8 n -- ) {: nptr:ptr nu:n :}
   NL-REPORT? @ if
      s" NAMESPACE " type NL-WHERE
      s" : global def '" type  nptr nu type  s" ' outside a package" type
      NL-NL
   then
   NL-BAD @ 1+ NL-BAD ! ;

: NL-LEGACY-HIT ( ptr u8 n -- ) {: nptr:ptr nu:n :}
   NL-REPORT? @ if
      s" NAMESPACE-LEGACY " type NL-WHERE
      s" : legacy scope-pair name '" type  nptr nu type  s" '" type NL-NL
   then
   NL-LEGACY @ 1+ NL-LEGACY ! ;

\ ---- token walk with package-scope tracking ---------------------------------
\ classify one definition at depth 0: primary finding, legacy tally, or exempt
: NL-DEF-AT ( n -- ) {: i:n :}
   i 1+ TN# @ >= if exit then           \ no name token follows (malformed tail)
   i 1+ TOK {: nptr:ptr nu:n :}
   nptr nu NL-LEGACY-NAME? if nptr nu NL-LEGACY-HIT exit then
   nptr nu NL-WHITELISTED? if exit then
   nptr nu NL-HIT ;

: NL-PACKAGE-CLOSE? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" ;package" LINT-STR=CI ;

: NL-STEP ( n -- n ) {: i:n :}          \ handle token i, return next index
   i TOK {: tp:ptr tu:n :}
   NL-INSTR @ if                                     \ skip string bodies wholesale
      tp tu NL-QUOTES-ODD? if NL-INSTR @ 0= NL-INSTR ! then
      i 1+ exit then
   tp tu s" package" LINT-STR=CI if
      NL-DEPTH @ 1+ NL-DEPTH !  i 2 + exit then      \ skip the package name token
   tp tu NL-PACKAGE-CLOSE? if
      NL-DEPTH @ 1- NL-DEPTH !  i 1+ exit then
   NL-DEPTH @ 0= tp tu NL-DEF-WORD? and if
      i NL-DEF-AT  i 2 + exit then                   \ skip past the defined name
   tp tu NL-QUOTES-ODD? if NL-INSTR @ 0= NL-INSTR ! then   \ e.g. an s" opener
   i 1+ ;

: NL-SCAN-TOKENS ( -- )
   0 NL-DEPTH !  0 NL-INSTR !
   0 begin dup TN# @ < while NL-STEP repeat drop ;

: NL-SCAN-STR ( ptr u8 n -- ) {: a:ptr u:n :}
   LINT-TRUE PARENS? !
   a u TOKENIZE
   NL-SCAN-TOKENS ;

\ findings from scanning one string in isolation (reset -> scan -> count); tests
: NL-COUNT-MODE ( ptr u8 n bool -- n )
   {: a:ptr u:n allow:bool :}
   NL-REPORT? @ {: report:bool :}
   NL-REPORT-OFF
   allow NL-ALLOW-E !
   0 NL-BAD !  0 NL-LEGACY !
   a u NL-SCAN-STR
   report NL-REPORT!
   NL-BAD @ ;

: NL-COUNT ( ptr u8 n -- n )
   LINT-TRUE NL-COUNT-MODE ;

: NL-STRICT-COUNT ( ptr u8 n -- n )
   LINT-FALSE NL-COUNT-MODE ;

: NL-LEGACY-COUNT ( ptr u8 n -- n )
   NL-REPORT? @ {: report:bool :}
   NL-REPORT-OFF
   0 NL-BAD !  0 NL-LEGACY !
   NL-SCAN-STR
   report NL-REPORT!
   NL-LEGACY @ ;

: NL-SCAN-FILE ( ptr u8 n -- ) {: a:ptr u:n :}
   a u NL-MAKI-SRC? {: maki:bool :}
   maki 0= a u NL-OWNED-TOOL? 0= and if exit then
   maki a u NL-SKIP-FILE? and if exit then
   maki NL-ALLOW-E !
   a u NL-PATH!
   NL-FILES @ 1+ NL-FILES !
   a u NL-BUF NL-CAP READ-FILE NL-SCAN-STR ;

: NL-RUN ( -- )
   0 NL-BAD !  0 NL-LEGACY !  0 NL-FILES !
   s" maki/" [: NL-SCAN-FILE ;] WALK-FILES
   s" tools/" [: NL-SCAN-FILE ;] WALK-FILES ;

: NL-SUMMARY ( -- )
   s" namespace-lint: " type
   NL-FILES  @ NL-U. s"  governed file(s), " type
   NL-BAD    @ NL-U. s"  global-def finding(s), " type
   NL-LEGACY @ NL-U. s"  legacy-pair(s)" type NL-NL ;

\ report view: prints the ledger without throwing (NAMESPACE-LINT-STRICT enforces)
: NAMESPACE-LINT ( -- )
   NL-REPORT-ON  NL-RUN  NL-SUMMARY ;

\ gate entry: any governed global definition outside a package fails
: NAMESPACE-LINT-STRICT ( -- )
   NAMESPACE-LINT
   NL-BAD @ 0 > if 1 throw then ;
