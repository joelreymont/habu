\ namespace-lint-test.f - checked fixtures for the maki namespace ledger lint.
\ Run: bin/hb --load tools/namespace-lint-test.f
\ Requiring the core also lets MNLT-LIVE enforce the real repo ledger (must be clean).
\ Load after lib/test.f and tools/namespace-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/namespace-lint-core.f

: MNLT-FILES ( -- )
   \ maki source selection + documented exemptions
   s" maki/eval.f"       NL-MAKI-SRC? TTRUE
   s" lib/string.f"      NL-MAKI-SRC? TFALSE
   s" maki/eval-test.f"     NL-SKIP-FILE? TTRUE  \ test scaffolding is exempt
   s" maki/array.f"         NL-SKIP-FILE? TTRUE  \ documented ARRAY substrate
   s" maki/device-smoke.f"  NL-SKIP-FILE? TTRUE  \ gate device-FFI canary (smoke suite)
   s" maki/eval.f"          NL-SKIP-FILE? TFALSE ;

: MNLT-DETECT ( -- )
   \ a definition at global scope is a finding
   s" : SQUARE dup * ;"  NL-COUNT 1 T=
   s" 5 constant FOO"     NL-COUNT 1 T=
   s" variable V"         NL-COUNT 1 T=
   s" create BUF 4 allot" NL-COUNT 1 T=
   s" 2 LAYOUT-BUFFER BUF sample" NL-COUNT 1 T=
   s" DEFTYPE handle"     NL-COUNT 1 T=
   s" KERNEL: K dup ;"    NL-COUNT 1 T= ;

: MNLT-CASE ( -- )
   \ the dictionary is case-insensitive: upper-case definers define globals too
   s" CREATE BUF 4 allot"  NL-COUNT 1 T=
   s" VARIABLE V"          NL-COUNT 1 T=
   s" layout-buffer BUF sample 2" NL-COUNT 1 T=
   \ upper-case package words still open/close scope: no false positive inside,
   \ and an upper-case closer must not leave the scan stuck at depth > 0
   s" PACKAGE MK : F dup ; ;package"     NL-COUNT 0 T=
   s" package MK ;package : LATE dup ;"  NL-COUNT 1 T=
   s" package MK ;PaCkAgE : LATE dup ;"     NL-COUNT 1 T= ;

: MNLT-SCOPE ( -- )
   \ inside a package it is NOT a finding; after ;package it is again
   s" package MK : SQUARE dup * ; ;package"   NL-COUNT 0 T=
   s" package MK KERNEL: K dup ; ;package"    NL-COUNT 0 T=
   s" package MK 2 LAYOUT-BUFFER BUF sample ;package" NL-COUNT 0 T=
   s" package MK ;package : LATE dup ;"       NL-COUNT 1 T=
   s" package MK ;package : LATE dup ;"          NL-COUNT 1 T= ;

: MNLT-WHITELIST ( -- )
   \ E-* cross-cutting error constants are exempt
   s" -5 constant E-FOO"  NL-COUNT 0 T=
   \ legacy BEGIN-/END- names count as legacy pairs, not primary findings
   s" : BEGIN-BLOCK dup ;"  NL-COUNT 0 T=
   s" : BEGIN-BLOCK dup ;"  NL-LEGACY-COUNT 1 T=
   s" : END-BLOCK dup ;"    NL-LEGACY-COUNT 1 T= ;

: MNLT-NO-FALSE-POSITIVE ( -- )
   \ `\` line comments and `( )` stack comments are stripped by TOKENIZE
   s" \ : NOTADEF in prose"        NL-COUNT 0 T=
   s" : F ( a : b -- n ) dup ;"    NL-COUNT 1 T=
   \ a defining word inside an s" ... " string body is NOT a definition
   s\" : F .\" x : y\" ;"           NL-COUNT 1 T= ;

: MNLT-LIVE ( -- )
   \ the real maki tree is clean: every def lives in a package (enforcing check)
   NAMESPACE-LINT-STRICT ;

: MNLT-MAIN ( -- )
   T-RESET
   MNLT-FILES
   MNLT-DETECT
   MNLT-CASE
   MNLT-SCOPE
   MNLT-WHITELIST
   MNLT-NO-FALSE-POSITIVE
   T-REPORT
   MNLT-LIVE ;

MNLT-MAIN
