\ maki-dep-lint-test.f - checked fixtures for the one-way habu<-maki dependency lint.
\
\ Every fixture is driven through MAKI-DEP-LINT:COUNT, the production scan, so the
\ rules are read off the real lexer table and not off a model of it.
\
\ The FOOL group is the point of the file: each of its cases was measured against
\ the substring scan this lint used to run, and each one came back wrong. A fixture
\ that needs a `"`, a `\` or a line break composes its text from named byte helpers
\ rather than from an escaped literal - the idiom tools/error-code-lint-test.f uses -
\ because an escaped blob is hard to read, easy to get wrong, and a `\` inside a
\ string body is itself one of the shapes under test.
\
\ Load after lib/test.f and tools/maki-dep-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/source-lex.f
require tools/maki-dep-lint-core.f

\ The fixtures live in a package of their own, as docs/forth.md § Testing asks:
\ the MDLT- prefix every name used to carry is what a package scope is for.
package MAKI-DEP-LINT-TEST

private

$200 constant FIX-CAP

create FIX FIX-CAP allot
variable FIX-U

: FIX-RESET ( -- )
   0 FIX-U ! ;

: FIX+ ( ptr u8 n -- ) {: a:ptr u:n :}
   FIX-U @ u + FIX-CAP > if E-TEST-CAPACITY throw then
   a FIX FIX-U @ + u LINT-BMOVE
   FIX-U @ u + FIX-U ! ;

: FIX-C+ ( n -- ) {: c:n :}
   FIX-U @ 1+ FIX-CAP > if E-TEST-CAPACITY throw then
   c FIX FIX-U @ + c!
   FIX-U @ 1+ FIX-U ! ;

: Q+ ( -- )   \ one literal double-quote byte
   DQUOTE FIX-C+ ;

: EOL+ ( -- )   \ one literal newline byte
   10 FIX-C+ ;

: FIX$ ( -- ptr u8 n )
   FIX FIX-U @ ;

: EXT ( -- )
   s" lib/ptx/cg.f"  MAKI-DEP-LINT:SRC? TTRUE
   s" test/run.fs"   MAKI-DEP-LINT:SRC? TTRUE
   s" docs/forth.md" MAKI-DEP-LINT:SRC? TFALSE
   s" maki/array.f"  MAKI-DEP-LINT:SRC? TTRUE ;

: DETECT ( -- )
   \ a bare load token referencing maki/ is a finding
   s" --load maki/array.f"        MAKI-DEP-LINT:COUNT 1 T=
   \ two bare tokens are two findings
   s" maki/eval/eval.f maki/optim.f"    MAKI-DEP-LINT:COUNT 2 T= ;

\ ---- the payload rule -------------------------------------------------------
\ A literal's payload is read word by word, so a path keeps its own boundaries
\ whether it stands alone in the literal or shares it with other words.
: PAYLOAD ( -- )
   \ `s" maki/array.f"` - the whole payload is the path
   FIX-RESET s" s" FIX+ Q+ s"  maki/array.f" FIX+ Q+
   FIX$ MAKI-DEP-LINT:COUNT 1 T=
   \ `s" --load maki/x.f"` - the path is one word of a longer literal
   FIX-RESET s" s" FIX+ Q+ s"  --load maki/x.f" FIX+ Q+
   FIX$ MAKI-DEP-LINT:COUNT 1 T=
   \ `s" ../maki/x.f"` - `maki` as an interior path component
   FIX-RESET s" s" FIX+ Q+ s"  ../maki/x.f" FIX+ Q+
   FIX$ MAKI-DEP-LINT:COUNT 1 T=
   \ the escaped opener carries a payload the same way
   FIX-RESET s" s\" FIX+ Q+ s"  maki/array.f" FIX+ Q+
   FIX$ MAKI-DEP-LINT:COUNT 1 T= ;

: NO-FALSE-POSITIVE ( -- )
   \ `\` line comments are consumed -> mentioning maki/ in prose is NOT a finding
   s" \ this loads maki/array.f at the maki layer" MAKI-DEP-LINT:COUNT 0 T=
   \ `( )` stack-effect comments are consumed too
   s" : F ( maki/x -- n ) dup ;"   MAKI-DEP-LINT:COUNT 0 T=
   \ clean core code never trips
   s" : SQUARE ( n -- n ) dup * ;" MAKI-DEP-LINT:COUNT 0 T= ;

\ ---- fixtures built to fool the scan ----------------------------------------
\ Each case is annotated with what the old substring scan over tools/lint/token.f
\ tokens answered, measured on master before the port to LINT-LEX.
: FOOL ( -- )
   \ old: 1. `.( ... )` is a printing comment; its body is not code. The old
   \ splitter only treated `(` as an opener, so the whole body read as tokens.
   s" .( loading maki/array.f )"  MAKI-DEP-LINT:COUNT 0 T=
   \ old: 1. A `(` that opens a comment before a NEWLINE is still a comment. The
   \ old splitter demanded the next byte be a SPACE exactly, so a multi-line stack
   \ comment read as code.
   FIX-RESET s" (" FIX+ EOL+ s" maki/array.f" FIX+ EOL+ s" )" FIX+
   FIX$ MAKI-DEP-LINT:COUNT 0 T=
   \ old: 1. `premaki/thing` has the path component `premaki`, not `maki`; the old
   \ scan matched `maki/` as bytes anywhere in the token.
   FIX-RESET s" s" FIX+ Q+ s"  premaki/thing" FIX+ Q+
   FIX$ MAKI-DEP-LINT:COUNT 0 T=
   \ old: 0, and this is the evasion. A `\` inside a string body is ordinary text,
   \ but the old splitter read it as a line comment, swallowed the closing quote
   \ and skipped to end of line - hiding the real load that followed it.
   FIX-RESET s" s" FIX+ Q+ s"  a \ b" FIX+ Q+ s"  --load maki/x.f" FIX+
   FIX$ MAKI-DEP-LINT:COUNT 1 T=
   \ the same shape spread over two lines, which is how it occurs in real source
   FIX-RESET s" s" FIX+ Q+ s"  a \ b" FIX+ Q+ EOL+ s" --load maki/x.f" FIX+
   FIX$ MAKI-DEP-LINT:COUNT 1 T= ;

: GATE-ROUTE ( -- )
   \ dot habu-route-the-maki-e61d8a1b: the gate harness may name the maki suite
   \ entry it spawns. Exactly those paths, in exactly test/run-lib.f, are allowed;
   \ the s" quote form is proven end-to-end by LIVE-LINT scanning the real file.
   s" test/run-lib.f" MAKI-DEP-LINT:PATH!
   s" --load maki/test.f"     MAKI-DEP-LINT:COUNT 0 T=   \ sanctioned bare token: allowed
   s" --load maki/test-core.f" MAKI-DEP-LINT:COUNT 0 T=  \ a parallel slice loader: allowed
   s" --load maki/report.f"   MAKI-DEP-LINT:COUNT 1 T=   \ any other maki/ token here still fails
   s" --load maki/test.fs"    MAKI-DEP-LINT:COUNT 1 T=   \ 12-char near-miss is not the .f form
   \ the allowance is the PATH, not the bytes around it: the payload of a literal
   \ is compared without the closing quote the old splitter used to chop off
   FIX-RESET s" s" FIX+ Q+ s"  maki/test.f" FIX+ Q+
   FIX$ MAKI-DEP-LINT:COUNT 0 T=
   \ a comment naming another maki path in this file is still not a reference
   s" \ spawns maki/report.f as a child" MAKI-DEP-LINT:COUNT 0 T=
   s" test/other.f" MAKI-DEP-LINT:PATH!
   s" --load maki/test.f"     MAKI-DEP-LINT:COUNT 1 T=   \ sanctioned token in another file still fails
   0 MAKI-DEP-LINT:PATHU ! ;                             \ reset so later scans are file-agnostic

: LIVE-LINT ( -- )
   \ the real tree (src/ lib/ test/) must be maki-free -> the walk returns clean
   MAKI-DEP-LINT:RUN ;

: MAIN ( -- )
   T-RESET
   EXT
   DETECT
   PAYLOAD
   NO-FALSE-POSITIVE
   FOOL
   GATE-ROUTE
   LIVE-LINT
   T-REPORT ;

public

EXPORT MAIN

;package

MAKI-DEP-LINT-TEST:MAIN
