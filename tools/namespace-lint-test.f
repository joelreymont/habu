\ namespace-lint-test.f - checked fixtures for the maki namespace ledger lint.
\ Run: bin/hb --load tools/namespace-lint-test.f
\
\ Every fixture is driven through NAMESPACE-LINT:COUNT, the production scan, so
\ the rules are read off the real lexer table and not off a model of it, and
\ requiring the core also lets LIVE enforce the real repo ledger (must be clean).
\
\ The FOOL group is the point of the file: each of its cases was measured against
\ the quote-parity scan this lint used to run, and each one came back wrong. A
\ fixture that needs a `"`, a `\` or a line break composes its text from named
\ byte helpers rather than from an escaped literal - the idiom
\ tools/error-code-lint-test.f uses - because an escaped blob is hard to read,
\ easy to get wrong, and a `\` inside a string body is itself one of the shapes
\ under test.
\
\ Load after lib/test.f and tools/namespace-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/source-lex.f
require tools/namespace-lint-core.f

\ The fixtures live in a package of their own, as docs/forth.md § Testing asks:
\ the MNLT- prefix every name used to carry is what a package scope is for.
package NAMESPACE-LINT-TEST

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

: FILES ( -- )
   \ maki source selection + documented exemptions
   s" maki/eval/eval.f"      NAMESPACE-LINT:MAKI-SRC? TTRUE
   s" lib/string.f"          NAMESPACE-LINT:MAKI-SRC? TFALSE
   \ the walk is rooted at maki/, so the selector is an anchored prefix: a path
   \ that merely CONTAINS the four bytes is not a maki source
   s" lib/maki/x.f"          NAMESPACE-LINT:MAKI-SRC? TFALSE
   s" maki/eval/eval-test.f" NAMESPACE-LINT:SKIP-FILE? TTRUE  \ test scaffolding is exempt
   s" maki/array.f"          NAMESPACE-LINT:SKIP-FILE? TTRUE  \ documented ARRAY substrate
   s" maki/device-smoke.f"   NAMESPACE-LINT:SKIP-FILE? TTRUE  \ gate device-FFI canary (smoke suite)
   s" maki/eval/eval.f"      NAMESPACE-LINT:SKIP-FILE? TFALSE ;

: DETECT ( -- )
   \ a definition at global scope is a finding
   s" : SQUARE dup * ;"           NAMESPACE-LINT:COUNT 1 T=
   s" 5 constant FOO"             NAMESPACE-LINT:COUNT 1 T=
   s" variable V"                 NAMESPACE-LINT:COUNT 1 T=
   s" create BUF 4 allot"         NAMESPACE-LINT:COUNT 1 T=
   s" 2 LAYOUT-BUFFER BUF sample" NAMESPACE-LINT:COUNT 1 T=
   s" KERNEL: K dup ;"            NAMESPACE-LINT:COUNT 1 T= ;

: CASE-FOLD ( -- )
   \ the dictionary is case-insensitive: upper-case definers define globals too
   s" CREATE BUF 4 allot"  NAMESPACE-LINT:COUNT 1 T=
   s" VARIABLE V"          NAMESPACE-LINT:COUNT 1 T=
   s" layout-buffer BUF sample 2" NAMESPACE-LINT:COUNT 1 T=
   \ upper-case package words still open/close scope: no false positive inside,
   \ and an upper-case closer must not leave the scan stuck at depth > 0
   s" PACKAGE MK : F dup ; ;package"     NAMESPACE-LINT:COUNT 0 T=
   s" package MK ;package : LATE dup ;"  NAMESPACE-LINT:COUNT 1 T=
   s" package MK ;PaCkAgE : LATE dup ;"  NAMESPACE-LINT:COUNT 1 T= ;

: SCOPE ( -- )
   \ inside a package it is NOT a finding; after ;package it is again
   s" package MK : SQUARE dup * ; ;package"   NAMESPACE-LINT:COUNT 0 T=
   s" package MK KERNEL: K dup ; ;package"    NAMESPACE-LINT:COUNT 0 T=
   s" package MK 2 LAYOUT-BUFFER BUF sample ;package" NAMESPACE-LINT:COUNT 0 T=
   s" package MK ;package : LATE dup ;"       NAMESPACE-LINT:COUNT 1 T=
   \ `end-package` is not a closer - the engine refuses it (E-UNDEFINED, proven
   \ by test/gate-dictionary-lib.f) - so the package is still open after it
   s" package MK end-package : LATE dup ;"    NAMESPACE-LINT:COUNT 0 T=
   \ a comment between `package` and its name does not lose the name token
   s" package ( subsystem ) MK : F dup ; ;package" NAMESPACE-LINT:COUNT 0 T= ;

: WHITELIST ( -- )
   \ E-* cross-cutting error constants are exempt
   s" -5 constant E-FOO"  NAMESPACE-LINT:COUNT 0 T=
   \ legacy BEGIN-/END- names count as legacy pairs, not primary findings
   s" : BEGIN-BLOCK dup ;"  NAMESPACE-LINT:COUNT 0 T=
   s" : BEGIN-BLOCK dup ;"  NAMESPACE-LINT:LEGACY-COUNT 1 T=
   s" : END-BLOCK dup ;"    NAMESPACE-LINT:LEGACY-COUNT 1 T= ;

: NO-FALSE-POSITIVE ( -- )
   \ `\` line comments and `( )` stack comments are consumed by the lexer
   s" \ : NOTADEF in prose"        NAMESPACE-LINT:COUNT 0 T=
   s" : F ( a : b -- n ) dup ;"    NAMESPACE-LINT:COUNT 1 T=
   \ The name is the next WORD, not the next TOKEN: an inert span between a
   \ definer and its name must not be read as the name. Each arm below turns on
   \ the COUNT rather than on the text reported, because a scan that took the
   \ comment for the name would still report one finding for the first line and
   \ prove nothing.
   s" create ( four cells ) BUF 4 allot" NAMESPACE-LINT:COUNT 1 T=
   \ an E-* name is exempt; a comment read as the name is not
   s" -5 constant ( throw code ) E-FOO"  NAMESPACE-LINT:COUNT 0 T=
   \ a BEGIN-/END- name is a legacy tally, not a primary finding
   s" : ( note ) BEGIN-BLOCK dup ;"      NAMESPACE-LINT:COUNT 0 T=
   s" : ( note ) BEGIN-BLOCK dup ;"      NAMESPACE-LINT:LEGACY-COUNT 1 T= ;

\ ---- fixtures built to fool the scan ----------------------------------------
\ Each case is annotated with what the old quote-parity scan over
\ tools/lint/token.f tokens answered, measured on master before the port.
: FOOL ( -- )
   \ old: 0, and this is the blindness the dot names. One bare `[char] "` is a
   \ literal byte, not a string opener; the old scan counted the quote, set its
   \ in-string flag, and skipped every definition in the REST OF THE FILE while
   \ still printing a clean summary.
   FIX-RESET s" [char] " FIX+ Q+ s"  : SNEAK dup ;" FIX+
   FIX$ NAMESPACE-LINT:COUNT 1 T=
   \ and the operand itself is parsed, not executed: `[char] :` declares nothing
   FIX-RESET s" [char] : dup" FIX+
   FIX$ NAMESPACE-LINT:COUNT 0 T=
   s" ['] constant drop"    NAMESPACE-LINT:COUNT 0 T=
   \ old: 1. `.( ... )` is a printing comment; its body is not code. The old
   \ splitter only treated `(` as an opener, so the whole body read as tokens.
   s" .( defining : GHOST here )" NAMESPACE-LINT:COUNT 0 T=
   \ old: 1. A `(` that opens a comment before a NEWLINE is still a comment. The
   \ old splitter demanded the next byte be a SPACE exactly, so a multi-line
   \ stack comment read as code.
   FIX-RESET s" (" FIX+ EOL+ s" : SNEAK dup ;" FIX+ EOL+ s" )" FIX+
   FIX$ NAMESPACE-LINT:COUNT 0 T=
   \ a defining word inside a string body is NOT a definition (both openers)
   FIX-RESET s" : F ." FIX+ Q+ s"  x : y" FIX+ Q+ s"  ;" FIX+
   FIX$ NAMESPACE-LINT:COUNT 1 T=
   FIX-RESET s" : F s\" FIX+ Q+ s"  : y" FIX+ Q+ s"  drop ;" FIX+
   FIX$ NAMESPACE-LINT:COUNT 1 T=
   \ old: 1, where the truth is 2. A `\` inside a string body is ordinary text,
   \ but the old splitter read it as a line comment, swallowed the closing quote
   \ and skipped to end of line - inverting the in-string flag for everything
   \ after it, so the second definition was never seen.
   FIX-RESET s" : F ." FIX+ Q+ s"  a \ b" FIX+ Q+ s"  ; : LATER dup ;" FIX+
   FIX$ NAMESPACE-LINT:COUNT 2 T=
   \ the same shape spread over two lines, which is how it occurs in real source
   FIX-RESET s" : F ." FIX+ Q+ s"  a \ b" FIX+ Q+ s"  ;" FIX+ EOL+ s" : LATER dup ;" FIX+
   FIX$ NAMESPACE-LINT:COUNT 2 T= ;

: LIVE ( -- )
   \ the real maki tree is clean: every def lives in a package (enforcing check)
   NAMESPACE-LINT:STRICT ;

: MAIN ( -- )
   T-RESET
   FILES
   DETECT
   CASE-FOLD
   SCOPE
   WHITELIST
   NO-FALSE-POSITIVE
   FOOL
   T-REPORT
   LIVE ;

public

EXPORT MAIN

;package

NAMESPACE-LINT-TEST:MAIN
