\ render-parse-lint-test.f - fixtures for the renderer-output fence.
\
\ The gate in tools/render-parse-lint-core.f is only worth having if it bites on
\ the thing it forbids and stays quiet on everything that merely looks like it.
\ Each fixture below is one source, built byte by byte so a backslash or a quote
\ in the fixture is unmistakable, and each is scanned through the lint's own
\ COUNT entry - the same scan the tree-wide gate runs, not a copy of it.
\
\ THE POSITIVE FIXTURES are the three ways compiler code could get its hands on
\ rendered text: naming a word of either package, reopening either package to
\ call its words bare, and loading either source. Each must be exactly one
\ finding, and the lower-case spelling must be caught too, because the dictionary
\ is case-insensitive and a case-sensitive scan would be an evasion.
\
\ THE HOSTILE FIXTURES are the ways a substring search would be fooled: the same
\ text inside a `\` line comment, inside a `( ... )` stack comment, inside an
\ `s" ... "` string body, and inside an escaped `s\" ... \"` body. None may
\ report anything, because none of them is code. A stray quote earlier in the
\ file must not blind the scan either, and an unterminated string must be a named
\ refusal rather than a quiet zero.
\
\ THE WRONG-ROLE FIXTURES are names that share a prefix without naming the stage:
\ a longer package name, a word whose name merely begins with the letters, and a
\ qualified name in some other package that ends with them.
\
\ THE ORDER AND DUPLICATE FIXTURES check that a finding does not depend on where
\ in the file it sits and that two of them count twice.
\
\ THE EXEMPTION FIXTURES check the ledger: the renderer stage's own two files are
\ not scanned, every other compiler source is, and nothing outside src/compiler/
\ is.

require lib/test.f
require lib/errors.f
require lib/string.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/token.f
require tools/lint/lib.f
require tools/render-parse-lint-core.f

package RENDER-PARSE-LINT-TEST
private

4096 constant FX-CAP

\ Fixture-buffer bound, not a product error code: a fixture longer than the
\ buffer is a mistake in this file, not a condition the lint can meet.
9003 constant E-FX-CAP

create FX FX-CAP allot

variable FXU

\ ---- building one fixture source ---------------------------------------------
: FX-RESET ( -- )
   0 FXU ! ;

: FB ( n -- )
   {: b:n :}
   FXU @ FX-CAP >= if E-FX-CAP throw then
   b FX FXU @ + c!
   FXU @ 1+ FXU ! ;

: FS ( ptr u8 n -- )
   {: p:ptr u:n :}
   u 0 ?do
      p i + c@ FB
   loop ;

: FNL ( -- )
   $0A FB ;

: FBS ( -- )
   $5C FB ;

: FQ ( -- )
   $22 FB ;

: FX$ ( -- ptr u8 n )
   FX FXU @ ;

: FINDINGS ( -- n )
   FX$ RENDER-PARSE-LINT:COUNT ;

\ ---- the positive fixtures ---------------------------------------------------
: FX-QUALIFIED ( -- )
   FX-RESET
   s" package HIR" FS FNL
   s" : SHOW ( -- ) IR-RENDER:RENDER drop ;" FS FNL
   s" ;package" FS FNL ;

: FX-DIFF-QUALIFIED ( -- )
   FX-RESET
   s" package HIR" FS FNL
   s" : CHECK ( -- ) IR-DIFF:DIFF drop ;" FS FNL ;

: FX-LOWER-CASE ( -- )
   FX-RESET
   s" : show ( -- ) ir-render:render drop ;" FS FNL ;

: FX-REOPEN ( -- )
   FX-RESET
   s" package IR-RENDER" FS FNL
   s" : SNEAK ( -- ) RENDER drop ;" FS FNL
   s" ;package" FS FNL ;

: FX-REQUIRE ( -- )
   FX-RESET
   s" require src/compiler/ir/render.f" FS FNL
   s" package HIR" FS FNL ;

: FX-REQUIRE-DIFF ( -- )
   FX-RESET
   s" require src/compiler/ir/diff.f" FS FNL ;

\ ---- the hostile fixtures ---------------------------------------------------
\ A `\` line comment carrying the forbidden name.
: FX-LINE-COMMENT ( -- )
   FX-RESET
   s" package HIR" FS FNL
   FBS s"  the renderer is IR-RENDER:RENDER and we never call it" FS FNL
   s" : SHOW ( -- ) 0 drop ;" FS FNL ;

\ A `( ... )` stack comment carrying it.
: FX-PAREN-COMMENT ( -- )
   FX-RESET
   s" : SHOW ( see IR-RENDER:RENDER -- ) 0 drop ;" FS FNL ;

\ A plain string body carrying it, which is how a diagnostic message would.
: FX-STRING ( -- )
   FX-RESET
   s" : WHY ( -- ) s" FS FQ
   s"  IR-RENDER:RENDER is not called" FS FQ
   s"  type ;" FS FNL ;

\ An escaped string body carrying it, and a quote inside that body, which is the
\ shape that used to blind a quote-counting scan for the rest of the file.
: FX-ESCAPED-STRING ( -- )
   FX-RESET
   s" : WHY ( -- ) s" FS FBS FQ
   s"  not " FS FBS FQ
   s" IR-DIFF:DIFF" FS FBS FQ
   FQ
   s"  type ;" FS FNL ;

\ ---- the wrong-role fixtures ------------------------------------------------
\ A longer package name that merely starts with the same letters.
: FX-LONGER-PACKAGE ( -- )
   FX-RESET
   s" package IR-RENDERER" FS FNL
   s" : SHOW ( -- ) 0 drop ;" FS FNL
   s" ;package" FS FNL ;

\ A word whose own name begins with the letters but names no package.
: FX-SIMILAR-WORD ( -- )
   FX-RESET
   s" : IR-RENDER-HINT ( -- ) 0 drop ;" FS FNL
   s" : SHOW ( -- ) IR-RENDER-HINT ;" FS FNL ;

\ A qualified name in a different package whose tail ends with the letters.
: FX-OTHER-PACKAGE ( -- )
   FX-RESET
   s" : SHOW ( -- ) IR-SCHEMA:RENDERER@ drop ;" FS FNL
   s" : ALSO ( -- ) MY-IR-DIFF:DIFF drop ;" FS FNL ;

\ A require of some other compiler source.
: FX-OTHER-REQUIRE ( -- )
   FX-RESET
   s" require src/compiler/ir/canon.f" FS FNL ;

\ ---- order and duplication --------------------------------------------------
\ The same finding as the last token of the file rather than the middle.
: FX-REORDERED ( -- )
   FX-RESET
   s" package HIR" FS FNL
   s" : SHOW ( -- ) 0 drop ;" FS FNL
   s" : LATE ( -- ) IR-RENDER:RENDER drop ;" FS FNL ;

\ Two forbidden references in one source count twice.
: FX-DUPLICATE ( -- )
   FX-RESET
   s" : ONE ( -- ) IR-RENDER:RENDER drop ;" FS FNL
   s" : TWO ( -- ) IR-DIFF:DIFF drop ;" FS FNL ;

\ ---- the fail-closed fixture -------------------------------------------------
\ A string literal that runs past end of input. Every token after it is
\ unreadable, so the lint must refuse by name rather than report zero.
: FX-UNTERMINATED ( -- )
   FX-RESET
   s" : WHY ( -- ) s" FS FQ
   s"  open forever" FS FNL ;

: UNTERMINATED-RUN ( -- )
   FX-UNTERMINATED
   FINDINGS drop ;

public

: RUN ( -- )
   T-RESET

   s" a qualified renderer word in compiler code is one finding" T-LABEL
   FX-QUALIFIED FINDINGS 1 T=

   s" a qualified diff word in compiler code is one finding" T-LABEL
   FX-DIFF-QUALIFIED FINDINGS 1 T=

   s" the lower-case spelling is caught too" T-LABEL
   FX-LOWER-CASE FINDINGS 1 T=

   s" reopening the renderer package is one finding" T-LABEL
   FX-REOPEN FINDINGS 1 T=

   s" requiring the renderer source is one finding" T-LABEL
   FX-REQUIRE FINDINGS 1 T=

   s" requiring the diff source is one finding" T-LABEL
   FX-REQUIRE-DIFF FINDINGS 1 T=

   s" the name in a line comment reports nothing" T-LABEL
   FX-LINE-COMMENT FINDINGS 0 T=

   s" the name in a stack comment reports nothing" T-LABEL
   FX-PAREN-COMMENT FINDINGS 0 T=

   s" the name in a string body reports nothing" T-LABEL
   FX-STRING FINDINGS 0 T=

   s" the name in an escaped string body reports nothing" T-LABEL
   FX-ESCAPED-STRING FINDINGS 0 T=

   s" a longer package name is not the renderer package" T-LABEL
   FX-LONGER-PACKAGE FINDINGS 0 T=

   s" a word whose name starts with the letters is not a reference" T-LABEL
   FX-SIMILAR-WORD FINDINGS 0 T=

   s" a qualified name in another package is not a reference" T-LABEL
   FX-OTHER-PACKAGE FINDINGS 0 T=

   s" requiring another compiler source reports nothing" T-LABEL
   FX-OTHER-REQUIRE FINDINGS 0 T=

   s" the finding does not depend on where in the file it sits" T-LABEL
   FX-REORDERED FINDINGS 1 T=

   s" two forbidden references count twice" T-LABEL
   FX-DUPLICATE FINDINGS 2 T=

   s" an unterminated string literal is a named refusal" T-LABEL
   [: UNTERMINATED-RUN ;] RENDER-PARSE-LINT:E-RPL-QUOTE TTHROWSQ

   s" the renderer's own source is exempt from the fence" T-LABEL
   FX-QUALIFIED
   s" src/compiler/ir/render.f" FX$ RENDER-PARSE-LINT:COUNT-AS 0 T=

   s" the diff's own source is exempt from the fence" T-LABEL
   FX-DIFF-QUALIFIED
   s" src/compiler/ir/diff.f" FX$ RENDER-PARSE-LINT:COUNT-AS 0 T=

   s" any other compiler source is not exempt" T-LABEL
   FX-QUALIFIED
   s" src/compiler/ir/op.f" FX$ RENDER-PARSE-LINT:COUNT-AS 1 T=

   s" a compiler source other than the stage's own is fenced" T-LABEL
   s" src/compiler/ir/op.f" RENDER-PARSE-LINT:FENCED? TTRUE

   s" the renderer's own source is not fenced" T-LABEL
   s" src/compiler/ir/render.f" RENDER-PARSE-LINT:FENCED? TFALSE

   s" a test source is not fenced" T-LABEL
   s" test/compiler/ir-render.f" RENDER-PARSE-LINT:FENCED? TFALSE

   T-REPORT ;

;package

RENDER-PARSE-LINT-TEST:RUN
