\ error-code-lint-test.f - checked fixtures for the E- throw-code uniqueness lint.
\ Run: bin/hb --load tools/error-code-lint-test.f
\ Requiring the core also lets LIVE enforce the real repo ledger (clean).
\
\ Fixtures that need a `"` or a line break compose their text from named byte
\ helpers instead of escaped literals. Two reasons. Escaped blobs are hard to
\ read and easy to get wrong, and the lint scans this file along with the rest
\ of the tree, so composing the text keeps every fixture's claim inside a plain
\ string body where the scan can see it is not code. The old escaped fixture on
\ line 57 of this file was itself the shape that fed the live ledger phantom
\ claims: a `\` written inside a string body.
\
\ Load after lib/test.f and tools/error-code-lint-core.f.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/memory.f
require lib/vector.f
require lib/fs.f
require tools/lint/text.f
require tools/lint/intern.f
require tools/lint/token.f
require tools/lint/source-lex.f
require tools/error-code-lint-core.f

package ERROR-CODE-LINT-TEST
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

\ ---- fixtures whose text carries a quote or a line break --------------------

\ `[char] " drop  -9001 constant E-XA  -9001 constant E-XB`
\ One bare quote used as a literal byte. It opens nothing, so both claims are
\ code and they collide.
: BARE-QUOTE$ ( -- ptr u8 n )
   FIX-RESET
   s" [char] " FIX+  Q+
   s"  drop  -9001 constant E-XA  -9001 constant E-XB" FIX+
   FIX$ ;

\ `s" \ -9001 constant E-XA "  -9001 constant E-XB  -9001 constant E-XC`
\ A `\` inside a string body is ordinary text, not a comment opener, so the
\ string still closes at its own quote and the two claims after it collide.
: BACKSLASH-IN-STRING$ ( -- ptr u8 n )
   FIX-RESET
   s" s" FIX+  Q+
   s"  \ -9001 constant E-XA " FIX+  Q+
   s"   -9001 constant E-XB  -9001 constant E-XC" FIX+
   FIX$ ;

\ The same shape spread over two lines, which is how it occurs in real source.
\ A scan that loses the closing quote reads the rest of the file as string body
\ and reports nothing at all.
: BACKSLASH-THEN-LINE$ ( -- ptr u8 n )
   FIX-RESET
   s" s" FIX+  Q+
   s"  \ -9001 constant E-XA " FIX+  Q+
   EOL+
   s" -9001 constant E-XB  -9001 constant E-XC" FIX+
   FIX$ ;

\ `-9001 constant E-XA  s" \ ignored "  -9001 constant E-XB`
\ A real claim before such a string and a real claim after it must both be
\ counted, so they collide.
: CLAIM-ACROSS-BACKSLASH$ ( -- ptr u8 n )
   FIX-RESET
   s" -9001 constant E-XA  s" FIX+  Q+
   s"  \ ignored " FIX+  Q+
   s"   -9001 constant E-XB" FIX+
   FIX$ ;

\ `-9001 constant E-XC  s\" -9001 constant E-XA \" -9001 constant E-XB "`
\ The escaped opener treats an escaped quote as literal text, so the body runs
\ to the third quote and neither inner claim is code. Only E-XC is, and nothing
\ collides. A scan that counted quotes would close the body early and report
\ E-XB as a second claimant of -9001.
: ESC-OPENER$ ( -- ptr u8 n )
   FIX-RESET
   s" -9001 constant E-XC  s\" FIX+  Q+
   s"  -9001 constant E-XA \" FIX+  Q+
   s"  -9001 constant E-XB " FIX+  Q+
   FIX$ ;

\ `( note " )  -9001 constant E-XA  -9001 constant E-XB`
\ An unpaired quote inside a paren comment is comment text. It changes nothing,
\ so the two claims after it still collide.
: QUOTE-IN-PAREN$ ( -- ptr u8 n )
   FIX-RESET
   s" ( note " FIX+  Q+
   s"  )  -9001 constant E-XA  -9001 constant E-XB" FIX+
   FIX$ ;

\ `\ -9001 constant E-XA` then `-9001 constant E-XB  -9001 constant E-XC`
\ The commented claim is not code; the two on the next line are, and collide.
\ Three findings would mean the line comment leaked into the scan.
: COMMENT-THEN-LINE$ ( -- ptr u8 n )
   FIX-RESET
   s" \ -9001 constant E-XA" FIX+
   EOL+
   s" -9001 constant E-XB  -9001 constant E-XC" FIX+
   FIX$ ;

\ `-9001 constant E-XB  s" -9001 constant "  E-XA`
\ A claim split across a string boundary: the number and `constant` sit inside
\ the body and the name outside. They must not be joined into a second claim.
: SPLIT-CLAIM$ ( -- ptr u8 n )
   FIX-RESET
   s" -9001 constant E-XB  s" FIX+  Q+
   s"  -9001 constant " FIX+  Q+
   s"   E-XA" FIX+
   FIX$ ;

\ `: BAD s" nope` - the literal opened here never closes, so the lexer stops the
\ scan at that quote and every claim after it in the source is unreadable.
: UNTERM$ ( -- ptr u8 n )
   FIX-RESET
   s" : BAD s" FIX+  Q+
   s"  nope" FIX+
   FIX$ ;

\ ---- groups -----------------------------------------------------------------

: DETECT ( -- )
   \ one code under two names is a finding; distinct codes are not
   s" -9001 constant E-XA  -9001 constant E-XB" ERROR-CODE-LINT:COUNT 1 T=
   s" -9001 constant E-XA  -9002 constant E-XB" ERROR-CODE-LINT:COUNT 0 T=
   \ hex and decimal literals claim the same numeric code
   s" -$10 constant E-HA  -16 constant E-HB" ERROR-CODE-LINT:COUNT 1 T=
   \ three claimants report every colliding pair
   s" -9001 constant E-XA  -9001 constant E-XB  -9001 constant E-XC" ERROR-CODE-LINT:COUNT 3 T= ;

: ALLOWANCES ( -- )
   \ exact (code, name) re-registration is a shim, not a collision
   s" -9001 constant E-XA  -9001 constant E-XA" ERROR-CODE-LINT:COUNT 0 T=
   \ positive values are sysexits-style process exit codes, shared by design
   s" 76 constant E-XA  76 constant E-XB" ERROR-CODE-LINT:COUNT 0 T=
   \ -FIRST/-LAST range sentinels alias member codes deliberately
   s" -9100 constant E-X-FIRST  -9100 constant E-XM" ERROR-CODE-LINT:COUNT 0 T=
   s" -9199 constant E-X-LAST  -9199 constant E-XZ" ERROR-CODE-LINT:COUNT 0 T= ;

: RANGES ( -- )
   \ FIRST/LAST reserve [FIRST,LAST]; a foreign file minting inside is flagged
   \ even though the owning block has not yet minted that exact member code
   s" -9100 constant E-FOO-FIRST  -9199 constant E-FOO-LAST"
   s" -9150 constant E-BAR" ERROR-CODE-LINT:COUNT2 1 T=
   \ the reservation boundaries are inclusive on both ends
   s" -9100 constant E-FOO-FIRST  -9199 constant E-FOO-LAST"
   s" -9100 constant E-BND" ERROR-CODE-LINT:COUNT2 1 T=
   s" -9100 constant E-FOO-FIRST  -9199 constant E-FOO-LAST"
   s" -9199 constant E-BND" ERROR-CODE-LINT:COUNT2 1 T=
   \ the owning file's own members inside its own block are not foreign
   s" -9100 constant E-FOO-FIRST  -9199 constant E-FOO-LAST  -9100 constant E-FOO-A  -9101 constant E-FOO-B"
   ERROR-CODE-LINT:COUNT 0 T=
   \ a foreign claim OUTSIDE the reserved range passes
   s" -9100 constant E-FOO-FIRST  -9199 constant E-FOO-LAST"
   s" -9250 constant E-BAZ" ERROR-CODE-LINT:COUNT2 0 T=
   \ an incomplete reservation (FIRST or LAST alone) reserves nothing
   s" -9100 constant E-FOO-FIRST"
   s" -9150 constant E-BAR" ERROR-CODE-LINT:COUNT2 0 T= ;

: NOT-CLAIMS ( -- )
   \ a whole line after `\` is a comment
   s" \ -9001 constant E-XA  -9001 constant E-XB" ERROR-CODE-LINT:COUNT 0 T=
   \ non-numeric values (constant aliases) and non-E- names are not claims
   s" E-XA constant E-XB  -9001 constant E-XB" ERROR-CODE-LINT:COUNT 0 T=
   s" -9001 constant XA  -9001 constant E-XB" ERROR-CODE-LINT:COUNT 0 T=
   \ a paren comment between a claim's own tokens is inert, not a separator
   s" -9001 constant ( note ) E-XA  -9001 constant E-XB" ERROR-CODE-LINT:COUNT 1 T= ;

\ The print-paren `.( ... )` prints its body while the source loads; that body is
\ text, never code, and cannot declare a constant. So only E-XB is a claim here
\ and there is nothing for it to collide with.
: PRINT-PAREN ( -- )
   s" .( -9001 constant E-XA )  -9001 constant E-XB" ERROR-CODE-LINT:COUNT 0 T= ;

\ String membership is a structural fact the shared lexer decides, not a quote
\ count. The blinding-quote fixture is paired with the plain form that must keep
\ the same verdict, so a scan that goes blind at the quote fails right here.
: STRINGS ( -- )
   BARE-QUOTE$ ERROR-CODE-LINT:COUNT 1 T=
   s" -9001 constant E-XA  -9001 constant E-XB" ERROR-CODE-LINT:COUNT 1 T=
   BACKSLASH-IN-STRING$ ERROR-CODE-LINT:COUNT 1 T=
   BACKSLASH-THEN-LINE$ ERROR-CODE-LINT:COUNT 1 T=
   CLAIM-ACROSS-BACKSLASH$ ERROR-CODE-LINT:COUNT 1 T= ;

: HOSTILE ( -- )
   ESC-OPENER$ ERROR-CODE-LINT:COUNT 0 T=
   QUOTE-IN-PAREN$ ERROR-CODE-LINT:COUNT 1 T=
   COMMENT-THEN-LINE$ ERROR-CODE-LINT:COUNT 1 T=
   SPLIT-CLAIM$ ERROR-CODE-LINT:COUNT 0 T= ;

\ A source the lexer cannot read to the end must stop the gate by name. Reporting
\ zero findings for the part it did read is the failure mode this whole change
\ exists to remove.
: FAIL-CLOSED ( -- )
   [: UNTERM$ ERROR-CODE-LINT:COUNT drop ;] ERROR-CODE-LINT:E-QUOTE TTHROWSQ ;

: LIVE ( -- )
   \ the real tree is clean: every negative E- code has exactly one owner
   ERROR-CODE-LINT:STRICT ;

: MAIN ( -- )
   T-RESET
   DETECT
   ALLOWANCES
   RANGES
   NOT-CLAIMS
   PRINT-PAREN
   STRINGS
   HOSTILE
   FAIL-CLOSED
   T-REPORT
   LIVE ;

MAIN

;package
