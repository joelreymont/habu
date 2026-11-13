\ The checker owner's later fields. This module loads before the checker owner
\ is published.

package CHECKER-OWNER-EXT
public

$1E8 constant RECORD-PUBLISHED-OFF
$1F0 constant GRID-BEFORE-OFF
$1F8 constant GRID-AFTER-OFF
$200 constant GRID-REVOKE-OFF
$208 constant GRID-RETIRE-OFF
$210 constant PUBLICATION-USE-OFF
$218 constant VERIFY-CTOR-DECL-OFF
$220 constant EXPORT-ACCEPT-OFF
\ Native diagnostics.
$228 constant RESET-REPORT-OFF
$230 constant JSON-REPORTED-OFF
\ A does> clause scanned in its parent's unjudged regime (TRUSTED: or hookless).
$238 constant DOES-UNJUDGED-OFF
\ The report of a does> clause either compiler's check refused, named for the
\ definer it belongs to.
$240 constant NATIVE-DOES-REPORT-OFF
\ ( -- ) closes the created body pending at a load boundary (checker.f
\ CHECKER-BODY-CLOSE).
$248 constant BODY-CLOSE-OFF
\ ( -- bool ) the checker counted a refusal of the scanned definition's row
\ (checker.f SIG-COUNTED).
$250 constant SIG-COUNTED-OFF
\ ( -- bool ) the last unjudged scan closed every construct it opened
\ (checker.f UNJUDGED-CLOSED?).
$258 constant UNJUDGED-CLOSED-OFF
\ ( -- n n bool ) the last scan's refused token as offsets in its text, equal
\ when none was pinned, and whether it is undefined (checker.f PIN-SPAN).
$260 constant PIN-SPAN-OFF
\ ( -- ptr u8 n ) that token's text, empty when none was pinned (checker.f
\ PIN-TOKEN).
$268 constant PIN-TOKEN-OFF
PIN-TOKEN-OFF CELL + constant OWNER-BYTES

;package
