\ The checker owner's later fields. This module loads before the checker owner
\ is published.

package CHECKER-OWNER-EXT
public

$218 constant RECORD-PUBLISHED-OFF
$220 constant GRID-BEFORE-OFF
$228 constant GRID-AFTER-OFF
$230 constant GRID-REVOKE-OFF
$238 constant GRID-RETIRE-OFF
$240 constant PUBLICATION-USE-OFF
$248 constant VERIFY-CTOR-DECL-OFF
$250 constant EXPORT-ACCEPT-OFF
\ Native diagnostics.
$258 constant RESET-REPORT-OFF
$260 constant JSON-REPORTED-OFF
\ A does> clause scanned in its parent's unjudged regime (TRUSTED: or hookless).
$268 constant DOES-UNJUDGED-OFF
\ The report of a does> clause either compiler's check refused, named for the
\ definer it belongs to.
$270 constant NATIVE-DOES-REPORT-OFF
\ ( -- ) closes the created body pending at a load boundary (checker.f
\ CHECKER-BODY-CLOSE).
$278 constant BODY-CLOSE-OFF
\ ( -- bool ) the checker counted a refusal of the scanned definition's row
\ (checker.f SIG-COUNTED).
$280 constant SIG-COUNTED-OFF
\ ( -- bool ) the last unjudged scan closed every construct it opened
\ (checker.f UNJUDGED-CLOSED?).
$288 constant UNJUDGED-CLOSED-OFF
\ ( -- n n bool ) the last scan's refused token as offsets in its text, equal
\ when none was pinned, and whether it is undefined (checker.f PIN-SPAN).
$290 constant PIN-SPAN-OFF
\ ( -- ptr u8 n ) that token's text, empty when none was pinned (checker.f
\ PIN-TOKEN).
$298 constant PIN-TOKEN-OFF
PIN-TOKEN-OFF CELL + constant OWNER-BYTES

;package
