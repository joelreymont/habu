\ The checker owner's later fields, and the byte representation of an exact
\ dictionary-record contract. This module loads before the checker owner is
\ published.

package CHECKER-OWNER-EXT
public

$350 constant RECORD-CONTRACT-OFF
$358 constant RECORD-PUBLISHED-OFF
$360 constant GRID-BEFORE-OFF
$368 constant GRID-AFTER-OFF
$370 constant GRID-REVOKE-OFF
$378 constant GRID-RETIRE-OFF
$380 constant PUBLICATION-USE-OFF
$388 constant VERIFY-CTOR-DECL-OFF
$390 constant EXPORT-ACCEPT-OFF
\ Native multi-error recovery and diagnostics.
$398 constant MULTI-ERROR-OFF
$3A0 constant RESET-REPORT-OFF
$3A8 constant JSON-REPORTED-OFF
$3B0 constant RECOVERY-USED-OFF
\ A does> clause scanned in its parent's unjudged regime (TRUSTED: or hookless).
$3B8 constant DOES-UNJUDGED-OFF
\ The report of a does> clause either compiler's check refused, named for the
\ definer it belongs to.
$3C0 constant NATIVE-DOES-REPORT-OFF
\ ( n -- n n n ) the checker tape's event at an ordinal (checker.f CHECKER-TAPE:EVENT).
$3C8 constant TAPE-EVENT-OFF
\ ( -- n ) the width-fact rows of the last scan (checker.f WF-N@).
$3D0 constant WIDTH-ROWS-OFF
\ ( n -- n n n ) width-fact row i: the token's offset in the scanned text, the
\ operand position and its cells (checker.f WF-FACT@).
$3D8 constant WIDTH-ROW-OFF
\ ( -- n ) the binds the last scan counted (checker.f LOCW-HW-N@).
$3E0 constant LOCAL-COUNT-OFF
\ ( -- ) closes the created body pending at a load boundary (checker.f
\ CHECKER-BODY-CLOSE).
$3E8 constant BODY-CLOSE-OFF
\ ( -- n ) each: an unrecorded certified effect's cells (W-EFFECT-NOT-RECORDED),
\ else CELLS-NONE (checker.f CHECK-UNRECORDED!).
$3F0 constant UNRECORDED-IN-OFF
$3F8 constant UNRECORDED-OUT-OFF
\ ( -- bool ) the checker counted a refusal of the scanned definition's row
\ (checker.f SIG-COUNTED).
$400 constant SIG-COUNTED-OFF
\ ( -- bool ) the last unjudged scan closed every construct it opened
\ (checker.f UNJUDGED-CLOSED?).
$408 constant UNJUDGED-CLOSED-OFF
\ ( -- n n bool ) the last scan's refused token as offsets in its text, equal
\ when none was pinned, and whether it is undefined (checker.f PIN-SPAN).
$410 constant PIN-SPAN-OFF
\ ( -- ptr u8 n ) that token's text, empty when none was pinned (checker.f
\ PIN-TOKEN).
$418 constant PIN-TOKEN-OFF
\ ( n -- bool ) whether a family is niche-null (type-family.f TFAM-NICHE?).
$420 constant FAMILY-NICHE-OFF
FAMILY-NICHE-OFF CELL + constant OWNER-BYTES

$524543434F4E5431 constant CONTRACT-MAGIC
10 cells constant MIN-BYTES
0 constant FIXED-NONE
1 constant FIXED-VAL
2 constant FIXED-ADDR

;package
