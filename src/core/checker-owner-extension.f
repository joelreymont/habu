\ Fields appended after the retained host's checker owner, and the byte
\ representation of an exact dictionary-record contract. This module loads
\ before the fresh checker owner is published; a retained host can require it
\ without reopening its sealed checker ABI package.

package CHECKER-OWNER-EXT
public

\ $3F0, $3F8 and $400 are retired and stay zero.
$408 constant RECORD-CONTRACT-OFF
\ $410 is retired and stays zero.
$418 constant RECORD-PUBLISHED-OFF
$420 constant GRID-BEFORE-OFF
$428 constant GRID-AFTER-OFF
$430 constant GRID-REVOKE-OFF
$438 constant GRID-RETIRE-OFF
$440 constant RECORD-RESTORE-OFF
\ $448, $450 and $458 are retired and stay zero.
$488 constant PUBLICATION-USE-OFF
\ $490 is retired and stays zero.
$498 constant RECORD-CAPTURE-OFF
$4A0 constant RECORD-SEED-OFF
$4A8 constant VERIFY-CTOR-DECL-OFF
$4B0 constant EXPORT-ACCEPT-OFF
\ Native multi-error recovery and diagnostics.
$4B8 constant MULTI-ERROR-OFF
$4C0 constant RESET-REPORT-OFF
$4C8 constant JSON-REPORTED-OFF
\ $4D0 is retired and stays zero.
$4D8 constant RECOVERY-USED-OFF
\ A does> clause scanned in its parent's unjudged regime (TRUSTED: or hookless).
$4E0 constant DOES-UNJUDGED-OFF
\ The report of a does> clause either compiler's check refused, named for the
\ definer it belongs to.
$4E8 constant NATIVE-DOES-REPORT-OFF
\ ( -- n ) the unit-export fact count (checker.f CHECKER-UNIT-FACTS).
$508 constant UNIT-FACTS-OFF
\ ( n -- n n n ) the checker tape's event at an ordinal (checker.f CHECKER-TAPE:EVENT).
$510 constant TAPE-EVENT-OFF
\ ( -- n ) the width-fact rows of the last scan (checker.f WF-N@).
$518 constant WIDTH-ROWS-OFF
\ ( n -- n n n ) width-fact row i: the token's offset in the scanned text, the
\ operand position and its cells (checker.f WF-FACT@).
$520 constant WIDTH-ROW-OFF
\ ( -- n ) the binds the last scan counted (checker.f LOCW-HW-N@).
$528 constant LOCAL-COUNT-OFF
\ ( -- ) closes the created body pending at a load boundary (checker.f
\ CHECKER-BODY-CLOSE).
$530 constant BODY-CLOSE-OFF
\ ( ptr u8 n n -- n bool ) whether a captured body still binds from a cut on
\ (checker.f CHECKER-PREFIX:BIND?); no engine code asks it.
$538 constant PREFIX-BIND-OFF
\ ( -- n ) each: an unrecorded certified effect's cells (W-EFFECT-NOT-RECORDED),
\ else CELLS-NONE (checker.f CHECK-UNRECORDED!).
$540 constant UNRECORDED-IN-OFF
$548 constant UNRECORDED-OUT-OFF
\ ( -- bool ) the checker counted a refusal of the scanned definition's row
\ (checker.f SIG-COUNTED).
$550 constant SIG-COUNTED-OFF
\ ( -- bool ) the last unjudged scan closed every construct it opened
\ (checker.f UNJUDGED-CLOSED?).
$558 constant UNJUDGED-CLOSED-OFF
\ ( -- n n bool ) the last scan's refused token as offsets in its text, equal
\ when none was pinned, and whether it is undefined (checker.f PIN-SPAN).
$560 constant PIN-SPAN-OFF
\ ( -- ptr u8 n ) that token's text, empty when none was pinned (checker.f
\ PIN-TOKEN).
$568 constant PIN-TOKEN-OFF
\ ( n -- bool ) whether a family is niche-null (type-family.f TFAM-NICHE?).
$570 constant FAMILY-NICHE-OFF
FAMILY-NICHE-OFF CELL + constant OWNER-BYTES

$524543434F4E5431 constant CONTRACT-MAGIC
10 cells constant MIN-BYTES
0 constant FIXED-NONE
1 constant FIXED-VAL
2 constant FIXED-ADDR

;package
