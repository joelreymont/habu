\ Fields appended after the retained host's checker owner, and the byte
\ representation of an exact dictionary-record contract. This module loads
\ before the fresh checker owner is published; a retained host can require it
\ without reopening its sealed checker ABI package.

package CHECKER-OWNER-EXT
public

$410 constant UNIT-EXPORT-REFS-OFF
$418 constant UNIT-PREPARE-REFS-OFF
$420 constant UNIT-PERSIST-STORES-OFF
$428 constant RECORD-CONTRACT-OFF
$430 constant UNIT-STORAGE-CELLS-OFF
$438 constant RECORD-PUBLISHED-OFF
$440 constant GRID-BEFORE-OFF
$448 constant GRID-AFTER-OFF
$450 constant GRID-REVOKE-OFF
$458 constant GRID-RETIRE-OFF
$460 constant RECORD-RESTORE-OFF
$468 constant UNIT-SCRATCH-RELEASE-OFF
\ $470 is retired and stays zero so later offsets keep their positions.
$478 constant ORIGIN-SYM-OFF
$4A8 constant UNIT-SYM-OFF
$4B0 constant PRIMITIVE-STATE-OFF
$4B8 constant RECORD-FIXED-READ-OFF
$4C0 constant SOURCE-HANDLES-OFF
$4C8 constant SOURCE-LINK-OFF
$4D0 constant PENDING-MARK-OFF
$4D8 constant PENDING-EXPORT-REFS-OFF
$4E0 constant PENDING-PREPARE-OFF
$4E8 constant PENDING-INSTALL-OFF
$4F0 constant PENDING-DISCARD-OFF
$4F8 constant PUBLICATION-USE-OFF
$500 constant PENDING-IDLE-OFF
$508 constant RECORD-CAPTURE-OFF
$510 constant RECORD-SEED-OFF
$518 constant VERIFY-CTOR-DECL-OFF
$520 constant EXPORT-ACCEPT-OFF
\ Native multi-error recovery and diagnostics.
$528 constant MULTI-ERROR-OFF
$530 constant RESET-REPORT-OFF
$538 constant JSON-REPORTED-OFF
\ $540 held the using refusal's spelling and has no reader. A host's baked
\ compiler asks the live checker's record at the offsets it was built with, so
\ the cell stays reserved and RECOVERY-USED-OFF keeps $548.
$548 constant RECOVERY-USED-OFF
\ A does> clause scanned in its parent's unjudged regime (TRUSTED: or hookless).
$550 constant DOES-UNJUDGED-OFF
\ The report of a does> clause either compiler's check refused, named for the
\ definer it belongs to.
$558 constant NATIVE-DOES-REPORT-OFF
\ The lowering certificate the owner's last scan produced, the keep of a does>
\ clause's certificate while its definer's head is scanned, and the kept one.
\ Tier 0 freezes them from the owner that scanned (habu2.f LOWER-TXN FREEZE).
$560 constant CERT-BYTES-OFF
$568 constant CERT-KEEP-OFF
$570 constant CERT-KEPT-OFF
\ ( -- n ) the unit-export fact count (checker.f CHECKER-UNIT-FACTS).
$578 constant UNIT-FACTS-OFF
\ ( n -- n n n ) the checker tape's event at an ordinal (checker.f CHECKER-TAPE:EVENT).
$580 constant TAPE-EVENT-OFF
\ ( -- n ) the width-fact rows of the last scan (checker.f WF-N@).
$588 constant WIDTH-ROWS-OFF
\ ( n -- n n n ) width-fact row i: the token's offset in the scanned text, the
\ operand position and its cells (checker.f WF-FACT@).
$590 constant WIDTH-ROW-OFF
\ ( -- n ) the binds the last scan counted (checker.f LOCW-HW-N@).
$598 constant LOCAL-COUNT-OFF
\ ( -- ) closes the created body pending at a load boundary (checker.f
\ CHECKER-BODY-CLOSE).
$5A0 constant BODY-CLOSE-OFF
\ ( ptr u8 n n -- n bool ) whether a captured body still binds from a cut on
\ (checker.f CHECKER-PREFIX:BIND?), asked by habu2.f EM-CAPTURE-BIND.
$5A8 constant PREFIX-BIND-OFF
\ ( -- n ) each: an unrecorded certified effect's cells (W-EFFECT-NOT-RECORDED),
\ else CELLS-NONE (checker.f CHECK-UNRECORDED!).
$5B0 constant UNRECORDED-IN-OFF
$5B8 constant UNRECORDED-OUT-OFF
\ ( -- bool ) the checker counted a refusal of the scanned definition's row
\ (checker.f SIG-COUNTED).
$5C0 constant SIG-COUNTED-OFF
SIG-COUNTED-OFF CELL + constant OWNER-BYTES

$524543434F4E5431 constant CONTRACT-MAGIC
5 cells constant FIXED-KIND-OFF
6 cells constant FIXED-VALUE-OFF
10 cells constant MIN-BYTES
0 constant FIXED-NONE
1 constant FIXED-VAL
2 constant FIXED-ADDR

;package
