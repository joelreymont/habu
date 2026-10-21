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
$470 constant UNIT-STATE-SPANS-OFF
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
\ Native multi-error recovery and diagnostics. JSON-REPORTED, REFUSAL-TOKEN and
\ RECOVERY-USED stay adjacent: a compiler built before the last two reads them
\ at fixed cells after JSON-REPORTED (src/compiler/native/checker-owner.f).
$528 constant MULTI-ERROR-OFF
$530 constant RESET-REPORT-OFF
$538 constant JSON-REPORTED-OFF
$540 constant REFUSAL-TOKEN-OFF
$548 constant RECOVERY-USED-OFF
RECOVERY-USED-OFF CELL + constant OWNER-BYTES

$524543434F4E5431 constant CONTRACT-MAGIC
5 cells constant FIXED-KIND-OFF
6 cells constant FIXED-VALUE-OFF
10 cells constant MIN-BYTES
0 constant FIXED-NONE
1 constant FIXED-VAL
2 constant FIXED-ADDR

;package
