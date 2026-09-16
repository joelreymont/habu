\ The canary values test/snapshot-writer-poison.f plants in the live return
\ stack and test/snapshot-writer.f proves absent from the persisted image.
package SNAP-WRITER-POISON
public
$5253544B4C4F0001 constant LO-CANARY   \ "RSTKLO" + 1
$5253544B48490002 constant HI-CANARY   \ "RSTKHI" + 2
;package
