\ Physical VM stack extents, shared by source-loaded and recovered emitters.
\ The live data-stack allocation changes at run-in-stack/task entry and unwind.
package STACK-ABI
public

$1D0 constant BASE-CELL
$47E8 constant CAP-CELL
$47F0 constant REPL-BASE-CELL
$47F8 constant REPL-CAP-CELL
$4000 constant BOOT-BYTES

$2800 constant RETURN-OFF
$3000 constant RETURN-END
RETURN-END RETURN-OFF - 8 / constant RETURN-CELLS

$600 constant LOOP-OFF
$800 constant LOOP-END
16 constant LOOP-FRAME-BYTES
LOOP-END LOOP-OFF - LOOP-FRAME-BYTES / constant LOOP-FRAMES

\ Preserve the old frame fields; append the active allocation descriptor.
$40 constant CATCH-BASE
$48 constant CATCH-CAP
$50 constant CATCH-BYTES
$CA7CF4A3E00E constant CATCH-MAGIC
$80 constant EVAL-BASE
$88 constant EVAL-CAP
$90 constant EVAL-BYTES

;package
