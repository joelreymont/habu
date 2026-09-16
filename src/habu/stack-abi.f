\ Physical VM stack extents, shared by source-loaded and recovered emitters.
\ The live data-stack allocation changes at run-in-stack/task entry and unwind.
\
\ EVERY VM STACK IS A GUARDED MAPPING, never a band inside the DATA header.
\ A stack of CAP bytes occupies [base - PAGE-BYTES, base + CAP + PAGE-BYTES):
\ one inaccessible page below it, the capacity itself, one inaccessible page
\ above it. A push past the capacity or a read below the base therefore FAULTS,
\ and src/habu/crash.f turns that fault into `hb: stack bounds exceeded (<which>)`
\ with the ENGINE-ERROR:STACK-BOUNDS exit. That is what replaced the
\ per-transfer bounds check the engine used to run at every push and pop: the
\ capacity is enforced by the MMU, so compiled code carries no guard at all.
\
\ THE MAPPED CAPACITY IS THE DECLARED CAPACITY. RETURN-BYTES/LOOP-BYTES are the
\ mapping sizes and RETURN-CELLS/LOOP-FRAMES are those same extents counted in
\ slots: if the two ever disagreed, the guard page would sit past the declared
\ bound and an overflow would run on into ordinary memory before faulting.
package STACK-ABI
public

$1D0 constant BASE-CELL
$47E8 constant CAP-CELL
$47F0 constant REPL-BASE-CELL
$47F8 constant REPL-CAP-CELL

\ The guard granule. It is the MAXIMUM arm64 page a supported target can boot
\ with (64 KiB), not this host's 16 KiB: a guard smaller than the running page
\ size is not a separate page at all and the protection silently disappears,
\ while a guard larger than it is still a whole number of real pages. layout.f's
\ PROT-PAGE-MAX is this same number under the name the protection window uses.
$10000 constant PAGE-BYTES

\ Boot data stack: one guarded page of cells (8192).
PAGE-BYTES constant BOOT-BYTES

\ User return stack and DO/LOOP frame stack. Both moved out of the DATA header
\ when they gained guard pages -- a $8000 header has no room for an inaccessible
\ page -- so each is its own mapping whose base lives in a header cell. The cells
\ are PER TASK, because DATA is per task (habu1.f task entry loads x20 from the
\ TCB), and so are the mappings: lib/task.f PREPARE maps a task's own pair.
$4800 constant RETURN-BASE-CELL
$4808 constant LOOP-BASE-CELL

PAGE-BYTES constant RETURN-BYTES
RETURN-BYTES 8 / constant RETURN-CELLS
16 constant LOOP-FRAME-BYTES
PAGE-BYTES constant LOOP-BYTES
LOOP-BYTES LOOP-FRAME-BYTES / constant LOOP-FRAMES

\ run-in-stack's refusal code. lib/errors.f owns it as E-STACK-UNGUARDED; the
\ engine emitters (src/habu/habu1.f BRUNSTACK) and the Gforth recovery mirror
\ compile before any lib/ file exists, so the same (code, name) pair is
\ re-registered here -- the one form tools/error-code-lint.f admits -- and
\ test/stack-guard.f keeps the two spellings equal.
-3802 constant E-STACK-UNGUARDED

\ Preserve the old frame fields; append the active allocation descriptor.
$40 constant CATCH-BASE
$48 constant CATCH-CAP
$50 constant CATCH-BYTES
$CA7CF4A3E00E constant CATCH-MAGIC
$80 constant EVAL-BASE
$88 constant EVAL-CAP
$90 constant EVAL-BYTES

;package
