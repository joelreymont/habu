\ stack-abi.f - physical VM stack extents, shared by source-loaded and recovered
\ emitters. Every VM stack is a guarded mapping [base - PAGE-BYTES, base + CAP +
\ PAGE-BYTES), so compiled code carries no bounds check: an overflow or underflow
\ faults and src/habu/crash.f reports `hb: stack bounds exceeded (<which>)`, except
\ a data-stack access below the base, thrown as the underdepth reject (crash.f ONE
\ FAULT IS NOT AN EXIT).
package STACK-ABI
public

\ The live data-stack allocation changes at run-in-stack or task entry and at unwind.
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

\ User return stack and DO/LOOP frame stack: each its own mapping with its base in
\ a header cell, since a $8000 header has no room for a guard page. Cells and
\ mappings are per task, as DATA is (habu1.f task entry loads x20 from the TCB;
\ src/lib/task.f PREPARE maps a task's own pair).
$4800 constant RETURN-BASE-CELL
$4808 constant LOOP-BASE-CELL

\ The mapped capacity is the declared capacity: RETURN-CELLS and LOOP-FRAMES count
\ the mapping sizes in slots, or the guard page would sit past the declared bound
\ and an overflow would run into ordinary memory before faulting.
PAGE-BYTES constant RETURN-BYTES
RETURN-BYTES 8 / constant RETURN-CELLS
16 constant LOOP-FRAME-BYTES
PAGE-BYTES constant LOOP-BYTES
LOOP-BYTES LOOP-FRAME-BYTES / constant LOOP-FRAMES

\ run-in-stack's refusal code. src/lib/errors.f owns it as E-STACK-UNGUARDED; the
\ engine emitters (src/habu/habu1.f BRUNSTACK) and the Gforth recovery mirror
\ compile before any src/lib/ file exists, so the same (code, name) pair is
\ re-registered here -- the one form tools/error-code-lint.f admits -- and
\ test/stack-guard.f keeps the two spellings equal.
-3802 constant E-STACK-UNGUARDED

\ evaluate-closed's refusal of a text that left cells above its floor
\ (src/habu/habu1.f B-EVAL-CLOSED), re-registered for the same reason:
\ src/lib/errors.f owns it as E-EVAL-RESIDUE and test/compiler/native-eval.f keeps
\ the two spellings equal.
-3804 constant E-EVAL-RESIDUE

\ The active allocation descriptor follows the frame's existing fields.
$40 constant CATCH-BASE
$48 constant CATCH-CAP
$50 constant CATCH-BYTES
$CA7CF4A3E00E constant CATCH-MAGIC
$80 constant EVAL-BASE
$88 constant EVAL-CAP
\ $90..$128 are layout.f's: EVAL-INB, EVAL-FRAME:USE-FLOOR, the USE-MAX cells
\ of EVAL-FRAME:USE-WIDS and EVAL-FRAME:PEND at $120. $128 is held for the
\ frame's saved next wordlist id (WIDN).
\ The data stack the frame owns: evaluate-closed's pooled stack, which the clean
\ return and the throw recovery give back to the pool; 0 for evaluate. $138 is
\ layout.f's EVAL-FRAME:ROWS, the checker's record end as the buffer began, and
\ $140..$158 its source-observer cells (EVAL-FRAME:SX-CTX, SX-TICKET, SX-SRC);
\ $158 pads the frame to EVAL-BYTES, a multiple of 16 for the machine stack.
$130 constant EVAL-SEG
$160 constant EVAL-BYTES

\ THE MACHINE STACK IS THE OS THREAD'S OWN, so its guard is the kernel's and its
\ overflow faults with sp at or in that guard, where the kernel cannot build a
\ signal frame. Every thread that runs Habu code therefore carves SIGNAL-BYTES
\ off its machine stack just below its entry's frame and registers them with
\ sigaltstack before it runs any (src/habu/crash.f C-SIGNAL-STACK, its x86-64
\ twin src/habu/boot-x64.f SIGNAL-STACK,): the crash handler is installed
\ SA_ONSTACK and runs there. A fault whose address lies within MACHINE-REACH
\ of the interrupted sp, either side, is that overflow: the faulting access
\ lies within one frame of sp, below it when the access itself moves sp (a
\ push, a pre-indexed store) and above it when sp moved first. A frame wider
\ than MACHINE-REACH that faults farther from sp keeps the register dump.
$10000 constant SIGNAL-BYTES
$10000 constant MACHINE-REACH

;package
