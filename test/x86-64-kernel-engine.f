\ x86-64-kernel-engine.f - the engine-state rows of the x86-64 kernel
\ (src/habu/kernel-x64.f) in the booted harness, cross-built for an x86-64
\ peer.
require src/core/exit-status.f
require src/habu/address-cells.f
require test/x86-64-boot-harness.f
require src/os/linux/layout.f
require src/habu/kernel-x64.f
require src/lib/test/assert.f
require test/x86-64-peer-harness.f
require src/arch/x86-64/asm.f
require src/arch/x86-64/icode.f

package X64K-ENGINE
using LINUX-LAYOUT   \ the guard

PROT-PAGE-MAX constant FILL-BYTES
create FILL FILL-BYTES allot

\ TASK-LIVE-GUARD, then an empty data stack. With no task live, as the boot
\ leaves TASKS-LIVE-CELL, the guard passes; -armed marks a task live first.
: BUILD ( bool bool str -- ) {: negative:bool armed:bool path:str :}
   negative X64HARNESS:BOOT-OPEN,
   armed if
      EXIT-STATUS:TASK-COMPILE X64HARNESS:EXPECT-EXIT
      1 TASKS-LIVE-CELL X64HARNESS:CELL!,
   then
   X64KERNEL:TASK-LIVE-GUARD,
   0 X64HARNESS:EXPECT-DEPTH,
   path X64HARNESS:BOOT-CLOSE, ;

\ Text past one PROT-PAGE-MAX behind the kernel, as the whole kernel will carry,
\ so the read-write segment lands a page further on; it pushes the text's span.
: BUILD-WINDOW ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   FILL FILL-BYTES >str X64HARNESS:PUSH-TEXT,
   2 X64HARNESS:EXPECT-DEPTH,
   FILL-BYTES X64HARNESS:EXPECT-POP,
   path X64HARNESS:BOOT-CLOSE, ;

\ The search images seed five records with X64HARNESS:RECORD. Without an index
\ `bare-search-wl` and `xref-search-wl` scan the one wordlist; with HIDX-BUILD first
\ they probe.
7 constant OTHER-WID                   \ a public wordlist past the fixed ones

\ Longer than DNAME-INL, so its record points at its bytes.
: LONG$ ( -- str ) s" A-Name-Longer-Than-Sixteen" ;
: LONG-UPPER$ ( -- str ) s" A-NAME-LONGER-THAN-SIXTEEN" ;

\ Records 0-4, the code cells 1-5. The names mix case, so a compare that folds
\ one side only misses them.
: SEED, ( -- )
   s" Alpha" 0 0 X64HARNESS:RECORD,
   s" ALPHA" OTHER-WID 0 X64HARNESS:RECORD,
   LONG$ 0 0 X64HARNESS:RECORD,
   s" secret" OWNER-API-PRI-WID 0 X64HARNESS:RECORD,
   s" hidden" 0 DNAME-INT X64HARNESS:RECORD, ;

\ Push a name and a wid and call the row.
: ASK, ( str n str -- ) {: s:str wid:n row:str :}
   s X64HARNESS:PUSH-TEXT,
   wid X64HARNESS:PUSH,
   row X64HARNESS:CALL-ROW, ;

: SEARCH, ( str n -- ) s" bare-search-wl" ASK, ;
: XREF, ( str n -- ) s" xref-search-wl" ASK, ;

\ Ten checks, the whole of an image's case statuses.
: SEARCHES, ( -- )
   s" aLPHA" 0 SEARCH,  1 X64HARNESS:EXPECT-POP,         \ folded
   s" alpha" OTHER-WID SEARCH,  2 X64HARNESS:EXPECT-POP,
   LONG-UPPER$ 0 SEARCH,  3 X64HARNESS:EXPECT-POP,
   s" gamma" 0 SEARCH,  0 X64HARNESS:EXPECT-POP,         \ absent
   s" secret" OWNER-API-PRI-WID SEARCH,  0 X64HARNESS:EXPECT-POP,
   s" secret" OWNER-API-PRI-WID XREF,  3 X64HARNESS:EXPECT-ROW,
   s" hidden" 0 SEARCH,  0 X64HARNESS:EXPECT-POP,        \ DNAME-INT
   s" hidden" 0 XREF,  4 X64HARNESS:EXPECT-ROW,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED, ;

: BUILD-SEARCH ( bool str -- ) {: indexed:bool path:str :}
   false X64HARNESS:BOOT-OPEN,
   SEED,
   indexed if X64KERNEL:HIDX-BUILD, then
   SEARCHES,
   path X64HARNESS:BOOT-CLOSE, ;

\ eccwbv and hktfoj are names of one length whose keys share a first slot under
\ wid 0, so the insert and the probe for hktfoj walk the chain past eccwbv's
\ record. After the build, a record the index is not told of stays absent until
\ HIDX-ADD, and another until HIDX-REBUILD, and one under WL:RETIRED is
\ found by the scan the probe leaves that wid to.
: BUILD-UPKEEP ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   SEED,
   s" eccwbv" 0 0 X64HARNESS:RECORD,                     \ record 5
   s" hktfoj" 0 0 X64HARNESS:RECORD,                     \ record 6
   X64KERNEL:HIDX-BUILD,
   s" hktfoj" 0 SEARCH,  7 X64HARNESS:EXPECT-POP,
   s" eccwbv" 0 SEARCH,  6 X64HARNESS:EXPECT-POP,
   s" delta" 0 0 X64HARNESS:RECORD,                      \ record 7
   s" delta" 0 SEARCH,  0 X64HARNESS:EXPECT-POP,
   X64KERNEL:HIDX-ADD,
   s" delta" 0 SEARCH,  8 X64HARNESS:EXPECT-POP,
   s" epsilon" 0 0 X64HARNESS:RECORD,                    \ record 8
   s" epsilon" 0 SEARCH,  0 X64HARNESS:EXPECT-POP,
   X64KERNEL:HIDX-REBUILD,
   s" epsilon" 0 SEARCH,  9 X64HARNESS:EXPECT-POP,
   s" omega" WL:RETIRED 0 X64HARNESS:RECORD,        \ record 9
   s" omega" WL:RETIRED SEARCH,  10 X64HARNESS:EXPECT-POP,
   s" alpha" 0 SEARCH,  1 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path X64HARNESS:BOOT-CLOSE, ;

$1122334455667788 constant CELL-VALUE

: ROW ( str -- ) X64HARNESS:CALL-ROW, ;

: ALLOT, ( n -- ) X64HARNESS:PUSH,  s" allot" ROW ;

\ Store the DATA address of an offset into a DATA cell.
: DATA-CELL!, ( n n -- ) {: at:n off:n :}
   at X64HARNESS:PUSH-DATA,  off X64HARNESS:PUSH-DATA,  s" !" ROW ;

\ The heap bounds, which the linker writes in a linked image.
: HEAP-BOUNDS, ( -- )
   DATA-START BOOT:DP-FLOOR-CELL DATA-CELL!,
   X64KERNEL:DP-CEILING BOOT:DP-CEILING-CELL DATA-CELL!, ;

\ DP starts at the heap floor, DATA-START. Three bytes and an align reach the
\ next cell; `,` fills it and c, stores the low byte of its cell alone. The walk
\ returns to the floor.
: BUILD-HEAP ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   HEAP-BOUNDS,
   3 ALLOT,  s" align" ROW
   CELL-VALUE X64HARNESS:PUSH,  s" ," ROW
   $1AB X64HARNESS:PUSH,  s" c," ROW
   s" align" ROW  s" align" ROW
   s" here" ROW  DATA-START 24 + X64HARNESS:EXPECT-POP-DATA,
   0 DATA-START X64HARNESS:EXPECT-CELL,
   CELL-VALUE DATA-START 8 + X64HARNESS:EXPECT-CELL,
   $AB DATA-START 16 + X64HARNESS:EXPECT-CELL,
   -24 ALLOT,
   s" here" ROW  DATA-START X64HARNESS:EXPECT-POP-DATA,
   0 X64HARNESS:EXPECT-DEPTH,
   path X64HARNESS:BOOT-CLOSE, ;

\ DP may reach the ceiling; one byte past it throws E-DATA-SPACE, which nothing
\ catches or reports: fd 2 is empty.
: BUILD-HEAP-HIGH ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   EXIT-STATUS:UNCAUGHT X64HARNESS:EXPECT-EXIT
   HEAP-BOUNDS,
   X64KERNEL:DP-CEILING DATA-START - ALLOT,
   s" here" ROW  X64KERNEL:DP-CEILING X64HARNESS:EXPECT-POP-DATA,
   1 ALLOT,
   path X64HARNESS:BOOT-CLOSE, ;

\ One byte below the floor throws E-DATA-SPACE: fd 2 is empty.
: BUILD-HEAP-LOW ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   EXIT-STATUS:UNCAUGHT X64HARNESS:EXPECT-EXIT
   HEAP-BOUNDS,
   -1 ALLOT,
   path X64HARNESS:BOOT-CLOSE, ;

\ `,` on a task's thread.
: BUILD-HEAP-ARMED ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   EXIT-STATUS:TASK-COMPILE X64HARNESS:EXPECT-EXIT
   1 TASK-TCB-CELL X64HARNESS:CELL!,
   1 X64HARNESS:PUSH,  s" ," ROW
   path X64HARNESS:BOOT-CLOSE, ;

$8000000000000000 constant MIN-CELL
$7FFFFFFFFFFFFFFF constant MAX-CELL

: DOT, ( n -- ) X64HARNESS:PUSH,  s" ." ROW ;
: EMIT, ( n -- ) X64HARNESS:PUSH,  s" emit" ROW ;

\ fd 1: the four cells ., -1 and 0 u., "A B", "hello" and .s of 7 -3, one per
\ line, the fixed bytes the ARM64 engine writes for the same values; .s leaves
\ the cells, and depth counts them.
: BUILD-PRINT ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   MIN-CELL DOT,  MAX-CELL DOT,  0 DOT,  -1 DOT,
   -1 X64HARNESS:PUSH,  s" u." ROW
   0 X64HARNESS:PUSH,  s" u." ROW
   [char] A EMIT,  s" space" ROW  [char] B EMIT,  s" cr" ROW
   s" hello" X64HARNESS:PUSH-TEXT,  s" type" ROW  s" cr" ROW
   7 X64HARNESS:PUSH,  -3 X64HARNESS:PUSH,  s" .s" ROW
   s" depth" ROW  2 X64HARNESS:EXPECT-POP,
   -3 X64HARNESS:EXPECT-POP,  7 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   path X64HARNESS:BOOT-CLOSE, ;

: TYPE, ( str -- ) X64HARNESS:PUSH-TEXT,  s" type" ROW ;
: OUT! ( n -- ) GENIO-ABI:OUT-CELL X64HARNESS:CELL!, ;

\ Device 1's write routine: `[`, the span it is handed, `]`. Its own output
\ runs while the funnel is busy, so it reaches fd 1 and not the routine again.
: BRACKET, ( -- label )
   [: [char] [ EMIT,  s" type" ROW  [char] ] EMIT, ;] X64HARNESS:ROUTINE, ;

\ fd 1: `[hi][42\n][Z]` through device 1, the funnel idle again and the
\ caller's active device back afterwards; then `e` for a device with no row,
\ `x` for an index past DEVICES and `b` while the funnel is busy.
: BUILD-GENIO ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   BRACKET, GENIO-ABI:WRITE-OFF X64HARNESS:LABEL-CELL!,
   5 GENIO-ABI:ACTIVE-CELL X64HARNESS:CELL!,
   1 OUT!
   s" hi" TYPE,  42 DOT,  [char] Z EMIT,
   0 GENIO-ABI:BUSY-CELL X64HARNESS:EXPECT-CELL,
   5 GENIO-ABI:ACTIVE-CELL X64HARNESS:EXPECT-CELL,
   2 OUT!  s" e" TYPE,
   GENIO-ABI:DEVICES 1+ OUT!  s" x" TYPE,
   1 OUT!  1 GENIO-ABI:BUSY-CELL X64HARNESS:CELL!,  s" b" TYPE,
   0 X64HARNESS:EXPECT-DEPTH,
   path X64HARNESS:BOOT-CLOSE, ;

: CHECK-HOOK! ( n -- ) X64HARNESS:PUSH-REGION,  s" set-check" ROW ;
: PREFLIGHT! ( n -- ) X64HARNESS:PUSH-REGION,  s" set-preflight" ROW ;

\ The code window is [DBASE, CP), and the boot's CP is DICT-SIZE past DBASE.
\ The preflight hook installs once; the same xt again is inert, and
\ `0 set-check` suspends the armed hook with its preflight and empties both,
\ so another preflight installs. `0 set-check` while off keeps the parked
\ pair (the inner disarm of a nested window); re-arming the parked hook
\ resumes its preflight, and any other hook starts without one.
: BUILD-HOOKS ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   0 CHECK-HOOK!
   s" check@" ROW  0 X64HARNESS:EXPECT-POP-REGION,
   64 PREFLIGHT!  64 PREFLIGHT!
   0 X64HARNESS:PUSH,  s" set-check" ROW
   s" check@" ROW  0 X64HARNESS:EXPECT-POP,
   128 PREFLIGHT!
   0 X64HARNESS:PUSH,  s" set-check" ROW
   0 CHECK-HOOK!
   COMPILE-PREFLIGHT-CELL X64HARNESS:PUSH-DATA-CELL,  64 X64HARNESS:EXPECT-POP-REGION,
   0 X64HARNESS:PUSH,  s" set-check" ROW
   DICT-SIZE 1- CHECK-HOOK!
   0 COMPILE-PREFLIGHT-CELL X64HARNESS:EXPECT-CELL,
   0 X64HARNESS:EXPECT-DEPTH,
   path X64HARNESS:BOOT-CLOSE, ;

\ Each of these ends in one hook row's fd-2 rejection.
: BUILD-HOOKS-CHECK ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   EXIT-STATUS:REJECTED X64HARNESS:EXPECT-EXIT
   DICT-SIZE CHECK-HOOK!
   path X64HARNESS:BOOT-CLOSE, ;

: BUILD-HOOKS-REPLACED ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   EXIT-STATUS:REJECTED X64HARNESS:EXPECT-EXIT
   64 PREFLIGHT!  128 PREFLIGHT!
   path X64HARNESS:BOOT-CLOSE, ;

: BUILD-HOOKS-EMPTY ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   EXIT-STATUS:REJECTED X64HARNESS:EXPECT-EXIT
   0 X64HARNESS:PUSH,  s" set-preflight" ROW
   path X64HARNESS:BOOT-CLOSE, ;

40 constant FIRST-WID                  \ WIDN-CELL before the first wordlist

\ The getters answer what the boot published, and wordlist counts WIDN-CELL.
: BUILD-STATE ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   s" cp@" ROW  DICT-SIZE X64HARNESS:EXPECT-POP-REGION,
   s" dbase@" ROW  0 X64HARNESS:EXPECT-POP-REGION,
   s" data-base" ROW  0 X64HARNESS:EXPECT-POP-DATA,
   s" rbase" ROW  LINUX-LAYOUT:CODE-OFF REGION-OFF - X64HARNESS:EXPECT-POP-REGION,
   s" ndict@" ROW  0 X64HARNESS:EXPECT-POP,
   FIRST-WID WIDN-CELL X64HARNESS:CELL!,
   s" wordlist" ROW  s" wordlist" ROW
   FIRST-WID 1+ X64HARNESS:EXPECT-POP,  FIRST-WID X64HARNESS:EXPECT-POP,
   FIRST-WID 2 + WIDN-CELL X64HARNESS:EXPECT-CELL,
   OTHER-WID CUR-CELL X64HARNESS:CELL!,  s" get-current" ROW  OTHER-WID X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,

   path X64HARNESS:BOOT-CLOSE, ;

: CP!, ( n -- ) X64HARNESS:PUSH-REGION,  s" cp!" ROW ;

\ The code slot, and the last one of the region.
X64KERNEL:CODE-SLOT constant SLOT-BYTES
REGION SLOT-BYTES - constant TOP-SLOT

\ cp! admits a code slot in [DICT-SIZE, TOP-SLOT] of the region, both bounds
\ included, and cp@ reads each back.
: BUILD-CP ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   TOP-SLOT CP!,  s" cp@" ROW  TOP-SLOT X64HARNESS:EXPECT-POP-REGION,
   DICT-SIZE 64 + CP!,  s" cp@" ROW  DICT-SIZE 64 + X64HARNESS:EXPECT-POP-REGION,
   DICT-SIZE CP!,  s" cp@" ROW  DICT-SIZE X64HARNESS:EXPECT-POP-REGION,
   0 X64HARNESS:EXPECT-DEPTH,
   path X64HARNESS:BOOT-CLOSE, ;

: NDICT!, ( n -- ) X64HARNESS:PUSH,  s" ndict!" ROW ;
: NDICT?, ( n -- ) s" ndict@" ROW  X64HARNESS:EXPECT-POP, ;

\ Lowering the count hides the records past it.
: BUILD-NDICT ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   SEED,  X64KERNEL:HIDX-BUILD,
   5 NDICT?,
   4 DREC * LASTC-CELL X64HARNESS:REGION-ADDR!,
   2 NDICT!,  2 NDICT?,
   0 LASTC-CELL X64HARNESS:EXPECT-CELL,
   s" secret" OWNER-API-PRI-WID XREF,  0 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path X64HARNESS:BOOT-CLOSE, ;

: APPEND, ( n -- ) X64HARNESS:PUSH,  s" ndict-append" ROW ;

\ Record 5, delta, written and pending but not counted, with a compiler latched.
: PENDING, ( -- )
   SEED,  X64KERNEL:HIDX-BUILD,
   1 NCOMP-DISPATCH:COMP-LATCH-CELL X64HARNESS:CELL!,
   5 DREC * PEND-CELL X64HARNESS:REGION-ADDR!,
   s" delta" 0 0 X64HARNESS:RECORD,  5 NDICT!, ;

\ ndict-append counts the pending record and then, with a clause pending
\ (CLAUSE-CELL), its companion one record past it; the index learns each as it lands.
: BUILD-APPEND ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   PENDING,
   5 APPEND,  6 NDICT?,
   s" delta" 0 SEARCH,  6 X64HARNESS:EXPECT-POP,
   1 CLAUSE-CELL X64HARNESS:CELL!,
   s" echo" 0 0 X64HARNESS:RECORD,  6 NDICT!,
   6 APPEND,  7 NDICT?,
   s" echo" 0 SEARCH,  7 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   path X64HARNESS:BOOT-CLOSE, ;

\ The companion's place with no clause pending.
: BUILD-APPEND-FOREIGN ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   EXIT-STATUS:SEAL-VIOLATION X64HARNESS:EXPECT-EXIT
   PENDING,  6 NDICT!,  6 APPEND,
   path X64HARNESS:BOOT-CLOSE, ;

\ Slot n of the pending pre-trust defer table.
: SLOT ( n -- n ) PD-SLOT * PD-TABLE-OFF PD-SLOTS-REL + + ;

\ Stage a pending defer's name and signature in slot n.
: DEFER-SLOT, ( str str n -- ) {: ns:str ss:str ix:n :}
   ns STR:LEN ix SLOT PD-NLEN-OFF + X64HARNESS:CELL!,
   ns ix SLOT PD-NAME-OFF + X64HARNESS:TEXT!,
   ss STR:LEN ix SLOT PD-SLEN-OFF + X64HARNESS:CELL!,
   ss ix SLOT PD-SIG-OFF + X64HARNESS:TEXT!, ;

: PENDING-DEFERS, ( -- )
   s" ab" s" ( -- )" 0 DEFER-SLOT,
   s" xyz" s" ( n -- n )" 1 DEFER-SLOT,
   2 PD-TABLE-OFF X64HARNESS:CELL!, ;

\ The target checker's record, at the heap floor, which TARGET-DECL-CELL
\ names: its trust-decl writes T, the signature and the name on fd 1, and its
\ checker-defer D and the name.
: DECL, ( -- )
   [: [char] T EMIT,  s" type" ROW  s" type" ROW  s" cr" ROW ;] X64HARNESS:ROUTINE,
   DATA-START NCOMP-DISPATCH:DECL-EFFECT-OFF + X64HARNESS:LABEL-CELL!,
   [: [char] D EMIT,  s" type" ROW  s" cr" ROW ;] X64HARNESS:ROUTINE,
   DATA-START NCOMP-DISPATCH:DECL-DEFER-OFF + X64HARNESS:LABEL-CELL!,
   DATA-START NCOMP-DISPATCH:TARGET-DECL-CELL X64HARNESS:DATA-ADDR!, ;

\ DRAIN-PRETRUST replays the table from the top, slot 1 and then slot 0, and
\ empties it; a second drain calls nothing.
: BUILD-DRAIN ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   PENDING-DEFERS,  DECL,
   s" DRAIN-PRETRUST" ROW
   0 PD-TABLE-OFF X64HARNESS:EXPECT-CELL,
   s" DRAIN-PRETRUST" ROW
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path X64HARNESS:BOOT-CLOSE, ;

\ A pending table the drain meets without a checker to take it: DRAIN-PRETRUST
\ writes `trust-decl` on fd 2.
: BUILD-PENDING ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   EXIT-STATUS:INTERNAL X64HARNESS:EXPECT-EXIT
   PENDING-DEFERS,
   s" DRAIN-PRETRUST" ROW
   path X64HARNESS:BOOT-CLOSE, ;

\ wide-mark sets DNAME-WIDE on the newest record, hidden, and leaves the one
\ before it alone. The record's pages start read and execute only, as a
\ published record's are, so the row writes only through its alias.
: BUILD-WIDE-MARK ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   SEED,  4 5 X64HARNESS:PROT-RECORD,  s" wide-mark" ROW
   DNAME-WIDE DNAME-INT or 6 or  4 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,
   6  3 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,
   0 X64HARNESS:EXPECT-DEPTH,
   path X64HARNESS:BOOT-CLOSE, ;

: ADDRESS-HEADER, ( n -- ) {: cap:n :}
   0 RELOC:XTCELL-N-CELL X64HARNESS:CELL!,
   ADDRESS-CELLS:MAGIC RELOC:XTCELL-N-CELL ADDRESS-CELLS:MAGIC-FIELD + X64HARNESS:CELL!,
   ADDRESS-CELLS:BOOT-OFF RELOC:XTCELL-N-CELL ADDRESS-CELLS:BASE-FIELD + X64HARNESS:CELL!,
   cap RELOC:XTCELL-N-CELL ADDRESS-CELLS:CAP-FIELD + X64HARNESS:CELL!,
   0 RELOC:XTCELL-N-CELL ADDRESS-CELLS:MODE-FIELD + X64HARNESS:CELL!, ;

\ xt! stores into a scratch cell and ptr-cell-mark admits one.
: BUILD-XT-STORE ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   ADDRESS-CELLS:BOOT-CAP ADDRESS-HEADER,
   CELL-VALUE X64HARNESS:PUSH,  0 X64HARNESS:PUSH-SCRATCH,  s" xt!" ROW
   CELL-VALUE 0 X64HARNESS:EXPECT-SCRATCH,
   1 RELOC:XTCELL-N-CELL X64HARNESS:EXPECT-CELL,
   X64HARNESS:SCRATCH-OFF ADDRESS-CELLS:BOOT-OFF X64HARNESS:EXPECT-CELL,
   CELL X64HARNESS:PUSH-SCRATCH,  s" ptr-cell-mark" ROW
   2 RELOC:XTCELL-N-CELL X64HARNESS:EXPECT-CELL,
   X64HARNESS:SCRATCH-OFF CELL + RELOC:XTCELL-DATA-TAG or
   ADDRESS-CELLS:BOOT-OFF CELL + X64HARNESS:EXPECT-CELL,
   CELL X64HARNESS:PUSH-SCRATCH,  s" ptr-cell-mark" ROW
   2 RELOC:XTCELL-N-CELL X64HARNESS:EXPECT-CELL,
   0 X64HARNESS:EXPECT-DEPTH,
   path X64HARNESS:BOOT-CLOSE, ;

: BUILD-ADDR-GROW ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   2 ADDRESS-HEADER,
   0 X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   CELL X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   2 cells X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   3 cells X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   4 cells X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   0 X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   5 RELOC:XTCELL-N-CELL X64HARNESS:EXPECT-CELL,
   1 RELOC:XTCELL-N-CELL ADDRESS-CELLS:MODE-FIELD + X64HARNESS:EXPECT-CELL,
   8 RELOC:XTCELL-N-CELL ADDRESS-CELLS:CAP-FIELD + X64HARNESS:EXPECT-CELL,
   0 X64HARNESS:EXPECT-DEPTH,
   path X64HARNESS:BOOT-CLOSE, ;

\ A task installs its private context in rbp while declaring a cell in the
\ shared image DATA. The marker must find the one shared header and lock.
: BUILD-ADDR-TASK ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   ADDRESS-CELLS:BOOT-CAP ADDRESS-HEADER,
   0 X64HARNESS:PUSH-SCRATCH,
   X64ASM:RBP X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RAX X64ASM:RBP X64HARNESS:SCRATCH-OFF X64ASM:MEM-OFF
      X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RBP X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   s" ptr-cell-mark" ROW
   X64ASM:RBP X64CODE:ASM-SINK X64ASM:ENC-POP
   1 RELOC:XTCELL-N-CELL X64HARNESS:EXPECT-CELL,
   X64HARNESS:SCRATCH-OFF RELOC:XTCELL-DATA-TAG or
      ADDRESS-CELLS:BOOT-OFF X64HARNESS:EXPECT-CELL,
   0 X64HARNESS:EXPECT-DEPTH,
   path X64HARNESS:BOOT-CLOSE, ;

\ An xt! over a cell ptr-cell-mark marked: E-XTKIND on fd 2.
: BUILD-ADDR-KIND ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   EXIT-STATUS:ADDRESS-CELL X64HARNESS:EXPECT-EXIT
   ADDRESS-CELLS:BOOT-CAP ADDRESS-HEADER,
   0 X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   CELL-VALUE X64HARNESS:PUSH, 0 X64HARNESS:PUSH-SCRATCH, s" xt!" ROW
   path X64HARNESS:BOOT-CLOSE, ;

\ A ptr-cell-mark whose cell runs past the data band: E-XTBAND on fd 2.
: BUILD-ADDR-BAND ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   EXIT-STATUS:ADDRESS-CELL X64HARNESS:EXPECT-EXIT
   ADDRESS-CELLS:BOOT-CAP ADDRESS-HEADER,
   LINUX-LAYOUT:DATA-SIZE CELL - 1+ X64HARNESS:PUSH-DATA, s" ptr-cell-mark" ROW
   path X64HARNESS:BOOT-CLOSE, ;

\ A ptr-cell-mark with no address-cell header: E-XTCELL on fd 2.
: BUILD-ADDR-HEADER ( str -- ) {: path:str :}
   false X64HARNESS:BOOT-OPEN,
   EXIT-STATUS:ADDRESS-CELL X64HARNESS:EXPECT-EXIT
   0 X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   path X64HARNESS:BOOT-CLOSE, ;

\ Exercise the booted kernel's reader through its public primitive. Rejected
\ tokens must discard any partial value and clear both flags.
: NUM-CASE, ( str n n n -- )
   {: s:str value:n float:n ok:n :}
   s X64HARNESS:PUSH-TEXT,
   s" num-parse" X64HARNESS:CALL-ROW,
   ok X64HARNESS:EXPECT-POP,
   float X64HARNESS:EXPECT-POP,
   value X64HARNESS:EXPECT-POP, ;

\ The harness reserves statuses 21..30 for a case; each spelling needs three
\ checks, and stack balance takes two more. Two spellings fit per image.
: NUM-CARD, ( n -- ) {: card:n :}
   card 0 = if
      s" 0" 0 0 -1 NUM-CASE,  s" -0" 0 0 -1 NUM-CASE, exit then
   card 1 = if
      s" 9223372036854775807" $7FFFFFFFFFFFFFFF 0 -1 NUM-CASE,
      s" -9223372036854775808" $8000000000000000 0 -1 NUM-CASE, exit then
   card 2 = if
      s" 9223372036854775808" 0 0 0 NUM-CASE,
      s" -9223372036854775809" 0 0 0 NUM-CASE, exit then
   card 3 = if
      s" $FFFFFFFFFFFFFFFF" -1 0 -1 NUM-CASE,
      s" -$8000000000000000" $8000000000000000 0 -1 NUM-CASE, exit then
   card 4 = if
      s" $10000000000000000" 0 0 0 NUM-CASE,
      s" 18446744073709551616" 0 0 0 NUM-CASE, exit then
   card 5 = if
      s" 12a" 0 0 0 NUM-CASE,  s" 1.2.3" 0 0 0 NUM-CASE, exit then
   card 6 = if
      s" 1." 0 0 0 NUM-CASE,  s" -$" 0 0 0 NUM-CASE, exit then
   card 7 = if
      s" +1" 0 0 0 NUM-CASE,  s" $aBcDeF" $ABCDEF 0 -1 NUM-CASE, exit then
   card 8 = if
      s" .5" $3FE0000000000000 -1 -1 NUM-CASE,
      s" -.5" $BFE0000000000000 -1 -1 NUM-CASE, exit then
   card 9 = if
      s" 1.5" $3FF8000000000000 -1 -1 NUM-CASE,
      s" -0.0" $8000000000000000 -1 -1 NUM-CASE, exit then
   card 10 = if
      s" 0.000000000000000000" 0 -1 -1 NUM-CASE,
      s" 0.0000000000000000000" 0 0 0 NUM-CASE, exit then
   card 11 = if
      s" 9223372036854775808.0" 0 0 0 NUM-CASE,
      s" 99999999999999999999." 0 0 0 NUM-CASE, exit then
   card 12 = if
      s" 0.1" $3FB999999999999A -1 -1 NUM-CASE,
      s" 123.456" $405EDD2F1A9FBE77 -1 -1 NUM-CASE, exit then
   card 13 = if
      s" 9223372036854775807.5" $43E0000000000000 -1 -1 NUM-CASE,
      s" -9223372036854775808.5" 0 0 0 NUM-CASE, exit then
   s" x64k-engine: invalid numeric card" 1 die ;

: BUILD-NUM ( n str -- ) {: card:n path:str :}
   false X64HARNESS:BOOT-OPEN,
   card NUM-CARD,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path X64HARNESS:BOOT-CLOSE, ;

public

: RUN ( -- )
   T-RESET
   X64HARNESS:INIT
   false false s" hb-x64-kernel-engine" TMP-PATH BUILD
   true false s" hb-x64-kernel-engine-negative" TMP-PATH BUILD
   false true s" hb-x64-kernel-engine-armed" TMP-PATH BUILD
   s" hb-x64-kernel-window" TMP-PATH BUILD-WINDOW
   false s" hb-x64-kernel-search" TMP-PATH BUILD-SEARCH
   true s" hb-x64-kernel-search-index" TMP-PATH BUILD-SEARCH
   s" hb-x64-kernel-index-upkeep" TMP-PATH BUILD-UPKEEP
   s" hb-x64-kernel-heap" TMP-PATH BUILD-HEAP
   s" hb-x64-kernel-heap-high" TMP-PATH BUILD-HEAP-HIGH
   s" hb-x64-kernel-heap-low" TMP-PATH BUILD-HEAP-LOW
   s" hb-x64-kernel-heap-armed" TMP-PATH BUILD-HEAP-ARMED
   s" hb-x64-kernel-print" TMP-PATH BUILD-PRINT
   s" hb-x64-kernel-genio" TMP-PATH BUILD-GENIO
   s" hb-x64-kernel-hooks" TMP-PATH BUILD-HOOKS
   s" hb-x64-kernel-hooks-check" TMP-PATH BUILD-HOOKS-CHECK
   s" hb-x64-kernel-hooks-replaced" TMP-PATH BUILD-HOOKS-REPLACED
   s" hb-x64-kernel-hooks-empty" TMP-PATH BUILD-HOOKS-EMPTY
   s" hb-x64-kernel-state" TMP-PATH BUILD-STATE
   s" hb-x64-kernel-cp" TMP-PATH BUILD-CP
   s" hb-x64-kernel-ndict" TMP-PATH BUILD-NDICT
   s" hb-x64-kernel-append" TMP-PATH BUILD-APPEND
   s" hb-x64-kernel-append-foreign" TMP-PATH BUILD-APPEND-FOREIGN
   s" hb-x64-kernel-drain" TMP-PATH BUILD-DRAIN
   s" hb-x64-kernel-drain-unset" TMP-PATH BUILD-PENDING
   s" hb-x64-kernel-wide-mark" TMP-PATH BUILD-WIDE-MARK
   s" hb-x64-kernel-xt-store" TMP-PATH BUILD-XT-STORE
   s" hb-x64-kernel-addr-grow" TMP-PATH BUILD-ADDR-GROW
   s" hb-x64-kernel-addr-task" TMP-PATH BUILD-ADDR-TASK
   s" hb-x64-kernel-addr-kind" TMP-PATH BUILD-ADDR-KIND
   s" hb-x64-kernel-addr-band" TMP-PATH BUILD-ADDR-BAND
   s" hb-x64-kernel-addr-header" TMP-PATH BUILD-ADDR-HEADER
   0 s" hb-x64-kernel-number-00" TMP-PATH BUILD-NUM
   1 s" hb-x64-kernel-number-01" TMP-PATH BUILD-NUM
   2 s" hb-x64-kernel-number-02" TMP-PATH BUILD-NUM
   3 s" hb-x64-kernel-number-03" TMP-PATH BUILD-NUM
   4 s" hb-x64-kernel-number-04" TMP-PATH BUILD-NUM
   5 s" hb-x64-kernel-number-05" TMP-PATH BUILD-NUM
   6 s" hb-x64-kernel-number-06" TMP-PATH BUILD-NUM
   7 s" hb-x64-kernel-number-07" TMP-PATH BUILD-NUM
   8 s" hb-x64-kernel-number-08" TMP-PATH BUILD-NUM
   9 s" hb-x64-kernel-number-09" TMP-PATH BUILD-NUM
   10 s" hb-x64-kernel-number-10" TMP-PATH BUILD-NUM
   11 s" hb-x64-kernel-number-11" TMP-PATH BUILD-NUM
   12 s" hb-x64-kernel-number-12" TMP-PATH BUILD-NUM
   13 s" hb-x64-kernel-number-13" TMP-PATH BUILD-NUM
   X64HARNESS:DISPOSE
   T-REPORT ;

;using   \ LINUX-LAYOUT
;package

X64K-ENGINE:RUN
