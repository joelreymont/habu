\ x86-64-kernel-engine.f - the engine-state rows of the x86-64 kernel
\ (src/habu/kernel-x64.f) in the booted harness, cross-built for an x86-64
\ peer. Each image runs TASK-LIVE-GUARD, and then checks the data stack is
\ empty. hb-x64-kernel-engine runs with no task live, as the boot leaves
\ TASKS-LIVE-CELL, so the guard passes and the image exits 0;
\ hb-x64-kernel-engine-negative expects the wrong depth and exits 21.
\ hb-x64-kernel-engine-armed marks a task live first, so the guard exits 79.
\ hb-x64-kernel-window carries text past one PROT-PAGE-MAX behind the kernel,
\ as the whole kernel will, so its read-write segment lands a page further
\ on; it pushes the text's span and exits 0.
\
\ The dictionary search images seed the same five records with
\ X64HARNESS:RECORD, and exit 0. hb-x64-kernel-search asks `search-wl` and
\ `xref-search-wl` with no index, so the one-wordlist search scans;
\ hb-x64-kernel-search-index builds the index first and asks the same, so it
\ probes. hb-x64-kernel-index-upkeep also seeds eccwbv and hktfoj, two names of
\ one length whose keys share a first slot under wid 0, so both the insert and
\ the probe for hktfoj walk the chain past eccwbv's record. After the build it
\ seeds more records: one the index is not told of stays absent until
\ HIDX-ADD, and one until HIDX-REBUILD,, and one under DICT-WL:RETIRED is found
\ by the scan the probe leaves that wid to.
\
\ The heap, printer and hook rows, one image per outcome; the peer runs each
\ and compares its status and its fd-1 and fd-2 bytes (src/habu/kernel-x64.f):
\ - hb-x64-kernel-heap walks DP from the heap floor through allot, align, `,`
\   and c, checks the cells they filled and returns to the floor, and exits 0;
\ - hb-x64-kernel-heap-high allots up to the ceiling, which DP may reach, and
\   one byte past it, which exits 76 with the DP line on fd 2;
\ - hb-x64-kernel-heap-low allots one byte below the floor and exits 76;
\ - hb-x64-kernel-heap-armed runs `,` with a task live and exits 79;
\ - hb-x64-kernel-print writes the fixed bytes the ARM64 engine writes for
\   the same values on fd 1 and exits 0;
\ - hb-x64-kernel-genio sends output through a device row whose routine
\   brackets the span in [ ], and past it on each path that writes fd 1;
\ - hb-x64-kernel-hooks installs, reads and clears the three hooks at the
\   code window's edges, suspends and resumes the checker hook with its
\   preflight, and exits 0;
\ - hb-x64-kernel-hooks-check, -top, -replaced and -empty each end in one
\   hook row's fd-2 refusal and exit 70.
\
\ The register, dictionary, seal, wordlist and scope rows, the same way:
\ - hb-x64-kernel-state reads the boot's registers and DATA cells through the
\   getters and counts wordlists, and exits 0;
\ - hb-x64-kernel-cp moves CP to both bounds of the code area and back, and
\   -cp-low, -cp-high and -cp-unslotted each refuse one CP with 83: a code
\   slot below the area, the region's end and a 4-aligned CP between slots;
\ - hb-x64-kernel-ndict lowers and raises the count over an index, and exits
\   0; -ndict-cap admits DICT-CAP and exits 74 with the fd-2 text for one
\   more, and -ndict-floor exits 83 below the seal floor;
\ - hb-x64-kernel-seed-ndict lowers through the floor and exits 0, and
\   -seed-ndict-high exits 74;
\ - hb-x64-kernel-append publishes a pending record and its does> companion
\   and exits 0, and -append-foreign exits 83;
\ - hb-x64-kernel-seal exits 0; -seal-undrained names two pending defers on
\   fd 2 and exits 73;
\ - hb-x64-kernel-drain replays two pending defers through the target
\   checker's routines, which write them on fd 1, and exits 0;
\   -drain-unset writes `trust-decl` on fd 2 and exits 70;
\ - hb-x64-kernel-prot-wid, -wide-mark, -xt-store, -tier and -scope exit 0;
\   -prot-wid-bound exits 84 with its fd-2 text, -xt-store-armed and
\   -mark-armed exit 83, -tier-zero exits 70 with its fd-2 text, and
\   -snap-rebase exits 76 with the REFUSE text.
\ The host checks each image's ELF header; running them is the peer's.
require src/habu/snapshot-format.f
require src/habu/address-cells.f
require test/x86-64-boot-harness.f
require src/os/linux-x86-64/target-layout.f

package X64K-ENGINE
using X64LAYOUT   \ the guard: a bare layout name refuses (target-layout.f)

PROT-PAGE-MAX constant FILL-BYTES
create FILL FILL-BYTES allot

: BUILD ( bool bool ptr u8 n -- ) {: negative:bool armed:bool path:ptr pathu:n :}
   negative X64HARNESS:BOOT-OPEN,
   armed if 1 TASKS-LIVE-CELL X64HARNESS:CELL!, then
   X64KERNEL:TASK-LIVE-GUARD,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-WINDOW ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   FILL FILL-BYTES X64HARNESS:PUSH-TEXT,
   2 X64HARNESS:EXPECT-DEPTH,
   FILL-BYTES X64HARNESS:EXPECT-POP,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- the dictionary search ---------------------------------------------------
7 constant OTHER-WID                   \ a public wordlist past the fixed ones

\ Longer than DNAME-INL, so its record points at its bytes.
: LONG$ ( -- ptr u8 n ) s" A-Name-Longer-Than-Sixteen" ;
: LONG-UPPER$ ( -- ptr u8 n ) s" A-NAME-LONGER-THAN-SIXTEEN" ;

\ Records 0-4, the code cells 1-5. The names mix case, so a compare that folds
\ one side only misses them.
: SEED, ( -- )
   s" Alpha" 0 0 X64HARNESS:RECORD,
   s" ALPHA" OTHER-WID 0 X64HARNESS:RECORD,
   LONG$ 0 0 X64HARNESS:RECORD,
   s" secret" OWNER-API-PRI-WID 0 X64HARNESS:RECORD,
   s" hidden" 0 DNAME-INT X64HARNESS:RECORD, ;

\ Push a name and a wid and call the row.
: ASK, ( ptr u8 n n ptr u8 n -- ) {: a:ptr u:n wid:n row:ptr rowu:n :}
   a u X64HARNESS:PUSH-TEXT,
   wid X64HARNESS:PUSH,
   row rowu X64HARNESS:CALL-ROW, ;

: SEARCH, ( ptr u8 n n -- ) s" search-wl" ASK, ;
: XREF, ( ptr u8 n n -- ) s" xref-search-wl" ASK, ;

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

: BUILD-SEARCH ( bool ptr u8 n -- ) {: indexed:bool path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   SEED,
   indexed if X64KERNEL:HIDX-BUILD, then
   SEARCHES,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-UPKEEP ( ptr u8 n -- ) {: path:ptr pathu:n :}
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
   s" omega" DICT-WL:RETIRED 0 X64HARNESS:RECORD,        \ record 9
   s" omega" DICT-WL:RETIRED SEARCH,  10 X64HARNESS:EXPECT-POP,
   s" alpha" 0 SEARCH,  1 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- heap ----------------------------------------------------------------------
$1122334455667788 constant CELL-VALUE

: ROW ( ptr u8 n -- ) X64HARNESS:CALL-ROW, ;

: ALLOT, ( n -- ) X64HARNESS:PUSH,  s" allot" ROW ;

\ DP starts at the heap floor, DATA-START. Three bytes and an align reach the
\ next cell; `,` fills it and c, stores the low byte of its cell alone.
: BUILD-HEAP ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
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
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-HEAP-HIGH ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   X64KERNEL:DP-CEILING DATA-START - ALLOT,
   s" here" ROW  X64KERNEL:DP-CEILING X64HARNESS:EXPECT-POP-DATA,
   1 ALLOT,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-HEAP-LOW ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   -1 ALLOT,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-HEAP-ARMED ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   1 TASKS-LIVE-CELL X64HARNESS:CELL!,
   1 X64HARNESS:PUSH,  s" ," ROW
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- printers ------------------------------------------------------------------
$8000000000000000 constant MIN-CELL
$7FFFFFFFFFFFFFFF constant MAX-CELL

: DOT, ( n -- ) X64HARNESS:PUSH,  s" ." ROW ;
: EMIT, ( n -- ) X64HARNESS:PUSH,  s" emit" ROW ;

\ fd 1: the four cells ., -1 and 0 u., "A B", "hello" and .s of 7 -3, one per
\ line; .s leaves the cells, and depth counts them.
: BUILD-PRINT ( ptr u8 n -- ) {: path:ptr pathu:n :}
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
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- the output device ---------------------------------------------------------
: TYPE, ( ptr u8 n -- ) X64HARNESS:PUSH-TEXT,  s" type" ROW ;
: OUT! ( n -- ) GENIO-ABI:OUT-CELL X64HARNESS:CELL!, ;

\ Device 1's write routine: `[`, the span it is handed, `]`. Its own output
\ runs while the funnel is busy, so it reaches fd 1 and not the routine again.
: BRACKET, ( -- label )
   [: [char] [ EMIT,  s" type" ROW  [char] ] EMIT, ;] X64HARNESS:ROUTINE, ;

\ fd 1: `[hi][42\n][Z]` through device 1, the funnel idle again and the
\ caller's active device back afterwards; then `e` for a device with no row,
\ `x` for an index past DEVICES and `b` while the funnel is busy.
: BUILD-GENIO ( ptr u8 n -- ) {: path:ptr pathu:n :}
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
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- hooks ---------------------------------------------------------------------
: CHECK-HOOK! ( n -- ) X64HARNESS:PUSH-REGION,  s" set-check" ROW ;
: TOP-HOOK! ( n -- ) X64HARNESS:PUSH-REGION,  s" set-top-check" ROW ;
: PREFLIGHT! ( n -- ) X64HARNESS:PUSH-REGION,  s" set-preflight" ROW ;

\ The code window is [DBASE, CP), and the boot's CP is DICT-SIZE past DBASE.
\ The preflight hook installs once; the same xt again is inert, and
\ `0 set-check` suspends the armed hook with its preflight and empties both,
\ so another preflight installs. `0 set-check` while off keeps the parked
\ pair (the inner disarm of a nested window); re-arming the parked hook
\ resumes its preflight, and any other hook starts without one.
: BUILD-HOOKS ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   0 CHECK-HOOK!
   s" check@" ROW  0 X64HARNESS:EXPECT-POP-REGION,
   DICT-SIZE 1- TOP-HOOK!
   s" top-check@" ROW  DICT-SIZE 1- X64HARNESS:EXPECT-POP-REGION,
   64 PREFLIGHT!  64 PREFLIGHT!
   0 X64HARNESS:PUSH,  s" set-check" ROW
   s" check@" ROW  0 X64HARNESS:EXPECT-POP,
   128 PREFLIGHT!
   0 X64HARNESS:PUSH,  s" set-top-check" ROW
   s" top-check@" ROW  0 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:PUSH,  s" set-check" ROW
   0 CHECK-HOOK!
   COMPILE-PREFLIGHT-CELL X64HARNESS:PUSH-DATA-CELL,  64 X64HARNESS:EXPECT-POP-REGION,
   0 X64HARNESS:PUSH,  s" set-check" ROW
   DICT-SIZE 1- CHECK-HOOK!
   0 COMPILE-PREFLIGHT-CELL X64HARNESS:EXPECT-CELL,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-HOOKS-CHECK ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   DICT-SIZE CHECK-HOOK!
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-HOOKS-TOP ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   -1 TOP-HOOK!
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-HOOKS-REPLACED ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   64 PREFLIGHT!  128 PREFLIGHT!
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-HOOKS-EMPTY ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   0 X64HARNESS:PUSH,  s" set-preflight" ROW
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- engine state ------------------------------------------------------------
40 constant FIRST-WID                  \ WIDN-CELL before the first wordlist

\ The getters answer what the boot published, and wordlist counts WIDN-CELL.
: BUILD-STATE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   s" cp@" ROW  DICT-SIZE X64HARNESS:EXPECT-POP-REGION,
   s" dbase@" ROW  0 X64HARNESS:EXPECT-POP-REGION,
   s" data-base" ROW  0 X64HARNESS:EXPECT-POP-DATA,
   s" rbase" ROW  X64LAYOUT:CODE-OFF REGION-OFF - X64HARNESS:EXPECT-POP-REGION,
   s" ndict@" ROW  0 X64HARNESS:EXPECT-POP,
   FIRST-WID WIDN-CELL X64HARNESS:CELL!,
   s" wordlist" ROW  s" wordlist" ROW
   FIRST-WID 1+ X64HARNESS:EXPECT-POP,  FIRST-WID X64HARNESS:EXPECT-POP,
   FIRST-WID 2 + WIDN-CELL X64HARNESS:EXPECT-CELL,
   OTHER-WID X64HARNESS:PUSH,  s" set-current" ROW
   s" get-current" ROW  OTHER-WID X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ Boot, call one row and close: for a row that ends the image itself.
: BUILD-CALL ( ptr u8 n ptr u8 n -- ) {: row:ptr rowu:n path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   row rowu ROW
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- the code pointer --------------------------------------------------------
: CP!, ( n -- ) X64HARNESS:PUSH-REGION,  s" cp!" ROW ;

\ The code slot, and the last one of the region.
X64KERNEL:CODE-SLOT constant SLOT-BYTES
REGION SLOT-BYTES - constant TOP-SLOT

\ cp! admits a code slot in [DICT-SIZE, TOP-SLOT] of the region, both bounds
\ included, and cp@ reads each back.
: BUILD-CP ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   TOP-SLOT CP!,  s" cp@" ROW  TOP-SLOT X64HARNESS:EXPECT-POP-REGION,
   DICT-SIZE 64 + CP!,  s" cp@" ROW  DICT-SIZE 64 + X64HARNESS:EXPECT-POP-REGION,
   DICT-SIZE CP!,  s" cp@" ROW  DICT-SIZE X64HARNESS:EXPECT-POP-REGION,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ One refused CP, n bytes into the region: the row exits 83.
: BUILD-CP-BAD ( n ptr u8 n -- ) {: at:n path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   at CP!,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- the dictionary count ----------------------------------------------------
: NDICT!, ( n -- ) X64HARNESS:PUSH,  s" ndict!" ROW ;
: NDICT?, ( n -- ) s" ndict@" ROW  X64HARNESS:EXPECT-POP, ;

\ Lowering the count hides the records past it. Raising it rebuilds the index,
\ so delta, written while the count was low and never indexed, is found, and
\ LONG$, whose record delta overwrote, is not.
: BUILD-NDICT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   SEED,  X64KERNEL:HIDX-BUILD,
   5 NDICT?,
   4 DREC * LASTC-CELL X64HARNESS:REGION-ADDR!,
   2 NDICT!,  2 NDICT?,
   0 LASTC-CELL X64HARNESS:EXPECT-CELL,
   s" secret" OWNER-API-PRI-WID XREF,  0 X64HARNESS:EXPECT-POP,
   s" delta" 0 0 X64HARNESS:RECORD,                      \ record 2
   2 NDICT!,  3 NDICT!,
   s" delta" 0 SEARCH,  3 X64HARNESS:EXPECT-POP,
   LONG-UPPER$ 0 SEARCH,  0 X64HARNESS:EXPECT-POP,
   5 NDICT!,
   s" secret" OWNER-API-PRI-WID XREF,  3 X64HARNESS:EXPECT-ROW,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ DICT-CAP is a count ndict! admits; one more exits 74 with the ARM64 text.
: BUILD-NDICT-CAP ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   DICT-CAP NDICT!,  DICT-CAP NDICT?,
   DICT-CAP 1+ NDICT!,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ Below the floor SEAL-CAPTURE records, ndict! exits 83.
: BUILD-NDICT-FLOOR ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   SEED,  s" SEAL-CAPTURE" ROW
   4 NDICT!,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: SEED-NDICT!, ( n -- ) X64HARNESS:PUSH,  s" seed-ndict!" ROW ;

\ seed-ndict! lowers the count through the seal floor and clears the floor.
: BUILD-SEED-NDICT ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   SEED,  X64KERNEL:HIDX-BUILD,  s" SEAL-CAPTURE" ROW
   4 DREC * LASTC-CELL X64HARNESS:REGION-ADDR!,
   2 SEED-NDICT!,  2 NDICT?,
   0 LASTC-CELL X64HARNESS:EXPECT-CELL,
   0 SEAL-NDICT-CELL X64HARNESS:EXPECT-CELL,
   s" alpha" OTHER-WID SEARCH,  2 X64HARNESS:EXPECT-POP,
   LONG$ 0 SEARCH,  0 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ A count that does not lower the live one exits 74.
: BUILD-SEED-NDICT-HIGH ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   SEED,  5 SEED-NDICT!,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- native publication ------------------------------------------------------
: APPEND, ( n -- ) X64HARNESS:PUSH,  s" ndict-append" ROW ;

\ Record 5, delta, written and pending but not counted, under the native tier.
: PENDING, ( -- )
   SEED,  X64KERNEL:HIDX-BUILD,
   1 NCOMP-DISPATCH:DEF-TIER-CELL X64HARNESS:CELL!,
   5 DREC * PEND-CELL X64HARNESS:REGION-ADDR!,
   s" delta" 0 0 X64HARNESS:RECORD,  5 NDICT!, ;

\ ndict-append counts the pending record and then, with a clause pending
\ (CLAUSE-CELL), its companion one record past it; the index learns each as it lands.
: BUILD-APPEND ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   PENDING,
   5 APPEND,  6 NDICT?,
   s" delta" 0 SEARCH,  6 X64HARNESS:EXPECT-POP,
   1 CLAUSE-CELL X64HARNESS:CELL!,
   s" echo" 0 0 X64HARNESS:RECORD,  6 NDICT!,
   6 APPEND,  7 NDICT?,
   s" echo" 0 SEARCH,  7 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ The companion's place with no clause pending exits 83.
: BUILD-APPEND-FOREIGN ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   PENDING,  6 NDICT!,  6 APPEND,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- the seal ----------------------------------------------------------------
: BUILD-SEAL ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   s" seal-captured?" ROW  0 X64HARNESS:EXPECT-POP,
   SEED,  s" SEAL-CAPTURE" ROW
   5 SEAL-NDICT-CELL X64HARNESS:EXPECT-CELL,
   s" seal-captured?" ROW  -1 X64HARNESS:EXPECT-POP,
   s" SEAL-FRIEND" ROW
   FRIEND-ARENA-LEN FRIEND-LATCH-CELL X64HARNESS:EXPECT-CELL,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ Slot n of the pending pre-trust defer table.
: SLOT ( n -- n ) PD-SLOT * PD-TABLE-OFF PD-SLOTS-REL + + ;

\ Stage a pending defer's name and signature in slot n.
: DEFER-SLOT, ( ptr u8 n ptr u8 n n -- ) {: na:ptr nu:n sa:ptr su:n ix:n :}
   nu ix SLOT PD-NLEN-OFF + X64HARNESS:CELL!,
   na nu ix SLOT PD-NAME-OFF + X64HARNESS:TEXT!,
   su ix SLOT PD-SLEN-OFF + X64HARNESS:CELL!,
   sa su ix SLOT PD-SIG-OFF + X64HARNESS:TEXT!, ;

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
: BUILD-DRAIN ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   PENDING-DEFERS,  DECL,
   s" DRAIN-PRETRUST" ROW
   0 PD-TABLE-OFF X64HARNESS:EXPECT-CELL,
   s" DRAIN-PRETRUST" ROW
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ A pending table the drain or the seal row meets without a checker to take it.
: BUILD-PENDING ( ptr u8 n ptr u8 n -- ) {: row:ptr rowu:n path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   PENDING-DEFERS,
   row rowu ROW
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- protected wordlists -----------------------------------------------------
: PROT-WID!, ( n -- ) X64HARNESS:PUSH,  s" prot-wid-add" ROW ;
: BITS ( n -- n ) CELL * PROT-BITS-OFF + ;   \ the bitmap's cell n

\ Wid 70's bit is bit 6 of cell 1 and 71's the next; a second add is inert,
\ an engine-reserved wid counts as protected and keeps its bit clear, and the
\ last wid below PROT-WID-MAX is the last cell's top bit. prot-wid-room
\ answers the wids left below the bound, and 0 past it.
: BUILD-PROT-WID ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   70 PROT-WID!,  $40 1 BITS X64HARNESS:EXPECT-CELL,
   71 PROT-WID!,  70 PROT-WID!,  $C0 1 BITS X64HARNESS:EXPECT-CELL,
   OWNER-API-PUB-WID PROT-WID!,  0 0 BITS X64HARNESS:EXPECT-CELL,
   PROT-WID-MAX 1- PROT-WID!,
   MIN-CELL PROT-BITS-BYTES CELL / 1- BITS X64HARNESS:EXPECT-CELL,
   FIRST-WID WIDN-CELL X64HARNESS:CELL!,
   s" prot-wid-room" ROW  PROT-WID-MAX FIRST-WID - X64HARNESS:EXPECT-POP,
   PROT-WID-MAX 1+ WIDN-CELL X64HARNESS:CELL!,
   s" prot-wid-room" ROW  0 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ A wid with no bit exits 84 with the ARM64 text.
: BUILD-PROT-WID-BOUND ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   PROT-WID-MAX PROT-WID!,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- record marks and persisted cells ----------------------------------------
\ wide-mark sets DNAME-WIDE on the newest record, hidden, and leaves the one
\ before it alone. The record's pages start read and execute only, as a
\ published record's are, so the row writes only through its own flip.
: BUILD-WIDE-MARK ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   SEED,  4 5 X64HARNESS:PROT-RECORD,  s" wide-mark" ROW
   DNAME-WIDE DNAME-INT or 6 or  4 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,
   6  3 X64KERNEL:REC-FLAGS X64HARNESS:EXPECT-RECORD,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: SEAL, ( -- ) FRIEND-ARENA-LEN FRIEND-LATCH-CELL X64HARNESS:CELL!, ;

: ADDRESS-HEADER, ( n -- ) {: cap:n :}
   0 SNAP-RELOC:XTCELL-N-CELL X64HARNESS:CELL!,
   ADDRESS-CELLS:MAGIC SNAP-RELOC:XTCELL-N-CELL ADDRESS-CELLS:MAGIC-FIELD + X64HARNESS:CELL!,
   ADDRESS-CELLS:BOOT-OFF SNAP-RELOC:XTCELL-N-CELL ADDRESS-CELLS:BASE-FIELD + X64HARNESS:CELL!,
   cap SNAP-RELOC:XTCELL-N-CELL ADDRESS-CELLS:CAP-FIELD + X64HARNESS:CELL!,
   0 SNAP-RELOC:XTCELL-N-CELL ADDRESS-CELLS:MODE-FIELD + X64HARNESS:CELL!, ;

\ With the latch sealed, xt! stores into a scratch cell and ptr-cell-mark
\ admits one.
: BUILD-XT-STORE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   ADDRESS-CELLS:BOOT-CAP ADDRESS-HEADER,
   SEAL,
   CELL-VALUE X64HARNESS:PUSH,  0 X64HARNESS:PUSH-SCRATCH,  s" xt!" ROW
   CELL-VALUE 0 X64HARNESS:EXPECT-SCRATCH,
   1 SNAP-RELOC:XTCELL-N-CELL X64HARNESS:EXPECT-CELL,
   X64HARNESS:SCRATCH-OFF ADDRESS-CELLS:BOOT-OFF X64HARNESS:EXPECT-CELL,
   CELL X64HARNESS:PUSH-SCRATCH,  s" ptr-cell-mark" ROW
   2 SNAP-RELOC:XTCELL-N-CELL X64HARNESS:EXPECT-CELL,
   X64HARNESS:SCRATCH-OFF CELL + SNAP-RELOC:XTCELL-DATA-TAG or
   ADDRESS-CELLS:BOOT-OFF CELL + X64HARNESS:EXPECT-CELL,
   CELL X64HARNESS:PUSH-SCRATCH,  s" ptr-cell-mark" ROW
   2 SNAP-RELOC:XTCELL-N-CELL X64HARNESS:EXPECT-CELL,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-ADDR-GROW ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   2 ADDRESS-HEADER,
   0 X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   CELL X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   2 cells X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   3 cells X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   4 cells X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   0 X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   5 SNAP-RELOC:XTCELL-N-CELL X64HARNESS:EXPECT-CELL,
   1 SNAP-RELOC:XTCELL-N-CELL ADDRESS-CELLS:MODE-FIELD + X64HARNESS:EXPECT-CELL,
   8 SNAP-RELOC:XTCELL-N-CELL ADDRESS-CELLS:CAP-FIELD + X64HARNESS:EXPECT-CELL,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ A task installs its private context in rbp while declaring a cell in the
\ shared image DATA. The marker must find the one shared header and lock.
: BUILD-ADDR-TASK ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   ADDRESS-CELLS:BOOT-CAP ADDRESS-HEADER,
   0 X64HARNESS:PUSH-SCRATCH,
   X64ASM:RBP X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RAX X64ASM:RBP X64HARNESS:SCRATCH-OFF X64ASM:MEM-OFF
      X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RBP X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   s" ptr-cell-mark" ROW
   X64ASM:RBP X64CODE:ASM-SINK X64ASM:ENC-POP
   1 SNAP-RELOC:XTCELL-N-CELL X64HARNESS:EXPECT-CELL,
   X64HARNESS:SCRATCH-OFF SNAP-RELOC:XTCELL-DATA-TAG or
      ADDRESS-CELLS:BOOT-OFF X64HARNESS:EXPECT-CELL,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-ADDR-KIND ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   ADDRESS-CELLS:BOOT-CAP ADDRESS-HEADER,
   0 X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   CELL-VALUE X64HARNESS:PUSH, 0 X64HARNESS:PUSH-SCRATCH, s" xt!" ROW
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-ADDR-BAND ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   ADDRESS-CELLS:BOOT-CAP ADDRESS-HEADER,
   X64LAYOUT:DATA-SIZE CELL - 1+ X64HARNESS:PUSH-DATA, s" ptr-cell-mark" ROW
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-ADDR-HEADER ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   0 X64HARNESS:PUSH-SCRATCH, s" ptr-cell-mark" ROW
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ With the latch sealed, the row aimed at the band cell UNIT-COMPILE-CELL exits
\ 83 before it stores: xt! when the flag is true, ptr-cell-mark otherwise.
: BUILD-BAND-CELL ( bool ptr u8 n -- ) {: store:bool path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   SEAL,
   store if CELL-VALUE X64HARNESS:PUSH, then
   UNIT-COMPILE-CELL X64HARNESS:PUSH-DATA,
   store if s" xt!" ROW else s" ptr-cell-mark" ROW then
   path pathu X64HARNESS:BOOT-CLOSE, ;

: BUILD-BAND-TASK ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   ADDRESS-CELLS:BOOT-CAP ADDRESS-HEADER,
   SEAL,
   UNIT-COMPILE-CELL X64HARNESS:PUSH-DATA,
   X64ASM:RBP X64CODE:ASM-SINK X64ASM:ENC-PUSH
   X64ASM:RAX X64ASM:RBP X64HARNESS:SCRATCH-OFF X64ASM:MEM-OFF
      X64CODE:ASM-SINK X64ASM:ENC-LEA
   X64ASM:RBP X64ASM:RAX X64CODE:ASM-SINK X64ASM:ENC-MOV-RR
   s" ptr-cell-mark" ROW
   X64ASM:RBP X64CODE:ASM-SINK X64ASM:ENC-POP
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- the compiler tier and the build scope -----------------------------------
\ The constant rows and tier 1.
: BUILD-TIER ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   s" addr-cells-abi" ROW  ADDRESS-CELLS:ABI-VERSION X64HARNESS:EXPECT-POP,
   s" snapshot-format" ROW  SNAPSHOT-FORMAT:VERSION X64HARNESS:EXPECT-POP,
   1 X64HARNESS:PUSH,  s" set-tier" ROW
   s" tier@" ROW  1 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ Not a tier: the caller's cell, which only the saved copy can restore.
$5A constant CALLER-TIER

: ENTER, ( -- ) s" executable-build-enter" ROW ;
: LEAVE, ( -- ) s" executable-build-leave" ROW ;
: TIER-CELL?, ( n -- ) NCOMP-DISPATCH:TIER-CELL X64HARNESS:EXPECT-CELL, ;

\ Two nested scopes keep the caller's tier and save it once; a tier selected
\ inside survives the inner leave, the outermost leave restores the caller's
\ and clears the saved copy, and a leave with no scope open changes nothing.
: BUILD-SCOPE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   CALLER-TIER NCOMP-DISPATCH:TIER-CELL X64HARNESS:CELL!,
   ENTER,  ENTER,
   CALLER-TIER TIER-CELL?,
   2 NCOMP-DISPATCH:BUILD-DEPTH-CELL X64HARNESS:EXPECT-CELL,
   CALLER-TIER NCOMP-DISPATCH:BUILD-TIER-CELL X64HARNESS:EXPECT-CELL,
   1 X64HARNESS:PUSH,  s" set-tier" ROW
   LEAVE,  1 TIER-CELL?,
   LEAVE,  CALLER-TIER TIER-CELL?,
   0 NCOMP-DISPATCH:BUILD-TIER-CELL X64HARNESS:EXPECT-CELL,
   LEAVE,
   0 NCOMP-DISPATCH:BUILD-DEPTH-CELL X64HARNESS:EXPECT-CELL,
   CALLER-TIER TIER-CELL?,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ Tier 0 exits 70 with the x86-64 text.
: BUILD-TIER-ZERO ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   0 X64HARNESS:PUSH,  s" set-tier" ROW
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ Exercise the booted kernel's reader through its public primitive. Rejected
\ tokens must discard any partial value and clear both flags.
: NUM-CASE, ( ptr u8 n n n n -- )
   {: a:ptr u:n value:n float:n ok:n :}
   a u X64HARNESS:PUSH-TEXT,
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

: BUILD-NUM ( n ptr u8 n -- ) {: card:n path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   card NUM-CARD,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu X64HARNESS:BOOT-CLOSE, ;

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
   s" hb-x64-kernel-hooks-top" TMP-PATH BUILD-HOOKS-TOP
   s" hb-x64-kernel-hooks-replaced" TMP-PATH BUILD-HOOKS-REPLACED
   s" hb-x64-kernel-hooks-empty" TMP-PATH BUILD-HOOKS-EMPTY
   s" hb-x64-kernel-state" TMP-PATH BUILD-STATE
   s" hb-x64-kernel-cp" TMP-PATH BUILD-CP
   DICT-SIZE SLOT-BYTES - s" hb-x64-kernel-cp-low" TMP-PATH BUILD-CP-BAD
   REGION s" hb-x64-kernel-cp-high" TMP-PATH BUILD-CP-BAD
   DICT-SIZE 8 + s" hb-x64-kernel-cp-unslotted" TMP-PATH BUILD-CP-BAD
   s" hb-x64-kernel-ndict" TMP-PATH BUILD-NDICT
   s" hb-x64-kernel-ndict-cap" TMP-PATH BUILD-NDICT-CAP
   s" hb-x64-kernel-ndict-floor" TMP-PATH BUILD-NDICT-FLOOR
   s" hb-x64-kernel-seed-ndict" TMP-PATH BUILD-SEED-NDICT
   s" hb-x64-kernel-seed-ndict-high" TMP-PATH BUILD-SEED-NDICT-HIGH
   s" hb-x64-kernel-append" TMP-PATH BUILD-APPEND
   s" hb-x64-kernel-append-foreign" TMP-PATH BUILD-APPEND-FOREIGN
   s" hb-x64-kernel-seal" TMP-PATH BUILD-SEAL
   s" SEAL-CAPTURE" s" hb-x64-kernel-seal-undrained" TMP-PATH BUILD-PENDING
   s" hb-x64-kernel-drain" TMP-PATH BUILD-DRAIN
   s" DRAIN-PRETRUST" s" hb-x64-kernel-drain-unset" TMP-PATH BUILD-PENDING
   s" hb-x64-kernel-prot-wid" TMP-PATH BUILD-PROT-WID
   s" hb-x64-kernel-prot-wid-bound" TMP-PATH BUILD-PROT-WID-BOUND
   s" hb-x64-kernel-wide-mark" TMP-PATH BUILD-WIDE-MARK
   s" hb-x64-kernel-xt-store" TMP-PATH BUILD-XT-STORE
   s" hb-x64-kernel-addr-grow" TMP-PATH BUILD-ADDR-GROW
   s" hb-x64-kernel-addr-task" TMP-PATH BUILD-ADDR-TASK
   s" hb-x64-kernel-addr-kind" TMP-PATH BUILD-ADDR-KIND
   s" hb-x64-kernel-addr-band" TMP-PATH BUILD-ADDR-BAND
   s" hb-x64-kernel-addr-header" TMP-PATH BUILD-ADDR-HEADER
   true s" hb-x64-kernel-xt-store-armed" TMP-PATH BUILD-BAND-CELL
   false s" hb-x64-kernel-mark-armed" TMP-PATH BUILD-BAND-CELL
   s" hb-x64-kernel-mark-task-armed" TMP-PATH BUILD-BAND-TASK
   s" hb-x64-kernel-tier" TMP-PATH BUILD-TIER
   s" hb-x64-kernel-scope" TMP-PATH BUILD-SCOPE
   s" hb-x64-kernel-tier-zero" TMP-PATH BUILD-TIER-ZERO
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
   s" snap-rebase" s" hb-x64-kernel-snap-rebase" TMP-PATH BUILD-CALL
   X64HARNESS:DISPOSE
   T-REPORT ;

;using   \ X64LAYOUT
;package

X64K-ENGINE:RUN
