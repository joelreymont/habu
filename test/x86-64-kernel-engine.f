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
\ and compares its status and its fd-1 and fd-2 bytes (docs/x86-64.md
\ "Engine-state rows"):
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
\   code window's edges and exits 0;
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
\ hb-x64-kernel-origin asks code-origin about the kernel's text and about spans
\ code-publish wrote, each with its int3 fill to the next code slot, and
\ -origin-patch about them once patch32 rewrote a word; both exit 0.
\ -origin-fill moves CP back over a native span and publishes a shorter one
\ bare there, and finds the bare span's fill unknown; it exits 0.
\ -origin-full publishes into a full table and exits 101 with its fd-2 text.
\ The host checks each image's ELF header; running them is the peer's.
require src/habu/snapshot-format.f
require src/habu/code-origin-x64.f
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
\ `0 set-check` empties it so another installs.
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

\ ndict-append counts the pending record and then, with a does> body pending,
\ its companion one record past it; the index learns each as it lands.
: BUILD-APPEND ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   PENDING,
   5 APPEND,  6 NDICT?,
   s" delta" 0 SEARCH,  6 X64HARNESS:EXPECT-POP,
   1 DOESB-CELL X64HARNESS:CELL!,
   s" echo" 0 0 X64HARNESS:RECORD,  6 NDICT!,
   6 APPEND,  7 NDICT?,
   s" echo" 0 SEARCH,  7 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ The companion's place with no does> body pending exits 83.
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

\ With the latch sealed, xt! stores into a scratch cell and ptr-cell-mark
\ admits one.
: BUILD-XT-STORE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   SEAL,
   CELL-VALUE X64HARNESS:PUSH,  0 X64HARNESS:PUSH-SCRATCH,  s" xt!" ROW
   CELL-VALUE 0 X64HARNESS:EXPECT-SCRATCH,
   CELL X64HARNESS:PUSH-SCRATCH,  s" ptr-cell-mark" ROW
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ With the latch sealed, the row aimed at the band cell TIER-PROV:N-CELL exits
\ 83 before it stores: xt! when the flag is true, ptr-cell-mark otherwise.
: BUILD-BAND-CELL ( bool ptr u8 n -- ) {: store:bool path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   SEAL,
   store if CELL-VALUE X64HARNESS:PUSH, then
   TIER-PROV:N-CELL X64HARNESS:PUSH-DATA,
   store if s" xt!" ROW else s" ptr-cell-mark" ROW then
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ ---- the compiler tier and the build scope -----------------------------------
\ The constant rows, code-origin's unknown answer and tier 1.
: BUILD-TIER ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   s" addr-cells-abi" ROW  0 X64HARNESS:EXPECT-POP,
   s" snapshot-format" ROW  SNAPSHOT-FORMAT:VERSION X64HARNESS:EXPECT-POP,
   0 X64HARNESS:PUSH-SCRATCH,  CELL X64HARNESS:PUSH,  s" code-origin" ROW
   -1 X64HARNESS:EXPECT-POP,
   1 X64HARNESS:PUSH,  s" set-tier" ROW
   s" tier@" ROW  1 X64HARNESS:EXPECT-POP,
   0 X64HARNESS:EXPECT-DEPTH,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ Not a tier: the caller's cell, which only the saved copy can restore.
$5A constant CALLER-TIER

: ENTER, ( -- ) s" executable-build-enter" ROW ;
: LEAVE, ( -- ) s" executable-build-leave" ROW ;
: TIER-CELL?, ( n -- ) NCOMP-DISPATCH:TIER-CELL X64HARNESS:EXPECT-CELL, ;

\ Two nested scopes select tier 1 and save the caller's tier once; the
\ outermost leave restores it and clears the saved copy, and a leave with no
\ scope open changes nothing.
: BUILD-SCOPE ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   CALLER-TIER NCOMP-DISPATCH:TIER-CELL X64HARNESS:CELL!,
   ENTER,  ENTER,
   1 TIER-CELL?,
   2 NCOMP-DISPATCH:BUILD-DEPTH-CELL X64HARNESS:EXPECT-CELL,
   CALLER-TIER NCOMP-DISPATCH:BUILD-TIER-CELL X64HARNESS:EXPECT-CELL,
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

\ ---- code provenance ---------------------------------------------------------
\ The spans the provenance images publish, as offsets into the region: CP
\ starts at DICT-SIZE, each publication appends at CP, and CP moves to the
\ first code slot at or past the span's end, so B, C and D each take a slot
\ whose last eight bytes are int3 fill.
16 constant SPAN-A                     \ native, then patched inside
8 constant SPAN-B                      \ published bare
8 constant SPAN-C                      \ C and D, native side by side
DICT-SIZE constant AT-A
AT-A SPAN-A + constant AT-B            \ A fills its slot
AT-B SLOT-BYTES + constant AT-C
AT-C SLOT-BYTES + constant AT-D
AT-D SLOT-BYTES + constant AT-FREE     \ CP after the four: never published
4 constant PATCH-AT                    \ the word patch32 rewrites, inside A
4 constant WORD-BYTES
$90909090 constant PATCH-WORD

\ Publish n bytes at the region offset, CP, from the scratch cells.
: PUBLISH, ( n n -- ) {: off:n len:n :}
   0 X64HARNESS:PUSH-SCRATCH,  off X64HARNESS:PUSH-REGION,  len X64HARNESS:PUSH,
   s" code-publish" ROW ;

\ The same inside a provenance window that a successful compile closes.
: NATIVE, ( n n -- ) X64PROV:OPEN,  PUBLISH,  1 X64PROV:CLOSE, ;

\ Check code-origin's answer for the region offsets [lo, hi).
: EXPECT-ORIGIN, ( n n n -- ) {: want:n lo:n hi:n :}
   lo X64HARNESS:PUSH-REGION,  hi X64HARNESS:PUSH-REGION,  s" code-origin" ROW
   want X64HARNESS:EXPECT-POP, ;

\ Rewrite the word at the region offset through patch32.
: PATCH, ( n -- ) {: off:n :}
   PATCH-WORD X64HARNESS:PUSH,  off X64HARNESS:PUSH-REGION,  s" patch32" ROW ;

\ Publish the four spans: A, and C and D side by side, inside windows a
\ successful compile closes, and B bare between them.
: SPANS, ( -- )
   AT-A SPAN-A NATIVE,  AT-B SPAN-B PUBLISH,
   AT-C SPAN-C NATIVE,  AT-D SPAN-C NATIVE, ;

\ The kernel's text is native from the boot on. A span published inside a
\ closed window is native, one published bare is unknown, and so is one never
\ published; each span's fill takes its origin. C and D share one row, so the
\ count holds the text's row, A's, B's and theirs.
: BUILD-ORIGIN ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   s" code-origin" X64KERNEL:ENTRY-LABEL {: entry:label :}
   entry 0 X64HARNESS:PUSH-LABEL,  entry 1 X64HARNESS:PUSH-LABEL,
   s" code-origin" ROW  1 X64HARNESS:EXPECT-POP,
   SPANS,
   1 AT-A AT-B EXPECT-ORIGIN,
   -1 AT-B AT-C EXPECT-ORIGIN,
   1 AT-C AT-FREE EXPECT-ORIGIN,
   4 TIER-PROV:N-CELL X64HARNESS:EXPECT-CELL,
   -1 AT-FREE AT-FREE CELL + EXPECT-ORIGIN,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ A patch32 inside A splits its row in three: the patched word turns unknown,
\ the bytes on both sides stay native, and B's and C-D's rows move up intact.
\ A patch32 where no row lies adds none.
: BUILD-ORIGIN-PATCH ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   SPANS,
   AT-A PATCH-AT + PATCH,
   1 AT-A AT-A PATCH-AT + EXPECT-ORIGIN,
   -1 AT-A PATCH-AT + AT-A PATCH-AT + WORD-BYTES + EXPECT-ORIGIN,
   1 AT-A PATCH-AT + WORD-BYTES + AT-B EXPECT-ORIGIN,
   -1 AT-B AT-C EXPECT-ORIGIN,
   1 AT-C AT-FREE EXPECT-ORIGIN,
   AT-FREE CELL + PATCH,
   6 TIER-PROV:N-CELL X64HARNESS:EXPECT-CELL,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ The fill takes its span's origin. Over a 32-byte native span, cp! back to its
\ start and an 8-byte bare publication leave its first slot unknown, the fill
\ [dst+8, dst+16) included, and its second slot native.
32 constant SPAN-WIDE

: BUILD-ORIGIN-FILL ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   AT-A SPAN-WIDE NATIVE,
   AT-A CP!,
   AT-A SPAN-B PUBLISH,
   -1 AT-A SPAN-B + AT-A SLOT-BYTES + EXPECT-ORIGIN,
   1 AT-A SLOT-BYTES + AT-A SPAN-WIDE + EXPECT-ORIGIN,
   0 X64HARNESS:EXPECT-DEPTH,
   X64HARNESS:EXPECT-BALANCED,
   path pathu X64HARNESS:BOOT-CLOSE, ;

\ A table of SPANS rows takes no more: the publication's row exits
\ CODE-ORIGIN-FULL with the ARM64 text on fd 2.
: BUILD-ORIGIN-FULL ( ptr u8 n -- ) {: path:ptr pathu:n :}
   false X64HARNESS:BOOT-OPEN,
   TIER-PROV:SPANS TIER-PROV:N-CELL X64HARNESS:CELL!,
   AT-A SPAN-B PUBLISH,
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
   true s" hb-x64-kernel-xt-store-armed" TMP-PATH BUILD-BAND-CELL
   false s" hb-x64-kernel-mark-armed" TMP-PATH BUILD-BAND-CELL
   s" hb-x64-kernel-tier" TMP-PATH BUILD-TIER
   s" hb-x64-kernel-scope" TMP-PATH BUILD-SCOPE
   s" hb-x64-kernel-tier-zero" TMP-PATH BUILD-TIER-ZERO
   s" hb-x64-kernel-origin" TMP-PATH BUILD-ORIGIN
   s" hb-x64-kernel-origin-patch" TMP-PATH BUILD-ORIGIN-PATCH
   s" hb-x64-kernel-origin-fill" TMP-PATH BUILD-ORIGIN-FILL
   s" hb-x64-kernel-origin-full" TMP-PATH BUILD-ORIGIN-FULL
   s" snap-rebase" s" hb-x64-kernel-snap-rebase" TMP-PATH BUILD-CALL
   X64HARNESS:DISPOSE
   T-REPORT ;

;using   \ X64LAYOUT
;package

X64K-ENGINE:RUN
