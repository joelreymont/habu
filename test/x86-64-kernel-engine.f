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
\ The host checks each image's ELF header; running them is the peer's.
require test/x86-64-boot-harness.f

package X64K-ENGINE

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
   X64HARNESS:DISPOSE
   T-REPORT ;

;package

X64K-ENGINE:RUN
