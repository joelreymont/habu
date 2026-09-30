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
   X64HARNESS:DISPOSE
   T-REPORT ;

;package

X64K-ENGINE:RUN
