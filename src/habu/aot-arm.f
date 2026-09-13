\ aot-arm.f — the one writer of the engine's capture-window cells.
\
\ The engine keeps two cells naming the AOT capture window that is currently open:
\ src/habu/layout.f AOT-WINDOW:D0-CELL (its first DATA address) and
\ AOT-WINDOW:B0-CELL (its first code address). habu2.f reads them in the
\ compile-mode inliner (AOT-WINDOW:EMIT-OUTSIDE) so a body holding an address the
\ window cannot describe is CALLED rather than copied. Arming them is a two-cell
\ write, and the two cells only mean anything together, so exactly one word does
\ it. A second writer is drift: half a window is a window whose inliner declines
\ against one axis and copies against the other.
\
\ WHY THAT WORD DOES NOT LIVE IN src/habu/aot-capture.f. The chain capture runs
\ inside a booted bin/hb and its window opens at the FIRST USER TOKEN — before
\ anything the capture tool needs has loaded. Loading aot-capture.f first was
\ tried and refuted by measurement: its own closure requires src/arch/arm64/asm.f
\ and src/arch/arm64/icode.f, the compiler chain requires asm.f too, and `require`
\ is a no-op the second time, so every chain word that calls an asm.f word ends up
\ calling the CAPTURE TOOL's copy of it — a word the booting engine has and no
\ target does. 98 of 18602 call sites, refused by name at aot-capture.f
\ ACAP-SITE-BAND (first: the chain's MASK calling A64ASM's LIMM?). So the arming
\ word has to be loadable with the chain still ahead of it, which means it may
\ depend on src/habu/layout.f and nothing else. That dependency is free where it
\ matters: layout.f is already registered in a booted engine, so requiring it
\ there compiles 0 records, while requiring asm.f compiles 178 (both measured).
\
\ WHO LOADS IT. Two processes, the same way aot-decl.f is loaded by two: the stdin
\ metabuild host compiles it ahead of the driver src/habu/stdin.f whose
\ CAPTURE-REPL calls it, and the chain
\ capture tool requires it in its own prelude behind layout.f alone.
\
\ EVERY CALLER CALLS THIS WORD, not a wrapper. src/habu/aot-capture.f briefly
\ carried a capture-side WINDOW-OPEN that forwarded here; it was deleted, because
\ a second name for a one-writer operation is the one thing that can grow a second
\ body. What this file publishes now is the whole opening and closing of a window
\ (WINDOW-OPEN / WINDOW-OPEN-UNARMED / WINDOW-CLOSE, and SIG-CLOSE for the
\ checker payload's own end, at the foot), so a producer
\ names the moment rather than spelling four cursor reads out; OPEN itself stays
\ public because those three are its only callers and the arming is still one
\ writer. Producers today: stdin.f CAPTURE-REPL, tools/aot-chain-capture.f,
\ test/aot-band-lib.f (which uses the unarmed variant on purpose),
\ test/aot-file-merge.f, and the driver text test/aot-wid-build.f generates.

require src/core/checker-owner-guard.f

package AOT-ARM

\ Raw cell boundary, the same shape aot-capture.f uses for the same two cells:
\ the live DATA base is a `ptr n` and storing a cell through it is what the
\ checker cannot state on its own. Retirement: habu-builder-trust-rows-c5d41af6.
: LIVE ( -- ptr n ) data-base ;
: CELL! ( n ptr n -- ) ! ;

public

\ Arm the window: b0 is the code cursor and d0 the DATA cursor as they stand at
\ the moment the window opens. Both cells are written, always, from this one
\ place. Passing 0 0 disarms — an unarmed window is a real state (the engine then
\ copies pre-window bodies instead of calling them), which test/aot-band-lib.f
\ OPEN-UNARMED uses to put a pre-window DATA literal in front of the capture's
\ DATA audit.
\
\ IT ARMS ON `0 0` AS WELL, and that is the whole point of keeping the two axes
\ apart. Passing 0 0 disarms the INLINER's window - it is what makes the engine
\ copy a pre-window body instead of calling it, which test/aot-band-lib.f
\ OPEN-UNARMED needs to put a pre-window DATA literal in front of the capture's
\ DATA audit.
: OPEN ( n n -- ) {: b0:n d0:n :}
   d0 LIVE AOT-WINDOW:D0-CELL + CELL!
   b0 LIVE AOT-WINDOW:B0-CELL + CELL! ;

\ The engine's next wordlist id. It is read here, beside the window's other base
\ cursors, because every producer needs it at the same two moments they need those
\ - when the window opens and when it closes.
: WIDN ( -- n ) LIVE WIDN-CELL + @ ;

\ ---- the window's four coordinates -------------------------------------------
\
\ A CAPTURE WINDOW IS FOUR SPANS, NOT ONE, and every producer needs all four at
\ the same two moments. Four of them wrote the pair of latch lines out by hand -
\ the stdin driver, the chain capture tool, the band fixture and the merge
\ fixture - and a fifth generated the text. Latching three of the four, or
\ reading one of them at some other moment, is a silent wrong capture rather than
\ a refusal: the wordlist counter especially, because a capture tool's own
\ tooling opens packages AFTER the window closes, so a WIDN read at capture time
\ answers for the tool and not for the window.
variable B0  variable B1      \ the window's code span
variable R0  variable R1      \ its dictionary record span
variable D0  variable D1      \ its DATA span
variable W0  variable W1      \ its wordlist span

\ A payload belongs to one concrete checker instance for the whole window.
PTR-VARIABLE PAYLOAD-OWNER
variable PAYLOAD-MODE                       \ 0 pending, 1 partial, 2 complete runtime
variable PAYLOAD-FROZEN
variable PAYLOAD-EXPORTED

private

: PAYLOAD-BAD ( -- )
   s" aot-arm: missing, changed or unfrozen checker payload owner" 74 die ;

: SOURCE-OWNER ( -- ptr u8 )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;

: OWNER! ( ptr u8 -- )
   CHECKER-OWNER-ABI:BYTES CHECKER-OWNER-GUARD:VALIDATE
   dup SOURCE-OWNER <> if drop PAYLOAD-BAD then
   PAYLOAD-OWNER ! ;

: PAYLOAD-FIELD ( n -- n )
   PAYLOAD-OWNER @ over CELL + CHECKER-OWNER-GUARD:VALIDATE
   swap + CELL-VIEW @ dup 0= if drop PAYLOAD-BAD then ;

TRUSTED: AS-ACTION ( n -- [ -- ] ) ;
TRUSTED: AS-LOOKUP ( n -- [ ptr u8 n bool ptr u8 n -- n bool ] ) ;
TRUSTED: AS-SPANS ( n -- [ -- ptr u8 n ptr u8 n ] ) ;
TRUSTED: AS-SAVE ( n -- [ ptr u8 n -- n ] ) ;

: RUN-ACTION ( n -- ) PAYLOAD-FIELD AS-ACTION execute ;

: CANCEL ( -- )
   PAYLOAD-MODE @ 1 = if
      CHECKER-OWNER-ABI:PAYLOAD-DISARM-OFF RUN-ACTION then
   0 PAYLOAD-MODE !  0 PAYLOAD-FROZEN ! 0 PAYLOAD-EXPORTED !
   NULL-PTR PAYLOAD-OWNER ! ;

public

: PAYLOAD-ARM ( ptr u8 -- )
   CANCEL OWNER!
   1 PAYLOAD-MODE !
   CHECKER-OWNER-ABI:PAYLOAD-ARM-OFF RUN-ACTION ;

: PAYLOAD-PERSISTENT ( ptr u8 -- )
   CANCEL OWNER!
   2 PAYLOAD-MODE ! ;

: SIG-CLOSE ( -- )
   PAYLOAD-FROZEN @ 0= 0= if exit then
   PAYLOAD-MODE @ 0= if PAYLOAD-BAD then
   PAYLOAD-OWNER @ SOURCE-OWNER <> if PAYLOAD-BAD then
   PAYLOAD-MODE @ 1 = if
      CHECKER-OWNER-ABI:PAYLOAD-FREEZE-OFF RUN-ACTION then
   -1 PAYLOAD-FROZEN ! ;

: ?FROZEN ( -- )
   PAYLOAD-FROZEN @ 0= if PAYLOAD-BAD then
   PAYLOAD-OWNER @ SOURCE-OWNER <> if PAYLOAD-BAD then ;

: PAYLOAD-LOOKUP ( ptr u8 n bool ptr u8 n -- n bool )
   ?FROZEN
   CHECKER-OWNER-ABI:PAYLOAD-LOOKUP-OFF PAYLOAD-FIELD AS-LOOKUP execute ;

: PAYLOAD-SPANS ( -- ptr u8 n ptr u8 n )
   ?FROZEN
   CHECKER-OWNER-ABI:PAYLOAD-SPANS-OFF PAYLOAD-FIELD AS-SPANS execute ;

: PAYLOAD-REG-SAVE ( ptr u8 n -- n )
   ?FROZEN
   CHECKER-OWNER-ABI:PAYLOAD-REG-SAVE-OFF PAYLOAD-FIELD AS-SAVE execute ;

private

: LATCH-OPEN ( -- )
   CANCEL
   cp@ B0 ! ndict@ R0 ! here D0 ! WIDN W0 ! ;

public

\ Open the window: latch the four cursors and arm the inliner against the two
\ the engine keeps cells for.
: WINDOW-OPEN ( -- )
   LATCH-OPEN
   B0 @ D0 @ OPEN
   SOURCE-OWNER PAYLOAD-ARM ;

\ The same window with the engine told nothing, so the inliner's decline never
\ fires and a copied pre-window body keeps its address - the only way to put a
\ pre-window DATA literal in front of the capture's DATA audit
\ (test/aot-band-lib.f). The four coordinates are latched either way: an unarmed
\ window is still a window being captured.
: WINDOW-OPEN-UNARMED ( -- )
   LATCH-OPEN
   0 0 OPEN
   SOURCE-OWNER PAYLOAD-ARM ;

: WINDOW-OPEN-PERSISTENT ( -- )
   LATCH-OPEN
   B0 @ D0 @ OPEN ;

\ Where the window's definitions end. The wordlist counter is latched HERE for
\ the reason above, and never read again at capture time.
: WINDOW-CLOSE ( -- )
   cp@ B1 ! ndict@ R1 ! here D1 ! WIDN W1 !
   SIG-CLOSE ;

\ The window as aot-capture.f CAPTURE takes it. One reader, so a caller cannot
\ hand the six in a different order than the next caller does.
: WINDOW$ ( -- n n n n n n )
   B0 @ B1 @  R0 @ R1 @  D0 @ D1 @ ;

;package
