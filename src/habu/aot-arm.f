\ Capture cursors and the checker payload share one opening/closing operation.
\ Load this before the captured source so the capture tool's own dependencies
\ cannot become unintended call targets in the artifact.

require src/core/checker-owner-guard.f

package AOT-ARM

: LIVE ( -- ptr n ) data-base ;
: CELL! ( n ptr n -- ) ! ;

public

\ Write the engine's window code base. Tier 0 folds a created word's address
\ into its user only when the word's entry is at or above B0 (habu2.f
\ EM-COMPILE-CALL), so a window word calls a pre-window created word, as the
\ capture requires. `0` folds every one; WINDOW-CLOSE writes it, so no
\ window's base outlives the window.
: OPEN ( n -- ) {: b0:n :}
   b0 LIVE AOT-CELLS:B0-CELL + CELL! ;

\ The engine's next wordlist id, read beside the window's other cursors: every
\ producer needs it when the window opens and when it closes.
: WIDN ( -- n ) LIVE WIDN-CELL + @ ;

\ `here` as an integer, like the other three cursors: every reader of the DATA
\ coordinate compares and differences it, never follows it. DATA is at the fixed
\ DATA-VA, so this is checked pointer arithmetic with no trust row or cast.
\ src/habu/aot-closure.f carries the same one-liner for package AOT-LINK, which
\ the stripped linker loads without this file.
: HERE-N ( -- n ) here BYTE-VIEW data-base BYTE-VIEW - DATA-VA VA>N + ;

\ Every producer latches all four spans at the same two moments: latching three,
\ or one at another moment, is a silent wrong capture, not a refusal. A capture
\ tool opens packages after the window closes, so a WIDN read at capture time
\ answers for the tool, not the window.
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

\ The payload owner record stores raw execution tokens; each view states the
\ signature of the field it calls.
CAST: AS-ACTION ( n -- [ -- ] )
CAST: AS-LOOKUP ( n -- [ ptr u8 n bool ptr u8 n -- n bool ] )
CAST: AS-SPANS ( n -- [ -- ptr u8 n ptr u8 n ] )
CAST: AS-SAVE ( n -- [ ptr u8 n -- n ] )

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

\ The window opens on a whole cell, as `create` publishes one: a capture's DATA
\ is one presence bit and value per grid cell (src/habu/aot-decl.f AOT-WINDOW),
\ every declared address cell falls on one, and a merge splices two windows'
\ grids (src/habu/aot-file.f ?BASES), so the base is the grid's origin. `allot`
\ never realigns and an app image restores its exact cursor, so an opener's DP
\ may be odd. The pad lies below the window and never travels.
: LATCH-OPEN ( -- )
   CANCEL
   align
   cp@ B0 ! ndict@ R0 ! HERE-N D0 ! WIDN W0 ! ;

public

: WINDOW-OPEN ( -- )
   LATCH-OPEN
   B0 @ OPEN
   SOURCE-OWNER PAYLOAD-ARM ;

: WINDOW-OPEN-PERSISTENT ( -- )
   LATCH-OPEN
   B0 @ OPEN ;

\ Where the window's definitions end. The wordlist counter is latched HERE for
\ the reason above, and never read again at capture time.
: WINDOW-CLOSE ( -- )
   cp@ B1 ! ndict@ R1 ! HERE-N D1 ! WIDN W1 !
   0 OPEN
   SIG-CLOSE ;

\ The window as aot-capture.f CAPTURE takes it. One reader, so a caller cannot
\ hand the six in a different order than the next caller does.
: WINDOW$ ( -- n n n n n n )
   B0 @ B1 @  R0 @ R1 @  D0 @ D1 @ ;

;package
