\ Capture cursors and the checker payload share one opening/closing operation.
\ Load this before the captured source so the capture tool's own dependencies
\ cannot become unintended call targets in the artifact.

require src/core/checker-owner-guard.f

package AOT-ARM

: LIVE ( -- ptr n ) data-base ;
: CELL! ( n ptr n -- ) ! ;

public

\ Write the engine's window coordinates. Tier 0 folds a created word's address
\ into its user only when the word's entry is at or above B0 (habu2.f
\ EM-COMPILE-CALL), so a window word calls a pre-window created word, as the
\ capture requires. `0 0` folds every one; WINDOW-CLOSE writes it, so no
\ window's base outlives the window.
: OPEN ( n n -- ) {: b0:n d0:n :}
   d0 LIVE AOT-CELLS:D0-CELL + CELL!
   b0 LIVE AOT-CELLS:B0-CELL + CELL! ;

\ The engine's next wordlist id. It is read here, beside the window's other base
\ cursors, because every producer needs it at the same two moments they need those
\ - when the window opens and when it closes.
: WIDN ( -- n ) LIVE WIDN-CELL + @ ;

\ `here` as an integer, which is what the window's other three cursors already
\ are: cp@, ndict@ and the wordlist counter are numeric primitives and `here` is
\ the one pointer among the four, while every reader of the DATA coordinate -
\ the capture's scans, the band audit, the literal-span check - compares and
\ differences it as a number and never follows it. DATA is mapped at the fixed
\ DATA-VA, so the offset from data-base plus that base IS the address; the
\ subtraction is ordinary checked pointer arithmetic, so this needs no trust row
\ and no pointer-to-integer cast. src/habu/aot-closure.f carries the same
\ one-liner for package AOT-LINK, which the stripped linker loads without this
\ file.
: HERE-N ( -- n ) here BYTE-VIEW data-base BYTE-VIEW - DATA-VA VA>N + ;

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

\ THE WINDOW OPENS ON A WHOLE CELL, the way `create` publishes one: `align`
\ before D0 is latched. A capture describes its DATA as one presence bit and one
\ value per grid cell (src/habu/aot-decl.f package AOT-WINDOW), every declared
\ address cell must fall on one of them, and a merge splices two windows' grids
\ (src/habu/aot-file.f ?BASES), so the base is the grid's origin. `allot` never
\ realigns, so the DP an opener meets is whatever the last buffer left: a
\ PATH-CAP path buffer or a SHA256-FILE-CTX-BYTES context is an odd byte count,
\ and an app image restores its exact cursor. The pad lies below the window and
\ never travels; an opener whose DP is already aligned gets no pad, so a build
\ that met an aligned DP writes the same bytes as before.
: LATCH-OPEN ( -- )
   CANCEL
   align
   cp@ B0 ! ndict@ R0 ! HERE-N D0 ! WIDN W0 ! ;

public

\ Latch all four spans and bind the payload to this checker owner.
: WINDOW-OPEN ( -- )
   LATCH-OPEN
   B0 @ D0 @ OPEN
   SOURCE-OWNER PAYLOAD-ARM ;

: WINDOW-OPEN-PERSISTENT ( -- )
   LATCH-OPEN
   B0 @ D0 @ OPEN ;

\ Where the window's definitions end. The wordlist counter is latched HERE for
\ the reason above, and never read again at capture time.
: WINDOW-CLOSE ( -- )
   cp@ B1 ! ndict@ R1 ! HERE-N D1 ! WIDN W1 !
   0 0 OPEN
   SIG-CLOSE ;

\ The window as aot-capture.f CAPTURE takes it. One reader, so a caller cannot
\ hand the six in a different order than the next caller does.
: WINDOW$ ( -- n n n n n n )
   B0 @ B1 @  R0 @ R1 @  D0 @ D1 @ ;

;package
