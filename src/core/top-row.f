\ top-row.f - tier-1 top-level row tracker (warnings only).
\ dot habu-typed-top-checker-82cf8b84 ; docs/typed-top-level.md §5 sub-dot 3.
\
\ The engine emits one per-token event to the installed top-row hook (habu2.f
\ LTOPHOOK, dot habu-typed-top-engine-2b2e88aa): ( token-a token-u class flags ).
\ This file keeps ONE live checker-side row shadowing the interpret data stack
\ and warns (rc unchanged, stderr) on the value/xt residuals a depth floor
\ cannot reach (docs §1 p1/p2/p3): an xt executed/caught below its target arity,
\ a non-xt handed to execute/catch, and a byte pointer flowing into a scalar
\ consumer. `depth` is the ground-truth anchor: every event resyncs row length to
\ live depth, so a sig-less word, a throw-recovery, or an unmodeled step degrades
\ to honest gray cells (depth still enforced) instead of a false warning.
\
\ Tier-1 scope: value knowledge is a coarse family (scalar / byte-pointer / xt /
\ gray) carried from literals, ticks and the pointer-arithmetic family only; every
\ other word grays its outputs. Tier-1.5 (this file) reads a certified word's
\ din/dout arity + families through the checker's minimal effect-read export API
\ (src/core/checker.f EFFECT-QUERY / EFFECT-DIN-N / EFFECT-DOUT-N / EFFECT-DIN-FAM /
\ EFFECT-DOUT-FAM, dot habu-expose-checker-effect-95e853eb) - used here only for the
\ zero-risk pure-consumer precise pop below. Full effect-record row unification with
\ precise OUTPUT family propagation (real per-word din/dout apply) is tier-2, dot
\ habu-typed-top-tier-589c550f. This file loads from the checkout at boot; the raw
\ effect-store internals stay hidden behind the export API's coarse family projection.

package TOP-ROW

\ ---- event-class ABI (src/habu/layout.f TOP-EV-*, loaded after this file) -----
1 constant TR-EV-NUM     2 constant TR-EV-STR     3 constant TR-EV-CSTR
4 constant TR-EV-CHAR    5 constant TR-EV-TICK    6 constant TR-EV-WORD

\ ---- value families ----------------------------------------------------------
0 constant TR-GRAY       \ unknown: unifies with any consumer, never warns
1 constant TR-SCALAR     \ a value cell (number / char / bool / address int)
2 constant TR-POINTER    \ a byte / region pointer (s" S\" c" and ptr arithmetic)
3 constant TR-XT         \ an execution token pushed by ' (tick)

\ ---- flags ABI (LFIND: bit0 found, bit1 DNAME-IMM, bits 8-15 DNAME-MIN-IN) ----
8 constant TR-MININ-SHIFT
255 constant TR-MININ-MASK

: TR-FLAGS>XIN ( n -- n )    \ certified-word declared input arity from LFIND flags
   TR-MININ-SHIFT rshift TR-MININ-MASK and ;

\ ---- persistent row state (var-free; survives the checker's per-check NEW) ----
256 constant TR-CAP
create TR-CLS TR-CAP cells allot        \ family per stack cell, index 0 = bottom
create TR-XIN TR-CAP cells allot        \ ticked-xt declared input arity (TR-XT cells)
variable TR-N                           \ tracked row depth (cells)
variable TR-DIRTY                       \ a step left the row unknown: reseed next event
variable TR-SUSP                        \ inside a 0 set-check window: enforcement suspended
variable TR-LASTZERO                    \ previous event was the literal 0 (0 set-check latch)

: TR-CLS@ ( n -- n ) cells TR-CLS + @ ;
: TR-CLS! ( n n -- ) cells TR-CLS + ! ;
: TR-XIN@ ( n -- n ) cells TR-XIN + @ ;
: TR-XIN! ( n n -- ) cells TR-XIN + ! ;

: TR-CLAMP ( n -- n )                   \ hold the tracked window inside [0, TR-CAP]
   dup 0 < if drop 0 exit then
   dup TR-CAP > if drop TR-CAP exit then ;

: TR-TOP ( -- n )                       \ family of the top cell (GRAY if empty/over-cap)
   TR-N @ dup 1 < if drop TR-GRAY exit then
   dup TR-CAP > if drop TR-GRAY exit then
   1 - TR-CLS@ ;

: TR-TOP-XIN ( -- n )                   \ declared input arity of the top xt cell
   TR-N @ dup 1 < if drop 0 exit then
   dup TR-CAP > if drop 0 exit then
   1 - TR-XIN@ ;

: TR-RESEED ( n -- )                    \ row := n gray cells (honest resync to live depth)
   TR-CLAMP TR-N !
   0 begin dup TR-N @ < while
      TR-GRAY over TR-CLS!  0 over TR-XIN!
      1 +
   repeat drop
   0 TR-DIRTY ! ;

: TR-PUSH ( n -- )                      \ push one cell of the given family (gray xin)
   TR-N @ dup TR-CAP < if
      swap over TR-CLS!  0 over TR-XIN!  1 + TR-N !
   else 2drop then ;

: TR-PUSH-XT ( n -- )                   \ push an xt cell carrying its target input arity
   TR-N @ dup TR-CAP < if
      TR-XT over TR-CLS!  swap over TR-XIN!  1 + TR-N !
   else 2drop then ;

: TR-POP ( -- )                         \ drop the top cell (never below base)
   TR-N @ dup 0 > if 1 - TR-N ! else drop then ;

\ ---- tier mode: warn (tier 1) vs reject (tier 2) -----------------------------
\ dot habu-typed-top-tier-589c550f (sub-dot 7). The mode cell defaults to tier-1
\ (warn, rc unchanged); HABU_TOP_TIER=2 stages tier-2, where every tier-1 warning
\ site instead REJECTS pre-execution with a catchable rc-70 throw (TR-REJECT-RC,
\ the same RC-REJECT the native DNAME-MIN-IN underdepth reject uses). The reject
\ fires from the WORD event, which the engine emits PRE-BLR (habu2.f
\ C-TOPHOOK-CALL), so the word never runs, the interpret stack is untouched, and
\ REPL/evaluate recovery reseeds the row from live depth at the next event (docs
\ §2.3). The 0 set-check window (TR-SUSP) still suspends ALL enforcement - inside
\ the audited escape window tier-2 degrades to silence (docs §2.4), never a reject.
\ Snapshot boots keep the hook disarmed (snap-lib.f SND-ZERO-LIVE), so tier-2, like
\ tier-1, is cold-boot only until the snapshot re-arm lands (habu-snapshot-rebase-
\ persisted-4bd33351 / docs §5.6); a re-armed snapshot must re-read TR-STAGED-TIER.
\ Trusted sites are the raw effect query and dynamic top-row hook install described
\ below. Retirement: habu-checker-self-typing-9ff8ba86 and
\ cap:checker-hook-identity, respectively.
1 constant TR-TIER1                      \ warn only, rc unchanged (default)
2 constant TR-TIER2                      \ reject pre-execution (rc 70 diagnostic)
70 constant TR-REJECT-RC                 \ RC-REJECT: catchable, rc 70 uncaught
variable TR-MODE                         \ TR-TIER1/TR-TIER2, staged at TR-INSTALL

: TR-TIER2? ( -- bool ) TR-MODE @ TR-TIER2 = ;
: TR-REJECT ( -- )                       \ tier-2: reject pre-execution after the diagnostic
   TR-TIER2? if TR-REJECT-RC throw then ;

\ ---- warning renderer (stderr; rc and execution unchanged at tier 1) ---------
2 constant TR-FD

: TR-FAM. ( n -- )
   dup TR-SCALAR  = if drop TR-FD s" n"   write drop exit then
   dup TR-POINTER = if drop TR-FD s" ptr" write drop exit then
   dup TR-XT      = if drop TR-FD s" xt"  write drop exit then
   drop TR-FD s" ?" write drop ;

: TR-WARN ( ptr u8 n n n -- )           \ hb: top-row: <tok> expected: <e> actual: <a>
   {: a:ptr u:n exp:n act:n :}
   TR-FD s" hb: top-row: " write drop
   TR-FD a u write drop
   TR-FD s"  expected: " write drop  exp TR-FAM.
   TR-FD s"  actual: " write drop     act TR-FAM.
   TR-FD S\" \n" write drop
   TR-REJECT ;

: TR-WARN-UNDER ( ptr u8 n -- )         \ xt executed/caught below its target arity
   {: a:ptr u:n :}
   TR-FD s" hb: top-row: " write drop
   TR-FD a u write drop
   TR-FD S\" : xt target underflows the interpret stack\n" write drop
   TR-REJECT ;

\ ---- pointer-arithmetic family propagation -----------------------------------
\ + / - keep byte-pointer identity so a downstream scalar consumer still sees it
\ (docs §1 p3). ptr - ptr is a scalar difference; one ptr operand stays a ptr.
: TR-ANY-PTR? ( n n -- bool ) TR-POINTER = swap TR-POINTER = or ;
: TR-BOTH-PTR? ( n n -- bool ) TR-POINTER = swap TR-POINTER = and ;

: TR-ADD-FAM ( n n -- n ) TR-ANY-PTR? if TR-POINTER else TR-SCALAR then ;
: TR-SUB-FAM ( n n -- n )
   2dup TR-BOTH-PTR? if 2drop TR-SCALAR exit then
   TR-ANY-PTR? if TR-POINTER else TR-SCALAR then ;

\ ---- word classifiers (tier-1 targeted residual set) -------------------------
: TR-XT-CONSUMER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" execute" CORE-STR=
   a u s" catch" CORE-STR= or
   a u s" run-in-stack" CORE-STR= or ;

: TR-SCALAR-CONSUMER? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   a u s" ." CORE-STR=
   a u s" u." CORE-STR= or ;

: TR-ARITH? ( ptr u8 n -- n )           \ 1 = '+', 2 = '-', 0 = neither
   {: a:ptr u:n :}
   a u s" +" CORE-STR= if 1 exit then
   a u s" -" CORE-STR= if 2 exit then
   0 ;

\ ---- per-class row transitions -----------------------------------------------
: TR-LIT-CELLS ( n -- n )               \ cells a literal event pushed before firing
   dup TR-EV-STR = if drop 2 exit then
   drop 1 ;

: TR-LIT ( n -- )                       \ push the cell(s) a literal event leaves
   dup TR-EV-NUM  = if drop TR-SCALAR TR-PUSH exit then
   dup TR-EV-CHAR = if drop TR-SCALAR TR-PUSH exit then
   dup TR-EV-CSTR = if drop TR-POINTER TR-PUSH exit then
   dup TR-EV-STR  = if drop TR-POINTER TR-PUSH TR-SCALAR TR-PUSH exit then
   drop ;

: TR-XT-STEP ( ptr u8 n n -- ) {: a:ptr u:n live:n :}
   TR-TOP {: top:n :}
   top TR-XT = if
      live 1 - TR-TOP-XIN < if a u TR-WARN-UNDER then
   else top TR-GRAY <> if
      a u TR-XT top TR-WARN
   then then
   1 TR-DIRTY ! ;                       \ target effect is dynamic: gray-resync next event

: TR-SCALAR-STEP ( ptr u8 n -- ) {: a:ptr u:n :}   \ ( x -- )
   TR-TOP {: top:n :}
   top TR-POINTER = top TR-XT = or if a u TR-SCALAR top TR-WARN then
   TR-POP ;

: TR-ARITH-STEP ( n -- )                \ kind: 1 = '+', 2 = '-' ; ( a b -- c )
   TR-N @ 2 < if drop 1 TR-DIRTY ! exit then
   TR-N @ 1 - TR-CLS@ {: t0:n :}        \ top operand
   TR-N @ 2 - TR-CLS@ {: t1:n :}        \ second operand
   1 = if t0 t1 TR-ADD-FAM else t0 t1 TR-SUB-FAM then {: r:n :}
   TR-POP TR-POP r TR-PUSH ;

\ ---- tier-1.5: certified pure-consumer precise pop (checker effect-read API) --
\ A certified word whose dout row has NO fixed terms (dout empty) consumes exactly
\ its declared physical din and leaves the tail untouched. Reading that from the
\ checker's effect-read export API (src/core/checker.f EFFECT-QUERY / EFFECT-DOUT-N,
\ dot habu-expose-checker-effect-95e853eb) lets the tracker pop the din precisely and
\ KEEP the tail's precise families, instead of graying the whole row at the next
\ event. Same warning gates; better shadow fidelity across pure consumers. The query
\ reads raw effect-store state the checker cannot type, so it sits behind a TRUSTED:
\ boundary - the same idiom TR-INSTALL uses. Precise OUTPUT family propagation and row
\ unification are tier-2, dot habu-typed-top-tier-589c550f.
TRUSTED: TR-CERT-DOUT-EMPTY? ( ptr u8 n -- bool )   \ certified word producing no fixed outputs?
   EFFECT-QUERY if EFFECT-DOUT-N 0= else 0 0= 0= then ;

: TR-CERT-STEP ( n -- ) {: din:n :}     \ pop din cells; row stays precise (no dirty resync)
   0 begin dup din < while TR-POP 1 + repeat drop
   0 TR-DIRTY ! ;

: TR-WORD ( ptr u8 n n n n -- ) {: a:ptr u:n flg:n live:n lz:n :}
   TR-TOP TR-GRAY =  lz 0=  and if 1 TR-DIRTY ! exit then  \ fast path: nothing definite to check
   a u s" set-check" CORE-STR= if
      lz 0 <> if 1 TR-SUSP ! TR-POP then exit
   then
   a u TR-XT-CONSUMER? if a u live TR-XT-STEP exit then
   a u TR-SCALAR-CONSUMER? if a u TR-SCALAR-STEP exit then
   a u TR-ARITH? dup 0 <> if TR-ARITH-STEP exit then drop
   a u TR-CERT-DOUT-EMPTY? if flg TR-FLAGS>XIN TR-CERT-STEP exit then
   1 TR-DIRTY ! ;                       \ unmodeled word: gray-resync at the next event

\ ---- the installed hook ------------------------------------------------------
: TR-EVENT ( ptr u8 n n n n n -- ) {: a:ptr u:n cls:n flg:n live:n lz:n :}
   TR-SUSP @ 0 <> if                    \ 0 set-check window: enforcement off
      cls TR-EV-WORD = a u s" set-check" CORE-STR= and lz 0= and if
         live TR-RESEED  0 TR-SUSP !    \ re-arm on a real hook xt: reseed from live depth
      then
      exit
   then
   cls TR-EV-TICK = if
      TR-DIRTY @ 0 <>  TR-N @ live 1 - <>  or if live 1 - TR-RESEED then
      flg TR-FLAGS>XIN TR-PUSH-XT       \ push xt below-synced, carrying target arity
      exit
   then
   cls TR-EV-WORD = if
      TR-DIRTY @ 0 <>  TR-N @ live <>  or if live TR-RESEED then
      a u flg live lz TR-WORD
      exit
   then
   cls TR-LIT-CELLS {: pc:n :}          \ literal: value already pushed; resync prior cells
   TR-DIRTY @ 0 <>  TR-N @ live pc - <>  or if live pc - TR-RESEED then
   cls TR-LIT ;

: TR-HOOK ( ptr u8 n n n -- ) {: a:ptr u:n cls:n flg:n :}
   depth {: live:n :}
   TR-LASTZERO @ {: lz:n :}
   cls TR-EV-NUM = a u s" 0" CORE-STR= and TR-LASTZERO !
   a u cls flg live lz TR-EVENT ;

: TR-STAGED-TIER ( -- n )               \ HABU_TOP_TIER=2 stages tier 2, else tier 1
   s" HABU_TOP_TIER" GETENV s" 2" CORE-STR= if TR-TIER2 else TR-TIER1 then ;

\ set-top-check is a guarded-deref trust-boundary prim (mirrors check-hook.f's
\ TRUSTED: INSTALL owning ' HOOK set-check); the one-line install is the named,
\ tested boundary the checker cannot express. The audited escape window is the
\ set-top-check install itself: only TR-HOOK may be installed (checked-boundary
\ lint UB-TOP-HOOK-ALLOWED? audit row).
TRUSTED: TR-INSTALL ( -- )
   0 TR-N !  0 TR-DIRTY !  0 TR-SUSP !  0 TR-LASTZERO !
   TR-STAGED-TIER TR-MODE !
   ['] TR-HOOK set-top-check ;

TR-INSTALL

;package
