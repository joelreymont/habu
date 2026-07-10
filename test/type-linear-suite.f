\ type-linear-suite.f — whole-bundle linear accounting for layout values
\ (PLAN item 11, docs/type-families.md §19). Run BY THE ENGINE over stdin:
\     bin/hb < test/type-linear-suite.f
\ A sum whose type args include a linear con is ONE linear unit: constructors
\ consume linear payloads and mint the bundle (empty-payload variants mint
\ without input, the option-NONE shape), pass-through and deeper-row flow
\ conserve it, and every copy/drop/transport/local-capture/unconsumed path
\ rejects (v1: construct + flow only; move-class transports and MATCH
\ consumption land with item 9). Width stays truthful: linear-arg layouts
\ expand to hidden fields exactly like non-linear ones (LAYOUT-ARGS-OPEN?),
\ so checker rows and runtime cells agree.

require test/checker-assert.f

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;

deflinear ltok
SUMTYPE lq2 2
  VARIANT ok  a ;VARIANT
  VARIANT err b ;VARIANT
;SUMTYPE
SUMTYPE lqmix 2
  VARIANT small a ;VARIANT
  VARIANT big a b n ;VARIANT
;SUMTYPE

\ ---------------------------------------------------------------------------
\ accepted: construction, minting, pass-through, deeper-row flow, wrappers.
\ ---------------------------------------------------------------------------
s" A1=" type s" C1 ( ltok -- lq2<ltok,n> ) LQ2:OK" CHECK-QUIET-CANDIDATE! -1 T=
s" A2=" type s" C2 ( n -- lq2<ltok,n> ) LQ2:ERR" CHECK-QUIET-CANDIDATE! -1 T=
s" A3=" type s" C3 ( lq2<ltok,n> -- lq2<ltok,n> )" CHECK-QUIET-CANDIDATE! -1 T=
\ transports beside the bundle stay legal: the reject targets the bundle only.
s" A4=" type s" C4 ( lq2<ltok,n> n -- lq2<ltok,n> ) drop" CHECK-QUIET-CANDIDATE! -1 T=
\ a generic wrapper flows the linear through construction (1-in/1-out var).
: LWRAP ( a -- lq2<a,b> ) LQ2:OK ;
s" A5=" type s" C5 ( ltok -- lq2<ltok,n> ) LWRAP" CHECK-QUIET-CANDIDATE! -1 T=
\ wider payloads: multi-cell construction with a linear arg mints one unit.
s" A6=" type s" C6 ( ltok n n -- lqmix<ltok,n> ) LQMIX:BIG" CHECK-QUIET-CANDIDATE! -1 T=
\ narrow variant with zero padding still consumes the linear payload exactly once.
s" A7=" type s" C7 ( ltok -- lqmix<ltok,n> ) LQMIX:SMALL" CHECK-QUIET-CANDIDATE! -1 T=
cr

\ ---------------------------------------------------------------------------
\ move-class transports (swap/rot/-rot/2swap and >r/2>r round trips) reorder the
\ bundle without copy or drop, so the linear count is CONSERVED -> accept. The
\ whole M+1-cell group moves atomically (XG-READ-HID), and LIN-CHECK conservation
\ classifies: a permutation keeps before=after, so it certifies.
\ ---------------------------------------------------------------------------
s" M1=" type s" M1 ( lq2<ltok,n> n -- n lq2<ltok,n> ) swap" CHECK-QUIET-CANDIDATE! -1 T=
s" M2=" type s" M2 ( lq2<ltok,n> n n -- n n lq2<ltok,n> ) rot" CHECK-QUIET-CANDIDATE! -1 T=
s" M3=" type s" M3 ( n n lq2<ltok,n> -- lq2<ltok,n> n n ) -rot" CHECK-QUIET-CANDIDATE! -1 T=
s" M4=" type s" M4 ( lq2<ltok,n> n n n -- n n lq2<ltok,n> n ) 2swap" CHECK-QUIET-CANDIDATE! -1 T=
s" M5=" type s" M5 ( lq2<ltok,n> -- lq2<ltok,n> ) >r r>" CHECK-QUIET-CANDIDATE! -1 T=
s" M6=" type s" M6 ( lq2<ltok,n> n -- lq2<ltok,n> n ) 2>r 2r>" CHECK-QUIET-CANDIDATE! -1 T=
\ two linear bundles swapped: both conserved -> accept.
s" M7=" type s" M7 ( lq2<ltok,n> lq2<ltok,n> -- lq2<ltok,n> lq2<ltok,n> ) swap" CHECK-QUIET-CANDIDATE! -1 T=
\ a move that then loses the bundle at the boundary, or strands it on the return
\ stack, still rejects: conservation is per-step, boundary balance catches the loss.
s" MR1=" type s" MR1 ( lq2<ltok,n> n -- n ) swap" CHECK-QUIET-CANDIDATE! 0 T=
s" MR2=" type s" MR2 ( lq2<ltok,n> -- ) >r" CHECK-QUIET-CANDIDATE! 0 T=
cr

\ ---------------------------------------------------------------------------
\ rejected: copy, drop, locals, return-stack copy, unconsumed, loss. Copy-class
\ (dup/over/tuck/2dup) raises the count, drop-class (drop/nip/2drop) lowers it,
\ and r@/2r@ re-push (copy) the group -> LIN-CHECK conservation fires.
\ ---------------------------------------------------------------------------
s" R1=" type s" B1 ( lq2<ltok,n> -- lq2<ltok,n> lq2<ltok,n> ) dup" CHECK-QUIET-CANDIDATE! 0 T=
s" R2=" type s" B2 ( lq2<ltok,n> -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
s" R3=" type s" B3 ( lq2<ltok,n> -- )" CHECK-QUIET-CANDIDATE! 0 T=
s" R4=" type s" B4 ( lq2<ltok,n> n -- lq2<ltok,n> n lq2<ltok,n> ) over" CHECK-QUIET-CANDIDATE! 0 T=
s" R5=" type s" B5 ( lq2<ltok,n> -- lq2<ltok,n> ) {: x :} x" CHECK-QUIET-CANDIDATE! 0 T=
s" R6=" type s" B6 ( n lq2<ltok,n> -- lq2<ltok,n> n lq2<ltok,n> ) tuck" CHECK-QUIET-CANDIDATE! 0 T=
s" R7=" type s" B7 ( lq2<ltok,n> -- lq2<ltok,n> lq2<ltok,n> ) >r r@ r>" CHECK-QUIET-CANDIDATE! 0 T=
s" R8=" type s" B8 ( ltok -- lq2<ltok,n> ) LQ2:OK LQ2:OK" CHECK-QUIET-CANDIDATE! 0 T=   \ payload reuse
s" R9=" type s" B9 ( ltok f -- lq2<ltok,n> ) if LQ2:OK then" CHECK-QUIET-CANDIDATE! 0 T=  \ branch loss
s" R10=" type s" B10 ( ltok n -- lq2<ltok,n> ) nip 0 0" CHECK-QUIET-CANDIDATE! 0 T=       \ raw forge
s" R11=" type s" B11 ( ltok n -- lq2<ltok,n> ) nip LQ2:ERR" CHECK-QUIET-CANDIDATE! 0 T=   \ payload dropped
s" R12=" type s" B12 ( lq2<ltok,n> n -- n ) nip" CHECK-QUIET-CANDIDATE! 0 T=              \ bundle dropped by nip
s" R13=" type s" B13 ( lq2<ltok,n> n -- lq2<ltok,n> n lq2<ltok,n> n ) 2dup" CHECK-QUIET-CANDIDATE! 0 T=  \ 2dup copies bundle
cr

\ ---------------------------------------------------------------------------
\ scalar linear discipline unchanged next to the bundle machinery.
\ ---------------------------------------------------------------------------
s" S1=" type s" D1 ( ltok -- ltok )" CHECK-QUIET-CANDIDATE! -1 T=
s" S2=" type s" D2 ( ltok -- ltok ltok ) dup" CHECK-QUIET-CANDIDATE! 0 T=
s" S3=" type s" D3 ( ltok -- ) drop" CHECK-QUIET-CANDIDATE! 0 T=
cr

\ ---------------------------------------------------------------------------
\ item 9 slice 2: the `construct` form conserves exactly like the generated
\ constructor words — same CHECKER-STEP accounting, no separate linear rules.
\ Consume (A1 parity), mint (A2), padded/multi-cell (A6/A7), and the reuse/
\ loss/transport rejects hold through the inline form.
\ ---------------------------------------------------------------------------
s" K1=" type s" KC1 ( ltok -- lq2<ltok,n> ) construct lq2 ok" CHECK-QUIET-CANDIDATE! -1 T=
s" K2=" type s" KC2 ( n -- lq2<ltok,n> ) construct lq2 err" CHECK-QUIET-CANDIDATE! -1 T=
s" K3=" type s" KC3 ( ltok n n -- lqmix<ltok,n> ) construct lqmix big" CHECK-QUIET-CANDIDATE! -1 T=
s" K4=" type s" KC4 ( ltok -- lqmix<ltok,n> ) construct lqmix small" CHECK-QUIET-CANDIDATE! -1 T=
cr
s" KR1=" type s" KB1 ( ltok n -- lq2<ltok,n> ) construct lq2 err" CHECK-QUIET-CANDIDATE! 0 T=   \ unconsumed linear payload
s" KR2=" type s" KB2 ( ltok -- lq2<ltok,n> lq2<ltok,n> ) construct lq2 ok dup" CHECK-QUIET-CANDIDATE! 0 T=
s" KR3=" type s" KB3 ( ltok n -- lq2<ltok,n> ) nip construct lq2 err" CHECK-QUIET-CANDIDATE! 0 T=   \ payload dropped before construction
cr

\ ---------------------------------------------------------------------------
\ report: "ok" on success, nonzero exit on any failure.
\ ---------------------------------------------------------------------------
: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" type-linear-suite: failures" 1 die ;
REPORT
