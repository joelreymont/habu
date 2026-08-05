\ maki/view-test.f - strided tensor views core (docs/strided-views.md SV-1..SV-4).
\ Test-local names are VT-/V- prefixed; the suite shares the MAKI wordlist and
\ qualifies the store words TENSOR:. Proves: SV-1 the grown record + derived
\ contiguity classification (degenerate = the old contiguous path); SV-2 the four
\ checked constructors with red-first out-of-bounds; SV-3 the immutability law made
\ loud (write-through-view + flat-buffer-from-view both fail closed); SV-4 the
\ scatter-add view adjoint - central-FD gradchecked per constructor class, with
\ fan-out accumulation and the out-of-view-perturbation proof.

require lib/test.f
require lib/float.f
require test/checker-assert.f
require maki/tensor-value.f

package MAKI

T-RESET
TENSOR:TV-RESET

\ ---- storage S: contiguous row-major 4x6, S[k] = k+1 (reads are exact) --------
create VS 24 cells allot
: VS-INIT ( -- )  24 0 ?do  i 1+ s>f  VS i T-SET  loop ;
VS-INIT
TYPED-VARIABLE VST TENSOR:tensor   VS 4 6 SHAPE TENSOR:TV-NEW  VST !

\ ===========================================================================
\ SV-1: representation - the degenerate view IS the old contiguous path.
\ ===========================================================================
VST @ TENSOR:TV-VIEW?       TFALSE          \ storage-ref = self => not a view
VST @ TENSOR:TV-OFF@   0 T=                  \ offset 0
VST @ TENSOR:TV-RSTR@  6 T=                  \ natural row stride = cols
VST @ TENSOR:TV-CSTR@  1 T=                  \ natural col stride = 1
VST @ TENSOR:TV-CONTIG-ROW? TTRUE
VST @ TENSOR:TV-CONTIG-COL? TFALSE
VST @ TENSOR:TV-STRIDED?    TFALSE
VST @ TENSOR:TV-STORE@ VST @ TENSOR:TV-EQUAL? TTRUE   \ storage-ref = self
VST @ TENSOR:TV-DATA@ VS =  TTRUE            \ flat buffer accessor unchanged (bit-identical)

\ col-major degenerate: natural strides (1, rows) => classified contiguous-col
TYPED-VARIABLE VC TENSOR:tensor   VS 4 6 SHAPE MAKI-DATATYPE:DF32 MAKI-LAYOUT:COL TENSOR:TV-NEW-HOST  VC !
VC @ TENSOR:TV-RSTR@  1 T=
VC @ TENSOR:TV-CSTR@  4 T=
VC @ TENSOR:TV-CONTIG-COL? TTRUE
VC @ TENSOR:TV-CONTIG-ROW? TFALSE
VC @ TENSOR:TV-STRIDED?    TFALSE

\ ===========================================================================
\ SV-2: constructors - extents, strides, and strided reads.
\ ===========================================================================
\ WINDOW rows [1,3): 2x6, offset 6, base strides inherited, off!=0 => strided.
TYPED-VARIABLE VW TENSOR:tensor   VST @ 1 3 TENSOR:TV-WINDOW  VW !
VW @ TENSOR:TV-ROWS@ ROWS-RAW 2 T=
VW @ TENSOR:TV-COLS@ COLS-RAW 6 T=
VW @ TENSOR:TV-OFF@  6 T=
VW @ TENSOR:TV-VIEW?    TTRUE
VW @ TENSOR:TV-STRIDED? TTRUE
VW @ TENSOR:TV-STORE@ VST @ TENSOR:TV-EQUAL? TTRUE
VW @ 0 0 TENSOR:TV-AT@  0.5 f+ f>s  7 T=          \ S[6]  = 7
VW @ 1 5 TENSOR:TV-AT@  0.5 f+ f>s 18 T=          \ S[17] = 18

\ TRANSPOSE-VIEW of a row-major 4x6 => a contiguous col-major 6x4 (strides swapped).
TYPED-VARIABLE VT TENSOR:tensor   VST @ TENSOR:TV-TRANSPOSE-VIEW  VT !
VT @ TENSOR:TV-ROWS@ ROWS-RAW 6 T=
VT @ TENSOR:TV-COLS@ COLS-RAW 4 T=
VT @ TENSOR:TV-CONTIG-COL? TTRUE                  \ classification: contiguous-col
VT @ TENSOR:TV-STRIDED?    TFALSE
VT @ TENSOR:TV-VIEW?       TTRUE                  \ still a view (aliases S), so read-only
VT @ 0 1 TENSOR:TV-AT@  0.5 f+ f>s  7 T=          \ S[6]  = 7  (transpose)
VT @ 2 3 TENSOR:TV-AT@  0.5 f+ f>s 21 T=          \ S[20] = 21

\ HEAD-SPLIT head 1 of a 4x(H*hd)=4x6 buffer (H=2, hd=3): 4x3, offset 3.
TYPED-VARIABLE VH TENSOR:tensor   VST @ 1 2 3 TENSOR:TV-HEAD-SPLIT  VH !
VH @ TENSOR:TV-ROWS@ ROWS-RAW 4 T=
VH @ TENSOR:TV-COLS@ COLS-RAW 3 T=
VH @ TENSOR:TV-OFF@  3 T=
VH @ TENSOR:TV-STRIDED? TTRUE
VH @ 0 0 TENSOR:TV-AT@  0.5 f+ f>s  4 T=          \ S[3]  = 4  (head 1, col 0)
VH @ 3 2 TENSOR:TV-AT@  0.5 f+ f>s 24 T=          \ S[23] = 24

\ general VIEW: offset 1, 2x2, row stride 6, col stride 2.
TYPED-VARIABLE VG TENSOR:tensor   VST @ 1 2 2 6 2 TENSOR:TV-VIEW  VG !
VG @ 0 0 TENSOR:TV-AT@  0.5 f+ f>s  2 T=          \ S[1]  = 2
VG @ 0 1 TENSOR:TV-AT@  0.5 f+ f>s  4 T=          \ S[3]  = 4
VG @ 1 0 TENSOR:TV-AT@  0.5 f+ f>s  8 T=          \ S[7]  = 8
VG @ 1 1 TENSOR:TV-AT@  0.5 f+ f>s 10 T=          \ S[9]  = 10

\ materialize (the §3 COPY remedy): strided read into a contiguous row-major buffer
create VM 24 cells allot
VG @ VM TENSOR:TV-MATERIALIZE
VM 0 T-GET 0.5 f+ f>s 2 T=   VM 3 T-GET 0.5 f+ f>s 10 T=   \ [2,4,8,10] row-major

\ ---- bounds fail-closed, red-first (E-MK-DIM) ------------------------------
: VT-BAD-WINDOW  ( -- )  VST @ 2 5 TENSOR:TV-WINDOW drop ;        \ r1=5 > 4 rows
: VT-BAD-HEAD-H  ( -- )  VST @ 2 2 3 TENSOR:TV-HEAD-SPLIT drop ;  \ h=2 >= H=2
: VT-BAD-HEAD-C  ( -- )  VST @ 0 2 4 TENSOR:TV-HEAD-SPLIT drop ;  \ C=8 != 6
: VT-BAD-VIEW    ( -- )  VST @ 20 2 2 6 1 TENSOR:TV-VIEW drop ;   \ maxidx 27 >= 24
: VT-NEG-STRIDE  ( -- )  VST @ 0 2 2 -1 1 TENSOR:TV-VIEW drop ;   \ negative row stride (v2)
' VT-BAD-WINDOW E-MK-DIM TTHROWS
' VT-BAD-HEAD-H E-MK-DIM TTHROWS
' VT-BAD-HEAD-C E-MK-DIM TTHROWS
' VT-BAD-VIEW   E-MK-DIM TTHROWS
' VT-NEG-STRIDE E-MK-DIM TTHROWS

\ ===========================================================================
\ SV-3: immutability law - loud. No write-through-view; no flat buffer from a view.
\ ===========================================================================
: VT-WRITE-VIEW ( -- )  99.0 VW @ 0 0 TENSOR:TV-AT! ;   \ write-through-view rejected
: VT-DATA-VIEW  ( -- )  VW @ TENSOR:TV-DATA@ drop ;     \ flat buffer from a view rejected
' VT-WRITE-VIEW E-TV-VIEW TTHROWS
' VT-DATA-VIEW  E-TV-VIEW TTHROWS
\ the owner MAY write its own (degenerate) storage through the same seam
77.0 VST @ 0 0 TENSOR:TV-AT!
VST @ 0 0 TENSOR:TV-AT@ 0.5 f+ f>s 77 T=
1.0 VST @ 0 0 TENSOR:TV-AT!                             \ restore S[0]

\ ===========================================================================
\ SV-4: scatter-add view adjoint - central-FD gradcheck + fan-out + out-of-view.
\ ===========================================================================
\ |a-b| < 1e-3 + 1e-2|a|  (the gradcheck.f tolerance, inlined so no heavy require)
: V-CLOSE? ( r r -- bool ) {: a:r b:r :}
   a b f- fabs   0.001  0.01 a fabs f* f+   f< ;

\ L(V) = sum_m materialized-V[m] * W[m]  (a linear readout with weights W)
: FD-LOSS ( tensor ptr r ptr r -- r ) {: v:tensor wp:ptr mp:ptr :}
   v mp TENSOR:TV-MATERIALIZE
   v TENSOR:TV-ROWS@ ROWS-RAW  v TENSOR:TV-COLS@ COLS-RAW *  {: n:n :}
   0.0  n 0 ?do  mp i T-GET  wp i T-GET  f*  f+  loop ;

\ central FD of L w.r.t. every storage element vs the analytic scatter-add adjoint.
variable FD-OK
: FD-CHECK ( tensor ptr r ptr r n ptr r ptr r -- bool )
   {: v:tensor wp:ptr sp:ptr selems:n ap:ptr mp:ptr :}
   0.0 ap selems T-FILL
   v wp ap TENSOR:TV-VIEW-ADJOINT+                 \ analytic: A[k] = sum_{idx(m)=k} W[m]
   true FD-OK !
   selems 0 ?do
      sp i T-GET {: s0:r :}
      s0 0.001 f+  sp i T-SET   v wp mp FD-LOSS {: lp:r :}
      s0 0.001 f-  sp i T-SET   v wp mp FD-LOSS {: lm:r :}
      s0 sp i T-SET                                \ restore exactly
      lp lm f-  0.002 f/  {: gnum:r :}             \ central finite difference
      gnum  ap i T-GET  V-CLOSE? 0= if false FD-OK ! then
   loop
   FD-OK @ ;

create VA  24 cells allot          \ storage adjoint (24 elems)
create VW-COT 12 cells allot        \ window 2x6 cotangent
create VT-COT 24 cells allot        \ transpose 6x4 cotangent
create VH-COT 12 cells allot        \ head 4x3 cotangent
create VG-COT  4 cells allot        \ general 2x2 cotangent
\ non-uniform cotangents so a real (not identically-zero) gradient is exercised
: COT-INIT ( ptr r n -- ) {: p:ptr n:n :}  n 0 ?do  i 1+ 3 mod 1+ s>f  p i T-SET  loop ;
VW-COT 12 COT-INIT   VT-COT 24 COT-INIT   VH-COT 12 COT-INIT   VG-COT 4 COT-INIT

VW @ VW-COT VS 24 VA VM FD-CHECK TTRUE   \ WINDOW    adjoint gradchecks
VT @ VT-COT VS 24 VA VM FD-CHECK TTRUE   \ TRANSPOSE adjoint gradchecks
VH @ VH-COT VS 24 VA VM FD-CHECK TTRUE   \ HEAD-SPLIT adjoint gradchecks
VG @ VG-COT VS 24 VA VM FD-CHECK TTRUE   \ general VIEW adjoint gradchecks

\ ---- out-of-view perturbation proof: an element outside the footprint gets 0 ---
\ WINDOW [1,3) covers S[6..17]; VW element (0,0)->S[6], (0,1)->S[7]. S[0..5] outside.
0.0 VA 24 T-FILL
VW @ VW-COT VA TENSOR:TV-VIEW-ADJOINT+
VA 0 T-GET 0.5 f+ f>s 0 T=                        \ S[0] outside footprint -> zero
VA 6 T-GET  VW-COT 0 T-GET  f- f>s 0 T=           \ S[6] gets W(0,0)
VA 7 T-GET  VW-COT 1 T-GET  f- f>s 0 T=           \ S[7] gets W(0,1)

\ ---- fan-out accumulation: two overlapping windows sum into one adjoint --------
\ W1=[0,2) rows{0,1} at offset 0 ; W2=[1,3) rows{1,2} at offset 6 ; row 1 (S[6..11])
\ is shared, so its adjoint accumulates BOTH cotangents (all 1.0 here).
TYPED-VARIABLE VW1 TENSOR:tensor   VST @ 0 2 TENSOR:TV-WINDOW  VW1 !
TYPED-VARIABLE VW2 TENSOR:tensor   VST @ 1 3 TENSOR:TV-WINDOW  VW2 !
create VONES 12 cells allot
: ONES ( -- )  12 0 ?do  1.0 VONES i T-SET  loop ;   ONES
0.0 VA 24 T-FILL
VW1 @ VONES VA TENSOR:TV-VIEW-ADJOINT+
VW2 @ VONES VA TENSOR:TV-VIEW-ADJOINT+
VA  0 T-GET 0.5 f+ f>s 1 T=      \ row 0: W1 only
VA  6 T-GET 0.5 f+ f>s 2 T=      \ row 1: W1 + W2 (fan-out accumulation)
VA 12 T-GET 0.5 f+ f>s 1 T=      \ row 2: W2 only
VA 18 T-GET 0.5 f+ f>s 0 T=      \ row 3: neither

T-REPORT

;package
