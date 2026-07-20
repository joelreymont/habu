\ mma-exact-lib-test.f - host-side coverage for the MMA element-exact library
\ (package MMA-EXACT) plus the import-safety invariant: loading the library
\ allocates no host/device memory and opens no device. Device-free by design;
\ the launch primitives are exercised only by the explicit commands on the GB10.

require lib/test.f
require tools/ptx/mma-exact-lib.f

package MMA-EXACT
private

\ ---- import-safety: `require` ran no campaign and touched no arena -----------
\ The seven host/packed buffers are heap-allocated only by MX-BUF-INIT, so until it
\ is called every base pointer is 0 and no device handle is open. This is the
\ committed counter proving import allocated nothing.
: MXT-IMPORT-SAFE ( -- )
   MX-BUF-READY? TFALSE                     \ host arena unallocated at load (all seven buffers guarded by MX-HA-P still null)
   MX-DA @ 0 T=  MX-DB @ 0 T=  MX-DC @ 0 T= ;   \ no device handle opened at load

\ ---- launch-independent exactness: integer fill + f64 reference (device-free) -
\ A 4^3 edge exercises the varied integer fill and the exact f64 matmul reference
\ with no GPU. A[m][k]=(3m+7k)%13+1, B[k][c]=(5k+2c)%11+1, REF[m][c]=sum_k A[m][k]*B[k][c].
\ Hand-computed: REF[0][0]=1*1+8*6+2*11+9*5=116; REF[1][1]=4*3+11*8+5*2+12*7=194.
: MXT-FILL-REF ( -- )
   MX-BUF-INIT
   MX-BUF-READY? TTRUE
   4 MX-N !
   MX-FILL  MX-REF
   MX-HREF 0 T-GET f>s 116 T=
   MX-HREF 1 4 * 1 + T-GET f>s 194 T= ;

\ ---- compare: exact match -> 0 mismatches, one perturbed cell -> exactly one --
\ Copy the reference into HC (a correct device readback would equal it) so the
\ zero-tolerance compare reports 0; then bump HC[0] by an exact 1.0 so the compare
\ reports one mismatch, first index 0, max error 1.0 - proving the compare gates.
: MXT-COMPARE ( -- )
   MX-N @ dup * 0 ?do  MX-HREF i T-GET  MX-HC i T-SET  loop
   MX-COMPARE 0 T=
   MX-BADI @ -1 T=
   MX-HC 0 T-GET 1 s>f f+  MX-HC 0 T-SET
   MX-COMPARE 1 T=
   MX-BADI @ 0 T=
   MX-MAXERR @ f>s 1 T= ;

\ ---- edge-capacity guard: 1..MX-MAX is the only in-bounds square edge ---------
\ The seven buffers hold MX-CAP=MX-MAX^2 elements; a bare n outside 1..MX-MAX would
\ index them out of bounds. MX-EDGE-OK? classifies the required edges and each
\ buffer-indexing word (via MX-CHECK-EDGE / MX-CHECK-E) dies named MX-E-CAP first.
: MXT-EDGE ( -- )
   -1 MX-EDGE-OK? TFALSE   0 MX-EDGE-OK? TFALSE   1 MX-EDGE-OK? TTRUE
   511 MX-EDGE-OK? TTRUE   MX-MAX MX-EDGE-OK? TTRUE   513 MX-EDGE-OK? TFALSE
   $7FFFFFFFFFFFFFFF MX-EDGE-OK? TFALSE                 \ max cell rejected by comparison, never multiplied
   -1 MX-N !  [: MX-CHECK-EDGE ;] MX-E-CAP TTHROWSQ
   0 MX-N !   [: MX-CHECK-EDGE ;] MX-E-CAP TTHROWSQ
   513 MX-N ! [: MX-CHECK-EDGE ;] MX-E-CAP TTHROWSQ
   $7FFFFFFFFFFFFFFF MX-N ! [: MX-CHECK-EDGE ;] MX-E-CAP TTHROWSQ
   1 MX-N !   MX-CHECK-EDGE   MX-MAX MX-N !  MX-CHECK-EDGE   \ valid edges fall through, no throw
   [: -1 MX-CHECK-E drop ;] MX-E-CAP TTHROWSQ               \ extent domain: negative rejected
   [: MX-CAP 1+ MX-CHECK-E drop ;] MX-E-CAP TTHROWSQ        \ extent domain: past capacity rejected
   MX-CAP MX-CHECK-E MX-CAP T=   0 MX-CHECK-E 0 T= ;        \ full + empty extent pass unchanged

\ ---- overflow: the domain guard makes every downstream multiply non-wrapping ---
\ A max-cell edge is refused by MX-EDGE-OK?'s comparison BEFORE n*n is formed, so the
\ wrap never happens; within 1..MX-MAX the products are exact and positive.
: MXT-OVERFLOW ( -- )
   $7FFFFFFFFFFFFFFF MX-EDGE-OK? TFALSE       \ rejected pre-multiply (n*n on this would wrap negative)
   MX-MAX dup *  MX-CAP T=                     \ n*n at max edge = MX-CAP (262144), exact
   MX-MAX dup * MX-MAX *  134217728 T=         \ n*n*n at max edge, exact & positive (no wrap)
   MX-MAX dup * 4 *  1048576 T= ;              \ device byte size n*n*4 at max edge, exact

\ ---- canary: max-edge O(n^2) writers stay inside [0,MX-CAP), guard cells survive
\ MX-FILL (HA,HB) and MX-PACK-AB (PA,PB) hit index MX-CAP-1 at n=MX-MAX; the seeded
\ sentinel one past each used region must be untouched (MX-REF shares the identical
\ i*n+j write bound - proven here by MX-FILL - and is correctness-checked at n=4).
: MXT-CANARY ( -- )
   MX-CANARY-SEED
   MX-MAX MX-N !
   MX-FILL                                    \ writes HA,HB over [0,MX-CAP)
   MX-N @ dup * MX-PACK-AB                     \ packs A,B into PA,PB over [0,MX-CAP) (host, device-free)
   MX-CANARY-INTACT? TTRUE ;                   \ no write crossed into any guard cell

\ ---- zero side effect: a rejected device word allocates nothing ---------------
\ MX-DEV-ALLOC guards its edge before the first DEVICE-ALLOC, so a bad edge leaves
\ every device handle 0 - the off-device witness that the rejection did no device work.
: MXT-DEV-REJECT ( -- )
   0 MX-DA !  0 MX-DB !  0 MX-DC !
   513 MX-N !  [: MX-DEV-ALLOC ;] MX-E-CAP TTHROWSQ
   MX-DA @ 0 T=  MX-DB @ 0 T=  MX-DC @ 0 T= ;

T-RESET
MXT-IMPORT-SAFE
MXT-FILL-REF
MXT-COMPARE
MXT-EDGE
MXT-OVERFLOW
MXT-CANARY
MXT-DEV-REJECT
T-REPORT

;package
