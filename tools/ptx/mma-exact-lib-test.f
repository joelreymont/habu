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

T-RESET
MXT-IMPORT-SAFE
MXT-FILL-REF
MXT-COMPARE
T-REPORT

;package
