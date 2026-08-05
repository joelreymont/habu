\ gpt2-clean-test-lib.f - real CUDA cleanup fault telemetry.

require lib/test.f
require maki/cuda-run.f

package GPT2-CLEAN-TEST
private

variable FREE-N
variable SYNC-N
variable EVENT
variable FREE-EVENT
variable SYNC-EVENT
variable FREE-RC
variable SYNC-RC

: TRACK-FREE ( cuda-devptr -- rc ) {: dev:cuda-devptr :}
   dev CUDA:CU-MEM-FREE RC>N {: real:n :}
   1 FREE-N +!
   1 EVENT +!
   EVENT @ FREE-EVENT !
   real 0<> if real >RC exit then
   FREE-RC @ >RC ;

: TRACK-SYNC ( CUDA:stream -- rc ) {: stream:CUDA:stream :}
   stream CUDA:CU-STREAM-SYNCHRONIZE RC>N {: real:n :}
   1 SYNC-N +!
   1 EVENT +!
   EVENT @ SYNC-EVENT !
   real 0<> if real >RC exit then
   SYNC-RC @ >RC ;

public

: TRACK-FAULTS ( n n -- ) {: free:n sync:n :}
   0 FREE-N !
   0 SYNC-N !
   0 EVENT !
   0 FREE-EVENT !
   0 SYNC-EVENT !
   free FREE-RC !
   sync SYNC-RC !
   [: TRACK-FREE ;] MKD:CUMEMFREE!
   [: TRACK-SYNC ;] MKD:STREAMSYNC! ;

: ASSERT-MODEL-SESSION ( -- )
   MKD:USE-REAL
   FREE-N @ 1 T=
   SYNC-N @ 1 T=
   FREE-EVENT @ 1 T=
   SYNC-EVENT @ 2 T= ;

: ASSERT-SESSION-ONLY ( -- )
   MKD:USE-REAL
   FREE-N @ 0 T=
   SYNC-N @ 1 T=
   SYNC-EVENT @ 1 T= ;

;package
