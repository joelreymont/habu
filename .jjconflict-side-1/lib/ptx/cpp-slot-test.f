\ cpp-slot-test.f - positive proof for the cp.async pipeline-slot typestate
\ (lib/ptx/cpp-slot.f): the in-order protocol certifies, and a bar.sync WAIT under
\ STRAIGHT-LINE control certifies (block-uniform), so the discipline admits the
\ correct pipeline while lib/ptx/cpp-slot-neg-test.f rejects every bad ordering.

require lib/ptx/test-prelude.f
require lib/ptx/cpp-slot.f

variable CST-N

: CST-CERT ( ptr u8 n ptr u8 n -- )   \ ( src label ) : assert the candidate CERTIFIES
   T-LABEL
   CHECK-CANDIDATE! 0 <> TTRUE ;

: CST-MAIN ( -- )
   T-RESET
   256 %BLOCK

   \ the full in-order protocol on a real staged tile certifies end to end:
   \ commit the issued slot, wait (drain + bar.sync), then read the tile at parity p
   s" GOOD ( mmstage<f32,block-256,geom-as64x32-bs32x64,mask-live,p> cpp-pending<p> -- mmstage<f32,block-256,geom-as64x32-bs32x64,mask-live,p> ) CPPSLOT:COMMIT CPPSLOT:WAIT CPPSLOT:READ-STAGE"
   s" in-order commit->wait->read-stage certifies" CST-CERT

   \ the plain-slot ordering (no tile) certifies: pending -> commit -> wait -> read
   s" GOOD-PLAIN ( cpp-pending<p> -- ) CPPSLOT:COMMIT CPPSLOT:WAIT CPPSLOT:READ"
   s" pending->commit->wait->read certifies" CST-CERT

   \ a WAIT (bar.sync fence) in STRAIGHT-LINE control is block-uniform: certifies
   \ (the divergent-control WAIT is the negative in cpp-slot-neg-test.f)
   s" GOOD-SL ( cpp-committed<p> -- ) CPPSLOT:WAIT CPPSLOT:READ"
   s" straight-line WAIT certifies (block-uniform)" CST-CERT

   T-REPORT ;

CST-MAIN
