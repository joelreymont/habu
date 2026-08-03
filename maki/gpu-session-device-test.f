\ gpu-session-device-test.f - mandatory real-device GPU session lifetime proof.

require lib/test.f
require maki/gpu-session.f

package GPU
private

: DT-MUST-OPEN ( -- GPU:session )
   OPEN MATCH result
      ok OF ENDOF
      err OF throw ENDOF
   ;MATCH ;

: DT-MUST-CLOSE ( GPU:session -- )
   CLOSE MATCH result
      ok OF 0 T= ENDOF
      err OF throw ENDOF
   ;MATCH ;

: DT-MUST-USE ( GPU:session -- GPU:session )
   GS-TAKE 0 GS-BIND GS-SYNC {: code:n :}
   code 0 <> if GS-MINT code throw then
   GS-MINT ;

: DT-NEWEST-FIRST ( -- )
   DT-MUST-OPEN DT-MUST-OPEN
   DT-MUST-CLOSE DT-MUST-USE DT-MUST-CLOSE ;

: DT-OLDEST-FIRST ( -- )
   DT-MUST-OPEN DT-MUST-OPEN swap
   DT-MUST-CLOSE DT-MUST-USE DT-MUST-CLOSE ;

: DT-RUN ( -- )
   T-RESET
   DT-NEWEST-FIRST
   DT-OLDEST-FIRST
   T-REPORT ;

DT-RUN

;package
s" gpu-session-device-test: ok" type cr
