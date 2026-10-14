\ Empty slices at the end of a full input buffer stay valid.
\ bin/hb --load test/native-file-inputs-empty.f

require src/lib/test.f
require tools/native-file-inputs.f

package NFILE-INPUTS-EMPTY-TEST

create PAYLOAD 64 allot

: SPAN-END ( -- )
   64 64 NFILE-INPUTS:SPAN {: a:ptr u:n :}
   a NULL-PTR = TTRUE
   u 0 T= ;

: FROM-END ( -- )
   64 NFILE-INPUTS:BYTES-FROM {: a:ptr u:n :}
   a NULL-PTR = TTRUE
   u 0 T= ;

public
: MAIN ( -- )
   T-RESET
   s" FILE inputs empty end span at capacity" T-LABEL
   NFILE-INPUTS:CLOSE
   64 0 ?do 0 PAYLOAD i + c! loop
   PAYLOAD 64 NFILE-INPUTS:APPEND
   NFILE-INPUTS:MARK 64 T=
   ['] SPAN-END catch 0 T=
   ['] FROM-END catch 0 T=
   NFILE-INPUTS:CLOSE
   T-REPORT ;

;package

NFILE-INPUTS-EMPTY-TEST:MAIN
