\ capture-backend-test.f - focused Habu capture backend tests.

require lib/errors.f
require lib/string.f
require lib/test.f
require odin/capture-backend.f

package HCAP

: HCAP-TEST-BASIC ( -- )
   RESET
   s" 306885122:cam_a0" CAMERA+
   CAMERA-N@ 1 T=
   s" /tmp/habu-capture-test" OUTPUT!
   SELF-TEST ;

: HCAP-TEST-RUN ( -- )
   T-RESET
   HCAP-TEST-BASIC ;

HCAP-TEST-RUN
T-REPORT

end-package
