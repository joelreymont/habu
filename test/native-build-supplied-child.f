\ Run the native build with the real writer supplied before the target window.
1 set-tier
require tools/native-build-core.f
require tools/native-emit.f

package NATIVE-BUILD
private
: ORIGIN ( n n -- n ) code-origin ;
public
: SUPPLIED-PROOF ( -- )
   1 SCRIPT-ARGV$ BUILD-TARGET:SELECT? 0= if 76 throw then
   DEFAULT-SOURCE-POLICY
   0 SCRIPT-ARGV$ OUTPUT!
   ['] ORIGIN false ['] NATIVE-EMIT:WRITE-C2 RUN-READY-RC {: rc:n :}
   s" " rc EXIT-RC die ;
;package

NATIVE-BUILD:SUPPLIED-PROOF
