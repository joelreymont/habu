\ Complete native image with exact public C2 memory entries.
1 set-tier
require tools/native-build-core.f

package NATIVE-BUILD
private
: C2-MEM-ORIGIN ( n n -- n ) code-origin ;

: C2-MEM-WRITER ( AOT-OWNED:capture ptr n n ptr u8 n -- )
   {: capture host:ptr count:n path:ptr size:n :}
   s" tools/native-emit.f" required
   capture host count path size
   s" NATIVE-EMIT:WRITE-C2" SOURCE-WRITER-NAMED execute
   \ The ordinary second image from the same capture must have no C2 kinds.
   capture host count 1 SCRIPT-ARGV$
   s" NATIVE-EMIT:WRITE" SOURCE-WRITER-NAMED execute ;

public
: RUN-C2-MEM ( -- )
   DEFAULT-SOURCE-POLICY
   0 SCRIPT-ARGV$ OUTPUT!
   ['] C2-MEM-ORIGIN false ['] C2-MEM-WRITER RUN-READY-RC {: rc:n :}
   rc 0= if REPORT-CLASS then
   s" " rc die ;
;package

NATIVE-BUILD:RUN-C2-MEM
