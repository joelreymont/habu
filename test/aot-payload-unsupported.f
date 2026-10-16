\ Unsupported partial-payload representations refuse through the real source owner.
require src/lib/string.f
require src/lib/test.f
require src/lib/fs-mutate.f
require src/lib/process.f
require src/lib/process-env.f
require test/whitebox-child.f
require test/window-child-args.f

package PAYLOAD-UNSUPPORTED-SUITE

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

\ The child runs test/native-window-owner-child.f, which reopens the engine's
\ build window: `hb: internal engine word: DECLARATIONS`, exit 70 on the sealed
\ product. So it runs on the engine test/whitebox-child.f names.
: PREPARE ( -- )
   CLEANUP-RESET
   s" aot-payload-unsupported" WHITEBOX-CHILD:PROVIDE ;

: ARGS ( ptr u8 n -- ) WINDOW-CHILD-ARGS:PAYLOAD WHITEBOX-CHILD:ENV! ;

: CASE-RUN ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: fixture:ptr fixtureu:n present:ptr presentu:n diagnostic:ptr diagnosticu:n :}
   fixture fixtureu ARGS
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 30000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 76 <> if OUT outu LEN>N type ERR erru LEN>N type cr then
   rc 76 T=
   OUT outu LEN>N present presentu CONTAINS? TTRUE
   OUT outu LEN>N diagnostic diagnosticu CONTAINS?
   ERR erru LEN>N diagnostic diagnosticu CONTAINS? or TTRUE ;

: CASES ( -- )
   s" test/aot-payload-exception-child.f"
   s" exceptional quotation rows are present"
   s" checker: exceptional quotation rows are not portable" CASE-RUN
   s" test/aot-payload-constructor-child.f"
   s" dynamic constructor schema is present"
   s" tfam: captured schema constructor has process-local identity" CASE-RUN ;

: RUN ( -- )
   T-RESET
   [: PREPARE CASES ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
