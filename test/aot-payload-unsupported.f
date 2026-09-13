\ Unsupported partial-payload representations refuse through the real source owner.
require lib/test.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require lib/engine-id.f
require lib/engine-candidate.f

package PAYLOAD-UNSUPPORTED-SUITE

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: ARGS ( ptr u8 n -- ) {: fixture:ptr fixtureu:n :}
   PROC-ARGV-RESET
   s" --load" ARG
   s" test/native-window-owner-child.f" ARG
   s" --" ARG
   fixture fixtureu ARG
   s" src/core/declaration-transaction.f" ARG
   s" src/core/generated-declaration.f" ARG
   s" src/core/decl-event.f" ARG
   s" src/core/structure-make.f" ARG
   s" src/core/structure-decl.f" ARG
   s" src/core/enum-decl.f" ARG
   s" src/core/structures.f" ARG
   s" src/core/bytes.f" ARG
   s" src/core/dynamic-storage.f" ARG
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" ARG s" src/os/linux/layout.f" ARG
   else
      s" src/os/macos/target.f" ARG s" src/os/macos/layout.f" ARG
   then
   s" src/habu/layout.f" ARG
   s" src/os/env-base.f" ARG
   s" src/core/include.f" ARG
   s" src/core/sha256.f" ARG
   s" lib/prelude.f" ARG
   PROC-ENV-RESET PROC-ENV-INHERIT-MISSING ;

: CASE-RUN ( ptr u8 n ptr u8 n ptr u8 n -- )
   {: fixture:ptr fixtureu:n present:ptr presentu:n diagnostic:ptr diagnosticu:n :}
   fixture fixtureu ARGS
   ENGINE-CANDIDATE:PATH$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 30000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 76 <> if OUT outu LEN>N type ERR erru LEN>N type cr then
   rc 76 T=
   OUT outu LEN>N present presentu CONTAINS? TTRUE
   OUT outu LEN>N diagnostic diagnosticu CONTAINS?
   ERR erru LEN>N diagnostic diagnosticu CONTAINS? or TTRUE ;

: RUN ( -- )
   T-RESET
   s" test/aot-payload-exception-child.f"
   s" exceptional quotation rows are present"
   s" checker: exceptional quotation rows are not portable" CASE-RUN
   s" test/aot-payload-constructor-child.f"
   s" dynamic constructor schema is present"
   s" tfam: captured schema constructor has process-local identity" CASE-RUN
   T-REPORT ;

RUN
;package
