\ Exercise current registry source through the real replacement-owner load.
require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/whitebox-child.f

package REGISTRY-IDENTITY-SUITE

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

\ The child runs test/native-window-owner-child.f, which reopens the engine's
\ build window: `hb: internal engine word: DECLARATIONS`, exit 70 on the sealed
\ product. So it runs on the engine test/whitebox-child.f names.
: PREPARE ( -- )
   CLEANUP-RESET
   s" aot-registry-identity" WHITEBOX-CHILD:PROVIDE ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" ARG
   s" test/native-window-owner-child.f" ARG
   s" --" ARG
   s" test/aot-registry-identity-child.f" ARG
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
   s" src/habu/stack-abi.f" ARG
   s" src/habu/layout.f" ARG
   s" src/os/env-base.f" ARG
   s" src/core/include.f" ARG
   s" src/core/sha256.f" ARG
   s" src/habu/code-span.f" ARG
   s" lib/prelude.f" ARG
   WHITEBOX-CHILD:ENV! ;

: CHECK ( -- )
   ARGS
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN 30000 >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   rc 0 <>
   OUT outu LEN>N s" registry identity and refusal atomicity: ok" CONTAINS? 0= or
   if OUT outu LEN>N type ERR erru LEN>N type cr then
   rc 0 T=
   OUT outu LEN>N S\" registry identity and refusal atomicity: ok\nwindow: 0\n" T$=
   erru LEN>N 0 T= ;

: RUN ( -- )
   T-RESET
   [: PREPARE CHECK ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
