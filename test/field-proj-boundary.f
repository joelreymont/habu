\ The current checker and production sealing pass own both child tiers.
require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/whitebox-child.f

package FIELD-BOUNDARY-SUITE

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

\ The child runs test/native-window-owner-child.f, which reopens the engine's
\ build window: `hb: internal engine word: DECLARATIONS`, exit 70 on the sealed
\ product. So it runs on the engine test/whitebox-child.f names.
: PREPARE ( -- )
   CLEANUP-RESET
   s" field-proj-boundary" WHITEBOX-CHILD:PROVIDE ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

: ARGS ( bool bool -- ) {: native:bool prefix:bool :}
   PROC-ARGV-RESET
   s" --load" ARG
   prefix if s" test/compiler/aot-mode.f" ARG then
   s" test/native-window-owner-child.f" ARG
   s" --" ARG
   s" test/field-proj-boundary-child.f" ARG
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
   s" test/field-proj-boundary-prepare.f" ARG
   native if s" test/compiler/aot-mode.f" ARG then
   prefix if s" test/field-proj-native-owner.f" ARG then
   WHITEBOX-CHILD:ENV! ;

\ The native case compiles the window's whole core prefix through the
\ optimizing chain, which is minutes on a loaded box (test/native-window-owner.f
\ measured 2m11s); the bound catches a hang, not a slow build.
600000 constant DEADLINE-MS

: RESULT ( -- )
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN DEADLINE-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N S\" ok\nfield boundary: ok\nwindow: 0\n" STR= 0= rc 0 <> or
      if OUT outu LEN>N type ERR erru LEN>N type cr then
   rc 0 T=
   OUT outu LEN>N S\" ok\nfield boundary: ok\nwindow: 0\n" T$=
   ERR erru LEN>N s" trust-boundary primitive" CONTAINS? TTRUE
   ERR erru LEN>N s" hb: internal engine word: FIELD-PROJ!" CONTAINS? TTRUE ;

: CASES ( -- )
   0 0= 0= dup ARGS RESULT
   0 0= 0 0= 0= ARGS RESULT
   \ The retained product owner must also accept the new checker at tier 1.
   \ Starting tier 1 only after owner transfer cannot exercise this boundary.
   0 0= dup ARGS RESULT ;

: RUN ( -- )
   T-RESET
   [: PREPARE CASES ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
