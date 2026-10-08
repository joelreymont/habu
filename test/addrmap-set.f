\ addrmap-set.f - runs test/addrmap-set-child.f, the address-literal map
\ primitive's contract, in the native build window.
\
\ The child calls addrmap-set through NPUB:ADDRMAP-MARK, a checked word
\ test/reloc-window-prepare.f defines in the reopened owner before the window's
\ seal. The product seals NPUB, so the child runs under
\ test/native-window-owner-child.f on the engine test/whitebox-child.f names.
require lib/test.f
require lib/fs-mutate.f
require lib/process.f
require lib/process-argv.f
require lib/process-env.f
require test/whitebox-child.f
require test/suite-budget.f              \ CHILD-MS, every child's hang guard

package ADDRMAP-SET-SUITE

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

: PREPARE ( -- )
   CLEANUP-RESET
   s" addrmap-set" WHITEBOX-CHILD:PROVIDE ;

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

\ The window stops at src/core/cell-effects.f. These are the prefix files the
\ include words and the seal stand on, in the build's order, before the owner
\ words and the seal (test/reloc-window-prepare.f).
: TARGET-ARGS ( -- )
   HB-TARGET-LINUX-X86-64? if
      s" src/os/linux-x86-64/target.f" ARG
      s" src/os/linux/layout-constants.f" ARG
      s" src/os/linux/layout.f" ARG exit
   then
   HB-TARGET-LINUX? if
      s" src/os/linux/target.f" ARG
      s" src/os/linux/layout-constants.f" ARG
      s" src/os/linux/layout.f" ARG exit
   then
   s" src/os/macos/target.f" ARG s" src/os/macos/layout.f" ARG ;

: ARGS ( -- )
   PROC-ARGV-RESET
   s" --load" ARG
   s" test/native-window-owner-child.f" ARG
   s" --" ARG
   s" test/addrmap-set-child.f" ARG
   s" src/core/declaration-transaction.f" ARG
   s" src/core/generated-declaration.f" ARG
   s" src/core/decl-event.f" ARG
   s" src/core/structure-make.f" ARG
   s" src/core/structure-decl.f" ARG
   s" src/core/enum-decl.f" ARG
   s" src/core/structures.f" ARG
   s" src/core/bytes.f" ARG
   TARGET-ARGS
   s" src/habu/stack-abi.f" ARG
   s" src/habu/layout.f" ARG
   s" src/os/env-base.f" ARG
   s" src/core/include.f" ARG
   s" src/habu/code-span.f" ARG
   s" test/reloc-window-prepare.f" ARG
   WHITEBOX-CHILD:ENV! ;

: PASS$ ( -- ptr u8 n ) S\" test: ok\naddrmap-set: ok\nwindow: 0\n" ;

: RESULT ( -- )
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN
   SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N PASS$ STR= 0= rc 0 <> or
      if OUT outu LEN>N type ERR erru LEN>N type cr then
   rc 0 T=
   OUT outu LEN>N PASS$ T$= ;

: RUN ( -- )
   T-RESET
   [: PREPARE ARGS RESULT ;] [: CLEANUP-RUN ;] finally
   T-REPORT ;

RUN
;package
