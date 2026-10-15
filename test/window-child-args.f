\ window-child-args.f - the argv of a native-window child: the owner child
\ entry, the subject, then the prefix files the include words and the seal
\ stand on, in the build's order (test/reloc-window-prepare.f), and the
\ prepare file. That window stops at src/core/cell-effects.f; PAYLOAD gives
\ the AOT payload rows' window, which runs through the prelude. EXPECT runs
\ the staged child on the whitebox engine and holds it to one exact stdout.

require src/lib/test.f
require src/lib/string.f
require src/lib/process.f
require src/lib/process-argv.f
require src/lib/process-env.f
require test/whitebox-child.f
require test/suite-budget.f              \ CHILD-MS, every child's hang guard

package WINDOW-CHILD-ARGS
private

$4000 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

: ARG ( ptr u8 n -- ) >LEN PROC-ARGV+ ;

\ The host layout and include words the build appends after the target
\ (tools/build-fixpoint.f BF-APPEND-HABU-LAYOUT … BF-APPEND-INCLUDE).
: BASE ( -- )
   s" src/habu/stack-abi.f" ARG
   s" src/habu/layout.f" ARG
   s" src/os/env-base.f" ARG
   s" src/core/include.f" ARG ;

public

: CHILD ( ptr u8 n -- )
   {: a:ptr u:n :}
   PROC-ARGV-RESET
   s" --load" ARG
   s" test/native-window-owner-child.f" ARG
   s" --" ARG
   a u ARG ;

: CORE ( -- )
   s" src/core/declaration-transaction.f" ARG
   s" src/core/generated-declaration.f" ARG
   s" src/core/decl-event.f" ARG
   s" src/core/structure-make.f" ARG
   s" src/core/structure-decl.f" ARG
   s" src/core/enum-decl.f" ARG
   s" src/core/structures.f" ARG
   s" src/core/bytes.f" ARG ;

: TARGET ( -- )
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

: HABU ( -- ) BASE s" src/habu/code-span.f" ARG ;

\ The window of the AOT payload rows: the core with dynamic storage, the
\ target, the base, sha256 and the prelude, in the build's order
\ (tools/build-fixpoint.f BF-APPEND-BOOT-CORE, BF-APPEND-COMMON), no code-span.
: PAYLOAD ( ptr u8 n -- )
   CHILD
   CORE s" src/core/dynamic-storage.f" ARG
   TARGET BASE
   s" src/core/sha256.f" ARG
   s" src/lib/prelude.f" ARG ;

: PREPARE ( ptr u8 n -- )
   ARG
   WHITEBOX-CHILD:ENV! ;

: EXPECT ( ptr u8 n -- )
   {: pass:ptr passu:n :}
   WHITEBOX-CHILD:ENGINE$ >LEN OUT IO-CAP >LEN ERR IO-CAP >LEN
   SUITE-BUDGET:CHILD-MS >MS
   RUN-ARGV-ENV-CAPTURE-OUTCOME PROC-OUTCOME>RC RC>N
   {: outu:len erru:len rc:n :}
   OUT outu LEN>N pass passu STR= 0= rc 0 <> or
      if OUT outu LEN>N type ERR erru LEN>N type cr then
   rc 0 T=
   OUT outu LEN>N pass passu T$= ;

;package
