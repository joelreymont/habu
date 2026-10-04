\ Patch x86-64 address carriers at a caller's fixed virtual addresses.
\ Closure and snapshot writers use these without loading the captured-window
\ linker or its kernel registry.
require lib/le.f
require src/habu/address-carrier.f

package X64PATCH

private

74 constant PATCH-RC
$7FFFFFFF constant REL-MAX
REL-MAX negate 1- constant REL-MIN

public

\ `at` points at an E8 call or E9 jump at `site-va`.
: REL32! ( ptr u8 n n -- ) {: at:ptr site-va:n target-va:n :}
   at c@ dup $E8 <> swap $E9 <> and if
      s" x64patch: expected rel32 call or jump" PATCH-RC die
   then
   target-va site-va 5 + - {: disp:n :}
   disp REL-MIN < disp REL-MAX > or if
      s" x64patch: rel32 target out of reach" PATCH-RC die
   then
   disp at 1+ LE:U32! ;

\ `at` points at a ten-byte `mov r64, imm64` site.
: MOVABS! ( ptr u8 n -- ) {: at:ptr va:n :}
   at ADDRESS-CARRIER:MOVABS-SITE? 0= if
      s" x64patch: expected movabs address site" PATCH-RC die
   then
   va at 2 + LE:U64! ;

;package
