\ A JIT-hosted source window need not record pre-hook declarations. Replay the
\ real arming declaration so the policy is tested with an active user effect.
package FIELD-BOUNDARY-PREPARE
public

TRUSTED: DECLARATION ( -- )
   s" FIELD-PROJ!" s" ptr u8 n n n --" TRUST-DECL
   \ The existing suite's other two trusted forwards need their real pre-hook
   \ signatures for native call lowering in this JIT-hosted source window.
   s" FIELD-PROJ-CLEAR" s" --" TRUST-DECL
   s" TFAM:TFAM-FIND-IN" s" ptr u8 n ptr u8 n -- n bool" TRUST-DECL
   s" FIELD-PROJ-A" s" -- ptr ptr u8" TRUST-DECL
   s" FIELD-PROJ-U" s" -- ptr n" TRUST-DECL
   s" FIELD-PROJ-FID" s" -- ptr n" TRUST-DECL
   s" FIELD-PROJ-OFF" s" -- ptr n" TRUST-DECL ;

TRUSTED: REQUIRE-USER-ROW ( ptr u8 n -- )
   CHECKER-GLOBAL-SYM? USIG-NEWEST
   dup 0= if s" field arming has no user effect" 76 die then
   1- E-PTR ER.ACTIVE @ 0= if s" field arming effect is inactive" 76 die then ;

;package
FIELD-BOUNDARY-PREPARE:DECLARATION
s" FIELD-PROJ!" FIELD-BOUNDARY-PREPARE:REQUIRE-USER-ROW
s" FIELD-PROJ-A" FIELD-BOUNDARY-PREPARE:REQUIRE-USER-ROW
s" FIELD-PROJ-U" FIELD-BOUNDARY-PREPARE:REQUIRE-USER-ROW
s" FIELD-PROJ-FID" FIELD-BOUNDARY-PREPARE:REQUIRE-USER-ROW
s" FIELD-PROJ-OFF" FIELD-BOUNDARY-PREPARE:REQUIRE-USER-ROW

\ Complete the production declaration participants before loading library
\ families in the projection suite. The last participant seals registration.
require src/core/prefix-boundary.f
include src/core/generated-declaration-dictionary.f
include src/core/generated-declaration-protection.f

\ Run the production pass over the current source's REG-PROTECT registrations.
include src/core/internal-mark.f
