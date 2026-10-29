\ frame.f - who owns which slot of a routine's own frame.

require src/lib/prelude.f
require src/lib/errors.f
require src/compiler/native-effect.f
require src/compiler/native/a64ir.f

package A64FRAME

private

0 constant LINK-IX

public

: LINK-KEPT? ( NEFF:traits NEFF:link -- bool )
   {: t:NEFF:traits l:NEFF:link :}
   t NEFF:T-CALL NEFF:TRAITS-HAS? 0= if false exit then
   l NEFF-LINK:PRESERVED NEFF-LINK:EQ ;

private

: PROLOGUE-SLOTS ( NEFF:traits NEFF:link -- n )
   LINK-KEPT? if 1 else 0 then ;

public

: LINK-SLOT ( -- n )
   LINK-IX A64IR:SLOT-WIDTH * ;

private

\ AArch64 writes the base register back as part of a load or store, and the
\ link lives at LINK-SLOT, the frame's base, so a link-only frame is opened by
\ `str x30,[sp,#-frame]!` and closed by `ldr x30,[sp],#frame`. The writeback
\ offset is nine signed bits of bytes; a wider frame takes the reserve/release
\ pair at each end.
$100 constant WRITEBACK-MAX          \ one past the widest offset the field holds

public

\ The one place this is decided: the selector and the register-allocation
\ verifier both ask here.
: FUSED? ( n -- bool )
   WRITEBACK-MAX < ;

\ Frame accesses below this offset are the prologue's; this and above are spills.
: SPILL-BASE ( NEFF:traits NEFF:link -- n )
   PROLOGUE-SLOTS A64IR:SLOT-WIDTH * ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
