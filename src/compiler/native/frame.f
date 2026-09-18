\ frame.f - who owns which slot of a routine's own frame.

require lib/prelude.f
require lib/errors.f
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

\ AArch64 writes the base register back as part of a load or a store, and the
\ link lives at LINK-SLOT, which is the frame's base. So a frame that keeps
\ only the link register is taken and given back by the transfer itself: `str
\ x30,[sp,#-frame]!` opens it and `ldr x30,[sp],#frame` closes it. The
\ writeback offset is nine SIGNED bits of BYTES, so a wider frame still takes
\ the reserve/release pair at each end -- measured 2026-09-16 over the engine's
\ own image, 30 of 9,068 frames.
$100 constant WRITEBACK-MAX          \ one past the widest offset the field holds

public

\ THE ONE PLACE THAT DECIDES IT. The selector emits the fused form from here and
\ the register-allocation verifier expects it from here, so the two cannot
\ disagree about which shape a routine's frame has.
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
