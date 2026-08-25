\ frame.f - who owns which slot of a routine's own frame.

require lib/prelude.f
require lib/errors.f
require src/compiler/a64-effect.f
require src/compiler/native/a64ir.f

package A64FRAME

private

0 constant LINK-IX

public

: LINK-KEPT? ( A64EFF:traits A64EFF:link -- bool )
   {: t:A64EFF:traits l:A64EFF:link :}
   t A64EFF:T-CALL A64EFF:TRAITS-HAS? 0= if false exit then
   l A64EFF-LINK:PRESERVED A64EFF-LINK:EQ ;

private

: PROLOGUE-SLOTS ( A64EFF:traits A64EFF:link -- n )
   LINK-KEPT? if 1 else 0 then ;

public

: LINK-SLOT ( -- n )
   LINK-IX A64IR:SLOT-WIDTH * ;

\ Frame accesses below this offset are the prologue's; this and above are spills.
: SPILL-BASE ( A64EFF:traits A64EFF:link -- n )
   PROLOGUE-SLOTS A64IR:SLOT-WIDTH * ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
