\ sentinel.f - device-readback poison sentinel for fail-closed golden proofs.
\
\ Every host buffer that receives a device->host copy-back of a golden result is
\ pre-filled with POISON before the launch. If the copy-back silently fails - a
\ dropped rc, a wrong byte count, a kernel that never launched - the buffer still
\ holds POISON, so the golden comparison cannot masquerade as a pass: GUARD throws
\ E-PTX-READBACK on any readback cell still equal to the sentinel. POISON is a
\ committed 32-bit pattern ($DEADBEEF) that no valid f32 golden in the device
\ suite equals (as f32 it is a large negative value, ~-6.26e18).

require lib/errors.f

-5003 constant E-PTX-READBACK

package PTXSENT

: WORD! ( n ptr u8 -- )                       \ store n as a little-endian 32-bit word at ptr
   {: v:n p:ptr :}
   v $FF and p c!
   v 8 rshift $FF and p 1 + c!
   v 16 rshift $FF and p 2 + c!
   v 24 rshift $FF and p 3 + c! ;

public

$DEADBEEF constant POISON

: WORD@ ( ptr u8 -- n )                       \ read a little-endian 32-bit word at ptr
   {: p:ptr :}
   p c@
   p 1 + c@ 8 lshift or
   p 2 + c@ 16 lshift or
   p 3 + c@ 24 lshift or ;

: FILL ( ptr u8 n -- )                         \ pre-fill n bytes (multiple of 4) with POISON
   {: p:ptr n:n :}
   n 4 / 0 ?do  POISON  p i 4 * +  WORD!  loop ;

: GUARD ( n -- n )                            \ throw if a readback cell is still the sentinel
   dup POISON = if E-PTX-READBACK throw then ;

;package
