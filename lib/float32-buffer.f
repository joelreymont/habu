\ float32-buffer.f - raw little-endian F32 byte-buffer conversion.

require lib/float32.f

package F32-BUF

using F32

public

: STORE ( n ptr u8 -- ) {: value:n dst:ptr :}
   value           $FF and  dst     c!
   value 8 rshift  $FF and  dst 1 + c!
   value 16 rshift $FF and  dst 2 + c!
   value 24 rshift $FF and  dst 3 + c! ;

: LOAD ( ptr u8 -- n ) {: src:ptr :}
   src     c@
   src 1 + c@ 8  lshift or
   src 2 + c@ 16 lshift or
   src 3 + c@ 24 lshift or ;

: PACK ( ptr r n ptr u8 -- ) {: src:ptr count:n dst:ptr :}
   count 0 ?do
      src i cells + @ NARROW
      dst i 4 * + STORE
   loop ;

: UNPACK ( ptr u8 n ptr r -- ) {: src:ptr count:n dst:ptr :}
   count 0 ?do
      src i 4 * + LOAD WIDEN
      dst i cells + !
   loop ;

;using
;package
