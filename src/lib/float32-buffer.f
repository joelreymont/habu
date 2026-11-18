split-str
\ float32-buffer.f - raw little-endian F32 byte-buffer conversion.
\
\ STORE, LOAD, PACK and UNPACK trust the caller for the capacity each needs.
\
\ STORAGE CLASS. CALLER-OWNED: the module keeps no state.

require src/lib/float32.f

package F32-BUF

using F32

public

: STORE ( n ^u8 -- ) {: value:n dst:^ :}
   value           $FF and  dst     c!
   value 8 rshift  $FF and  dst 1 + c!
   value 16 rshift $FF and  dst 2 + c!
   value 24 rshift $FF and  dst 3 + c! ;

: LOAD ( ^u8 -- n ) {: src:^ :}
   src     c@
   src 1 + c@ 8  lshift or
   src 2 + c@ 16 lshift or
   src 3 + c@ 24 lshift or ;

: PACK ( ^r n ^u8 -- ) {: src:^ count:n dst:^ :}
   count 0 ?do
      src i cells + @ NARROW
      dst i 4 * + STORE
   loop ;

: UNPACK ( ^u8 n ^r -- ) {: src:^ count:n dst:^ :}
   count 0 ?do
      src i 4 * + LOAD WIDEN
      dst i cells + !
   loop ;

;using
;package
