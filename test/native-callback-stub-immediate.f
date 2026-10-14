\ An anonymous native C entry must load every lane of its stable callback ID.
\ This ID is beyond both the former pool and the low 16-bit MOVZ lane.
1 set-tier
require src/lib/test.f
require src/lib/ffi-abi.f
require src/lib/le.f
require src/habu/code-bytes.f
require src/compiler/native/compiler.f

65537 -1 FFI:CALLBACK-ENTRY NCOMP:CALLBACK-STUB!
: CALLBACK-ID-ACCESSOR ( -- n ) 0 ;

package CALLBACK-ID-TEST
public

: RUN ( -- )
   NCOMP:CALLBACK-ENTRY 8 CODE-BYTES:AT drop {: p:ptr :}
   p LE:U32@ $D2800029 T=       \ MOVZ x9, #1
   p 4 + LE:U32@ $F2A00029 T=   \ MOVK x9, #1, LSL #16
   s" native-callback-stub-immediate: ok" type cr ;

;package

T-RESET
CALLBACK-ID-TEST:RUN
T-REPORT
