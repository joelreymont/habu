\ The two native code buffers. All positions exposed here are byte offsets.
require src/arch/arm64/icode.f
require src/arch/arm64/link.f
require src/arch/x86-64/icode.f
require src/arch/x86-64/link.f

package LINKER
private
package NATIVE
private

TYPED-VARIABLE MACHINE-X64 bool

public

: X64? ( -- bool ) MACHINE-X64 @ ;
: X64! ( bool -- ) MACHINE-X64 ! ;
: CODE ( -- ptr u8 ) X64? if X64CODE:CODE else A64ICODE:CODE then ;
: SIZE ( -- n ) X64? if X64CODE:ASM-LEN else A64ICODE:ASM-LEN then ;

: U64, ( n -- ) X64? if X64CODE:U64, else A64ICODE:DCQ, then ;
: BYTES, ( ptr u8 n -- )
   X64? if X64CODE:BYTES, else A64ICODE:BYTES, then ;

: SPACE ( n -- ptr u8 )
   X64? if X64CODE:SPACE exit then
   4 / {: words:n :}
   words A64ICODE:ASM-CP?
   A64ICODE:CODE A64ICODE:ASM-LEN +
   words A64ICODE:ASM-CP +! ;

: LABEL@ ( ptr n -- label ) @ >LABEL ;
: LABEL! ( label ptr n -- ) swap LABEL>N swap ! ;
: BIND ( label -- ) X64? if X64CODE:LBL, else A64ICODE:LBL, then ;
: BOUND? ( label -- bool )
   X64? if X64CODE:LABEL-BOUND? else A64ICODE:LBL-BOUND? then ;

: PAD8 ( -- )
   X64? if
      begin SIZE 7 and 0<> while 0 X64CODE:ASM-SINK BUF:APPEND-BYTE repeat
   else SIZE 7 and 0<> if 0 A64ICODE:EMITW then then ;

: CODE-ADDRESS! ( n n ptr u8 -- )
   X64? if X64:CODE-ADDRESS! else ARM64:CODE-ADDRESS! then ;

\ A cached host body can have a different machine from the product.
: PATCH ( n ptr u8 n [ n -- n ] [ n -- n ] -- )
   {: e:n dst:ptr at:n data resolve :}
   e NSHADOW:ARCH@ CTARGET-ARCH:X86-64 CTARGET-ARCH:EQ if
      e dst at data resolve X64:PATCH
   else e dst at data resolve ARM64:PATCH then ;

: PLACE ( n ptr u8 n [ n -- n ] [ n -- n ] -- )
   {: e:n dst:ptr at:n data resolve :}
   e NSHADOW:BYTES dst e NSHADOW:SIZE BYTE-COPY
   e dst at data resolve PATCH ;

;package
;package
