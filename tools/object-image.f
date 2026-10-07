\ object-image.f - assemble one validated cached object as a native image.
require lib/object-link.f

package OBJIMG
using A64ICODE

: NONEMPTY-TEXT ( -- )
   OBJLINK:TEXT-SIZE 0 <= if E-OBJ-SCHEMA throw then ;

public

: RESET ( -- ) OBJLINK:RESET ;
: ADD ( -- ) OBJLINK:ADD ;

;package

package OBJIMG

: LOAD-TARGET ( -- )
   HB-TARGET-LINUX? HB-TARGET-MACOS? or if
      s" tools/object-image-arm.f" required exit
   then
   HB-TARGET-LINUX-X86-64? if
      s" tools/object-image-x64.f" required exit
   then
   E-OBJ-SCHEMA throw ;

' LOAD-TARGET
;package
execute
