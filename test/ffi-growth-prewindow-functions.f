\ Pre-window function rows for the stripped FFI image regression.
require src/lib/ffi-abi.f
require src/lib/codegen.f

package FFI-GROW-PRE-FUNCTIONS

: SYSTEM$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" /usr/lib/libSystem.B.dylib" exit then
   s" libc.so.6" ;

s" getpid" SYSTEM$ FFI:LIBRARY-PATH 0 FFI:DECLARE constant EARLY

128 CODEGEN:BUFFER SYM
: SYM$ ( n -- ptr u8 n )
   {: id:n :}
   SYM CODEGEN:RESET
   s" habu-ffi-prewindow-" SYM CODEGEN:APPEND-STRING
   id SYM CODEGEN:APPEND-DECIMAL
   SYM CODEGEN:CONTENTS ;

: FILL ( -- )
   268 0 ?do i SYM$ FFI:PROCESS 0 FFI:DECLARE drop loop ;
FILL

s" getppid" FFI:PROCESS 0 FFI:DECLARE constant LATE

: WARM ( -- )
   FFI:RESET EARLY FFI:CALL 0 <=
   if s" ffi prewindow: initial row" 74 die then
   FFI:RESET LATE FFI:CALL 0 <=
   if s" ffi prewindow: final row" 74 die then ;
WARM
;package
