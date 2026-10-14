\ Pre-window library paths for the stripped FFI image regression.
require src/lib/ffi-abi.f
require src/lib/codegen.f

package FFI-GROW-PRE-PATHS
128 CODEGEN:BUFFER PATH
: PATH$ ( n -- ptr u8 n ) {: id:n :}
   PATH CODEGEN:RESET
   s" /tmp/habu-ffi-prewindow-missing-" PATH CODEGEN:APPEND-STRING
   id PATH CODEGEN:APPEND-DECIMAL
   PATH CODEGEN:CONTENTS ;
: FILL ( -- )
   270 0 ?do i PATH$ FFI:LIBRARY-PATH drop loop ;
FILL
;package
