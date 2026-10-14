\ Linker-only FFI declarations force both latched tables to move after the
\ application's DATA window closes. These rows must not enter its saved image.
require src/lib/ffi-abi.f
require src/lib/codegen.f

package FFI-GROW-LINKER
128 CODEGEN:BUFFER PATH
128 CODEGEN:BUFFER SYM
: PATH$ ( n -- ptr u8 n )
   {: id:n :}
   PATH CODEGEN:RESET
   s" /tmp/habu-ffi-linker-missing-" PATH CODEGEN:APPEND-STRING
   id PATH CODEGEN:APPEND-DECIMAL
   PATH CODEGEN:CONTENTS ;
: SYM$ ( n -- ptr u8 n )
   {: id:n :}
   SYM CODEGEN:RESET
   s" habu-ffi-linker-" SYM CODEGEN:APPEND-STRING
   id SYM CODEGEN:APPEND-DECIMAL
   SYM CODEGEN:CONTENTS ;
: FILL ( -- )
   270 0 ?do i SYM$ FFI:PROCESS 0 FFI:DECLARE drop loop
   270 0 ?do i PATH$ FFI:LIBRARY-PATH drop loop ;
FILL
;package
