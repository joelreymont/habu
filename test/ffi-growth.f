\ ffi-growth.f - declarations across the former fixed FFI table limit.
require src/lib/ffi-abi.f
require src/lib/codegen.f

PROCESS-SYMBOLS
FUNCTION: FFI-GROW-EARLY getpid ( -- i32 ) ;FUNCTION

: FFI-GROW-WARM ( -- )
   FFI-GROW-EARLY 0 <= if s" ffi growth: warm call" 74 die then ;
FFI-GROW-WARM

128 CODEGEN:BUFFER FFI-GROW-SYM

: FFI-GROW-SYM$ ( n -- ptr u8 n )
   {: id:n :}
   FFI-GROW-SYM CODEGEN:RESET
   s" habu-ffi-growth-" FFI-GROW-SYM CODEGEN:APPEND-STRING
   id FFI-GROW-SYM CODEGEN:APPEND-DECIMAL
   FFI-GROW-SYM CODEGEN:CONTENTS ;

: FFI-GROW-FILL ( -- )
   270 0 ?do
      i FFI-GROW-SYM$ FFI:PROCESS 0 FFI:DECLARE drop
   loop ;

FFI-GROW-FILL
FUNCTION: FFI-GROW-LATE strlen ( ptr u8 -- n ) ;FUNCTION

128 CODEGEN:BUFFER FFI-GROW-PATH
variable FFI-GROW-FIRST-LIB
variable FFI-GROW-LAST-LIB

: FFI-GROW-PATH$ ( n -- ptr u8 n ) {: id:n :}
   FFI-GROW-PATH CODEGEN:RESET
   s" /tmp/habu-ffi-growth-missing-" FFI-GROW-PATH CODEGEN:APPEND-STRING
   id FFI-GROW-PATH CODEGEN:APPEND-DECIMAL
   FFI-GROW-PATH CODEGEN:CONTENTS ;

: FFI-GROW-LIBRARIES ( -- )
   270 0 ?do
      i FFI-GROW-PATH$ FFI:LIBRARY-PATH
      i 0= if dup FFI-GROW-FIRST-LIB ! then
      FFI-GROW-LAST-LIB !
   loop ;

FFI-GROW-LIBRARIES

: FFI-GROW-SYSTEM$ ( -- ptr u8 n )
   HB-TARGET-MACOS? if s" /usr/lib/libSystem.B.dylib" exit then
   s" libc.so.6" ;

s" getpid" FFI-GROW-SYSTEM$ FFI:LIBRARY-PATH 0 FFI:DECLARE
constant FFI-GROW-LIB-ROW

: FFI-GROW-ID-CHECK ( -- )
   0 FFI-GROW-PATH$ FFI:LIBRARY-PATH FFI-GROW-FIRST-LIB @ <>
   if s" ffi growth: early library" 74 die then
   FFI-GROW-LAST-LIB @ FFI-GROW-FIRST-LIB @ <=
   if s" ffi growth: library ids" 74 die then ;

FFI-GROW-ID-CHECK

: FFI-GROW-CHECK ( -- )
   FFI-GROW-EARLY 0 <= if s" ffi growth: early call" 74 die then
   s\" abc\z" drop FFI-GROW-LATE 3 <>
   if s" ffi growth: late call" 74 die then
   FFI:RESET FFI-GROW-LIB-ROW FFI:CALL 0 <=
   if s" ffi growth: late library call" 74 die then
   s" ffi growth: ok" type cr ;

FFI-GROW-CHECK

: MAIN ( -- ) FFI-GROW-CHECK ;
