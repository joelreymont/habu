\ bypass.f - reaches C through the FFI.
require src/lib/ffi-abi.f
PROCESS-SYMBOLS
FUNCTION: DESIGN-GETPID getpid ( -- i32 ) ;FUNCTION
: CALL-C ( -- ) s" design called C, pid " type DESIGN-GETPID . cr ;
CALL-C
