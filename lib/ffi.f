\ ffi.f - compatibility entry for the sealed FFI package.

require lib/errors.f
require lib/string.f
require lib/ffi-abi.f

FFI:NOW constant RTLD-NOW

: DLOPEN ( ptr u8 n -- n ) FFI:DLOPEN ;
: DLSYM ( n ptr u8 -- n ) FFI:DLSYM ;
