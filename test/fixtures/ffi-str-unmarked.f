\ ffi-str-unmarked.f - a FUNCTION: str input declared in a file without
\ split-str, where str is one value.
require src/lib/ffi-abi.f

package FFI-STR-UNMARKED
public
VERSIONED-LIBRARY z 1
FUNCTION: FFI-U-CRC crc32 ( n str -- n ) ;FUNCTION

: FFI-U-CRC-A ( -- n ) 0 s" a" FFI-U-CRC ;
;package
