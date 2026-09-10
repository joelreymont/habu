\ zip-ffi.f - exact libzip/libc ABI bindings, private to ZIP.
require lib/ffi-abi.f
require lib/zip-types.f

package ZIP
using FFI

$40 constant SYMBOL-BYTES
$40 constant STAT-BYTES
create SYMBOL SYMBOL-BYTES allot
variable LIBRARY

: SYMBOL! ( ptr u8 n -- ptr u8 )
   SYMBOL CSTR SYMBOL ;

: LIBRARY-OPEN ( -- )
   s" libzip.so.5" SYMBOL! NOW DLOPEN
   dup 0= if E-LIBRARY throw then LIBRARY ! ;

: SYMBOL-FIND ( ptr u8 n -- n )
   SYMBOL! LIBRARY @ swap DLSYM
   dup 0= if E-LIBRARY throw then ;

LIBRARY-OPEN

\ The only asserted effects are exact C entry points. Habu owns all policy,
\ range checks and storage. Pointer results are libzip-owned opaque objects.
\ Retirement owner: ZIP, when the compiler supports declared C imports.

s" zip_open" SYMBOL-FIND constant OPEN-FN
TRUSTED: OPEN-CALL ( -- ptr u8 )
   ARGS REG-LENS 3 OPEN-FN ffi-call-bounded ;
: C-OPEN ( ptr u8 n -- ptr u8 ) {: path:ptr flags:n :}
   RESET path 0 READABLE! flags 1 VALUE! 0 2 VALUE!
   OPEN-CALL ;

s" zip_get_num_entries" SYMBOL-FIND constant COUNT-FN
TRUSTED: COUNT-CALL ( -- n )
   ARGS REG-LENS 2 COUNT-FN ffi-call-bounded ;
: C-COUNT ( ptr u8 -- n ) {: archive:ptr :}
   RESET archive 0 READABLE! 0 1 VALUE!
   COUNT-CALL ;

s" zip_stat_index" SYMBOL-FIND constant STAT-FN
TRUSTED: STAT-CALL ( -- n )
   ARGS REG-LENS 4 STAT-FN ffi-call-bounded ;
: C-STAT ( ptr u8 n ptr n -- n ) {: archive:ptr idx:n stat:ptr :}
   RESET archive 0 READABLE! idx 1 VALUE! 0 2 VALUE! stat STAT-BYTES 3 WRITABLE!
   STAT-CALL ;

s" zip_fopen_index" SYMBOL-FIND constant FOPEN-FN
TRUSTED: FOPEN-CALL ( -- ptr u8 )
   ARGS REG-LENS 3 FOPEN-FN ffi-call-bounded ;
: C-FOPEN ( ptr u8 n -- ptr u8 ) {: archive:ptr idx:n :}
   RESET archive 0 READABLE! idx 1 VALUE! 0 2 VALUE!
   FOPEN-CALL ;

s" zip_fread" SYMBOL-FIND constant FREAD-FN
TRUSTED: FREAD-CALL ( -- n )
   ARGS REG-LENS 3 FREAD-FN ffi-call-bounded ;
: C-FREAD ( ptr u8 ptr u8 n -- n ) {: file:ptr buf:ptr len:n :}
   RESET file 0 READABLE! buf len 1 WRITABLE! len 2 VALUE!
   FREAD-CALL ;

s" zip_fclose" SYMBOL-FIND constant FCLOSE-FN
TRUSTED: FCLOSE-CALL ( -- n )
   ARGS REG-LENS 1 FCLOSE-FN ffi-call-bounded ;
: C-FCLOSE ( ptr u8 -- n ) {: file:ptr :}
   RESET file 0 READABLE!
   FCLOSE-CALL ;

s" zip_discard" SYMBOL-FIND constant DISCARD-FN
TRUSTED: DISCARD-CALL ( -- )
   ARGS REG-LENS 1 DISCARD-FN ffi-call-bounded drop ;
: C-DISCARD ( ptr u8 -- ) {: archive:ptr :}
   RESET archive 0 READABLE!
   DISCARD-CALL ;

s" mkstemp" SYMBOL-FIND constant MKSTEMP-FN
TRUSTED: MKSTEMP-CALL ( -- n )
   ARGS REG-LENS 1 MKSTEMP-FN ffi-call-bounded ;
: C-MKSTEMP ( ptr u8 n -- n ) {: path:ptr len:n :}
   RESET path len 0 WRITABLE! MKSTEMP-CALL ;

s" close" SYMBOL-FIND constant CLOSE-FD-FN
TRUSTED: CLOSE-FD-CALL ( -- n )
   ARGS REG-LENS 1 CLOSE-FD-FN ffi-call-bounded ;
: C-CLOSE-FD ( n -- n )
   RESET 0 VALUE! CLOSE-FD-CALL ;

;using
;package
