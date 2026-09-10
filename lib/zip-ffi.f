\ zip-ffi.f - exact libzip ABI bindings, private to ZIP.
require lib/ffi-abi.f
require lib/zip-types.f

package ZIP
using FFI

$40 constant SYMBOL-BYTES
$40 constant STAT-BYTES
$40 constant NAME-RAW-FLAG
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

s" zip_get_name" SYMBOL-FIND constant NAME-FN
TRUSTED: NAME-CALL ( -- ptr u8 )
   ARGS REG-LENS 3 NAME-FN ffi-call-bounded ;
: C-NAME ( ptr u8 n -- ptr u8 ) {: archive:ptr idx:n :}
   RESET archive 0 READABLE! idx 1 VALUE! NAME-RAW-FLAG 2 VALUE!
   NAME-CALL ;

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

s" zip_source_buffer" SYMBOL-FIND constant SOURCE-FN
TRUSTED: SOURCE-CALL ( -- ptr u8 )
   ARGS REG-LENS 4 SOURCE-FN ffi-call-bounded ;
: C-SOURCE ( ptr u8 ptr u8 n -- ptr u8 ) {: archive:ptr buf:ptr len:n :}
   RESET archive 0 READABLE! buf 1 READABLE! len 2 VALUE! 0 3 VALUE!
   SOURCE-CALL ;

s" zip_file_replace" SYMBOL-FIND constant REPLACE-FN
TRUSTED: REPLACE-CALL ( -- n )
   ARGS REG-LENS 4 REPLACE-FN ffi-call-bounded ;
: C-REPLACE ( ptr u8 n ptr u8 -- n ) {: archive:ptr idx:n source:ptr :}
   RESET archive 0 READABLE! idx 1 VALUE! source 2 READABLE! 0 3 VALUE!
   REPLACE-CALL ;

s" zip_source_free" SYMBOL-FIND constant SOURCE-FREE-FN
TRUSTED: SOURCE-FREE-CALL ( -- )
   ARGS REG-LENS 1 SOURCE-FREE-FN ffi-call-bounded drop ;
: C-SOURCE-FREE ( ptr u8 -- ) {: source:ptr :}
   RESET source 0 READABLE!
   SOURCE-FREE-CALL ;

s" zip_close" SYMBOL-FIND constant COMMIT-FN
TRUSTED: COMMIT-CALL ( -- n )
   ARGS REG-LENS 1 COMMIT-FN ffi-call-bounded ;
: C-COMMIT ( ptr u8 -- n ) {: archive:ptr :}
   RESET archive 0 READABLE!
   COMMIT-CALL ;

s" zip_discard" SYMBOL-FIND constant DISCARD-FN
TRUSTED: DISCARD-CALL ( -- )
   ARGS REG-LENS 1 DISCARD-FN ffi-call-bounded drop ;
: C-DISCARD ( ptr u8 -- ) {: archive:ptr :}
   RESET archive 0 READABLE!
   DISCARD-CALL ;

;using
;package
