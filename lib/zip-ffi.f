\ zip-ffi.f - exact libzip/libc ABI bindings, private to ZIP.
require lib/ffi-abi.f
require lib/image-lifecycle.f
require lib/zip-state.f

package ZIP
using FFI

$40 constant SYMBOL-BYTES
FFI:LIBRARY-PATH-CAP CODEGEN:BUFFER NAME-BUF
create SYMBOL SYMBOL-BYTES allot
variable LIBRARY
variable INITIALIZED
variable REGISTERED
variable DLCLOSE-FN
variable OPEN-FN
variable COUNT-FN
variable STAT-FN
variable FOPEN-FN
variable FREAD-FN
variable FCLOSE-FN
variable DISCARD-FN
variable MKSTEMP-FN
variable CLOSE-FD-FN

: SYMBOL! ( ptr u8 n -- ptr u8 )
   SYMBOL CSTR SYMBOL ;

: LIBRARY-OPEN ( -- )
   s" zip" 5 NAME-BUF LIBRARY-NAME$ SYMBOL! NOW DLOPEN
   dup 0= if E-LIBRARY throw then LIBRARY ! ;

: SYMBOL-FIND ( ptr u8 n -- n )
   SYMBOL! LIBRARY @ swap DLSYM
   dup 0= if E-LIBRARY throw then ;


: GLOBAL-SYMBOL-FIND ( ptr u8 n -- n )
   SYMBOL! HB-TARGET-MACOS? if -2 else 0 then swap DLSYM
   dup 0= if E-LIBRARY throw then ;


TRUSTED: DLCLOSE-CALL ( -- n )
   ARGS REG-LENS 1 DLCLOSE-FN @ ffi-call-bounded ;


: LIBRARY-CLOSE ( -- )
   LIBRARY @ 0= if exit then
   RESET LIBRARY @ 0 VALUE!
   DLCLOSE-CALL 0<> if E-LIBRARY throw then
   0 LIBRARY ! ;


TRUSTED: DISCARD-CALL ( -- )
   ARGS REG-LENS 1 DISCARD-FN @ ffi-call-bounded drop ;


\ A live object already owns this library; retirement must not initialize it.
: C-DISCARD ( ptr u8 -- )
   RESET 0 READABLE! DISCARD-CALL ;


: DISCARD-NODE ( ptr n -- )
   dup OBJECT @ dup NULL? if drop else C-DISCARD then
   FORGET-ARCHIVE ;


: RETIRE-ARCHIVES ( -- )
   begin ARCHIVES @ dup NULL? 0= while DISCARD-NODE repeat drop
   STAT-BYTES 0 ?do 0 STAT-DATA i + c! loop ;


: CLEAR-NATIVE ( -- )
   0 LIBRARY !
   0 OPEN-FN ! 0 COUNT-FN ! 0 STAT-FN ! 0 FOPEN-FN !
   0 FREAD-FN ! 0 FCLOSE-FN ! 0 DISCARD-FN !
   0 MKSTEMP-FN ! 0 CLOSE-FD-FN ! 0 DLCLOSE-FN !
   0 INITIALIZED ! 0 REGISTERED ! ;


: CLEANUP-NATIVE ( -- )
   0 INITIALIZED !
   RETIRE-ARCHIVES
   LIBRARY-CLOSE
   CLEAR-NATIVE ;


: REGISTER-CLEANUP ( -- )
   REGISTERED @ 0= if
      [: CLEANUP-NATIVE ;] IMAGE-LIFECYCLE:REGISTER
      1 REGISTERED !
   then ;


: INITIALIZE ( -- )
   INITIALIZED @ 0<> if exit then
   REGISTER-CLEANUP
   DLCLOSE-FN @ 0= if s" dlclose" GLOBAL-SYMBOL-FIND DLCLOSE-FN ! then
   LIBRARY @ 0= if LIBRARY-OPEN then
   s" zip_open" SYMBOL-FIND OPEN-FN !
   s" zip_get_num_entries" SYMBOL-FIND COUNT-FN !
   s" zip_stat_index" SYMBOL-FIND STAT-FN !
   s" zip_fopen_index" SYMBOL-FIND FOPEN-FN !
   s" zip_fread" SYMBOL-FIND FREAD-FN !
   s" zip_fclose" SYMBOL-FIND FCLOSE-FN !
   s" zip_discard" SYMBOL-FIND DISCARD-FN !
   s" mkstemp" SYMBOL-FIND MKSTEMP-FN !
   s" close" SYMBOL-FIND CLOSE-FD-FN !
   1 INITIALIZED ! ;

\ The only asserted effects are exact C entry points. Habu owns all policy,
\ range checks and storage. Pointer results are libzip-owned opaque objects.
\ Retirement owner: ZIP, when the compiler supports declared C imports.

TRUSTED: OPEN-CALL ( -- ptr u8 )
   ARGS REG-LENS 3 OPEN-FN @ ffi-call-bounded ;
: C-OPEN ( ptr u8 n -- ptr u8 ) {: path:ptr flags:n :}
   INITIALIZE
   RESET path 0 READABLE! flags 1 VALUE! 0 2 VALUE!
   OPEN-CALL ;

TRUSTED: COUNT-CALL ( -- n )
   ARGS REG-LENS 2 COUNT-FN @ ffi-call-bounded ;
: C-COUNT ( ptr u8 -- n ) {: archive:ptr :}
   INITIALIZE
   RESET archive 0 READABLE! 0 1 VALUE!
   COUNT-CALL ;

TRUSTED: STAT-CALL ( -- n )
   ARGS REG-LENS 4 STAT-FN @ ffi-call-bounded ;
: C-STAT ( ptr u8 n ptr n -- n ) {: archive:ptr idx:n stat:ptr :}
   INITIALIZE
   RESET archive 0 READABLE! idx 1 VALUE! 0 2 VALUE! stat STAT-BYTES 3 WRITABLE!
   STAT-CALL ;

TRUSTED: FOPEN-CALL ( -- ptr u8 )
   ARGS REG-LENS 3 FOPEN-FN @ ffi-call-bounded ;
: C-FOPEN ( ptr u8 n -- ptr u8 ) {: archive:ptr idx:n :}
   INITIALIZE
   RESET archive 0 READABLE! idx 1 VALUE! 0 2 VALUE!
   FOPEN-CALL ;

TRUSTED: FREAD-CALL ( -- n )
   ARGS REG-LENS 3 FREAD-FN @ ffi-call-bounded ;
: C-FREAD ( ptr u8 ptr u8 n -- n ) {: file:ptr buf:ptr len:n :}
   INITIALIZE
   RESET file 0 READABLE! buf len 1 WRITABLE! len 2 VALUE!
   FREAD-CALL ;

TRUSTED: FCLOSE-CALL ( -- n )
   ARGS REG-LENS 1 FCLOSE-FN @ ffi-call-bounded ;
: C-FCLOSE ( ptr u8 -- n ) {: file:ptr :}
   INITIALIZE
   RESET file 0 READABLE!
   FCLOSE-CALL ;

TRUSTED: MKSTEMP-CALL ( -- n )
   ARGS REG-LENS 1 MKSTEMP-FN @ ffi-call-bounded ;
: C-MKSTEMP ( ptr u8 n -- n ) {: path:ptr len:n :}
   INITIALIZE
   RESET path len 0 WRITABLE! MKSTEMP-CALL ;

TRUSTED: CLOSE-FD-CALL ( -- n )
   ARGS REG-LENS 1 CLOSE-FD-FN @ ffi-call-bounded ;
: C-CLOSE-FD ( n -- n )
   INITIALIZE
   RESET 0 VALUE! CLOSE-FD-CALL ;

;using
;package
