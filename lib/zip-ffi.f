\ zip-ffi.f - exact libzip/libc ABI bindings, private to ZIP.
require lib/ffi-abi.f
require lib/image-lifecycle.f
require lib/zip-state.f

package ZIP

variable REGISTERED

\ The only asserted effects are exact C entry points. Habu owns all policy,
\ range checks and storage. Pointer results are libzip-owned opaque objects.
VERSIONED-LIBRARY zip 5
FUNCTION: OPEN-CALL zip_open ( ptr u8 n n -- ptr u8 ) ;FUNCTION
FUNCTION: COUNT-CALL zip_get_num_entries ( ptr u8 n -- n ) ;FUNCTION
FUNCTION: STAT-CALL zip_stat_index ( ptr u8 n n ptr u8 -- i32 )
   3 STAT-BYTES WRITES-BYTES             \ zip_stat_t
;FUNCTION
FUNCTION: FOPEN-CALL zip_fopen_index ( ptr u8 n n -- ptr u8 ) ;FUNCTION
FUNCTION: C-FREAD zip_fread ( ptr u8 ptr u8 n -- n )
   1 2 WRITES-ARG                        \ the caller's buffer
;FUNCTION
FUNCTION: C-FCLOSE zip_fclose ( ptr u8 -- i32 ) ;FUNCTION
FUNCTION: C-DISCARD zip_discard ( ptr u8 -- ) ;FUNCTION

PROCESS-SYMBOLS
FUNCTION: C-CLOSE-FD close ( n -- i32 ) ;FUNCTION

\ mkstemp rewrites its template in place, so the template's length is the
\ writable extent; no argument of the C call carries it, which FUNCTION: needs.
s" mkstemp" FFI:PROCESS 1 FFI:DECLARE constant MKSTEMP-ROW


: DISCARD-NODE ( ptr n -- )
   dup OBJECT @ dup NULL? if drop else C-DISCARD then
   FORGET-ARCHIVE ;


: RETIRE-ARCHIVES ( -- )
   begin ARCHIVES @ dup NULL? 0= while DISCARD-NODE repeat drop
   STAT-BYTES 0 ?do 0 STAT-DATA i + c! loop ;


\ A libzip object is a process-local address: preparing an image discards every
\ live archive, and the archives opened after that register again.
: CLEANUP-NATIVE ( -- )
   RETIRE-ARCHIVES
   0 REGISTERED ! ;


: REGISTER-CLEANUP ( -- )
   REGISTERED @ 0= if
      [: CLEANUP-NATIVE ;] IMAGE-LIFECYCLE:REGISTER
      1 REGISTERED !
   then ;


: C-OPEN ( ptr u8 n -- ptr u8 ) {: path:ptr flags:n :}
   REGISTER-CLEANUP
   path flags 0 OPEN-CALL ;

: C-COUNT ( ptr u8 -- n ) 0 COUNT-CALL ;

: C-STAT ( ptr u8 n ptr n -- n ) {: archive:ptr idx:n stat:ptr :}
   archive idx 0 stat BYTE-VIEW STAT-CALL ;

: C-FOPEN ( ptr u8 n -- ptr u8 ) 0 FOPEN-CALL ;

: C-MKSTEMP ( ptr u8 n -- n ) {: path:ptr len:n :}
   FFI:RESET path len 0 FFI:WRITABLE!
   MKSTEMP-ROW FFI:CALL $FFFFFFFF and
   dup $80000000 and 0 <> if $FFFFFFFF00000000 or then ;

;package
