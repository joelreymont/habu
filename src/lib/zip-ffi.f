\ zip-ffi.f - exact libzip/libc ABI bindings, private to ZIP.
require src/lib/ffi-abi.f
require src/lib/image-lifecycle.f
require src/lib/zip-state.f

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
FUNCTION: C-PREAD pread ( n ptr u8 n n -- n )
   1 2 WRITES-ARG
;FUNCTION
FUNCTION: C-PWRITE pwrite ( n ptr u8 n n -- n ) ;FUNCTION

\ mkstemp rewrites its template in place, so the template's length is the
\ writable extent; no argument of the C call carries it, which FUNCTION: needs.
s" mkstemp" FFI:PROCESS 1 FFI:DECLARE constant MKSTEMP-ROW

: DETACH-OWNED ( ptr n -- ptr n ) {: owner:ptr :}
   STATE-GET
   INPUTS begin dup @ NULL? 0= while
      dup @ INPUT-OWNER @ owner = if
         dup @ {: found:ptr :}
         found LINK @ swap !
         STATE-RELEASE found exit
      then
      @ LINK
   repeat
   @ STATE-RELEASE ;

: CLOSE-INPUT-OBJECT ( ptr n -- bool ) {: node:ptr :}
   node INPUT-KIND @ 1 = if node INPUT-FD @ C-CLOSE-FD 0<> exit then
   node INPUT-KIND @ 2 = if false exit then
   node INPUT-OBJECT @ dup NULL? if drop false else C-FCLOSE 0<> then ;

: CLOSE-OWNED ( ptr n -- bool ) {: owner:ptr :}
   false begin owner DETACH-OWNED dup NULL? 0= while
      {: input:ptr :}
      input CLOSE-INPUT-OBJECT or
      input NODE-FREE
   repeat drop ;

: DISCARD-NODE ( ptr n -- )
   {: node:ptr :}
   node CLOSE-OWNED
   node OBJECT @ dup NULL? if drop else C-DISCARD then
   node SOURCE-FD @ dup 0 >= if C-CLOSE-FD 0<> or else drop then
   node SNAP-FD @ dup 0 >= if C-CLOSE-FD 0<> or else drop then
   node RAW-PTR @ dup NULL? if drop else node RAW-LEN @ munmap 0<> or then
   node SOURCE-LIVE @ 0<> if node SOURCE-PTR @ unlink 0<> or then
   node FORGET-ARCHIVE
   if E-READ throw then ;


: RETIRE-ARCHIVES ( -- )
   begin ARCHIVES @ dup NULL? 0= while DISCARD-NODE repeat drop ;


\ Archive descriptors and optional libzip objects are process-local: preparing
\ an image discards every live archive, and later opens register again.
: CLEANUP-NATIVE ( -- )
   RETIRE-ARCHIVES
   0 REGISTERED ! ;


: REGISTER-CLEANUP-LOCKED ( -- )
   REGISTERED @ 0= if
      [: CLEANUP-NATIVE ;] IMAGE-LIFECYCLE:REGISTER
      1 REGISTERED !
   then ;

: REGISTER-CLEANUP ( -- )
   STATE-GET [: REGISTER-CLEANUP-LOCKED ;] [: STATE-RELEASE ;] finally ;


: C-OPEN ( ptr u8 n -- ptr u8 ) {: path:ptr flags:n :}
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
