split-str
\ zip-ffi.f - exact libzip/libc ABI bindings, private to ZIP.
\ STORAGE CLASS. None of its own: the registries it walks are zip-state.f's.
require src/lib/ffi-abi.f
require src/lib/zip-state.f
require src/lib/fs-mutate.f

package ZIP

\ The only asserted effects are exact C entry points. Habu owns all policy,
\ range checks and storage. Pointer results are libzip-owned opaque objects.
VERSIONED-LIBRARY zip 5,4                 \ Debian and Ubuntu ship libzip as libzip.so.4
FUNCTION: OPEN-CALL zip_open ( ^u8 n n -- ^u8 ) ;FUNCTION
FUNCTION: COUNT-CALL zip_get_num_entries ( ^u8 n -- n ) ;FUNCTION
FUNCTION: STAT-CALL zip_stat_index ( ^u8 n n ^u8 -- i32 ) ;FUNCTION
FUNCTION: FOPEN-CALL zip_fopen_index ( ^u8 n n -- ^u8 ) ;FUNCTION
FUNCTION: C-FREAD zip_fread ( ^u8 ^u8 n -- n ) ;FUNCTION
FUNCTION: C-FCLOSE zip_fclose ( ^u8 -- i32 ) ;FUNCTION
FUNCTION: C-DISCARD zip_discard ( ^u8 -- ) ;FUNCTION

PROCESS-SYMBOLS
FUNCTION: C-CLOSE-FD close ( n -- i32 ) ;FUNCTION
FUNCTION: C-PREAD pread ( n ^u8 n n -- n ) ;FUNCTION
FUNCTION: C-PWRITE pwrite ( n ^u8 n n -- n ) ;FUNCTION

: DETACH-OWNED ( ^n -- ^n ) {: owner:^ :}
   STATE-GET
   INPUTS begin dup @ NULL? 0= while
      dup @ INPUT-OWNER @ owner = if
         dup @ {: found:^ :}
         found LINK @ swap !
         STATE-RELEASE found exit
      then
      @ LINK
   repeat
   @ STATE-RELEASE ;

\ 0, or E-FS-IO for a failed descriptor close, E-READ for a failed libzip one.
: CLOSE-INPUT-OBJECT ( ^n -- n ) {: node:^ :}
   node INPUT-KIND @ 1 = if
      node INPUT-FD @ C-CLOSE-FD 0<> if E-FS-IO else 0 then exit
   then
   node INPUT-KIND @ 2 = if 0 exit then
   node INPUT-OBJECT @ dup NULL? if drop 0 exit then
   C-FCLOSE 0<> if E-READ else 0 then ;

: WORSE-CODE ( n n -- n ) {: a:n b:n :}
   a E-FS-IO = b E-FS-IO = or if E-FS-IO exit then
   a 0<> if a else b then ;

: CLOSE-OWNED ( ^n -- n ) {: owner:^ :}
   0 begin owner DETACH-OWNED dup NULL? 0= while
      {: input:^ :}
      input CLOSE-INPUT-OBJECT WORSE-CODE
      input NODE-FREE
   repeat drop ;

: DISCARD-NODE ( ^n -- )
   {: node:^ :}
   node CLOSE-OWNED {: inputs:n :}
   node OBJECT @ dup NULL? if drop else C-DISCARD then
   false
   node SOURCE-FD @ dup 0 >= if C-CLOSE-FD 0<> or else drop then
   node SNAP-FD @ dup 0 >= if C-CLOSE-FD 0<> or else drop then
   node RAW-PTR @ dup NULL? if drop else node RAW-LEN @ munmap 0<> or then
   node SOURCE-LIVE @ 0<> if
      node SOURCE-PTR @ node SOURCE-LEN @ FS:CLEANUP-FORGET
      node SOURCE-PTR @ unlink 0<> or
   then
   node FORGET-ARCHIVE
   if E-FS-IO throw then
   inputs 0<> if inputs throw then ;


: C-OPEN ( ^u8 n -- ^u8 ) {: path:^ flags:n :}
   path flags 0 OPEN-CALL ;

: C-COUNT ( ^u8 -- n ) 0 COUNT-CALL ;

: C-STAT ( ^u8 n ^entry-stat -- n ) {: archive:^ idx:n stat:^ :}
   archive idx 0 stat BYTE-VIEW STAT-CALL ;

: C-FOPEN ( str -- ^u8 ) 0 FOPEN-CALL ;

;package
