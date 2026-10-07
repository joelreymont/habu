\ FFI releases the library references its rows own when an image is prepared.
\ test/ffi-library-lifetime.f runs CHECK under --load, and test/stripped-image.f's
\ application runs RUN in a stripped executable, whose line guards that
\ executable's PREPARE: it starts with an empty lifecycle registry and FFI's
\ registered flag clear and replays no declaration, so only its first resolution
\ can arm FFI's cleanup. Without it the PREPARE left libzip loaded.
\
\ A FUNCTION: row in a library dlopens that library at its first call, and the
\ handle is an owned reference. IMAGE-LIFECYCLE:PREPARE must release it: a
\ PREPARE that only forgot the handle left the library loaded, and every later
\ first call took another reference. RTLD_NOLOAD asks the loader whether the
\ library is loaded without loading it, and each open it answers is closed
\ again, so the probe holds no reference of its own.
\
\ The query names the image by the path the loader recorded for it, which the
\ probe learns once through dladdr on a reference it opens and closes again:
\ under RTLD_NOLOAD dyld finds a loaded image by absolute path, and answers
\ NULL for the rendered libzip.5.dylib while that image is loaded.

require lib/test.f
require lib/codegen.f
require lib/image-lifecycle.f
require lib/ffi-abi.f

package FFI-LIBRARY-LIFETIME

$4A constant FAILURE-RC

\ libzip, by the name lib/zip-ffi.f renders for it.
VERSIONED-LIBRARY zip 5
FUNCTION: ZIP-VERSION zip_libzip_version ( -- n ) ;FUNCTION

PROCESS-SYMBOLS
FUNCTION: PROBE-OPEN dlopen ( ptr u8 n -- n ) ;FUNCTION
FUNCTION: PROBE-CLOSE dlclose ( n -- i32 ) ;FUNCTION
FUNCTION: PROBE-SYMBOL dlsym ( n ptr u8 -- n ) ;FUNCTION
FUNCTION: PROBE-IMAGE dladdr ( n ptr u8 -- i32 )
   1 $20 WRITES-BYTES                 \ Dl_info: four pointers, dli_fname first
;FUNCTION
FUNCTION: PROBE-COPY strncpy ( ptr u8 n n -- n ) 0 2 WRITES-ARG ;FUNCTION

FFI:LIBRARY-PATH-CAP constant PATH-BYTES

PATH-BYTES CODEGEN:BUFFER ZIP-NAME
PATH-BYTES BUFFER: ZIP-NAME-Z
PATH-BYTES BUFFER: ZIP-PATH               \ strncpy leaves its last byte the NUL
create DL-INFO $20 allot

\ <dlfcn.h> RTLD_NOLOAD: 0x10 on macOS, 0x4 in glibc.
: NOLOAD ( -- n ) HB-TARGET-MACOS? if $10 else $4 then ;

\ The path the loader records for libzip, read through a reference this opens
\ and closes again; false when libzip does not open or name its image.
: FIND-ZIP-PATH ( -- bool )
   s" zip" 5 ZIP-NAME FFI:LIBRARY-NAME$ ZIP-NAME-Z FFI:CSTR
   ZIP-NAME-Z FFI:NOW PROBE-OPEN {: handle:n :}
   handle 0= if false exit then
   handle s\" zip_libzip_version\z" drop PROBE-SYMBOL DL-INFO PROBE-IMAGE 0<>
   {: found:bool :}
   found if ZIP-PATH DL-INFO @ PATH-BYTES 1 - PROBE-COPY drop then
   handle PROBE-CLOSE 0 T=
   found ;

: ZIP-LOADED? ( -- bool )
   ZIP-PATH NOLOAD FFI:NOW or PROBE-OPEN {: handle:n :}
   handle 0= if false exit then
   handle PROBE-CLOSE 0 T=
   true ;

: LIFETIME ( -- )
   s" libzip is not loaded before its row's first call" T-LABEL
   ZIP-LOADED? TFALSE
   s" the row's first call loads libzip" T-LABEL
   ZIP-VERSION 0 T<>
   ZIP-LOADED? TTRUE
   s" PREPARE releases the reference that call took" T-LABEL
   IMAGE-LIFECYCLE:PREPARE
   ZIP-LOADED? TFALSE
   s" the first call after PREPARE loads libzip again" T-LABEL
   ZIP-VERSION 0 T<>
   ZIP-LOADED? TTRUE
   s" a second PREPARE releases that reference too" T-LABEL
   IMAGE-LIFECYCLE:PREPARE
   ZIP-LOADED? TFALSE ;

public

\ The assertions, counted by lib/test.f; the caller resets and reports.
: CHECK ( -- )
   s" the loader names the image libzip.5 opens" T-LABEL
   FIND-ZIP-PATH dup TTRUE
   if LIFETIME then ;

\ The stripped application's line, after the failed assertions' own lines.
: RUN ( -- )
   T-RESET CHECK
   T-FAILURES 0<> if s" ffi-library-lifetime: assertion failed" FAILURE-RC die then
   s" ffi-library-lifetime: ok" type cr ;

;package
