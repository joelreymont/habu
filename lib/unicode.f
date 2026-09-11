\ unicode.f - Unicode whitespace and locale-independent full casefold equality.

require lib/unicode/class.f
require lib/utf8-scalar.f
require lib/ffi-abi.f
require lib/image-lifecycle.f

package UNICODE
using FFI

public

-9150 constant E-UTF8
-9151 constant E-LIBRARY
-9152 constant E-SYMBOL
-9153 constant E-CASEFOLD
-9154 constant E-CAPACITY

EXPORT UNICODE-CLASS:WHITE-SPACE?
EXPORT UNICODE-CLASS:ALPHABETIC?
EXPORT UNICODE-CLASS:UPPERCASE?

private

: VALID-LENGTH ( n -- )
   0 < if E-UTF8 throw then ;

: NEXT-SCALAR ( ptr u8 n n -- n )
   UTF8:NEXT MATCH UTF8:scalar-step
      scalar OF nip ENDOF
      raw-byte OF 2drop E-UTF8 throw ENDOF
   ;MATCH ;

: VALID-UTF8 ( ptr u8 n -- )
   {: source:ptr size:n :}
   size VALID-LENGTH
   0 begin dup size < while source size rot NEXT-SCALAR repeat drop ;

: LIBRARY$ ( -- ptr u8 )
   HB-TARGET-MACOS? if s\" libunistring.5.dylib\z" drop
   else s\" libunistring.so.5\z" drop then ;

variable LIBRARY
variable COMPARE-XT
variable FOLD-XT
variable FREE-XT
variable CLOSE-XT
variable REGISTERED
here data-base - negate 7 and allot
variable READY

: SYMBOL ( ptr u8 -- n )
   LIBRARY @ swap DLSYM dup 0= if E-SYMBOL throw then ;

\ Exact dlclose schema: one scalar library handle and one int result.
\ Owner: Unicode foreign bindings; exercised by the image lifecycle regression.
TRUSTED: CLOSE-CALL ( -- n )
   ARGS REG-LENS 1 CLOSE-XT @ ffi-call-bounded ;

: RELEASE-LIBRARY ( -- )
   0 COMPARE-XT ! 0 FOLD-XT ! 0 FREE-XT !
   LIBRARY @ 0<> if
      RESET LIBRARY @ 0 VALUE!
      CLOSE-CALL 0<> if E-LIBRARY throw then
      0 LIBRARY !
   then
   0 CLOSE-XT ! ;

\ Capture is quiescent. Release the owned dlopen reference and remove every
\ process address before the image records these mutable cells.
: RESET-SYMBOLS ( -- )
   0 READY atomic!
   RELEASE-LIBRARY
   0 REGISTERED ! ;

: REGISTER-CLEANUP ( -- )
   REGISTERED @ 0= if
      [: RESET-SYMBOLS ;] IMAGE-LIFECYCLE:REGISTER
      1 REGISTERED !
   then ;

\ RTLD_DEFAULT is -2 on Darwin and NULL on Linux.
: DEFAULT-HANDLE ( -- n )
   HB-TARGET-MACOS? if -2 else 0 then ;

: LOAD-SYMBOLS ( -- )
   REGISTER-CLEANUP
   RELEASE-LIBRARY
   DEFAULT-HANDLE s\" dlclose\z" drop DLSYM dup 0= if E-SYMBOL throw then CLOSE-XT !
   LIBRARY$ NOW DLOPEN dup 0= if E-LIBRARY throw then LIBRARY !
   s\" u8_casecmp\z" drop SYMBOL COMPARE-XT !
   s\" u8_casefold\z" drop SYMBOL FOLD-XT !
   s\" free\z" drop SYMBOL FREE-XT ! ;

: INIT ( -- )
   begin
      READY atomic@ 2 = if exit then
      0 1 READY atomic-cas 0= if
         [: LOAD-SYMBOLS ;] catch {: error:n :}
         error 0<> if
            [: RELEASE-LIBRARY ;] catch {: cleanup:n :}
            0 READY atomic!
            cleanup 0<> if cleanup throw then
            error throw
         then
         2 READY atomic! exit
      then
   again ;

\ Slot 15 is task-local FFI scratch, beyond this call's seven arguments.
\ Zero the complete cell, then expose exactly C's four-byte int output.
: RESULT ( -- ptr n )
   ARGS 15 cells + CELL-VIEW ;

\ Exact u8_casecmp schema: two counted, read-only UTF-8 spans, two NULL
\ policy pointers, and one four-byte writable result. No callable raw binding
\ or configurable foreign symbol escapes this package.
TRUSTED: COMPARE ( ptr u8 n ptr u8 n -- n )
   {: left:ptr left-size:n right:ptr right-size:n :}
   RESET
   left 0 READABLE! left-size 1 VALUE!
   right 2 READABLE! right-size 3 VALUE!
   0 4 VALUE! 0 5 VALUE!
   RESULT 4 6 WRITABLE!
   ARGS REG-LENS 7 COMPARE-XT @ ffi-call-bounded ;

\ NULL resultbuf requests one owned allocation. The returned pointer is
\ refined here; only the matched private free binding may consume it.
TRUSTED: FOLD-CALL ( ptr u8 n -- ptr u8 )
   {: source:ptr size:n :}
   RESET
   source 0 READABLE! size 1 VALUE!
   0 2 VALUE! 0 3 VALUE! 0 4 VALUE!
   RESULT CELL 5 WRITABLE!
   ARGS REG-LENS 6 FOLD-XT @ ffi-call-bounded
   dup 0= if E-CASEFOLD throw then ;

TRUSTED: RELEASE ( ptr u8 -- )
   RESET 0 READABLE!
   ARGS REG-LENS 1 FREE-XT @ ffi-call-bounded drop ;

: ALLOCATE-FOLD ( ptr u8 n -- ptr u8 n )
   2dup VALID-UTF8
   INIT
   0 RESULT ! FOLD-CALL RESULT @ ;

\ Keep the same stack shape on success and failure for the cleanup path.
: COPY-FOLD ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   {: source:ptr size:n out:ptr capacity:n :}
   capacity size < if E-CAPACITY throw then
   source out size BYTE-COPY
   source size out capacity ;

get-current prot-wid-add

public

: CASEFOLD= ( ptr u8 n ptr u8 n -- bool )
   {: left:ptr left-size:n right:ptr right-size:n :}
   left left-size VALID-UTF8 right right-size VALID-UTF8
   INIT
   0 RESULT !
   left left-size right right-size COMPARE
   0<> if E-CASEFOLD throw then
   RESULT @ 0= ;

: FOLDED-BYTES ( ptr u8 n -- n )
   ALLOCATE-FOLD swap RELEASE ;

: FOLD ( ptr u8 n ptr u8 n -- n )
   {: out:ptr capacity:n :}
   ALLOCATE-FOLD {: folded:ptr size:n :}
   folded size out capacity [: COPY-FOLD ;] catch
   {: error:n :} 2drop 2drop
   \ catch restores depth, not cells overwritten by a callee before throw.
   \ The owner is kept in this outer lexical frame, never recovered from args.
   folded RELEASE
   error 0<> if error throw then
   size ;

get-current prot-wid-add
;using
;package
