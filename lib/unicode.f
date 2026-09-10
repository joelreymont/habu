\ unicode.f - Unicode whitespace and locale-independent full casefold equality.

require lib/unicode/class.f
require lib/utf8-scalar.f
require lib/ffi-abi.f

package UNICODE
using FFI

public

-9150 constant E-UTF8
-9151 constant E-LIBRARY
-9152 constant E-SYMBOL
-9153 constant E-CASEFOLD

EXPORT UNICODE-CLASS:WHITE-SPACE?

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

: COMPARE-SYMBOL ( -- n )
   LIBRARY$ NOW DLOPEN dup 0= if E-LIBRARY throw then
   s\" u8_casecmp\z" drop DLSYM dup 0= if E-SYMBOL throw then ;

\ Bind once on source load, as other exact stdlib foreign bindings do.
COMPARE-SYMBOL constant COMPARE-XT

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
   ARGS REG-LENS 7 COMPARE-XT ffi-call-bounded ;

get-current prot-wid-add

public

: CASEFOLD= ( ptr u8 n ptr u8 n -- bool )
   {: left:ptr left-size:n right:ptr right-size:n :}
   left left-size VALID-UTF8 right right-size VALID-UTF8
   0 RESULT !
   left left-size right right-size COMPARE
   0<> if E-CASEFOLD throw then
   RESULT @ 0= ;

get-current prot-wid-add
;using
;package
