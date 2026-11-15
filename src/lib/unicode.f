\ unicode.f - Unicode whitespace and locale-independent full casefold equality.
\ CASEFOLD= compares whole UTF-8 spans by full case folding (Straße equals
\ STRASSE), with no normalization, keeping embedded NULs. Folding is
\ libunistring's u8_casecmp and u8_casefold with NULL language and normalization,
\ so the casefold data is the installed library's. STORAGE CLASS. TASK-LOCAL:
\ C's result cell, a TASK:+USERS row.

require src/lib/unicode/class.f
require src/lib/utf8-scalar.f
require src/lib/ffi-abi.f
s" src/lib/task.f" required

package UNICODE
using FFI

public

-9150 constant E-UTF8
-9153 constant E-CASEFOLD
-9154 constant E-CAPACITY

EXPORT UNICODE-CLASS:WHITE-SPACE?
EXPORT UNICODE-CLASS:ALPHABETIC?
EXPORT UNICODE-CLASS:UPPERCASE?
EXPORT UNICODE-CLASS:MARK?

private

: VALID-LENGTH ( n -- )
   0 < if E-UTF8 throw then ;

: VALID-UTF8 ( ptr u8 n -- )
   {: source:ptr size:n :}
   size VALID-LENGTH
   source size UTF8:VALID? 0= if E-UTF8 throw then ;

1 cells TASK:+USERS RESULT-ROW u8

\ C's four-byte int output, in a task-local cell each call zeroes first.
: RESULT ( -- ptr n ) 0 RESULT-ROW CELL-VIEW ;

\ Exact u8_casecmp schema: two counted, read-only UTF-8 spans, two NULL
\ policy pointers, and one four-byte writable result. NULL resultbuf asks
\ u8_casefold for one owned allocation, which only the matched private free
\ binding may consume. No callable raw binding or configurable foreign symbol
\ escapes this package.
VERSIONED-LIBRARY unistring 5
FUNCTION: COMPARE-CALL u8_casecmp ( ptr u8 n ptr u8 n n n ptr u8 -- i32 ) ;FUNCTION
FUNCTION: FOLD-CALL u8_casefold ( ptr u8 n n n n ptr u8 -- ptr u8 ) ;FUNCTION
PROCESS-SYMBOLS
FUNCTION: RELEASE free ( ptr u8 -- ) ;FUNCTION

: ALLOCATE-FOLD ( ptr u8 n -- ptr u8 n )
   2dup VALID-UTF8
   0 RESULT !
   0 0 0 RESULT BYTE-VIEW FOLD-CALL
   dup >CELL 0= if E-CASEFOLD throw then
   RESULT @ ;

\ Keep the same stack shape on success and failure for the cleanup path.
: COPY-FOLD ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   {: source:ptr size:n out:ptr capacity:n :}
   capacity size < if E-CAPACITY throw then
   source out size BYTE-COPY
   source size out capacity ;

public

: CASEFOLD= ( ptr u8 n ptr u8 n -- bool )
   {: left:ptr left-size:n right:ptr right-size:n :}
   left left-size VALID-UTF8 right right-size VALID-UTF8
   0 RESULT !
   left left-size right right-size 0 0 RESULT BYTE-VIEW COMPARE-CALL
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

seal-package
;using
;package
