\ unicode.f - Unicode whitespace, locale-independent full casefold equality
\ and full lowercasing. CASEFOLD= compares whole UTF-8 spans by full case
\ folding (Straße equals STRASSE), with no normalization, keeping embedded
\ NULs. FOLD writes a span's full case folding and LOWER its full lowercase
\ (Straße folds to strasse but lowercases to straße), and LOWERS-TO? tells
\ whether a span's full lowercase is another span byte for byte. They are
\ libunistring's u8_casecmp, u8_casefold and u8_tolower with NULL language
\ and normalization, so the case data is the installed library's. STORAGE
\ CLASS. TASK-LOCAL: C's result cell, a TASK:+USERS row.

require src/lib/unicode/class.f
require src/lib/utf8-scalar.f
require src/lib/ffi-abi.f
require src/lib/string.f
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
\ policy pointers, and one four-byte writable result. u8_casefold and
\ u8_tolower share one schema; NULL resultbuf asks either for one owned
\ allocation, which only the matched private free binding may consume. No
\ callable raw binding or configurable foreign symbol escapes this package.
VERSIONED-LIBRARY unistring 5
FUNCTION: COMPARE-CALL u8_casecmp ( ptr u8 n ptr u8 n n n ptr u8 -- i32 ) ;FUNCTION
FUNCTION: FOLD-CALL u8_casefold ( ptr u8 n n n n ptr u8 -- ptr u8 ) ;FUNCTION
FUNCTION: LOWER-CALL u8_tolower ( ptr u8 n n n n ptr u8 -- ptr u8 ) ;FUNCTION
PROCESS-SYMBOLS
FUNCTION: RELEASE free ( ptr u8 -- ) ;FUNCTION

\ Runs MAP, FOLD-CALL or LOWER-CALL, on a valid span and answers its
\ owned result and that result's length.
: ALLOCATE-MAP ( ptr u8 n [ ptr u8 n n n n ptr u8 -- ptr u8 ] -- ptr u8 n )
   {: source:ptr size:n map :}
   source size VALID-UTF8
   0 RESULT !
   source size 0 0 0 RESULT BYTE-VIEW map execute
   dup >CELL 0= if E-CASEFOLD throw then
   RESULT @ ;

\ Keep the same stack shape on success and failure for the cleanup path.
: COPY-MAPPED ( ptr u8 n ptr u8 n -- ptr u8 n ptr u8 n )
   {: source:ptr size:n out:ptr capacity:n :}
   capacity size < if E-CAPACITY throw then
   source out size BYTE-COPY
   source size out capacity ;

: MAPPED-BYTES ( ptr u8 n [ ptr u8 n n n n ptr u8 -- ptr u8 ] -- n )
   ALLOCATE-MAP swap RELEASE ;

: MAP-INTO ( ptr u8 n ptr u8 n [ ptr u8 n n n n ptr u8 -- ptr u8 ] -- n )
   {: out:ptr capacity:n map :}
   map ALLOCATE-MAP {: mapped:ptr size:n :}
   mapped size out capacity [: COPY-MAPPED ;] catch
   {: error:n :} 2drop 2drop
   \ catch restores depth, not cells overwritten by a callee before throw.
   \ The owner is kept in this outer lexical frame, never recovered from args.
   mapped RELEASE
   error 0<> if error throw then
   size ;

public

: CASEFOLD= ( ptr u8 n ptr u8 n -- bool )
   {: left:ptr left-size:n right:ptr right-size:n :}
   left left-size VALID-UTF8 right right-size VALID-UTF8
   0 RESULT !
   left left-size right right-size 0 0 RESULT BYTE-VIEW COMPARE-CALL
   0<> if E-CASEFOLD throw then
   RESULT @ 0= ;

: FOLDED-BYTES ( ptr u8 n -- n )
   [: FOLD-CALL ;] MAPPED-BYTES ;

: FOLD ( ptr u8 n ptr u8 n -- n )
   [: FOLD-CALL ;] MAP-INTO ;

: LOWERED-BYTES ( ptr u8 n -- n )
   [: LOWER-CALL ;] MAPPED-BYTES ;

: LOWER ( ptr u8 n ptr u8 n -- n )
   [: LOWER-CALL ;] MAP-INTO ;

\ The second span is checked before the first is lowered, so nothing after
\ ALLOCATE-MAP can throw and the owned result is always released.
: LOWERS-TO? ( ptr u8 n ptr u8 n -- bool )
   {: source:ptr size:n target:ptr target-size:n :}
   target target-size VALID-UTF8
   source size [: LOWER-CALL ;] ALLOCATE-MAP {: lowered:ptr lowered-size:n :}
   lowered lowered-size target target-size STR=
   lowered RELEASE ;

seal-package
;using
;package
