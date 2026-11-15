\ unicode.f - Unicode whitespace, locale-independent full casefold equality
\ and full lowercasing. CASEFOLD= compares whole UTF-8 spans by full case
\ folding (Straße equals STRASSE), with no normalization, keeping embedded
\ NULs. FOLD writes a span's full case folding and LOWER its full lowercase
\ (Straße folds to strasse but lowercases to straße), and LOWERS-TO? tells
\ whether a span's full lowercase is another span byte for byte. They are
\ libunistring's u8_casecmp, u8_casefold and u8_tolower with NULL language
\ and normalization, so the case data is the installed library's. STORAGE
\ CLASS. TASK-LOCAL: C's result cell, a TASK:+USERS row.
\ U+03A3 lowers by Unicode's Final_Sigma as Rust's str::to_lowercase does.

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

: VALID-UTF8 ( ^u8 n -- )
   {: source:^ size:n :}
   size VALID-LENGTH
   source size UTF8:VALID? 0= if E-UTF8 throw then ;

1 cells TASK:+USERS RESULT-ROW u8

\ C's four-byte int output, in a task-local cell each call zeroes first.
: RESULT ( -- ^n ) 0 RESULT-ROW CELL-VIEW ;

\ Exact u8_casecmp schema: two counted, read-only UTF-8 spans, two NULL
\ policy pointers, and one four-byte writable result. u8_casefold and
\ u8_tolower share one schema; NULL resultbuf asks either for one owned
\ allocation, which only the matched private free binding may consume. No
\ callable raw binding or configurable foreign symbol escapes this package.
VERSIONED-LIBRARY unistring 5
FUNCTION: COMPARE-CALL u8_casecmp ( ^u8 n ^u8 n n n ^u8 -- i32 ) ;FUNCTION
FUNCTION: FOLD-CALL u8_casefold ( ^u8 n n n n ^u8 -- ^u8 ) ;FUNCTION
FUNCTION: LOWER-CALL u8_tolower ( ^u8 n n n n ^u8 -- ^u8 ) ;FUNCTION
FUNCTION: CASED-CALL uc_is_property_cased ( u32 -- u8 ) ;FUNCTION
FUNCTION: IGNORABLE-CALL uc_is_property_case_ignorable ( u32 -- u8 )
;FUNCTION
PROCESS-SYMBOLS
FUNCTION: ALLOC-CALL malloc ( n -- nullable<u8> ) ;FUNCTION
FUNCTION: RELEASE free ( ^u8 -- ) ;FUNCTION

$3A3 constant CAPITAL-SIGMA
$CF constant SMALL-LEAD
$83 constant SMALL-TAIL
$82 constant FINAL-TAIL

: CASED? ( n -- bool ) CASED-CALL 0<> ;
: IGNORABLE? ( n -- bool ) IGNORABLE-CALL 0<> ;

\ The scalar at a cursor of valid UTF-8 and the cursor after it.
: SCALAR@ ( ^u8 n n -- n n )
   UTF8:NEXT MATCH UTF8:scalar-step
      scalar OF ENDOF
      raw-byte OF ENDOF
   ;MATCH ;

\ Whether the first scalar from a cursor that is not Case_Ignorable is
\ Cased; false at the end.
: CASED-AFTER? ( ^u8 n n -- bool )
   {: text:^ size:n at:n :}
   at begin dup size < while
      text size rot SCALAR@ {: cp:n next:n :}
      cp IGNORABLE? 0= if cp CASED? exit then
      next
   repeat
   drop false ;

\ Writes the sigma of the U+03A3 at a cursor: final when, past
\ Case_Ignorable scalars by libunistring's tests, a Cased one precedes
\ it and none follows. A scalar both Cased and Case_Ignorable is passed.
: SIGMA! ( bool ^u8 n n -- )
   {: before:bool text:^ size:n at:n :}
   before text size at 2 + CASED-AFTER? 0= and
   if FINAL-TAIL else SMALL-TAIL then
   text at + 1+ c!
   SMALL-LEAD text at + c! ;

\ Lowers every U+03A3 in valid UTF-8 in place. The flag carried
\ between turns is whether the last scalar not Case_Ignorable is Cased.
: SIGMAS! ( ^u8 n -- )
   {: text:^ size:n :}
   false 0 begin dup size < while
      {: before:bool at:n :}
      text size at SCALAR@ {: cp:n next:n :}
      cp CAPITAL-SIGMA = if before text size at SIGMA! then
      cp IGNORABLE? if before else cp CASED? then next
   repeat
   2drop ;

\ u8_tolower's schema. With no language, Final_Sigma is u8_tolower's
\ one context-dependent mapping, so a copy whose capital sigmas are
\ already lowered leaves it none to apply.
: SIGMA-LOWER ( ^u8 n n n n ^u8 -- ^u8 )
   {: text:^ size:n lang:n form:n buf:n out:^ :}
   text size s" Σ" CONTAINS? 0= if
      text size lang form buf out LOWER-CALL exit
   then
   size ALLOC-CALL MATCH nullable
      none OF E-CASEFOLD throw ENDOF
      some OF ENDOF
   ;MATCH {: copy:^ :}
   text copy size BYTE-COPY
   copy size SIGMAS!
   copy size lang form buf out LOWER-CALL
   copy RELEASE ;

\ Runs MAP, FOLD-CALL or SIGMA-LOWER, on a valid span and answers its
\ owned result and that result's length.
: ALLOCATE-MAP ( ^u8 n [ ^u8 n n n n ^u8 -- ^u8 ] -- ^u8 n )
   {: source:^ size:n map :}
   source size VALID-UTF8
   0 RESULT !
   source size 0 0 0 RESULT BYTE-VIEW map execute
   dup >CELL 0= if E-CASEFOLD throw then
   RESULT @ ;

\ Keep the same stack shape on success and failure for the cleanup path.
: COPY-MAPPED ( ^u8 n ^u8 n -- ^u8 n ^u8 n )
   {: source:^ size:n out:^ capacity:n :}
   capacity size < if E-CAPACITY throw then
   source out size BYTE-COPY
   source size out capacity ;

: MAPPED-BYTES ( ^u8 n [ ^u8 n n n n ^u8 -- ^u8 ] -- n )
   ALLOCATE-MAP swap RELEASE ;

: MAP-INTO ( ^u8 n ^u8 n [ ^u8 n n n n ^u8 -- ^u8 ] -- n )
   {: out:^ capacity:n map :}
   map ALLOCATE-MAP {: mapped:^ size:n :}
   mapped size out capacity [: COPY-MAPPED ;] catch
   {: error:n :} 2drop 2drop
   \ catch restores depth, not cells overwritten by a callee before throw.
   \ The owner is kept in this outer lexical frame, never recovered from args.
   mapped RELEASE
   error 0<> if error throw then
   size ;

public

: CASEFOLD= ( ^u8 n ^u8 n -- bool )
   {: left:^ left-size:n right:^ right-size:n :}
   left left-size VALID-UTF8 right right-size VALID-UTF8
   0 RESULT !
   left left-size right right-size 0 0 RESULT BYTE-VIEW COMPARE-CALL
   0<> if E-CASEFOLD throw then
   RESULT @ 0= ;

: FOLDED-BYTES ( ^u8 n -- n )
   [: FOLD-CALL ;] MAPPED-BYTES ;

: FOLD ( ^u8 n ^u8 n -- n )
   [: FOLD-CALL ;] MAP-INTO ;

: LOWERED-BYTES ( ^u8 n -- n )
   [: SIGMA-LOWER ;] MAPPED-BYTES ;

: LOWER ( ^u8 n ^u8 n -- n )
   [: SIGMA-LOWER ;] MAP-INTO ;

\ The second span is checked before the first is lowered, so nothing after
\ ALLOCATE-MAP can throw and the owned result is always released.
: LOWERS-TO? ( ^u8 n ^u8 n -- bool )
   {: source:^ size:n target:^ target-size:n :}
   target target-size VALID-UTF8
   source size [: SIGMA-LOWER ;] ALLOCATE-MAP
   {: lowered:^ lowered-size:n :}
   lowered lowered-size target target-size STR=
   lowered RELEASE ;

seal-package
;using
;package
