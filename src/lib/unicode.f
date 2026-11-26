\ unicode.f - Unicode whitespace, locale-independent full casefold equality
\ and full lowercasing. CASEFOLD= compares whole UTF-8 spans by full case
\ folding (Straße equals STRASSE), with no normalization, keeping embedded
\ NULs. FOLD writes a span's full case folding and LOWER its full lowercase
\ (Straße folds to strasse but lowercases to straße). LOWERS-TO? tells
\ whether a span's full lowercase is another span byte for byte, and LOWER=
\ whether two spans' full lowercases are the same bytes. With no
\ language, both mappings take an all-ASCII span (every byte below $80) to
\ itself with A-Z lowered to a-z (UnicodeData.txt, CaseFolding.txt; no
\ SpecialCasing entry has an ASCII source without a language), which these
\ words compute without libunistring; CASEFOLD= does so only when both spans
\ are ASCII, since other text can fold to ASCII (U+212A KELVIN SIGN folds to
\ k). Other text goes to libunistring's u8_casecmp, u8_casefold and
\ u8_tolower with NULL language and normalization, so its case data is the
\ installed library's.
\ STORAGE CLASS. TASK-LOCAL: C's result cell, a TASK:+USER row.
\ U+03A3 lowers by Unicode's Final_Sigma as Rust's str::to_lowercase does.

require src/lib/unicode/class.f
require src/lib/utf8-scalar.f
require src/lib/ffi-abi.f
require src/lib/string.f
s" src/lib/task.f" required

package UNICODE
using FFI

public

-9150 s" a negative length or malformed UTF-8" ERROR:CODE E-UTF8
-9153 s" libunistring failed to fold, lower or compare the text, or to allocate its result"
   ERROR:FAULT E-CASEFOLD
-9154 s" the destination is shorter than the mapped text: pass a larger destination"
   ERROR:CODE E-CAPACITY

EXPORT UNICODE-CLASS:WHITE-SPACE?
EXPORT UNICODE-CLASS:ALPHABETIC?
EXPORT UNICODE-CLASS:UPPERCASE?
EXPORT UNICODE-CLASS:MARK?

private

: VALID-LENGTH ( n -- )
   0 < if E-UTF8 throw then ;

: VALID-UTF8 ( str -- )
   {: source:str :}
   source STR:LEN VALID-LENGTH
   source UTF8:VALID? 0= if E-UTF8 throw then ;

\ C's four-byte int output, in a task-local cell each call zeroes first.
TASK:+USER RESULT n

\ Exact u8_casecmp schema: two counted, read-only UTF-8 spans, two NULL
\ policy pointers, and one four-byte writable result. u8_casefold and
\ u8_tolower share one schema; NULL resultbuf asks either for one owned
\ allocation, which only the matched private free binding may consume. No
\ callable raw binding or configurable foreign symbol escapes this package.
VERSIONED-LIBRARY unistring 5
FUNCTION: COMPARE-CALL u8_casecmp ( str str n n ^u8 -- i32 ) ;FUNCTION
FUNCTION: FOLD-CALL u8_casefold ( str n n n ^u8 -- ^u8 ) ;FUNCTION
FUNCTION: LOWER-CALL u8_tolower ( str n n n ^u8 -- ^u8 ) ;FUNCTION
FUNCTION: CASED-CALL uc_is_property_cased ( u32 -- u8 ) ;FUNCTION
FUNCTION: IGNORABLE-CALL uc_is_property_case_ignorable ( u32 -- u8 )
;FUNCTION
PROCESS-SYMBOLS
FUNCTION: ALLOC-CALL malloc ( n -- nullable<u8> ) ;FUNCTION
FUNCTION: RELEASE free ( ^u8 -- ) ;FUNCTION

$80 constant ASCII-LIMIT
$3A3 constant CAPITAL-SIGMA
$CF constant SMALL-LEAD
$83 constant SMALL-TAIL
$82 constant FINAL-TAIL

\ Whether every byte of a span is below $80; a negative length is E-UTF8.
: ASCII? ( str -- bool )
   {: text:str :}
   text STR:LEN VALID-LENGTH
   0 begin dup text STR:LEN < while
      dup text STR:DATA + c@ ASCII-LIMIT < 0= if drop false exit then
      1+
   repeat
   drop true ;

\ Writes an ASCII span's lowercase to a destination of a given capacity.
: ASCII-LOWER-INTO ( str ^u8 n -- n )
   {: text:str out:^ capacity:n :}
   capacity text STR:LEN < if E-CAPACITY throw then
   text STR:LEN 0 ?do text STR:DATA i + c@ STR:ASCII-LOWER out i + c! loop
   text STR:LEN ;

\ Whether an ASCII span's lowercase is another span byte for byte.
: ASCII-LOWERS-TO? ( str str -- bool )
   {: text:str target:str :}
   text STR:LEN target STR:LEN <> if false exit then
   text STR:LEN 0 ?do
      text STR:DATA i + c@ STR:ASCII-LOWER target STR:DATA i + c@ <> if
         false unloop exit
      then
   loop
   true ;

: CASED? ( n -- bool ) CASED-CALL 0<> ;
: IGNORABLE? ( n -- bool ) IGNORABLE-CALL 0<> ;

\ The scalar at a cursor of valid UTF-8 and the cursor after it.
: SCALAR@ ( str n -- n n )
   UTF8:NEXT MATCH UTF8:scalar-step
      scalar OF ENDOF
      raw-byte OF ENDOF
   ;MATCH ;

\ Whether the first scalar from a cursor that is not Case_Ignorable is
\ Cased; false at the end.
: CASED-AFTER? ( str n -- bool )
   {: text:str at:n :}
   at begin dup text STR:LEN < while
      text swap SCALAR@ {: cp:n next:n :}
      cp IGNORABLE? 0= if cp CASED? exit then
      next
   repeat
   drop false ;

\ Writes the sigma of the U+03A3 at a cursor: final when, past
\ Case_Ignorable scalars by libunistring's tests, a Cased one precedes
\ it and none follows. A scalar both Cased and Case_Ignorable is passed.
: SIGMA! ( bool str n -- )
   {: before:bool text:str at:n :}
   before text at 2 + CASED-AFTER? 0= and
   if FINAL-TAIL else SMALL-TAIL then
   text STR:DATA at + 1+ c!
   SMALL-LEAD text STR:DATA at + c! ;

\ Lowers every U+03A3 in valid UTF-8 in place. The flag carried
\ between turns is whether the last scalar not Case_Ignorable is Cased.
: SIGMAS! ( str -- )
   {: text:str :}
   false 0 begin dup text STR:LEN < while
      {: before:bool at:n :}
      text at SCALAR@ {: cp:n next:n :}
      cp CAPITAL-SIGMA = if before text at SIGMA! then
      cp IGNORABLE? if before else cp CASED? then next
   repeat
   2drop ;

\ u8_tolower's schema. With no language, Final_Sigma is u8_tolower's
\ one context-dependent mapping, so a copy whose capital sigmas are
\ already lowered leaves it none to apply.
: SIGMA-LOWER ( str n n n ^u8 -- ^u8 )
   {: text:str lang:n form:n buf:n out:^ :}
   text s" Σ" STR:CONTAINS? 0= if
      text lang form buf out LOWER-CALL exit
   then
   text STR:LEN ALLOC-CALL MATCH nullable
      none OF E-CASEFOLD throw ENDOF
      some OF ENDOF
   ;MATCH {: copy:^ :}
   text STR:DATA copy text STR:LEN BYTE-COPY
   copy text STR:LEN >str SIGMAS!
   copy text STR:LEN >str lang form buf out LOWER-CALL
   copy RELEASE ;

\ Runs MAP, FOLD-CALL or SIGMA-LOWER, on a valid span and answers its
\ owned result and that result's length.
: ALLOCATE-MAP ( str [ str n n n ^u8 -- ^u8 ] -- str )
   {: source:str map :}
   source VALID-UTF8
   0 RESULT !
   source 0 0 0 RESULT BYTE-VIEW map execute
   dup >CELL 0= if E-CASEFOLD throw then
   RESULT @ >str ;

\ Keep the same stack shape on success and failure for the cleanup path.
: COPY-MAPPED ( str ^u8 n -- str ^u8 n )
   {: source:str out:^ capacity:n :}
   capacity source STR:LEN < if E-CAPACITY throw then
   source STR:DATA out source STR:LEN BYTE-COPY
   source out capacity ;

: MAPPED-BYTES ( str [ str n n n ^u8 -- ^u8 ] -- n )
   {: source:str map :}
   source ASCII? if source STR:LEN exit then
   source map ALLOCATE-MAP dup STR:DATA RELEASE STR:LEN ;

: MAP-INTO ( str ^u8 n [ str n n n ^u8 -- ^u8 ] -- n )
   {: source:str out:^ capacity:n map :}
   source ASCII? if
      source out capacity ASCII-LOWER-INTO exit
   then
   source map ALLOCATE-MAP {: mapped:str :}
   mapped out capacity [: COPY-MAPPED ;] catch
   {: error:n :} 2drop drop
   \ catch restores depth, not cells overwritten by a callee before throw.
   \ The owner is kept in this outer lexical frame, never recovered from args.
   mapped STR:DATA RELEASE
   error 0<> if error throw then
   mapped STR:LEN ;

public

: CASEFOLD= ( str str -- bool )
   {: left:str right:str :}
   left ASCII? right ASCII? and if
      left right STR:=CI exit
   then
   left VALID-UTF8 right VALID-UTF8
   0 RESULT !
   left right 0 0 RESULT BYTE-VIEW COMPARE-CALL
   0<> if E-CASEFOLD throw then
   RESULT @ 0= ;

: FOLDED-BYTES ( str -- n )
   [: FOLD-CALL ;] MAPPED-BYTES ;

: FOLD ( str ^u8 n -- n )
   [: FOLD-CALL ;] MAP-INTO ;

: LOWERED-BYTES ( str -- n )
   [: SIGMA-LOWER ;] MAPPED-BYTES ;

: LOWER ( str ^u8 n -- n )
   [: SIGMA-LOWER ;] MAP-INTO ;

\ The second span is checked before the first is lowered, so nothing after
\ ALLOCATE-MAP can throw and the owned result is always released.
: LOWERS-TO? ( str str -- bool )
   {: source:str target:str :}
   target VALID-UTF8
   source ASCII? if
      source target ASCII-LOWERS-TO? exit
   then
   source [: SIGMA-LOWER ;] ALLOCATE-MAP
   {: lowered:str :}
   lowered target STR:=
   lowered STR:DATA RELEASE ;

\ Whether two spans' full lowercases are the same bytes.
: LOWER= ( str str -- bool )
   {: left:str right:str :}
   left VALID-UTF8 right VALID-UTF8
   left ASCII? right ASCII? and if
      left right STR:=CI exit
   then
   left [: SIGMA-LOWER ;] ALLOCATE-MAP
   {: lowered:str :}
   [: right lowered LOWERS-TO? ;] [: lowered STR:DATA RELEASE ;] finally ;

seal-package
;using
;package
