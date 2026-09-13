\ Persistent application state exercises DATA and compiled-code relocation.
require lib/unicode.f

package APP-IMAGE-SUBJECT

create FOLDED 32 allot

: CHECK-UNICODE ( -- )
   s" Straße" s" STRASSE" UNICODE:CASEFOLD= 0= if 70 throw then
   s" İ" FOLDED 32 UNICODE:FOLD
   FOLDED swap s" i̇" STR= 0= if 70 throw then ;

variable VALUE
PERSISTED-PTR-VARIABLE LINK
TYPED-VARIABLE ACTION [ n -- n ]
DYNAMIC-BUFFER SCRATCH n
variable REGISTRY-ID

\ Force both registry arrays and their name pool into grown mappings. Capture
\ must move them into DATA and preserve their lookup across both image boots.
TRUSTED: GROW-REGISTRY ( -- )
   CTN @ REGISTRY-ID !
   CTN @ CT-CAP-V !
   s" AIMG-REGISTRY-FIRST" CTN @ CT-ROLE 64 CS-NONE CT-SET
   CT-STR-U @ CT-STR-CAP-V !
   s" AIMG-REGISTRY-SECOND" CTN @ CT-ROLE 64 CS-NONE CT-SET
   CT-NAME-A-P @ CT-CAP-V @ cells REG-DATA-SPAN? if 70 throw then
   CT-STR-P @ CT-STR-CAP-V @ REG-DATA-SPAN? if 70 throw then ;


TRUSTED: CHECK-REGISTRY ( -- )
   s" AIMG-REGISTRY-FIRST" CT-FIND REGISTRY-ID @ <> if 70 throw then
   s" AIMG-REGISTRY-SECOND" CT-FIND REGISTRY-ID @ 1+ <> if 70 throw then
   CT-NAME-A-P @ CT-CAP-V @ cells REG-DATA-SPAN? 0= if 70 throw then
   CT-STR-P @ CT-STR-CAP-V @ REG-DATA-SPAN? 0= if 70 throw then ;

: USE-SCRATCH ( -- )
   1 SCRATCH-RESERVE
   23 0 SCRATCH !
   0 SCRATCH @ 23 <> if 70 throw then ;

: SCRATCH-ADDRESS ( -- ) 0 SCRATCH drop ;

: INCREMENT ( n -- n ) 1+ ;
: DOUBLE ( n -- n ) 2 * ;
: PUT ( [ a -- a ] ptr [ a -- a ] -- ) ! ;

: INITIALIZE ( -- )
   USE-SCRATCH
   CHECK-UNICODE
   GROW-REGISTRY
   42 VALUE !
   VALUE LINK !
   [: DOUBLE ;] ACTION PUT
   [: INCREMENT ;] ACTION PUT ;

INITIALIZE

public

\ Inspect bounds without dereferencing a stale process pointer. Both restored
\ generations must start released even though this declaration is never replayed.
: SCRATCH-CLEAN ( -- )
   [: SCRATCH-ADDRESS ;] catch 7122 <> if 70 throw then ;

: RUN ( -- n )
   USE-SCRATCH
   CHECK-UNICODE
   CHECK-REGISTRY
   LINK @ @ ACTION @ execute ;

;package
