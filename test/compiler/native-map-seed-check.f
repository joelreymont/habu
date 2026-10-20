\ Run in each freshly booted compiler bundle image. The names cross retired
\ records, a redefinition, and private bodies kept through public exports.
require src/lib/test.f
require src/lib/errors.f
require src/lib/string.f
require src/os/script-argv.f
require src/core/prefix-boundary.f
require src/habu/xref.f

package NATIVE-MAP-SEED-CHECK
private

CAST: CONTRACT-XT ( n -- [ n -- ptr u8 n bool ] )
CAST: CAPTURE-XT ( n -- [ [ n -- n ] [ -- ] -- ] )
6 constant CONTRACT-N
DYNAMIC-BUFFER SAVED u8
create SAVED-LEN CONTRACT-N cells allot
variable SAVED-U
variable CHECK-OFF

: CONTRACT ( ptr u8 n -- ptr u8 n bool )
   XREF-FIND-INDEX
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
   CHECKER-OWNER-ABI:RECORD-CONTRACT-OFF + CELL-VIEW @
      CONTRACT-XT execute ;

: NAME$ ( n -- ptr u8 n )
   dup 0= if drop s" NATIVE-MAP-ORIGIN:MAP-ORIGIN-B" exit then
   dup 1 = if drop s" NATIVE-MAP-ORIGIN:MAP-ORIGIN-C" exit then
   dup 2 = if drop s" NATIVE-MAP-ORIGIN:MAP-ORIGIN-D" exit then
   dup 3 = if drop s" NATIVE-MAP-ORIGIN:MAP-ORIGIN-FIXED" exit then
   dup 4 = if drop s" NATIVE-MAP-ORIGIN:MAP-ORIGIN-CELL" exit then
   drop s" NATIVE-MAP-ORIGIN:MAP-ORIGIN-DATA" ;

: SAVE-CONTRACTS ( -- )
   0 SAVED-U !
   CONTRACT-N 0 ?do
      i NAME$ CONTRACT {: bytes:ptr size:n found:bool :}
      found 0= if s" seeded record has no contract" 76 die then
      size CHECKER-OWNER-ABI:RECORD-CONTRACT-MIN-BYTES < if
         s" seeded contract is too short" 76 die then
      bytes CELL-VIEW @ CHECKER-OWNER-ABI:RECORD-CONTRACT-MAGIC T=
      size i cells SAVED-LEN + !
      SAVED-U @ size + SAVED-RESERVE
      bytes 0 SAVED BYTE-VIEW SAVED-U @ + size BYTE-COPY
      size SAVED-U +!
   loop ;

: SAME-CONTRACTS ( -- )
   0 CHECK-OFF !
   CONTRACT-N 0 ?do
      i NAME$ CONTRACT {: bytes:ptr size:n found:bool :}
      found 0= if s" seeded record lost its contract" 76 die then
      i cells SAVED-LEN + @ {: want:n :}
      size want T=
      bytes size 0 SAVED BYTE-VIEW CHECK-OFF @ + want STR= TTRUE
      want CHECK-OFF +!
   loop ;

: FIXED ( -- )
   s" NATIVE-MAP-ORIGIN:MAP-ORIGIN-FIXED" CONTRACT
      {: bytes:ptr size:n found:bool :}
   found TTRUE
   size CHECKER-OWNER-ABI:RECORD-CONTRACT-MIN-BYTES >= TTRUE
   bytes CHECKER-OWNER-ABI:RECORD-FIXED-KIND-OFF + CELL-VIEW @
      CHECKER-OWNER-ABI:RECORD-FIXED-VAL T=
   bytes CHECKER-OWNER-ABI:RECORD-FIXED-VALUE-OFF + CELL-VIEW @ 41 T= ;

: MISSING ( -- )
   \ The primitive is present in the dictionary but has no source link row.
   s" dup" XREF-FIND-INDEX 0 >= TTRUE
   s" dup" CONTRACT
      {: bytes:ptr size:n found:bool :}
   found TFALSE
   size 0 T=
   bytes NULL-PTR = TTRUE ;

: ARG-N ( n -- n )
   SCRIPT-ARGV$ STR>NUMBER? MATCH option
      none OF s" seed checker: invalid growth bound" 76 die ENDOF
      some OF ENDOF
   ;MATCH ;

: GROWTH ( -- )
   SCRIPT-ARGC 0= if exit then
   SCRIPT-ARGC 2 <> if s" seed checker: expected cap and highwater" 64 die then
   0 ARG-N {: cap:n :}
   1 ARG-N CORE-PREFIX:FIRST-RECORD + cap >= TTRUE
   s" NATIVE-MAP-ORIGIN:MAP-ORIGIN-FIXED"
      XREF-FIND-INDEX cap >= TTRUE ;

: CAPTURE ( [ -- ] -- ) {: copy :}
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @
   CHECKER-OWNER-ABI:RECORD-CAPTURE-OFF + CELL-VIEW @
      CAPTURE-XT {: capture :}
   [: 0 + ;] copy capture execute ;

: COPY-FAIL ( -- ) E-DIV-ZERO throw ;

: FRESH-CLIENT ( -- )
   s" : NATIVE-MAP-SEED-CLIENT ( -- n ) NATIVE-MAP-ORIGIN:MAP-ORIGIN-B NATIVE-MAP-ORIGIN:MAP-ORIGIN-C NATIVE-MAP-ORIGIN:MAP-ORIGIN-D NATIVE-MAP-ORIGIN:MAP-ORIGIN-FIXED + ;"
      evaluate-closed
   s" NATIVE-MAP-SEED-CLIENT 151 T= undefine NATIVE-MAP-SEED-CLIENT"
      evaluate-closed
   s" NATIVE-MAP-ORIGIN:MAP-ORIGIN-CELL @ 17 T= NATIVE-MAP-ORIGIN:MAP-ORIGIN-DATA @ 29 T="
      evaluate-closed
   s" : NATIVE-MAP-SEED-STORAGE ( -- n ) 19 NATIVE-MAP-ORIGIN:MAP-ORIGIN-CELL ! 23 NATIVE-MAP-ORIGIN:MAP-ORIGIN-DATA ! NATIVE-MAP-ORIGIN:MAP-ORIGIN-CELL @ NATIVE-MAP-ORIGIN:MAP-ORIGIN-DATA @ + ;"
      evaluate-closed
   s" NATIVE-MAP-SEED-STORAGE 42 T= undefine NATIVE-MAP-SEED-STORAGE 17 NATIVE-MAP-ORIGIN:MAP-ORIGIN-CELL ! 29 NATIVE-MAP-ORIGIN:MAP-ORIGIN-DATA !"
      evaluate-closed ;

: CHECK-SOURCE ( -- )
   SAME-CONTRACTS
   FIXED MISSING
   FRESH-CLIENT
   [: s" : NATIVE-MAP-SEED-WRONG ( -- ) NATIVE-MAP-ORIGIN:MAP-ORIGIN-B ;"
      evaluate-closed ;] 70 TTHROWSQ
   [: s" : NATIVE-MAP-SEED-BAD-C ( -- n ) NATIVE-MAP-ORIGIN:MAP-ORIGIN-C ;"
      evaluate-closed ;] 70 TTHROWSQ
   [: s" : NATIVE-MAP-SEED-BAD-CELL ( -- n ) NATIVE-MAP-ORIGIN:MAP-ORIGIN-CELL ;"
      evaluate-closed ;] 70 TTHROWSQ
   [: s" : NATIVE-MAP-SEED-BAD-DATA ( -- n ) NATIVE-MAP-ORIGIN:MAP-ORIGIN-DATA ;"
      evaluate-closed ;] 70 TTHROWSQ ;

public
: RUN ( -- )
   T-RESET
   s" seeded checker links follow shipped rows" T-LABEL
   SAVE-CONTRACTS
   GROWTH
   CHECK-SOURCE
   s" a failing DATA copy preserves its error and the live checker" T-LABEL
   [: [: COPY-FAIL ;] CAPTURE ;] catch E-DIV-ZERO T=
   CHECK-SOURCE
   s" repeated DATA copies preserve source contracts" T-LABEL
   2 0 ?do [: ;] CAPTURE CHECK-SOURCE loop
   T-REPORT ;
;package

NATIVE-MAP-SEED-CHECK:RUN
