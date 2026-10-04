\ Fixed engine slots keep their identities when source layout offsets move.
require tools/native-layout.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f

package NATIVE-LAYOUT-TEST
private

NATIVE-LAYOUT:CURRENT nip constant ROWS
create HOST-BUF ROWS 2 * cells allot
: HOST ( -- ptr n ) HOST-BUF CELL-VIEW ;

: RESET ( -- )
   NATIVE-LAYOUT:CURRENT {: source:ptr count:n :}
   source BYTE-VIEW HOST BYTE-VIEW count 2 * cells BYTE-COPY ;

: SAME ( -- )
   NATIVE-LAYOUT:CURRENT DATA-START NATIVE-LAYOUT:CHECK
   RESET
   HOST ROWS DATA-START NATIVE-LAYOUT:CHECK
   ROWS 0 ?do
      HOST i 2 * cells + {: row:ptr :}
      HOST ROWS row @ row cell+ @ 0<> NATIVE-LAYOUT:TRANSLATE row @ T=
   loop ;

: MOVED ( -- )
   RESET
   \ The host's hook lived above this source's hook: translate a shrinking layout.
   HOOK-CELL CELL + HOST !
   HOST ROWS DATA-START NATIVE-LAYOUT:CHECK
   HOST ROWS HOOK-CELL CELL + false NATIVE-LAYOUT:TRANSLATE HOOK-CELL T=
   \ Then below it: the same identity also translates a growing layout.
   HOOK-CELL CELL - HOST !
   HOST ROWS DATA-START NATIVE-LAYOUT:CHECK
   HOST ROWS HOOK-CELL CELL - false NATIVE-LAYOUT:TRANSLATE HOOK-CELL T= ;

256 constant IO-CAP
create OUT IO-CAP allot
create ERR IO-CAP allot

: REFUSES-OUT ( ptr u8 n ptr u8 n -- ) {: src:ptr srcu:n want:ptr wantu:n :}
   src srcu OUT IO-CAP >LEN ERR IO-CAP >LEN 1000 >MS SUBJECT:RUN {: outu:len erru:len oc :}
   src srcu OUT outu LEN>N ERR erru LEN>N oc 74 T-OUTCOME-EXITED=
   OUT outu LEN>N want wantu T$=
   ERR erru LEN>N S\" native-build: incompatible fixed engine layout\n" T$= ;

: REFUSES ( ptr u8 n -- ) s" " REFUSES-OUT ;

: REFUSED-LAYOUTS ( -- )
   s" package NATIVE-LAYOUT-TEST RESET HOST ROWS 1- DATA-START NATIVE-LAYOUT:CHECK ;package" REFUSES
   s" package NATIVE-LAYOUT-TEST RESET 1 HOST cell+ ! HOST ROWS DATA-START NATIVE-LAYOUT:CHECK ;package" REFUSES
   s" package NATIVE-LAYOUT-TEST RESET COMPILE-PREFLIGHT-CELL HOST ! HOST ROWS DATA-START NATIVE-LAYOUT:CHECK ;package" REFUSES
   s" package NATIVE-LAYOUT-TEST RESET DATA-START HOST ! HOST ROWS DATA-START NATIVE-LAYOUT:CHECK ;package" REFUSES
   s" package NATIVE-LAYOUT-TEST RESET HOST ROWS 1 false NATIVE-LAYOUT:TRANSLATE drop ;package"
      S\" native-build: unknown fixed cell offset 1\n" REFUSES-OUT
   s" package NATIVE-LAYOUT-TEST RESET HOST ROWS HOOK-CELL true NATIVE-LAYOUT:TRANSLATE drop ;package" REFUSES ;

public

: TEST ( -- )
   T-RESET
   s" fixed slot offsets and kinds agree with their source identities" T-LABEL
   SAME
   s" moved fixed slots translate in both directions" T-LABEL
   MOVED
   s" malformed and unknown fixed slots refuse before an image can be written" T-LABEL
   REFUSED-LAYOUTS
   T-REPORT ;

;package

NATIVE-LAYOUT-TEST:TEST
