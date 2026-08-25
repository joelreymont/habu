\ native-create-does.f - ordinary colon compiles defining words natively.

require lib/test.f
require src/compiler/native/compiler.f

package NATIVE-CREATE-DOES-TEST
private

: MAKE-CELL ( n -- n )
   dup create ,
   1 +
   does> ( -- n ) @ ;

\ The first spelling must remain a string payload, not become the split row.
: MAKE-INERT-CELL ( n -- n )
   s" does>" 2drop
   dup create ,
   1 +
   DoEs> ( -- n ) @ ;

: MAKE-ADDER ( n -- n )
   dup create ,
   1 +
   does> ( n -- n ) @ + ;

: MAKE-ADDRESS ( n -- )
   create ,
   does> ( -- ptr a ) ;

TRUSTED: TRUSTED-MAKE ( n -- )
   drop dbase@ drop
   create 55 ,
   DOES> ( -- n ) @ ;

: MAYBE-PATCH ( bool -- )
   create 99 ,
   if exit then
   does> ( -- n ) drop 7 ;

41 MAKE-CELL CREATED-CELL 42 T=
50 MAKE-INERT-CELL INERT-CELL 51 T=
5 MAKE-ADDER ADD5 6 T=
73 MAKE-ADDRESS CREATED-ADDRESS
9 TRUSTED-MAKE TRUSTED-CELL
0 0= MAYBE-PATCH UNPATCHED
1 0= MAYBE-PATCH PATCHED
UNPATCHED @ 99 T=

public

: RUN ( -- )
   s" created storage and the published output signature survive" T-LABEL
   CREATED-CELL 41 T=

   s" the clause consumes the created word's declared input" T-LABEL
   7 ADD5 12 T=

   s" an empty does> clause remains an address identity" T-LABEL
   CREATED-ADDRESS @ 73 T=

   s" does> inside a string is not the defining-word split" T-LABEL
   INERT-CELL 50 T=

   s" an uncheckable trusted parent still publishes its checked clause" T-LABEL
   TRUSTED-CELL 55 T=

   s" an explicit exit leaves CREATE behavior; fallthrough installs does>" T-LABEL
   PATCHED 7 T=

   T-REPORT ;

;package

NATIVE-CREATE-DOES-TEST:RUN
