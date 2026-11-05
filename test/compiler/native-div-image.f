\ native-div-image.f - signed remainders by a literal two, a stack alias, a
\ join's divisor and a parameter; test/tier0-product-subject.f runs it per tier.
package NDIV-IMAGE
private
: REM2 ( n -- n ) 2 mod ;
: ALIAS ( n -- n n ) dup 2 mod ;
: CHOICE ( n bool -- n ) if 2 else 3 then mod ;
: DYNAMIC ( n n -- n ) mod ;
public
: RUN ( -- )
   -7 REM2 . -8 REM2 . 0 REM2 . 7 REM2 . 8 REM2 .
   $8000000000000000 REM2 . $7FFFFFFFFFFFFFFF REM2 .
   -7 ALIAS . .
   -8 true CHOICE . -8 false CHOICE .
   -7 2 DYNAMIC . ;
;package
