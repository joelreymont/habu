\ pointer-storage-test.f - focused PTR-VARIABLE ownership and effect regression.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\   src/habu/verify-source.f test/pointer-storage-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require src/habu/verify-source.f

package POINTER-STORAGE-TEST

$400 constant SOURCE-CAP

create SOURCE SOURCE-CAP allot
variable SOURCE-U
create ZERO-CELL 0 ,
create TARGET 0 ,
PTR-VARIABLE SLOT

: ZERO-PTR ( -- ptr a )
   ZERO-CELL 0 ptr-field @ ;

: ADDRESS ( -- ptr ptr a )
   SLOT ;

: LOAD-SOURCE ( -- )
   s" src/core/pointer-storage.f" SOURCE SOURCE-CAP READ-ALL SOURCE-U ! ;

: HAS? ( ptr u8 n -- bool )
   SOURCE SOURCE-U @ 2swap CONTAINS? ;

: MUST-HAVE ( ptr u8 n -- )
   HAS? TTRUE ;

: MUST-LACK ( ptr u8 n -- )
   HAS? TFALSE ;

: RUNTIME ( -- )
   ADDRESS @ ZERO-PTR = TTRUE
   TARGET ADDRESS !
   ADDRESS @ TARGET = TTRUE ;

: VERIFY-EFFECT ( -- )
   [: s" PTR-VARIABLE VS-PTR : VS-ADDR ( -- ptr ptr a ) VS-PTR ;" VERIFY:SOURCE-BUF ;]
   catch 0 T= ;

: ISOLATION ( -- )
   LOAD-SOURCE
   s" PTR-VARIABLE" MUST-HAVE
   s" +FIELD" MUST-LACK
   s" CFIELD:" MUST-LACK
   s" STRUCT-BYTE+" MUST-LACK
   s" STRUCT-ACTIVE" MUST-LACK
   s" BEGIN-STRUCTURE" MUST-LACK
   s" END-STRUCTURE" MUST-LACK
   s" parse-name" MUST-LACK ;

: RUN ( -- )
   T-RESET
   RUNTIME
   VERIFY-EFFECT
   ISOLATION
   T-REPORT ;

RUN

;package
