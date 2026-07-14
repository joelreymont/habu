\ pointer-storage-test.f - focused PTR-VARIABLE ownership and effect regression.
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\   src/habu/verify-source.f test/pointer-storage-test.f

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require src/habu/verify-source.f
require test/checker-assert.f

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

\ Raw-definer VALUE-side mint (habu-nominal-storage-raw): create/variable/constant
\ publish a RAW cell whose fetch is a TVK-RAW var, so laundering an arity-0
\ nominal family through raw storage rejects, while a plain scalar round-trip
\ through the same raw cell still certifies. The definers are registered through
\ the verify-source RAW-TRUST-NEXT path (the enforcing gate); the laundering word
\ is then checked with the quiet candidate checker so the reject renders no stray
\ diagnostic. Verdict 0 = rejected, -1 = certified.
: REG-RAW-DEFINERS ( -- )
   s\" TYPEFAMILY rsvfam 0\nvariable RSVV\ncreate RSVC 8 allot\n7 constant RSVK" VERIFY:SOURCE-BUF-IN-SCOPE ;
: VERIFY-RAW-VALUE ( -- )
   REG-RAW-DEFINERS
   s" RSV-VAR-MINT ( n -- rsvfam ) RSVV ! RSVV @" CHECK-QUIET-CANDIDATE! 0 T=
   s" RSV-CREATE-MINT ( n -- rsvfam ) RSVC ! RSVC @" CHECK-QUIET-CANDIDATE! 0 T=
   s" RSV-CONST-MINT ( -- rsvfam ) RSVK" CHECK-QUIET-CANDIDATE! 0 T=
   s" RSV-VAR-N ( n -- n ) RSVV ! RSVV @" CHECK-QUIET-CANDIDATE! -1 T=
   s" RSV-CONST-N ( -- n ) RSVK" CHECK-QUIET-CANDIDATE! -1 T= ;

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
   VERIFY-RAW-VALUE
   ISOLATION
   T-REPORT ;

RUN

;package
