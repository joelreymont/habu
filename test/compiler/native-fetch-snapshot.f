\ Each source body owns a copied certificate before another scan can replace it.
require src/compiler/native/fetch.f
require lib/test.f

package NATIVE-FETCH-SNAPSHOT-TEST
using NFETCH
using CDIGEST

public
ENUM shade dark light ;ENUM
private

: TYPED$ ( -- ptr u8 n )
   s" FETCH-SNAPSHOT ( ptr NATIVE-FETCH-SNAPSHOT-TEST:shade -- ) @ drop" ;


\ CHECK publishes the certified name. A later body needs its own declaration,
\ while its identical fetch descriptor must still intern to the same address.
: SPLIT$ ( -- ptr u8 n )
   s" FETCH-SNAPSHOT-SPLIT ( ptr NATIVE-FETCH-SNAPSHOT-TEST:shade -- ) @ drop" ;


: RAW$ ( -- ptr u8 n )
   s" FETCH-RAW ( ptr n -- ) @ drop" ;


: SCAN ( ptr u8 n n -- ) {: source:ptr size:n base:n :}
   source size CHECKER-OWNER:CHECK -1 T=
   source size base CAPTURE ;


\ These bodies end in `@ drop`; the fetch starts six bytes before their end.
: OFFSET ( ptr u8 n -- n )
   nip 6 - ;


: SAVED ( n -- n )
   AT {: address:n bytes:n width:n :}
   width 1 T= bytes 4 cells T=
   address NSTR:OWNER-ROW TTRUE {: data:ptr size:n :}
   size bytes T=
   data 0 SLOT@ 1 T=
   data 1 SLOT@ 0 T=
   data 2 SLOT@ 2 T=
   data 3 SLOT@ 0 T=
   address ;

public

: TEST ( -- )
   T-RESET RELEASE
   s" typed fetch retains source-bound immutable bytes" T-LABEL
   TYPED$ 0 SCAN
   TYPED$ OFFSET SAVED {: first:n :}
   s" a split body retains both snapshots across subsequent scans" T-LABEL
   SPLIT$ 1000 SCAN
   SPLIT$ OFFSET 1000 + SAVED first T=
   RAW$ 2000 SCAN
   RAW$ OFFSET 2000 + CHECKED? TFALSE
   TYPED$ OFFSET SAVED first T=
   SPLIT$ OFFSET 1000 + SAVED first T=
   RELEASE
   TYPED$ OFFSET CHECKED? TFALSE
   T-REPORT ;

;using                                   \ CDIGEST
;using                                   \ NFETCH
;package

NATIVE-FETCH-SNAPSHOT-TEST:TEST
