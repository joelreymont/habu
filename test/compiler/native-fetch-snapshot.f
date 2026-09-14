\ Each source body owns a copied certificate before another scan can replace it.
require src/compiler/native/fetch.f
require lib/test.f

package NATIVE-FETCH-SNAPSHOT-TEST
public
ENUM shade dark light ;ENUM
private

: TYPED$ ( -- ptr u8 n )
   s" FETCH-SNAPSHOT ( ptr NATIVE-FETCH-SNAPSHOT-TEST:shade -- ) @ drop" ;

: RAW$ ( -- ptr u8 n )
   s" FETCH-RAW ( ptr n -- ) @ drop" ;

: SCAN ( ptr u8 n n -- ) {: source:ptr size:n base:n :}
   source size CHECKER-OWNER:CHECK -1 T=
   source size base NFETCH:CAPTURE ;

: OFFSET ( -- n ) TYPED$ nip 6 - ;

: SAVED ( n -- n )
   NFETCH:AT {: address:n bytes:n width:n :}
   width 1 T= bytes 4 cells T=
   address NSTR:OWNER-ROW TTRUE {: data:ptr size:n :}
   size bytes T=
   data 0 CDIGEST:SLOT@ 1 T=
   data 1 CDIGEST:SLOT@ 0 T=
   data 2 CDIGEST:SLOT@ 2 T=
   data 3 CDIGEST:SLOT@ 0 T=
   address ;

public

: TEST ( -- )
   T-RESET NFETCH:RELEASE
   s" typed fetch retains source-bound immutable bytes" T-LABEL
   TYPED$ 0 SCAN
   OFFSET SAVED {: first:n :}
   s" a split body retains both snapshots across subsequent scans" T-LABEL
   TYPED$ 1000 SCAN
   OFFSET 1000 + SAVED first T=
   RAW$ 2000 SCAN
   RAW$ nip 6 - 2000 + NFETCH:CHECKED? TFALSE
   OFFSET SAVED first T=
   OFFSET 1000 + SAVED first T=
   NFETCH:RELEASE
   OFFSET NFETCH:CHECKED? TFALSE
   T-REPORT ;

;package

NATIVE-FETCH-SNAPSHOT-TEST:TEST
