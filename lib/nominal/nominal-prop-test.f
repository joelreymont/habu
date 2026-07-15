\ nominal-prop-test.f - metamorphic property suite for the sealed immutable
\ nominal collection (dot habu-add-immutable-nominal-9290a81f).
\ Run: bin/hb --load lib/nominal/nominal-prop-test.f
\
\ Over many pseudo-random binding sets it asserts the substrate's identity
\ invariants: insertion order is not identity, adding a binding twice is
\ idempotent, UNION is commutative, and the canonical codec round-trips - all
\ decided by canonical CONTENTS (EQUAL?), never by handle number or build order.
\ A fixed seed keeps runs reproducible. Direct-loaded (lib/cad-num-types-test.f
\ precedent); reopens package NOM for BIND-IDX/MINT-BINDING array plumbing.

require lib/nominal/snapshot.f
require lib/test.f

create NP-BS 512 cells allot                 \ generated binding indices for one iteration
create NP-BUF 65536 allot
variable NP-SEED

package NOM
private

: NP-RAND ( -- n )
   NP-SEED @ 6364136223846793005 * 1442695040888963407 + dup NP-SEED !
   33 rshift $7FFFFFFF and ;

: NP-MKB ( n n n -- n ) {: a:n b:n slot:n :}   \ -> binding pool index
   a ROOT swap CONS b CONS slot BIND BIND-IDX ;

: NP-GEN ( n -- )                            \ fill NP-BS[0..m) with distinct-path bindings
   {: m:n :}
   m 0 do
      i  NP-RAND $FFFF and  NP-RAND $FFFF and  NP-MKB
      NP-BS i cells + !
   loop ;

: NP-ADD@ ( n -- binding )   NP-BS swap cells + @ MINT-BINDING ;

: NP-FWD ( n -- row ) {: m:n :}
   NEW m 0 do i NP-ADD@ ADD loop FREEZE ;
: NP-REV ( n -- row ) {: m:n :}
   NEW m 0 do m 1- i - NP-ADD@ ADD loop FREEZE ;
: NP-DUP ( n -- row ) {: m:n :}
   NEW m 0 do i NP-ADD@ ADD  i NP-ADD@ ADD loop FREEZE ;
: NP-HALF ( n n -- row ) {: lo:n hi:n :}     \ [lo,hi) as a row
   NEW hi lo do i NP-ADD@ ADD loop FREEZE ;

: NP-CODEC ( n -- bool ) {: m:n :}
   m NP-FWD {: r:row :}
   r NP-BUF 65536 ENCODE {: len:n :}
   NP-BUF len DECODE r EQUAL? ;

: NP-ONE ( n -- )                            \ one iteration over an m-binding set
   {: m:n :}
   RESET
   m NP-GEN
   m NP-FWD  m NP-REV  EQUAL? TTRUE           \ order is not identity
   m NP-FWD  m NP-DUP  EQUAL? TTRUE           \ per-binding duplicate idempotent
   m 2 /  {: h:n :}
   0 h NP-HALF  h m NP-HALF  UNION
   h m NP-HALF  0 h NP-HALF  UNION  EQUAL? TTRUE   \ union commutative
   m NP-CODEC TTRUE ;                         \ codec round-trip on content

: NP-ALL ( -- )
   T-RESET
   1 NP-SEED !
   64 0 do
      8 NP-RAND 40 mod +                      \ m in [8,48)
      NP-ONE
   loop
   T-REPORT ;

NP-ALL
;package
