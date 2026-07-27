\ id.f - shared compiler IR identity representation.

require lib/errors.f

package IR-ID
public

NEWTYPE ir-module-id 0
NEWTYPE ir-source-id 0
NEWTYPE ir-fun-id 0
NEWTYPE ir-block-id 0
NEWTYPE ir-op-id 0
NEWTYPE ir-value-id 0
NEWTYPE ir-type-id 0
NEWTYPE ir-attr-id 0
NEWTYPE ir-symbol-id 0
NEWTYPE ir-span-id 0
NEWTYPE ir-pool-offset 0
NEWTYPE ir-count 0

;package

package IR-RAW
private

$7FFFFFFF constant MODULE-MAX
$FFFFFFFF constant LOCAL-MAX
32 constant LOCAL-BITS

CAST: MINT-MODULE ( n -- IR-ID:ir-module-id )
   dup 0= if E-IR-MODULE-ZERO throw then
   dup 0 < over MODULE-MAX > or if E-IR-MODULE-RANGE throw then ;

CAST: MODULE>N ( IR-ID:ir-module-id -- n ) ;

CAST: MINT-COUNT ( n -- IR-ID:ir-count )
   dup 0 < if E-IR-SCALAR-RANGE throw then ;

CAST: COUNT>N ( IR-ID:ir-count -- n ) ;

CAST: MINT-POOL-OFF ( n -- IR-ID:ir-pool-offset )
   dup 0 < if E-IR-SCALAR-RANGE throw then ;

CAST: POOL-OFF>N ( IR-ID:ir-pool-offset -- n ) ;

CAST: MINT-SOURCE ( n -- IR-ID:ir-source-id ) ;
CAST: SOURCE>N ( IR-ID:ir-source-id -- n ) ;
CAST: MINT-FUN ( n -- IR-ID:ir-fun-id ) ;
CAST: FUN>N ( IR-ID:ir-fun-id -- n ) ;
CAST: MINT-BLOCK ( n -- IR-ID:ir-block-id ) ;
CAST: BLOCK>N ( IR-ID:ir-block-id -- n ) ;
CAST: MINT-OP ( n -- IR-ID:ir-op-id ) ;
CAST: OP>N ( IR-ID:ir-op-id -- n ) ;
CAST: MINT-VALUE ( n -- IR-ID:ir-value-id ) ;
CAST: VALUE>N ( IR-ID:ir-value-id -- n ) ;
CAST: MINT-TYPE ( n -- IR-ID:ir-type-id ) ;
CAST: TYPE>N ( IR-ID:ir-type-id -- n ) ;
CAST: MINT-ATTR ( n -- IR-ID:ir-attr-id ) ;
CAST: ATTR>N ( IR-ID:ir-attr-id -- n ) ;
CAST: MINT-SYMBOL ( n -- IR-ID:ir-symbol-id ) ;
CAST: SYMBOL>N ( IR-ID:ir-symbol-id -- n ) ;
CAST: MINT-SPAN ( n -- IR-ID:ir-span-id ) ;
CAST: SPAN>N ( IR-ID:ir-span-id -- n ) ;

: LOCAL-CHECK ( n -- n )
   dup 0 < over LOCAL-MAX > or if E-IR-INDEX-RANGE throw then ;

: PACK-N ( IR-ID:ir-module-id n -- n )
   {: owner:IR-ID:ir-module-id local:n :}
   owner MODULE>N LOCAL-BITS lshift
   local LOCAL-CHECK or ;

: OWNER-N ( n -- IR-ID:ir-module-id )
   LOCAL-BITS rshift MINT-MODULE ;

: LOCAL-N ( n -- n )
   LOCAL-MAX and ;

: CHECK-N ( IR-ID:ir-module-id IR-ID:ir-count n -- n )
   {: owner:IR-ID:ir-module-id bound:IR-ID:ir-count raw:n :}
   raw OWNER-N MODULE>N owner MODULE>N <> if E-IR-OWNER throw then
   raw LOCAL-N bound COUNT>N >= if E-IR-INDEX-BOUND throw then
   raw ;

: PACK-SOURCE ( IR-ID:ir-module-id n -- IR-ID:ir-source-id )
   PACK-N MINT-SOURCE ;
: SOURCE-OWNER ( IR-ID:ir-source-id -- IR-ID:ir-module-id )
   SOURCE>N OWNER-N ;
: SOURCE-LOCAL ( IR-ID:ir-source-id -- n )
   SOURCE>N LOCAL-N ;
: SOURCE-CHECK ( IR-ID:ir-module-id IR-ID:ir-count IR-ID:ir-source-id -- IR-ID:ir-source-id )
   SOURCE>N CHECK-N MINT-SOURCE ;

: PACK-FUN ( IR-ID:ir-module-id n -- IR-ID:ir-fun-id )
   PACK-N MINT-FUN ;
: FUN-OWNER ( IR-ID:ir-fun-id -- IR-ID:ir-module-id )
   FUN>N OWNER-N ;
: FUN-LOCAL ( IR-ID:ir-fun-id -- n )
   FUN>N LOCAL-N ;
: FUN-CHECK ( IR-ID:ir-module-id IR-ID:ir-count IR-ID:ir-fun-id -- IR-ID:ir-fun-id )
   FUN>N CHECK-N MINT-FUN ;

: PACK-BLOCK ( IR-ID:ir-module-id n -- IR-ID:ir-block-id )
   PACK-N MINT-BLOCK ;
: BLOCK-OWNER ( IR-ID:ir-block-id -- IR-ID:ir-module-id )
   BLOCK>N OWNER-N ;
: BLOCK-LOCAL ( IR-ID:ir-block-id -- n )
   BLOCK>N LOCAL-N ;
: BLOCK-CHECK ( IR-ID:ir-module-id IR-ID:ir-count IR-ID:ir-block-id -- IR-ID:ir-block-id )
   BLOCK>N CHECK-N MINT-BLOCK ;

: PACK-OP ( IR-ID:ir-module-id n -- IR-ID:ir-op-id )
   PACK-N MINT-OP ;
: OP-OWNER ( IR-ID:ir-op-id -- IR-ID:ir-module-id )
   OP>N OWNER-N ;
: OP-LOCAL ( IR-ID:ir-op-id -- n )
   OP>N LOCAL-N ;
: OP-CHECK ( IR-ID:ir-module-id IR-ID:ir-count IR-ID:ir-op-id -- IR-ID:ir-op-id )
   OP>N CHECK-N MINT-OP ;

: PACK-VALUE ( IR-ID:ir-module-id n -- IR-ID:ir-value-id )
   PACK-N MINT-VALUE ;
: VALUE-OWNER ( IR-ID:ir-value-id -- IR-ID:ir-module-id )
   VALUE>N OWNER-N ;
: VALUE-LOCAL ( IR-ID:ir-value-id -- n )
   VALUE>N LOCAL-N ;
: VALUE-CHECK ( IR-ID:ir-module-id IR-ID:ir-count IR-ID:ir-value-id -- IR-ID:ir-value-id )
   VALUE>N CHECK-N MINT-VALUE ;

: PACK-TYPE ( IR-ID:ir-module-id n -- IR-ID:ir-type-id )
   PACK-N MINT-TYPE ;
: TYPE-OWNER ( IR-ID:ir-type-id -- IR-ID:ir-module-id )
   TYPE>N OWNER-N ;
: TYPE-LOCAL ( IR-ID:ir-type-id -- n )
   TYPE>N LOCAL-N ;
: TYPE-CHECK ( IR-ID:ir-module-id IR-ID:ir-count IR-ID:ir-type-id -- IR-ID:ir-type-id )
   TYPE>N CHECK-N MINT-TYPE ;

: PACK-ATTR ( IR-ID:ir-module-id n -- IR-ID:ir-attr-id )
   PACK-N MINT-ATTR ;
: ATTR-OWNER ( IR-ID:ir-attr-id -- IR-ID:ir-module-id )
   ATTR>N OWNER-N ;
: ATTR-LOCAL ( IR-ID:ir-attr-id -- n )
   ATTR>N LOCAL-N ;
: ATTR-CHECK ( IR-ID:ir-module-id IR-ID:ir-count IR-ID:ir-attr-id -- IR-ID:ir-attr-id )
   ATTR>N CHECK-N MINT-ATTR ;

: PACK-SYMBOL ( IR-ID:ir-module-id n -- IR-ID:ir-symbol-id )
   PACK-N MINT-SYMBOL ;
: SYMBOL-OWNER ( IR-ID:ir-symbol-id -- IR-ID:ir-module-id )
   SYMBOL>N OWNER-N ;
: SYMBOL-LOCAL ( IR-ID:ir-symbol-id -- n )
   SYMBOL>N LOCAL-N ;
: SYMBOL-CHECK ( IR-ID:ir-module-id IR-ID:ir-count IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   SYMBOL>N CHECK-N MINT-SYMBOL ;

: PACK-SPAN ( IR-ID:ir-module-id n -- IR-ID:ir-span-id )
   PACK-N MINT-SPAN ;
: SPAN-OWNER ( IR-ID:ir-span-id -- IR-ID:ir-module-id )
   SPAN>N OWNER-N ;
: SPAN-LOCAL ( IR-ID:ir-span-id -- n )
   SPAN>N LOCAL-N ;
: SPAN-CHECK ( IR-ID:ir-module-id IR-ID:ir-count IR-ID:ir-span-id -- IR-ID:ir-span-id )
   SPAN>N CHECK-N MINT-SPAN ;

;package
