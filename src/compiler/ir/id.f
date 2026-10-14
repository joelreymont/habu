\ id.f - shared compiler IR identity and module-mint authority.

require src/lib/errors.f

package IR-ID
public

NEWTYPE ir-module-key
NEWTYPE ir-module-id
NEWTYPE ir-source-id
NEWTYPE ir-fun-id
NEWTYPE ir-block-id
NEWTYPE ir-op-id
NEWTYPE ir-value-id
NEWTYPE ir-type-id
NEWTYPE ir-attr-id
NEWTYPE ir-symbol-id
NEWTYPE ir-span-id
NEWTYPE ir-pool-offset
NEWTYPE ir-count

private

$7FFFFFFF constant MODULE-MAX
$FFFFFFFF constant LOCAL-MAX
32 constant LOCAL-BITS

here CELL 1- and CELL swap - CELL 1- and allot
variable NEXT-SERIAL
0 NEXT-SERIAL !

CAST: MINT-KEY ( n -- IR-ID:ir-module-key )
CAST: KEY>N ( IR-ID:ir-module-key -- n )

CAST: MINT-MODULE ( n -- IR-ID:ir-module-id )
CAST: MODULE>N ( IR-ID:ir-module-id -- n )

\ A cast declares a retype and carries no code, so a guarded mint is two things:
\ the retype, and the checked word that refuses a bad value before applying it.
CAST: N>COUNT ( n -- IR-ID:ir-count )
: MINT-COUNT ( n -- IR-ID:ir-count )
   dup 0 < if E-IR-SCALAR-RANGE throw then N>COUNT ;

CAST: COUNT>N ( IR-ID:ir-count -- n )

CAST: N>POOL-OFF ( n -- IR-ID:ir-pool-offset )
: MINT-POOL-OFF ( n -- IR-ID:ir-pool-offset )
   dup 0 < if E-IR-SCALAR-RANGE throw then N>POOL-OFF ;

CAST: POOL-OFF>N ( IR-ID:ir-pool-offset -- n )

CAST: MINT-SOURCE ( n -- IR-ID:ir-source-id )
CAST: SOURCE>N ( IR-ID:ir-source-id -- n )
CAST: MINT-FUN ( n -- IR-ID:ir-fun-id )
CAST: FUN>N ( IR-ID:ir-fun-id -- n )
CAST: MINT-BLOCK ( n -- IR-ID:ir-block-id )
CAST: BLOCK>N ( IR-ID:ir-block-id -- n )
CAST: MINT-OP ( n -- IR-ID:ir-op-id )
CAST: OP>N ( IR-ID:ir-op-id -- n )
CAST: MINT-VALUE ( n -- IR-ID:ir-value-id )
CAST: VALUE>N ( IR-ID:ir-value-id -- n )
CAST: MINT-TYPE ( n -- IR-ID:ir-type-id )
CAST: TYPE>N ( IR-ID:ir-type-id -- n )
CAST: MINT-ATTR ( n -- IR-ID:ir-attr-id )
CAST: ATTR>N ( IR-ID:ir-attr-id -- n )
CAST: MINT-SYMBOL ( n -- IR-ID:ir-symbol-id )
CAST: SYMBOL>N ( IR-ID:ir-symbol-id -- n )
CAST: MINT-SPAN ( n -- IR-ID:ir-span-id )
CAST: SPAN>N ( IR-ID:ir-span-id -- n )

: SERIAL-NEXT ( n -- n )
   dup 0 < over MODULE-MAX >= or if E-IR-MODULE-EXHAUSTED throw then
   1+ ;

: TRY-SERIAL ( -- n bool )
   NEXT-SERIAL atomic@ {: current:n :}
   current SERIAL-NEXT {: next:n :}
   current next NEXT-SERIAL atomic-cas current =
   if next 0 0= else 0 0 0 <> then ;

: TAKE-SERIAL ( -- n )
   begin
      TRY-SERIAL dup 0=
   while
      2drop
   repeat
   drop ;

: KEY-SERIAL ( IR-ID:ir-module-key -- n )
   KEY>N
   dup 0= if E-IR-MODULE-ZERO throw then
   dup 0 < over MODULE-MAX > or if E-IR-MODULE-RANGE throw then ;

: LOCAL-CHECK ( n -- n )
   dup 0 < over LOCAL-MAX > or if E-IR-INDEX-RANGE throw then ;

: PACK-N ( IR-ID:ir-module-key n -- n )
   {: key:IR-ID:ir-module-key local:n :}
   key KEY-SERIAL LOCAL-BITS lshift
   local LOCAL-CHECK or ;

: OWNER-N ( n -- IR-ID:ir-module-id )
   LOCAL-BITS rshift MINT-MODULE ;

: LOCAL-N ( n -- n )
   LOCAL-MAX and ;

: CHECK-N ( IR-ID:ir-module-key IR-ID:ir-count n -- n )
   {: key:IR-ID:ir-module-key bound:IR-ID:ir-count raw:n :}
   raw OWNER-N MODULE>N key KEY-SERIAL <> if E-IR-OWNER throw then
   raw LOCAL-N bound COUNT>N >= if E-IR-INDEX-BOUND throw then
   raw ;

public

: NEW-MODULE ( -- IR-ID:ir-module-key IR-ID:ir-module-id )
   TAKE-SERIAL dup MINT-KEY swap MINT-MODULE ;

\ FILE ranges can carry module identities in DATA. Replay the cold high-water
\ before importing one so a later mint cannot reuse a retired identity. Work
\ outside the range may already have advanced this counter further.
: SERIAL-HISTORY ( -- n ) NEXT-SERIAL atomic@ ;
: SERIAL-CHECK ( n -- n )
   dup 0 < over MODULE-MAX > or if E-IR-MODULE-EXHAUSTED throw then ;
: REPLAY-SERIAL ( n -- ) {: want:n :}
   want SERIAL-CHECK drop
   begin
      NEXT-SERIAL atomic@ {: old:n :}
      old want >= if exit then
      old want NEXT-SERIAL atomic-cas old =
   until ;

: MODULE-SAME? ( IR-ID:ir-module-id IR-ID:ir-module-id -- bool )
   MODULE>N swap MODULE>N = ;

: COUNT ( n -- IR-ID:ir-count )
   MINT-COUNT ;

: COUNT-N ( IR-ID:ir-count -- n )
   COUNT>N ;

: POOL-OFF ( n -- IR-ID:ir-pool-offset )
   MINT-POOL-OFF ;

: POOL-OFF-N ( IR-ID:ir-pool-offset -- n )
   POOL-OFF>N ;

: PACK-SOURCE ( IR-ID:ir-module-key n -- IR-ID:ir-source-id )
   PACK-N MINT-SOURCE ;
: SOURCE-OWNER ( IR-ID:ir-source-id -- IR-ID:ir-module-id )
   SOURCE>N OWNER-N ;
: SOURCE-LOCAL ( IR-ID:ir-source-id -- n )
   SOURCE>N LOCAL-N ;
: SOURCE-CHECK ( IR-ID:ir-module-key IR-ID:ir-count IR-ID:ir-source-id -- IR-ID:ir-source-id )
   SOURCE>N CHECK-N MINT-SOURCE ;

: PACK-FUN ( IR-ID:ir-module-key n -- IR-ID:ir-fun-id )
   PACK-N MINT-FUN ;
: FUN-OWNER ( IR-ID:ir-fun-id -- IR-ID:ir-module-id )
   FUN>N OWNER-N ;
: FUN-LOCAL ( IR-ID:ir-fun-id -- n )
   FUN>N LOCAL-N ;
: FUN-CHECK ( IR-ID:ir-module-key IR-ID:ir-count IR-ID:ir-fun-id -- IR-ID:ir-fun-id )
   FUN>N CHECK-N MINT-FUN ;

: PACK-BLOCK ( IR-ID:ir-module-key n -- IR-ID:ir-block-id )
   PACK-N MINT-BLOCK ;
: BLOCK-OWNER ( IR-ID:ir-block-id -- IR-ID:ir-module-id )
   BLOCK>N OWNER-N ;
: BLOCK-LOCAL ( IR-ID:ir-block-id -- n )
   BLOCK>N LOCAL-N ;
: BLOCK-CHECK ( IR-ID:ir-module-key IR-ID:ir-count IR-ID:ir-block-id -- IR-ID:ir-block-id )
   BLOCK>N CHECK-N MINT-BLOCK ;

: PACK-OP ( IR-ID:ir-module-key n -- IR-ID:ir-op-id )
   PACK-N MINT-OP ;
: OP-OWNER ( IR-ID:ir-op-id -- IR-ID:ir-module-id )
   OP>N OWNER-N ;
: OP-LOCAL ( IR-ID:ir-op-id -- n )
   OP>N LOCAL-N ;
: OP-CHECK ( IR-ID:ir-module-key IR-ID:ir-count IR-ID:ir-op-id -- IR-ID:ir-op-id )
   OP>N CHECK-N MINT-OP ;

: PACK-VALUE ( IR-ID:ir-module-key n -- IR-ID:ir-value-id )
   PACK-N MINT-VALUE ;
: VALUE-OWNER ( IR-ID:ir-value-id -- IR-ID:ir-module-id )
   VALUE>N OWNER-N ;
: VALUE-LOCAL ( IR-ID:ir-value-id -- n )
   VALUE>N LOCAL-N ;
: VALUE-CHECK ( IR-ID:ir-module-key IR-ID:ir-count IR-ID:ir-value-id -- IR-ID:ir-value-id )
   VALUE>N CHECK-N MINT-VALUE ;

: PACK-TYPE ( IR-ID:ir-module-key n -- IR-ID:ir-type-id )
   PACK-N MINT-TYPE ;
: TYPE-OWNER ( IR-ID:ir-type-id -- IR-ID:ir-module-id )
   TYPE>N OWNER-N ;
: TYPE-LOCAL ( IR-ID:ir-type-id -- n )
   TYPE>N LOCAL-N ;
: TYPE-CHECK ( IR-ID:ir-module-key IR-ID:ir-count IR-ID:ir-type-id -- IR-ID:ir-type-id )
   TYPE>N CHECK-N MINT-TYPE ;

: PACK-ATTR ( IR-ID:ir-module-key n -- IR-ID:ir-attr-id )
   PACK-N MINT-ATTR ;
: ATTR-OWNER ( IR-ID:ir-attr-id -- IR-ID:ir-module-id )
   ATTR>N OWNER-N ;
: ATTR-LOCAL ( IR-ID:ir-attr-id -- n )
   ATTR>N LOCAL-N ;
: ATTR-CHECK ( IR-ID:ir-module-key IR-ID:ir-count IR-ID:ir-attr-id -- IR-ID:ir-attr-id )
   ATTR>N CHECK-N MINT-ATTR ;

: PACK-SYMBOL ( IR-ID:ir-module-key n -- IR-ID:ir-symbol-id )
   PACK-N MINT-SYMBOL ;
: SYMBOL-OWNER ( IR-ID:ir-symbol-id -- IR-ID:ir-module-id )
   SYMBOL>N OWNER-N ;
: SYMBOL-LOCAL ( IR-ID:ir-symbol-id -- n )
   SYMBOL>N LOCAL-N ;
: SYMBOL-CHECK ( IR-ID:ir-module-key IR-ID:ir-count IR-ID:ir-symbol-id -- IR-ID:ir-symbol-id )
   SYMBOL>N CHECK-N MINT-SYMBOL ;

: PACK-SPAN ( IR-ID:ir-module-key n -- IR-ID:ir-span-id )
   PACK-N MINT-SPAN ;
: SPAN-OWNER ( IR-ID:ir-span-id -- IR-ID:ir-module-id )
   SPAN>N OWNER-N ;
: SPAN-LOCAL ( IR-ID:ir-span-id -- n )
   SPAN>N LOCAL-N ;
: SPAN-CHECK ( IR-ID:ir-module-key IR-ID:ir-count IR-ID:ir-span-id -- IR-ID:ir-span-id )
   SPAN>N CHECK-N MINT-SPAN ;

private
get-current prot-wid-add

public
get-current prot-wid-add

;package
