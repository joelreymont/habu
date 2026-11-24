\ num-types-test.f - the NUM boundary and static-rejection matrix.
\ Run: bin/hb --load src/lib/num-types-test.f; test/gate-stdlib-cases.f
\ schedules it. option<a> has no polymorphic eliminator (only whole-bundle
\ MATCH), so each `*-SOME?` word MATCHes one concrete instantiation.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/adt/option.f
require src/lib/num-types.f

\ The bounds mirror the library's independently.
$7FFFFFFFFFFFFFFF constant T-MAX-N                 \ largest nonnegative cell

: BL-SOME? ( option<NUM:byte-len> -- bool )
   MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;
: IC-SOME? ( option<NUM:item-count> -- bool )
   MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;
: IDX-SOME? ( option<NUM:index> -- bool )
   MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;
: BO-SOME? ( option<NUM:byte-off> -- bool )
   MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;
: AB-SOME? ( option<NUM:alloc-byte-len> -- bool )
   MATCH option
      none OF false ENDOF
      some OF drop true ENDOF
   ;MATCH ;

\ A negative raw cell is rejected by the base validator and never reaches the
\ allocator sink (its some arm is where AS-ALLOC-BYTE-LEN runs).
: ALLOC-SOME? ( n -- bool )
   NUM:BYTE-LEN
   MATCH option
      none OF false ENDOF
      some OF NUM:AS-ALLOC-BYTE-LEN AB-SOME? ENDOF
   ;MATCH ;

: RT-ORDINARY ( -- )                         \ zero valid; positive valid; negative rejected
   5 NUM:BYTE-LEN BL-SOME? TTRUE
   0 NUM:BYTE-LEN BL-SOME? TTRUE
   T-MAX-N NUM:BYTE-LEN BL-SOME? TTRUE
   -1 NUM:BYTE-LEN BL-SOME? TFALSE
   5 NUM:ITEM-COUNT IC-SOME? TTRUE
   0 NUM:ITEM-COUNT IC-SOME? TTRUE
   T-MAX-N NUM:ITEM-COUNT IC-SOME? TTRUE
   -1 NUM:ITEM-COUNT IC-SOME? TFALSE
   5 NUM:INDEX IDX-SOME? TTRUE
   0 NUM:INDEX IDX-SOME? TTRUE
   T-MAX-N NUM:INDEX IDX-SOME? TTRUE
   -1 NUM:INDEX IDX-SOME? TFALSE
   5 NUM:BYTE-OFF BO-SOME? TTRUE
   0 NUM:BYTE-OFF BO-SOME? TTRUE
   T-MAX-N NUM:BYTE-OFF BO-SOME? TTRUE
   -1 NUM:BYTE-OFF BO-SOME? TFALSE ;

: RT-ALLOC ( -- )                            \ the allocation sink rejects zero
   1 ALLOC-SOME? TTRUE
   T-MAX-N ALLOC-SOME? TTRUE
   0 ALLOC-SOME? TFALSE
   -1 ALLOC-SOME? TFALSE ;

: RT ( -- )
   T-RESET
   RT-ORDINARY
   RT-ALLOC
   T-REPORT ;
RT

\ CHECK-QUIET-CANDIDATE!: -1 accepted, 0 rejected (type error), 1 uncheckable.
: STAT ( -- )
   T-RESET
   \ positives: a role-preserving swap and the two constructor edges resolve.
   s" GOOD-OFF-SWAP ( NUM:byte-off NUM:byte-len -- NUM:byte-len NUM:byte-off ) swap"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" GOOD-BYTE-LEN ( n -- option<NUM:byte-len> ) NUM:BYTE-LEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" GOOD-AS-ALLOC ( NUM:byte-len -- option<NUM:alloc-byte-len> ) NUM:AS-ALLOC-BYTE-LEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ negatives: swapped roles, raw n where a role is required, cross-role sink.
   s" BAD-OFF-SWAP ( NUM:byte-off NUM:byte-len -- NUM:byte-off NUM:byte-len ) swap"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-RAW-OFF ( n -- NUM:byte-off )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-RAW-ALLOC ( n -- option<NUM:alloc-byte-len> ) NUM:AS-ALLOC-BYTE-LEN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-ROLE-ALLOC ( NUM:item-count -- option<NUM:alloc-byte-len> ) NUM:AS-ALLOC-BYTE-LEN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-ROLE-VALIDATOR ( NUM:byte-off -- option<NUM:byte-len> ) NUM:BYTE-LEN"
      CHECK-QUIET-CANDIDATE! 0 T=
   T-REPORT ;
STAT

T-RESET

package NUM-TEST
private

: YES   ( str -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO    ( str -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

\ Every role is one cell wide, so only the nominal family separates an item count
\ from a byte length: `some` refuses a wrong payload role and a wrong instantiation.
s" NF-OK ( NUM:byte-len -- option<NUM:byte-len> ) OPTION:SOME" YES
s" NF-RAW ( n -- option<NUM:byte-len> ) OPTION:SOME" NO
s" NF-BARE ( NUM:byte-len -- n ) OPTION:SOME" NO
s" NF-NONE ( -- option<NUM:byte-len> ) OPTION:SOME" NO
s" NF-XROLE ( NUM:item-count -- option<NUM:byte-len> ) OPTION:SOME" NO
s" NF-INST ( NUM:byte-len -- option<NUM:item-count> ) OPTION:SOME" NO
s" NF-PAY ( NUM:byte-len -- option<NUM:byte-len> ) OPTION:NONE" NO

;package

T-REPORT
