\ num-types-test.f - the NUM boundary and static-rejection matrix.
\ Run: bin/hb --load src/lib/num-types-test.f; test/gate-stdlib-cases.f
\ schedules it. numeric-result<a> has no polymorphic eliminator (only
\ whole-bundle MATCH), so each `*-CODE` word
\ MATCHes one concrete instantiation, mapping it to 0 (ok) or the exact E-NUM-*
\ refusal.

require src/lib/error.f
require src/lib/string.f
require src/lib/test.f
require src/lib/num-types.f

\ The bounds mirror the library's independently.
$7FFFFFFFFFFFFFFF constant T-MAX-N                 \ largest nonnegative cell

: BL-CODE ( NUM:numeric-result<NUM:byte-len> -- n )
   MATCH NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-NUM-NEGATIVE ENDOF
      zero OF E-NUM-ZERO ENDOF           overflow OF E-NUM-OVERFLOW ENDOF
      underflow OF E-NUM-UNDERFLOW ENDOF bad-alignment OF E-NUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-NUM-MISALIGNED ENDOF
   ;MATCH ;
: IC-CODE ( NUM:numeric-result<NUM:item-count> -- n )
   MATCH NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-NUM-NEGATIVE ENDOF
      zero OF E-NUM-ZERO ENDOF           overflow OF E-NUM-OVERFLOW ENDOF
      underflow OF E-NUM-UNDERFLOW ENDOF bad-alignment OF E-NUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-NUM-MISALIGNED ENDOF
   ;MATCH ;
: IDX-CODE ( NUM:numeric-result<NUM:index> -- n )
   MATCH NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-NUM-NEGATIVE ENDOF
      zero OF E-NUM-ZERO ENDOF           overflow OF E-NUM-OVERFLOW ENDOF
      underflow OF E-NUM-UNDERFLOW ENDOF bad-alignment OF E-NUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-NUM-MISALIGNED ENDOF
   ;MATCH ;
: BO-CODE ( NUM:numeric-result<NUM:byte-off> -- n )
   MATCH NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-NUM-NEGATIVE ENDOF
      zero OF E-NUM-ZERO ENDOF           overflow OF E-NUM-OVERFLOW ENDOF
      underflow OF E-NUM-UNDERFLOW ENDOF bad-alignment OF E-NUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-NUM-MISALIGNED ENDOF
   ;MATCH ;
: AB-CODE ( NUM:numeric-result<NUM:alloc-byte-len> -- n )
   MATCH NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-NUM-NEGATIVE ENDOF
      zero OF E-NUM-ZERO ENDOF           overflow OF E-NUM-OVERFLOW ENDOF
      underflow OF E-NUM-UNDERFLOW ENDOF bad-alignment OF E-NUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-NUM-MISALIGNED ENDOF
   ;MATCH ;

\ A negative raw cell is rejected by the base validator and never reaches the
\ allocator sink (its ok arm is where AS-ALLOC-BYTE-LEN runs).
: ALLOC-BYTES# ( n -- n )
   NUM:BYTE-LEN
   MATCH NUM:numeric-result
      ok OF NUM:AS-ALLOC-BYTE-LEN AB-CODE ENDOF
      negative OF E-NUM-NEGATIVE ENDOF   zero OF E-NUM-ZERO ENDOF
      overflow OF E-NUM-OVERFLOW ENDOF   underflow OF E-NUM-UNDERFLOW ENDOF
      bad-alignment OF E-NUM-BAD-ALIGNMENT ENDOF  misaligned OF E-NUM-MISALIGNED ENDOF
   ;MATCH ;

: RT-ORDINARY ( -- )                         \ zero valid; positive valid; negative rejected
   5 NUM:BYTE-LEN BL-CODE 0 T=
   0 NUM:BYTE-LEN BL-CODE 0 T=
   T-MAX-N NUM:BYTE-LEN BL-CODE 0 T=
   -1 NUM:BYTE-LEN BL-CODE E-NUM-NEGATIVE T=
   5 NUM:ITEM-COUNT IC-CODE 0 T=
   0 NUM:ITEM-COUNT IC-CODE 0 T=
   T-MAX-N NUM:ITEM-COUNT IC-CODE 0 T=
   -1 NUM:ITEM-COUNT IC-CODE E-NUM-NEGATIVE T=
   5 NUM:INDEX IDX-CODE 0 T=
   0 NUM:INDEX IDX-CODE 0 T=
   T-MAX-N NUM:INDEX IDX-CODE 0 T=
   -1 NUM:INDEX IDX-CODE E-NUM-NEGATIVE T=
   5 NUM:BYTE-OFF BO-CODE 0 T=
   0 NUM:BYTE-OFF BO-CODE 0 T=
   T-MAX-N NUM:BYTE-OFF BO-CODE 0 T=
   -1 NUM:BYTE-OFF BO-CODE E-NUM-NEGATIVE T= ;

: RT-ALLOC ( -- )                            \ the allocation sink rejects zero
   1 ALLOC-BYTES# 0 T=
   T-MAX-N ALLOC-BYTES# 0 T=
   0 ALLOC-BYTES# E-NUM-ZERO T=
   -1 ALLOC-BYTES# E-NUM-NEGATIVE T= ;

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
   s" GOOD-BYTE-LEN ( n -- NUM:numeric-result<NUM:byte-len> ) NUM:BYTE-LEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" GOOD-AS-ALLOC ( NUM:byte-len -- NUM:numeric-result<NUM:alloc-byte-len> ) NUM:AS-ALLOC-BYTE-LEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ negatives: swapped roles, raw n where a role is required, cross-role sink.
   s" BAD-OFF-SWAP ( NUM:byte-off NUM:byte-len -- NUM:byte-off NUM:byte-len ) swap"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-RAW-OFF ( n -- NUM:byte-off )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-RAW-ALLOC ( n -- NUM:numeric-result<NUM:alloc-byte-len> ) NUM:AS-ALLOC-BYTE-LEN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-ROLE-ALLOC ( NUM:item-count -- NUM:numeric-result<NUM:alloc-byte-len> ) NUM:AS-ALLOC-BYTE-LEN"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-ROLE-VALIDATOR ( NUM:byte-off -- NUM:numeric-result<NUM:byte-len> ) NUM:BYTE-LEN"
      CHECK-QUIET-CANDIDATE! 0 T=
   T-REPORT ;
STAT

T-RESET

package NUM-TEST
private

: YES   ( ^u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO    ( ^u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

public

\ nr-twin is NUM:numeric-result's SHAPE under another name: same arity, the
\ same seven cases in the same order, the same named payload field. It exists only
\ so the negatives below can prove result identity is NOMINAL - two identically
\ shaped families never unify, in either direction. The positive control builds
\ through the twin's own ok, so neither negative could pass by being unresolvable
\ instead of ill-typed.
ENUM nr-twin<a>
   VARIANT ok FIELD role a ;VARIANT
   VARIANT negative ;VARIANT
   VARIANT zero ;VARIANT
   VARIANT overflow ;VARIANT
   VARIANT underflow ;VARIANT
   VARIANT bad-alignment ;VARIANT
   VARIANT misaligned ;VARIANT
;ENUM

private

s" NC-OK ( NUM:byte-len -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:OK" YES
s" NC-NEG ( -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:NEGATIVE" YES
s" NC-ZERO ( -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:ZERO" YES
s" NC-OVER ( -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:OVERFLOW" YES
s" NC-UNDER ( -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:UNDERFLOW" YES
s" NC-BADAL ( -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:BAD-ALIGNMENT" YES
s" NC-MISAL ( -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:MISALIGNED" YES
\ the payload parameter really is a parameter: a second, unrelated role instantiates it
s" NC-OK-IC ( NUM:item-count -- NUM:numeric-result<NUM:item-count> ) NUM-NUMERIC--RESULT:OK" YES

\ NF-XROLE and NF-INST are the sharp ones. Every role in this package is one cell
\ wide, so nothing but the nominal family separates an item count from a byte
\ length; the ok constructor must refuse the wrong role in the payload and must
\ refuse to hand back a result instantiated at a different role than it consumed.
s" NF-RAW ( n -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:OK" NO
s" NF-BARE ( NUM:byte-len -- n ) NUM-NUMERIC--RESULT:OK" NO
s" NF-NONE ( -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:OK" NO
s" NF-XROLE ( NUM:item-count -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:OK" NO
s" NF-INST ( NUM:byte-len -- NUM:numeric-result<NUM:item-count> ) NUM-NUMERIC--RESULT:OK" NO
s" NF-PAY ( NUM:byte-len -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:NEGATIVE" NO

s" NT-OK ( NUM:byte-len -- nr-twin<NUM:byte-len> ) NUM--TEST-NR--TWIN:OK" YES
s" NT-X1 ( NUM:byte-len -- nr-twin<NUM:byte-len> ) NUM-NUMERIC--RESULT:OK" NO
s" NT-X2 ( NUM:byte-len -- NUM:numeric-result<NUM:byte-len> ) NUM--TEST-NR--TWIN:OK" NO

;package

T-REPORT
