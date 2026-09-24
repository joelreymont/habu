\ num-types-test.f - the NUM B5.1 boundary + static-rejection matrix
\ (dot habu-implement-cad-num-962bf5d9). Run:
\   bin/hb --load lib/num-types-test.f
\
\ Focused direct-loaded gate home for the B5.1 contract. Production modules load
\ these types transitively through lib/num-arithmetic.f; this file owns the
\ validator boundary matrix, not library authority. The canonical native registry
\ schedules it through test/gate-stdlib-cases.f.
\
\ numeric-result<a> has no polymorphic eliminator yet (whole-bundle MATCH, dot
\ habu-typestate-result-drop-5ae048a7), so a caller MATCHes the concrete
\ instantiation it holds. Each `*-CODE` word below is that concrete classifier:
\ it maps a role's numeric-result to 0 (ok) or the named E-NUM-* refusal code,
\ so the runtime matrix asserts the EXACT refusal, not merely "not ok".

require lib/errors.f
require lib/string.f
require lib/test.f
require test/checker-assert.f
require lib/num-types.f

\ ---- boundary constants (independently mirror the library bound) --------------
$7FFFFFFFFFFFFFFF constant T-MAX-N                 \ largest nonnegative cell
T-MAX-N 1 cells / constant T-MAX-ALLOC-CELLS       \ MAX-N / CELL-BYTES
$4000000000000000 constant T-LARGEST-POW2          \ largest positive power-of-two alignment

\ ---- per-role concrete classifiers: numeric-result<role> -> code (0 = ok) -----
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
: CC-CODE ( NUM:numeric-result<NUM:cell-count> -- n )
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
: CO-CODE ( NUM:numeric-result<NUM:cell-off> -- n )
   MATCH NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-NUM-NEGATIVE ENDOF
      zero OF E-NUM-ZERO ENDOF           overflow OF E-NUM-OVERFLOW ENDOF
      underflow OF E-NUM-UNDERFLOW ENDOF bad-alignment OF E-NUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-NUM-MISALIGNED ENDOF
   ;MATCH ;
: AL-CODE ( NUM:numeric-result<NUM:alignment> -- n )
   MATCH NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-NUM-NEGATIVE ENDOF
      zero OF E-NUM-ZERO ENDOF           overflow OF E-NUM-OVERFLOW ENDOF
      underflow OF E-NUM-UNDERFLOW ENDOF bad-alignment OF E-NUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-NUM-MISALIGNED ENDOF
   ;MATCH ;
: PD-CODE ( NUM:numeric-result<NUM:positive-divisor> -- n )
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
: AC-CODE ( NUM:numeric-result<NUM:alloc-cell-count> -- n )
   MATCH NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-NUM-NEGATIVE ENDOF
      zero OF E-NUM-ZERO ENDOF           overflow OF E-NUM-OVERFLOW ENDOF
      underflow OF E-NUM-UNDERFLOW ENDOF bad-alignment OF E-NUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-NUM-MISALIGNED ENDOF
   ;MATCH ;

\ ---- allocation caller path: build a zero-admitting role, then narrow it ------
\ A negative raw cell is rejected by the base validator and never reaches the
\ allocator sink (its ok arm is where AS-ALLOC-* runs).
: ALLOC-BYTES# ( n -- n )
   NUM:BYTE-LEN
   MATCH NUM:numeric-result
      ok OF NUM:AS-ALLOC-BYTE-LEN AB-CODE ENDOF
      negative OF E-NUM-NEGATIVE ENDOF   zero OF E-NUM-ZERO ENDOF
      overflow OF E-NUM-OVERFLOW ENDOF   underflow OF E-NUM-UNDERFLOW ENDOF
      bad-alignment OF E-NUM-BAD-ALIGNMENT ENDOF  misaligned OF E-NUM-MISALIGNED ENDOF
   ;MATCH ;
: ALLOC-CELLS# ( n -- n )
   NUM:CELL-COUNT
   MATCH NUM:numeric-result
      ok OF NUM:AS-ALLOC-CELL-COUNT AC-CODE ENDOF
      negative OF E-NUM-NEGATIVE ENDOF   zero OF E-NUM-ZERO ENDOF
      overflow OF E-NUM-OVERFLOW ENDOF   underflow OF E-NUM-UNDERFLOW ENDOF
      bad-alignment OF E-NUM-BAD-ALIGNMENT ENDOF  misaligned OF E-NUM-MISALIGNED ENDOF
   ;MATCH ;

\ ---- runtime boundary matrix: every role x its applicable boundaries ----------
: RT-ORDINARY ( -- )                         \ zero valid; positive valid; negative rejected
   5 NUM:BYTE-LEN BL-CODE 0 T=
   0 NUM:BYTE-LEN BL-CODE 0 T=
   T-MAX-N NUM:BYTE-LEN BL-CODE 0 T=
   -1 NUM:BYTE-LEN BL-CODE E-NUM-NEGATIVE T=
   5 NUM:ITEM-COUNT IC-CODE 0 T=
   0 NUM:ITEM-COUNT IC-CODE 0 T=
   T-MAX-N NUM:ITEM-COUNT IC-CODE 0 T=
   -1 NUM:ITEM-COUNT IC-CODE E-NUM-NEGATIVE T=
   5 NUM:CELL-COUNT CC-CODE 0 T=
   0 NUM:CELL-COUNT CC-CODE 0 T=
   T-MAX-N NUM:CELL-COUNT CC-CODE 0 T=
   -1 NUM:CELL-COUNT CC-CODE E-NUM-NEGATIVE T=
   5 NUM:INDEX IDX-CODE 0 T=
   0 NUM:INDEX IDX-CODE 0 T=
   T-MAX-N NUM:INDEX IDX-CODE 0 T=
   -1 NUM:INDEX IDX-CODE E-NUM-NEGATIVE T=
   5 NUM:BYTE-OFF BO-CODE 0 T=
   0 NUM:BYTE-OFF BO-CODE 0 T=
   T-MAX-N NUM:BYTE-OFF BO-CODE 0 T=
   -1 NUM:BYTE-OFF BO-CODE E-NUM-NEGATIVE T=
   5 NUM:CELL-OFF CO-CODE 0 T=
   0 NUM:CELL-OFF CO-CODE 0 T=
   T-MAX-N NUM:CELL-OFF CO-CODE 0 T=
   -1 NUM:CELL-OFF CO-CODE E-NUM-NEGATIVE T= ;

: RT-ALIGNMENT ( -- )                        \ positive power of two only; all else bad-alignment
   1 NUM:ALIGNMENT AL-CODE 0 T=
   2 NUM:ALIGNMENT AL-CODE 0 T=
   T-LARGEST-POW2 NUM:ALIGNMENT AL-CODE 0 T=
   0 NUM:ALIGNMENT AL-CODE E-NUM-BAD-ALIGNMENT T=
   -1 NUM:ALIGNMENT AL-CODE E-NUM-BAD-ALIGNMENT T=
   3 NUM:ALIGNMENT AL-CODE E-NUM-BAD-ALIGNMENT T=
   T-MAX-N NUM:ALIGNMENT AL-CODE E-NUM-BAD-ALIGNMENT T= ;

: RT-DIVISOR ( -- )                          \ positive only; zero rejected; negative rejected
   1 NUM:POSITIVE-DIVISOR PD-CODE 0 T=
   T-MAX-N NUM:POSITIVE-DIVISOR PD-CODE 0 T=
   0 NUM:POSITIVE-DIVISOR PD-CODE E-NUM-ZERO T=
   -1 NUM:POSITIVE-DIVISOR PD-CODE E-NUM-NEGATIVE T= ;

: RT-ALLOC ( -- )                            \ allocation sinks reject zero (and overflow)
   1 ALLOC-BYTES# 0 T=
   T-MAX-N ALLOC-BYTES# 0 T=
   0 ALLOC-BYTES# E-NUM-ZERO T=
   -1 ALLOC-BYTES# E-NUM-NEGATIVE T=
   1 ALLOC-CELLS# 0 T=
   T-MAX-ALLOC-CELLS ALLOC-CELLS# 0 T=
   0 ALLOC-CELLS# E-NUM-ZERO T=
   T-MAX-ALLOC-CELLS 1 + ALLOC-CELLS# E-NUM-OVERFLOW T=
   -1 ALLOC-CELLS# E-NUM-NEGATIVE T= ;

: RT ( -- )
   T-RESET
   RT-ORDINARY
   RT-ALIGNMENT
   RT-DIVISOR
   RT-ALLOC
   T-REPORT ;
RT

\ ---- static rejection matrix: raw n and cross-role swaps reject at CHECK -------
\ CHECK-QUIET-CANDIDATE!: -1 accepted, 0 rejected (type error), 1 uncheckable.
: STAT ( -- )
   T-RESET
   \ positives: a role-preserving swap and the two constructor edges resolve.
   s" GOOD-OFF-SWAP ( NUM:byte-off NUM:cell-off -- NUM:cell-off NUM:byte-off ) swap"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" GOOD-BYTE-LEN ( n -- NUM:numeric-result<NUM:byte-len> ) NUM:BYTE-LEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" GOOD-AS-ALLOC ( NUM:cell-count -- NUM:numeric-result<NUM:alloc-cell-count> ) NUM:AS-ALLOC-CELL-COUNT"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ negatives: swapped roles, raw n where a role is required, cross-role sink.
   s" BAD-OFF-SWAP ( NUM:byte-off NUM:cell-off -- NUM:byte-off NUM:cell-off ) swap"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-RAW-OFF ( n -- NUM:byte-off )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-RAW-ALLOC ( n -- NUM:numeric-result<NUM:alloc-cell-count> ) NUM:AS-ALLOC-CELL-COUNT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-ROLE-ALLOC ( NUM:byte-len -- NUM:numeric-result<NUM:alloc-cell-count> ) NUM:AS-ALLOC-CELL-COUNT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-ROLE-VALIDATOR ( NUM:byte-off -- NUM:numeric-result<NUM:byte-len> ) NUM:BYTE-LEN"
      CHECK-QUIET-CANDIDATE! 0 T=
   T-REPORT ;
STAT

T-RESET

package NUM-TEST
private

: YES   ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO    ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

\ ---- compiled round trip: construct through all seven generated constructors --
\ The checker pins further down prove the constructors resolve and type-check as
\ candidate text. This word proves they work in COMPILED code: it takes a real
\ numeric-result apart and puts it back together arm by arm, so all seven
\ generated constructors are compiled, and the ok arm binds its payload through a
\ typed local before handing it back to the ok constructor. The seven-arm shape is
\ repeated rather than factored because a polymorphic eliminator over the whole
\ bundle is not expressible yet (dot habu-typestate-result-drop-5ae048a7), which
\ is the same reason the ten classifiers at the top of this file repeat it.
: REBUILD-CC ( NUM:numeric-result<NUM:cell-count> -- NUM:numeric-result<NUM:cell-count> )
   MATCH NUM:numeric-result
      ok OF {: got:NUM:cell-count :} got NUM-NUMERIC--RESULT:OK ENDOF
      negative OF NUM-NUMERIC--RESULT:NEGATIVE ENDOF
      zero OF NUM-NUMERIC--RESULT:ZERO ENDOF
      overflow OF NUM-NUMERIC--RESULT:OVERFLOW ENDOF
      underflow OF NUM-NUMERIC--RESULT:UNDERFLOW ENDOF
      bad-alignment OF NUM-NUMERIC--RESULT:BAD-ALIGNMENT ENDOF
      misaligned OF NUM-NUMERIC--RESULT:MISALIGNED ENDOF
   ;MATCH ;

\ The payload's VALUE, not just its presence: the recovered cell count is handed
\ straight to the production allocator narrowing, which answers `zero` for 0, ok
\ up to MAX-ALLOC-CELLS and `overflow` past it. A round trip that dropped or
\ zeroed the payload would report zero for every input, and one that truncated it
\ would move the overflow boundary.
: ALLOC-CC# ( NUM:numeric-result<NUM:cell-count> -- n )
   MATCH NUM:numeric-result
      ok OF {: got:NUM:cell-count :} got NUM:AS-ALLOC-CELL-COUNT AC-CODE ENDOF
      negative OF E-NUM-NEGATIVE ENDOF   zero OF E-NUM-ZERO ENDOF
      overflow OF E-NUM-OVERFLOW ENDOF   underflow OF E-NUM-UNDERFLOW ENDOF
      bad-alignment OF E-NUM-BAD-ALIGNMENT ENDOF  misaligned OF E-NUM-MISALIGNED ENDOF
   ;MATCH ;

: RT-TAG# ( n -- n )      \ tag survives the round trip (0 = ok, else refusal code)
   NUM:CELL-COUNT REBUILD-CC CC-CODE ;
: RT-VALUE# ( n -- n )    \ and so does the exact payload magnitude
   NUM:CELL-COUNT REBUILD-CC ALLOC-CC# ;

public

\ nr-twin is NUM:numeric-result's SHAPE under another name: same arity, the
\ same seven cases in the same order, the same named payload field. It exists only
\ so the negatives below can prove result identity is NOMINAL - two identically
\ shaped families never unify, in either direction. It has to be public, because a
\ private family publishes no constructors and the positive control builds through
\ the twin's own ok, so neither negative could pass by being unresolvable instead
\ of ill-typed. Its generated constructor package NUM--TEST-NR--TWIN is 23
\ bytes, inside the 32-byte readable-spelling cap TF-CTOR-NAME-LIMIT
\ (src/core/type-family.f), so it keeps the readable escaped spelling instead of
\ falling back to the opaque SHA form; the pin on that exact spelling below is
\ what would notice if it ever crossed the cap.
ENUM nr-twin 1
   VARIANT ok FIELD role a ;VARIANT
   VARIANT negative ;VARIANT
   VARIANT zero ;VARIANT
   VARIANT overflow ;VARIANT
   VARIANT underflow ;VARIANT
   VARIANT bad-alignment ;VARIANT
   VARIANT misaligned ;VARIANT
;ENUM

private

\ ---- generated constructors: exact spelling + exact effect --------------------
s" NC-OK ( NUM:byte-len -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:OK" YES
s" NC-NEG ( -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:NEGATIVE" YES
s" NC-ZERO ( -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:ZERO" YES
s" NC-OVER ( -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:OVERFLOW" YES
s" NC-UNDER ( -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:UNDERFLOW" YES
s" NC-BADAL ( -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:BAD-ALIGNMENT" YES
s" NC-MISAL ( -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:MISALIGNED" YES
\ the payload parameter really is a parameter: a second, unrelated role instantiates it
s" NC-OK-AL ( NUM:alignment -- NUM:numeric-result<NUM:alignment> ) NUM-NUMERIC--RESULT:OK" YES

\ ---- forge negatives on the ok payload slot -----------------------------------
\ NF-XROLE and NF-INST are the sharp ones. Every role in this package is one cell
\ wide, so nothing but the nominal family separates a cell count from a byte
\ length; the ok constructor must refuse the wrong role in the payload and must
\ refuse to hand back a result instantiated at a different role than it consumed.
s" NF-RAW ( n -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:OK" NO
s" NF-BARE ( NUM:byte-len -- n ) NUM-NUMERIC--RESULT:OK" NO
s" NF-NONE ( -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:OK" NO
s" NF-XROLE ( NUM:cell-count -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:OK" NO
s" NF-INST ( NUM:byte-len -- NUM:numeric-result<NUM:cell-count> ) NUM-NUMERIC--RESULT:OK" NO
s" NF-PAY ( NUM:byte-len -- NUM:numeric-result<NUM:byte-len> ) NUM-NUMERIC--RESULT:NEGATIVE" NO

s" NT-OK ( NUM:byte-len -- nr-twin<NUM:byte-len> ) NUM--TEST-NR--TWIN:OK" YES
s" NT-X1 ( NUM:byte-len -- nr-twin<NUM:byte-len> ) NUM-NUMERIC--RESULT:OK" NO
s" NT-X2 ( NUM:byte-len -- NUM:numeric-result<NUM:byte-len> ) NUM--TEST-NR--TWIN:OK" NO

\ ---- compiled round trip through every generated constructor ------------------
5 RT-TAG# 0 T=                            \ ok is rebuilt as ok ...
0 RT-TAG# 0 T=
T-MAX-N RT-TAG# 0 T=
-1 RT-TAG# E-NUM-NEGATIVE T=           \ ... and negative as negative
1 RT-VALUE# 0 T=                          \ payload magnitudes come back exactly:
T-MAX-ALLOC-CELLS RT-VALUE# 0 T=          \ the largest allocatable count is still ok
0 RT-VALUE# E-NUM-ZERO T=              \ zero is still zero, so nothing was invented
T-MAX-ALLOC-CELLS 1 + RT-VALUE# E-NUM-OVERFLOW T=   \ the overflow edge did not move
-1 RT-VALUE# E-NUM-NEGATIVE T=

;package

T-REPORT
