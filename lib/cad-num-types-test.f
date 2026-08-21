\ cad-num-types-test.f - the CAD-NUM B5.1 boundary + static-rejection matrix
\ (dot habu-implement-cad-num-962bf5d9). Run:
\   bin/hb --load lib/cad-num-types-test.f
\
\ Focused direct-loaded gate home for the B5.1 contract. Production modules load
\ these types transitively through lib/cad-num-arithmetic.f; this file owns the
\ validator boundary matrix, not library authority. Permanent sealing is dot
\ habu-seal-cad-num-36dbeec6; the MODEL-CAD-V2-PLAN.md B5.5 final-integration dot
\ (ba510e2e) wires this test into a scheduled gate suite. Until then it is not in
\ test/gate-stdlib-cases.f, so no gate slice schedules it.
\
\ numeric-result<a> has no polymorphic eliminator yet (whole-bundle MATCH, dot
\ habu-typestate-result-drop-5ae048a7), so a caller MATCHes the concrete
\ instantiation it holds. Each `*-CODE` word below is that concrete classifier:
\ it maps a role's numeric-result to 0 (ok) or the named E-CADNUM-* refusal code,
\ so the runtime matrix asserts the EXACT refusal, not merely "not ok".

require lib/errors.f
require lib/string.f
require lib/test.f
require test/checker-assert.f
require lib/cad-num-types.f

\ ---- boundary constants (independently mirror the library bound) --------------
$7FFFFFFFFFFFFFFF constant T-MAX-N                 \ largest nonnegative cell
T-MAX-N 1 cells / constant T-MAX-ALLOC-CELLS       \ MAX-N / CELL-BYTES
$4000000000000000 constant T-LARGEST-POW2          \ largest positive power-of-two alignment

\ ---- per-role concrete classifiers: numeric-result<role> -> code (0 = ok) -----
: BL-CODE ( CAD-NUM:numeric-result<CAD-NUM:byte-len> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: IC-CODE ( CAD-NUM:numeric-result<CAD-NUM:item-count> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: CC-CODE ( CAD-NUM:numeric-result<CAD-NUM:cell-count> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: IDX-CODE ( CAD-NUM:numeric-result<CAD-NUM:index> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: BO-CODE ( CAD-NUM:numeric-result<CAD-NUM:byte-off> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: CO-CODE ( CAD-NUM:numeric-result<CAD-NUM:cell-off> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: AL-CODE ( CAD-NUM:numeric-result<CAD-NUM:alignment> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: PD-CODE ( CAD-NUM:numeric-result<CAD-NUM:positive-divisor> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: AB-CODE ( CAD-NUM:numeric-result<CAD-NUM:alloc-byte-len> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: AC-CODE ( CAD-NUM:numeric-result<CAD-NUM:alloc-cell-count> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF drop 0 ENDOF                    negative OF E-CADNUM-NEGATIVE ENDOF
      zero OF E-CADNUM-ZERO ENDOF           overflow OF E-CADNUM-OVERFLOW ENDOF
      underflow OF E-CADNUM-UNDERFLOW ENDOF bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF
      misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;

\ ---- allocation caller path: build a zero-admitting role, then narrow it ------
\ A negative raw cell is rejected by the base validator and never reaches the
\ allocator sink (its ok arm is where AS-ALLOC-* runs).
: ALLOC-BYTES# ( n -- n )
   CAD-NUM:BYTE-LEN
   MATCH CAD-NUM:numeric-result
      ok OF CAD-NUM:AS-ALLOC-BYTE-LEN AB-CODE ENDOF
      negative OF E-CADNUM-NEGATIVE ENDOF   zero OF E-CADNUM-ZERO ENDOF
      overflow OF E-CADNUM-OVERFLOW ENDOF   underflow OF E-CADNUM-UNDERFLOW ENDOF
      bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF  misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;
: ALLOC-CELLS# ( n -- n )
   CAD-NUM:CELL-COUNT
   MATCH CAD-NUM:numeric-result
      ok OF CAD-NUM:AS-ALLOC-CELL-COUNT AC-CODE ENDOF
      negative OF E-CADNUM-NEGATIVE ENDOF   zero OF E-CADNUM-ZERO ENDOF
      overflow OF E-CADNUM-OVERFLOW ENDOF   underflow OF E-CADNUM-UNDERFLOW ENDOF
      bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF  misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;

\ ---- runtime boundary matrix: every role x its applicable boundaries ----------
: RT-ORDINARY ( -- )                         \ zero valid; positive valid; negative rejected
   5 CAD-NUM:BYTE-LEN BL-CODE 0 T=
   0 CAD-NUM:BYTE-LEN BL-CODE 0 T=
   T-MAX-N CAD-NUM:BYTE-LEN BL-CODE 0 T=
   -1 CAD-NUM:BYTE-LEN BL-CODE E-CADNUM-NEGATIVE T=
   5 CAD-NUM:ITEM-COUNT IC-CODE 0 T=
   0 CAD-NUM:ITEM-COUNT IC-CODE 0 T=
   T-MAX-N CAD-NUM:ITEM-COUNT IC-CODE 0 T=
   -1 CAD-NUM:ITEM-COUNT IC-CODE E-CADNUM-NEGATIVE T=
   5 CAD-NUM:CELL-COUNT CC-CODE 0 T=
   0 CAD-NUM:CELL-COUNT CC-CODE 0 T=
   T-MAX-N CAD-NUM:CELL-COUNT CC-CODE 0 T=
   -1 CAD-NUM:CELL-COUNT CC-CODE E-CADNUM-NEGATIVE T=
   5 CAD-NUM:INDEX IDX-CODE 0 T=
   0 CAD-NUM:INDEX IDX-CODE 0 T=
   T-MAX-N CAD-NUM:INDEX IDX-CODE 0 T=
   -1 CAD-NUM:INDEX IDX-CODE E-CADNUM-NEGATIVE T=
   5 CAD-NUM:BYTE-OFF BO-CODE 0 T=
   0 CAD-NUM:BYTE-OFF BO-CODE 0 T=
   T-MAX-N CAD-NUM:BYTE-OFF BO-CODE 0 T=
   -1 CAD-NUM:BYTE-OFF BO-CODE E-CADNUM-NEGATIVE T=
   5 CAD-NUM:CELL-OFF CO-CODE 0 T=
   0 CAD-NUM:CELL-OFF CO-CODE 0 T=
   T-MAX-N CAD-NUM:CELL-OFF CO-CODE 0 T=
   -1 CAD-NUM:CELL-OFF CO-CODE E-CADNUM-NEGATIVE T= ;

: RT-ALIGNMENT ( -- )                        \ positive power of two only; all else bad-alignment
   1 CAD-NUM:ALIGNMENT AL-CODE 0 T=
   2 CAD-NUM:ALIGNMENT AL-CODE 0 T=
   T-LARGEST-POW2 CAD-NUM:ALIGNMENT AL-CODE 0 T=
   0 CAD-NUM:ALIGNMENT AL-CODE E-CADNUM-BAD-ALIGNMENT T=
   -1 CAD-NUM:ALIGNMENT AL-CODE E-CADNUM-BAD-ALIGNMENT T=
   3 CAD-NUM:ALIGNMENT AL-CODE E-CADNUM-BAD-ALIGNMENT T=
   T-MAX-N CAD-NUM:ALIGNMENT AL-CODE E-CADNUM-BAD-ALIGNMENT T= ;

: RT-DIVISOR ( -- )                          \ positive only; zero rejected; negative rejected
   1 CAD-NUM:POSITIVE-DIVISOR PD-CODE 0 T=
   T-MAX-N CAD-NUM:POSITIVE-DIVISOR PD-CODE 0 T=
   0 CAD-NUM:POSITIVE-DIVISOR PD-CODE E-CADNUM-ZERO T=
   -1 CAD-NUM:POSITIVE-DIVISOR PD-CODE E-CADNUM-NEGATIVE T= ;

: RT-ALLOC ( -- )                            \ allocation sinks reject zero (and overflow)
   1 ALLOC-BYTES# 0 T=
   T-MAX-N ALLOC-BYTES# 0 T=
   0 ALLOC-BYTES# E-CADNUM-ZERO T=
   -1 ALLOC-BYTES# E-CADNUM-NEGATIVE T=
   1 ALLOC-CELLS# 0 T=
   T-MAX-ALLOC-CELLS ALLOC-CELLS# 0 T=
   0 ALLOC-CELLS# E-CADNUM-ZERO T=
   T-MAX-ALLOC-CELLS 1 + ALLOC-CELLS# E-CADNUM-OVERFLOW T=
   -1 ALLOC-CELLS# E-CADNUM-NEGATIVE T= ;

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
   s" GOOD-OFF-SWAP ( CAD-NUM:byte-off CAD-NUM:cell-off -- CAD-NUM:cell-off CAD-NUM:byte-off ) swap"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" GOOD-BYTE-LEN ( n -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD-NUM:BYTE-LEN"
      CHECK-QUIET-CANDIDATE! -1 T=
   s" GOOD-AS-ALLOC ( CAD-NUM:cell-count -- CAD-NUM:numeric-result<CAD-NUM:alloc-cell-count> ) CAD-NUM:AS-ALLOC-CELL-COUNT"
      CHECK-QUIET-CANDIDATE! -1 T=
   \ negatives: swapped roles, raw n where a role is required, cross-role sink.
   s" BAD-OFF-SWAP ( CAD-NUM:byte-off CAD-NUM:cell-off -- CAD-NUM:byte-off CAD-NUM:cell-off ) swap"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-RAW-OFF ( n -- CAD-NUM:byte-off )"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-RAW-ALLOC ( n -- CAD-NUM:numeric-result<CAD-NUM:alloc-cell-count> ) CAD-NUM:AS-ALLOC-CELL-COUNT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-ROLE-ALLOC ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:alloc-cell-count> ) CAD-NUM:AS-ALLOC-CELL-COUNT"
      CHECK-QUIET-CANDIDATE! 0 T=
   s" BAD-ROLE-VALIDATOR ( CAD-NUM:byte-off -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD-NUM:BYTE-LEN"
      CHECK-QUIET-CANDIDATE! 0 T=
   T-REPORT ;
STAT

\ ---- what the unified ENUM declaration registered and generated ---------------
\ numeric-result moved off the retired legacy sum opener onto the unified ENUM
\ front end in full mode. Nothing above this line changed, and no consumer changed,
\ which is precisely why this section exists: the two declaration forms are
\ MATCH-identical and width-identical, so the sections above cannot see the
\ difference and therefore cannot see a REGRESSION either. What follows reads the
\ live type registry and asks the checker directly.
\
\ Three properties are pinned here that nothing else in the repository observes
\ for this family. First, the recorded shape: kind, arity, width, visibility, and
\ the case ORDER, which fixes the tags - MATCH dispatches on case name and is
\ blind to a reorder. Second, the generated constructor spellings and their exact
\ checked effects, since every consumer calls them by name from another package.
\ Third, the named payload FIELD the migration introduced, which is the one thing
\ the move actually changes in the registry: under the old positional payload the
\ ok arm owned NO field row at all.
T-RESET

using TFAM

package CAD-NUM-TEST
private

\ CHECK-QUIET-CANDIDATE! answers -1 accepted, 0 refused, 1 unresolvable, and all
\ three verdicts are load-bearing below. YES demands -1, so it can only pass if
\ the checker resolved exactly the name written in the candidate. NO demands 0,
\ which is reachable only after resolving the name and then refusing the types.
\ UNRES demands 1 and is the calibration: it is spent on names that must NOT
\ exist, so a YES line cannot be quietly passing for a family that was renamed.
: YES   ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO    ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;
: UNRES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  1 T= ;

\ A family is identified to REFLECT by its tail AND the constructor package its
\ variants carry, because a bare tail is not unique across packages. FAMS answers
\ how many registered families match, so the 1 pinned below is the uniqueness
\ assertion that makes every other pin on that identity trustworthy.
: NR$ ( -- ptr u8 n ptr u8 n )   s" numeric-result" s" CAD--NUM-NUMERIC--RESULT" ;
: TW$ ( -- ptr u8 n ptr u8 n )   s" nr-twin" s" CAD--NUM--TEST-NR--TWIN" ;

\ ---- compiled round trip: construct through all seven generated constructors --
\ The checker pins further down prove the constructors resolve and type-check as
\ candidate text. This word proves they work in COMPILED code: it takes a real
\ numeric-result apart and puts it back together arm by arm, so all seven
\ generated constructors are compiled, and the ok arm binds its payload through a
\ typed local before handing it back to the ok constructor. The seven-arm shape is
\ repeated rather than factored because a polymorphic eliminator over the whole
\ bundle is not expressible yet (dot habu-typestate-result-drop-5ae048a7), which
\ is the same reason the ten classifiers at the top of this file repeat it.
: REBUILD-CC ( CAD-NUM:numeric-result<CAD-NUM:cell-count> -- CAD-NUM:numeric-result<CAD-NUM:cell-count> )
   MATCH CAD-NUM:numeric-result
      ok OF {: got:CAD-NUM:cell-count :} got CAD--NUM-NUMERIC--RESULT:OK ENDOF
      negative OF CAD--NUM-NUMERIC--RESULT:NEGATIVE ENDOF
      zero OF CAD--NUM-NUMERIC--RESULT:ZERO ENDOF
      overflow OF CAD--NUM-NUMERIC--RESULT:OVERFLOW ENDOF
      underflow OF CAD--NUM-NUMERIC--RESULT:UNDERFLOW ENDOF
      bad-alignment OF CAD--NUM-NUMERIC--RESULT:BAD-ALIGNMENT ENDOF
      misaligned OF CAD--NUM-NUMERIC--RESULT:MISALIGNED ENDOF
   ;MATCH ;

\ The payload's VALUE, not just its presence: the recovered cell count is handed
\ straight to the production allocator narrowing, which answers `zero` for 0, ok
\ up to MAX-ALLOC-CELLS and `overflow` past it. A round trip that dropped or
\ zeroed the payload would report zero for every input, and one that truncated it
\ would move the overflow boundary.
: ALLOC-CC# ( CAD-NUM:numeric-result<CAD-NUM:cell-count> -- n )
   MATCH CAD-NUM:numeric-result
      ok OF {: got:CAD-NUM:cell-count :} got CAD-NUM:AS-ALLOC-CELL-COUNT AC-CODE ENDOF
      negative OF E-CADNUM-NEGATIVE ENDOF   zero OF E-CADNUM-ZERO ENDOF
      overflow OF E-CADNUM-OVERFLOW ENDOF   underflow OF E-CADNUM-UNDERFLOW ENDOF
      bad-alignment OF E-CADNUM-BAD-ALIGNMENT ENDOF  misaligned OF E-CADNUM-MISALIGNED ENDOF
   ;MATCH ;

: RT-TAG# ( n -- n )      \ tag survives the round trip (0 = ok, else refusal code)
   CAD-NUM:CELL-COUNT REBUILD-CC CC-CODE ;
: RT-VALUE# ( n -- n )    \ and so does the exact payload magnitude
   CAD-NUM:CELL-COUNT REBUILD-CC ALLOC-CC# ;

public

\ nr-twin is CAD-NUM:numeric-result's SHAPE under another name: same arity, the
\ same seven cases in the same order, the same named payload field. It exists only
\ so the negatives below can prove result identity is NOMINAL - two identically
\ shaped families never unify, in either direction. It has to be public, because a
\ private family publishes no constructors and the positive control builds through
\ the twin's own ok, so neither negative could pass by being unresolvable instead
\ of ill-typed. Its generated constructor package CAD--NUM--TEST-NR--TWIN is 23
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

\ ---- live registry: numeric-result --------------------------------------------
NR$ REFLECT:FAMS 1 T=
NR$ REFLECT:KIND TK-SUM T=          \ a family with a payload is a general sum ...
NR$ REFLECT:KIND TK-ENUM = 0 T=     \ ... never recorded as a payloadless enum
NR$ REFLECT:ARITY 1 T=              \ the one type parameter the validated role rides in
NR$ REFLECT:WIDTH 2 T=              \ tag + one payload cell
NR$ REFLECT:VIS 1 T=
NR$ REFLECT:VARS 7 T=
NR$ 0 REFLECT:ARM$ s" ok" T$=       \ case order is what fixes the tags
NR$ 1 REFLECT:ARM$ s" negative" T$=
NR$ 2 REFLECT:ARM$ s" zero" T$=
NR$ 3 REFLECT:ARM$ s" overflow" T$=
NR$ 4 REFLECT:ARM$ s" underflow" T$=
NR$ 5 REFLECT:ARM$ s" bad-alignment" T$=
NR$ 6 REFLECT:ARM$ s" misaligned" T$=
NR$ 7 REFLECT:ARM$ s" <missing>" T$=   \ and there is no eighth case to reorder into
NR$ 0 REFLECT:ARM-CTOR$ s" CAD--NUM-NUMERIC--RESULT" T$=
NR$ 6 REFLECT:ARM-CTOR$ s" CAD--NUM-NUMERIC--RESULT" T$=

\ The named payload field. This is the registry row the migration adds: the legacy
\ positional payload registered NO field row, so `1` here fails on the previous
\ declaration and `0` is the slot the ok arm binds first.
NR$ 0 REFLECT:ARM-FLDS 1 T=
NR$ 0 s" role" REFLECT:ARM-SLOT 0 T=
NR$ 0 s" role" REFLECT:ARM-CELLS 1 T=
NR$ 0 s" value" REFLECT:ARM-SLOT -1 T=   \ and it is spelled `role`, nothing else
NR$ 1 REFLECT:ARM-FLDS 0 T=              \ the reject arms carry no payload at all
NR$ 6 REFLECT:ARM-FLDS 0 T=
NR$ 7 REFLECT:ARM-FLDS -1 T=             \ a case that does not exist answers the sentinel

\ ---- generated constructors: exact spelling + exact effect --------------------
s" NC-OK ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD--NUM-NUMERIC--RESULT:OK" YES
s" NC-NEG ( -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD--NUM-NUMERIC--RESULT:NEGATIVE" YES
s" NC-ZERO ( -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD--NUM-NUMERIC--RESULT:ZERO" YES
s" NC-OVER ( -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD--NUM-NUMERIC--RESULT:OVERFLOW" YES
s" NC-UNDER ( -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD--NUM-NUMERIC--RESULT:UNDERFLOW" YES
s" NC-BADAL ( -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD--NUM-NUMERIC--RESULT:BAD-ALIGNMENT" YES
s" NC-MISAL ( -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD--NUM-NUMERIC--RESULT:MISALIGNED" YES
\ the payload parameter really is a parameter: a second, unrelated role instantiates it
s" NC-OK-AL ( CAD-NUM:alignment -- CAD-NUM:numeric-result<CAD-NUM:alignment> ) CAD--NUM-NUMERIC--RESULT:OK" YES

\ Calibration for the eight YES lines above: these three spellings do not exist,
\ and an unresolvable name answers 1. If a renamed family made the real
\ constructors unresolvable too, they would answer 1 as well and every YES would
\ fail - which is what makes -1 mean "the checker resolved exactly this name".
s" NC-X-SEP ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD-NUM-NUMERIC--RESULT:OK" UNRES
s" NC-X-TAIL ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD--NUM-NUMERIC-RESULT:OK" UNRES
s" NC-X-ARM ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD--NUM-NUMERIC--RESULT:OKAY" UNRES

\ ---- forge negatives on the ok payload slot -----------------------------------
\ NF-XROLE and NF-INST are the sharp ones. Every role in this package is one cell
\ wide, so nothing but the nominal family separates a cell count from a byte
\ length; the ok constructor must refuse the wrong role in the payload and must
\ refuse to hand back a result instantiated at a different role than it consumed.
s" NF-RAW ( n -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD--NUM-NUMERIC--RESULT:OK" NO
s" NF-BARE ( CAD-NUM:byte-len -- n ) CAD--NUM-NUMERIC--RESULT:OK" NO
s" NF-NONE ( -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD--NUM-NUMERIC--RESULT:OK" NO
s" NF-XROLE ( CAD-NUM:cell-count -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD--NUM-NUMERIC--RESULT:OK" NO
s" NF-INST ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:cell-count> ) CAD--NUM-NUMERIC--RESULT:OK" NO
s" NF-PAY ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD--NUM-NUMERIC--RESULT:NEGATIVE" NO

\ ---- live registry + non-unification for the shape twin ----------------------
TW$ REFLECT:FAMS 1 T=
TW$ REFLECT:KIND TK-SUM T=
TW$ REFLECT:ARITY 1 T=
TW$ REFLECT:WIDTH 2 T=
TW$ REFLECT:VIS 1 T=
TW$ REFLECT:VARS 7 T=
TW$ 0 REFLECT:ARM-CTOR$ s" CAD--NUM--TEST-NR--TWIN" T$=   \ readable spelling, not the SHA form
TW$ 0 REFLECT:ARM-FLDS 1 T=
TW$ 0 s" role" REFLECT:ARM-SLOT 0 T=
s" NT-OK ( CAD-NUM:byte-len -- nr-twin<CAD-NUM:byte-len> ) CAD--NUM--TEST-NR--TWIN:OK" YES
s" NT-X1 ( CAD-NUM:byte-len -- nr-twin<CAD-NUM:byte-len> ) CAD--NUM-NUMERIC--RESULT:OK" NO
s" NT-X2 ( CAD-NUM:byte-len -- CAD-NUM:numeric-result<CAD-NUM:byte-len> ) CAD--NUM--TEST-NR--TWIN:OK" NO

\ ---- compiled round trip through every generated constructor ------------------
5 RT-TAG# 0 T=                            \ ok is rebuilt as ok ...
0 RT-TAG# 0 T=
T-MAX-N RT-TAG# 0 T=
-1 RT-TAG# E-CADNUM-NEGATIVE T=           \ ... and negative as negative
1 RT-VALUE# 0 T=                          \ payload magnitudes come back exactly:
T-MAX-ALLOC-CELLS RT-VALUE# 0 T=          \ the largest allocatable count is still ok
0 RT-VALUE# E-CADNUM-ZERO T=              \ zero is still zero, so nothing was invented
T-MAX-ALLOC-CELLS 1 + RT-VALUE# E-CADNUM-OVERFLOW T=   \ the overflow edge did not move
-1 RT-VALUE# E-CADNUM-NEGATIVE T=

;package

T-REPORT

;using
