\ cad-num-types-test.f - the CAD-NUM B5.1 boundary + static-rejection matrix
\ (dot habu-implement-cad-num-962bf5d9). Run:
\   bin/hb --load lib/cad-num-types-test.f
\
\ This is the SOLE consumer of lib/cad-num-types.f (the package is unsealed and
\ no production entry loads it). Permanent sealing is dot
\ habu-seal-cad-num-36dbeec6; the MODEL-CAD-V2-PLAN.md B5.5 final-integration dot
\ (ba510e2e) registers these files and wires this test into a scheduled gate
\ suite. Until then this test is intentionally direct-loaded (its gate home) and
\ is not in test/gate-stdlib-cases.f, so suite-coverage-lint has nothing to
\ schedule for it.
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
