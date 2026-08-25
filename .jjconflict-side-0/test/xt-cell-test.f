\ xt-cell-test.f - xt<effect> storage cells (dot habu-typed-xt-storage-ddad4af8).
\ Run: bin/hb --load lib/errors.f lib/string.f lib/test.f lib/fs.f
\   src/habu/verify-source.f test/checker-assert.f test/xt-cell-test.f
\
\ Pins the xt<effect> storage-cell capability: `TYPED-VARIABLE NAME [ in -- out ]`
\ declares a persistent monomorphic code cell whose @ recovers xt<E> as a T-QUOT.
\ A typed store (`[: W ;] NAME !`) fit-checks W's certified effect against E and
\ rejects on mismatch; fetch+execute (`NAME @ execute`) fit-checks the row against
\ E exactly like executing a literal xt<E> (reuses RSEXEC). Persistence beats the
\ E-INST raw-var freshening because the accessor's declared ( -- ptr [ in -- out ] )
\ bakes a CONCRETE T-QUOT into its usig scheme, freshened alpha-equivalently per
\ occurrence — never the erased raw address var the parent RCA describes.

require lib/errors.f
require lib/string.f
require lib/test.f
require lib/fs.f
require src/habu/verify-source.f
require test/checker-assert.f

package XT-CELL-TEST

7121 constant E-STORAGE

\ words under test: SP / SP2 fit ( n -- n ); DBL ( n -- n n ) misfits the cell
: SP ( n -- n ) 1 + ;
: SP2 ( n -- n ) 2 + ;
: DBL ( n -- n n ) dup ;

\ the xt<( n -- n )> storage cell, plus an indexed xt<( n -- n )> buffer
TYPED-VARIABLE HK [ n -- n ]
2 TYPED-BUFFER HKB [ n -- n ]

\ define-time reject verdicts (INCLUDE-EVALUATE under catch)
variable XC-A  variable XC-U
: XC-RUN ( -- ) XC-A @ XC-U @ INCLUDE-EVALUATE ;
: XC-EVAL ( ptr u8 n -- n ) XC-U ! XC-A ! [: XC-RUN ;] catch ;

\ ---- checker seam: store / fetch / execute fit-checks ------------------------
: XC-SECTION-CHECKER ( -- )
   \ typed store of a matching word certifies (fit-check on the certified effect)
   s" S1 ( -- ) [: SP ;] HK !" CHECK-QUIET-CANDIDATE! -1 T=
   \ wrong-effect store rejects: DBL is ( n -- n n )
   s" S2 ( -- ) [: DBL ;] HK !" CHECK-QUIET-CANDIDATE! 0 T=
   \ fetch + execute with the right row certifies
   s" F1 ( n -- n ) HK @ execute" CHECK-QUIET-CANDIDATE! -1 T=
   \ wrong-row execute rejects: E needs an n input, none on ( -- )
   s" F2 ( -- ) HK @ execute" CHECK-QUIET-CANDIDATE! 0 T=
   \ row polymorphism: surplus depth under the arg is preserved
   s" F3 ( n n -- n n ) HK @ execute" CHECK-QUIET-CANDIDATE! -1 T=
   \ the accessor value itself is a pointer to the declared quotation cell
   s" F4 ( -- ptr [ n -- n ] ) HK" CHECK-QUIET-CANDIDATE! -1 T= ;

\ ---- live round-trip through the cell ---------------------------------------
: HK-STORE ( -- ) [: SP ;] HK ! ;
: HK-STORE2 ( -- ) [: SP2 ;] HK ! ;
: HK-RUN ( n -- n ) HK @ execute ;

: XC-SECTION-LIVE ( -- )
   HK-STORE
   5 HK-RUN 6 T=
   9 HK-RUN 10 T=
   \ re-store another matching word: the same cell now runs SP2
   HK-STORE2
   5 HK-RUN 7 T=
   9 HK-RUN 11 T= ;

\ ---- TYPED-BUFFER of xt<E>: the buffer machinery generalizes trivially -------
: HKB-STORE ( -- ) [: SP ;] 0 HKB !  [: SP2 ;] 1 HKB ! ;
: HKB-RUN ( n n -- n ) HKB @ execute ;

: XC-SECTION-BUFFER ( -- )
   \ indexed store fit-checks; wrong-effect rejects; fetch+execute fit-checks
   s" B1 ( -- ) [: SP ;] 0 HKB !" CHECK-QUIET-CANDIDATE! -1 T=
   s" B2 ( -- ) [: DBL ;] 0 HKB !" CHECK-QUIET-CANDIDATE! 0 T=
   s" B3 ( n -- n ) 0 HKB @ execute" CHECK-QUIET-CANDIDATE! -1 T=
   s" B4 ( -- ) 0 HKB @ execute" CHECK-QUIET-CANDIDATE! 0 T=
   \ distinct indices hold independent words
   HKB-STORE
   5 0 HKB-RUN 6 T=
   5 1 HKB-RUN 7 T= ;

\ ---- tick store: `['] W HK !` retypes the tick so the store fit-checks W's -----
\ certified effect against the cell (dot habu-typed-xt-cells-08e1dc2c). Before the
\ BTICK lookahead recognised a typed-xt-cell accessor as an xt sink, the tick
\ erased to a plain n and even a MATCHING store rejected on `actual: n`; T1
\ certifying is the proof the retype now fires (a plain-n erasure would reject it),
\ and T1 vs T2 proves the store discriminates on the EFFECT. Candidate-path only,
\ like every direct-tick fixture (the reconstructed --load body drops the target).
: XC-SECTION-TICK-STORE ( -- )
   \ matching-effect tick store certifies: the retype fired (else this rejects)
   s" T1 ( -- ) ['] SP HK !" CHECK-QUIET-CANDIDATE! -1 T=
   \ mismatched-effect tick store rejects on the effect (DBL is ( n -- n n ))
   s" T2 ( -- ) ['] DBL HK !" CHECK-QUIET-CANDIDATE! 0 T=
   \ a non-xt (plain number) store still rejects
   s" T3 ( -- ) 42 HK !" CHECK-QUIET-CANDIDATE! 0 T=
   \ a tick before a scalar sink stays a plain n: the retype is targeted, not global
   s" T4 ( n -- n ) ['] SP +" CHECK-QUIET-CANDIDATE! -1 T=
   \ a buffer slot store stays out of scope (the index splits the tick from the
   \ accessor); [: SP ;] 0 HKB ! is the supported quotation-store buffer surface
   s" T5 ( -- ) ['] SP 0 HKB !" CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- define-time admissibility: malformed quotation types still reject -------
: XC-SECTION-ADMISSIBILITY ( -- )
   \ missing -- : not a well-formed effect
   s" TYPED-VARIABLE BADQ1 [ n n ]" XC-EVAL E-STORAGE T=
   \ unterminated quotation (no closer)
   s" TYPED-VARIABLE BADQ2 [ n -- n" XC-EVAL E-STORAGE T=
   \ bogus pointee type inside the quotation body
   s" TYPED-VARIABLE BADQ3 [ n -- zzz ]" XC-EVAL E-STORAGE T=
   \ none of the rejected names ever reached the dictionary
   s" BADQ1" 0 search-wl 0= TTRUE
   s" BADQ2" 0 search-wl 0= TTRUE
   s" BADQ3" 0 search-wl 0= TTRUE ;

\ ---- STAGE-3: plain raw variable @ execute now REJECTS (E-EXEC-OPAQUE-XT) ------
\ dot habu-checker-exec-of-5923c543 flipped the RSEXEC T-VAR branch: executing an
\ xt fetched from untyped memory (a raw variable or create cell) is rejected
\ because its true stack effect is not statically known. The XCRAW launder that
\ used to CERTIFY now REJECTS; the typed xt cell above is the SOUND alternative
\ that fit-checks the row. XC-CODE< runs a candidate with JSON diagnostics into a
\ buffer and asserts the reject carries the named code.
create XC-DBUF 8192 allot
: XC-CODE< ( ptr u8 n -- )   \ run a candidate with JSON diags captured; assert reject (verdict 0)
   XC-DBUF 8192 DIAG-BUFFER!  0 0= DIAG-JSON!
   CHECK-CANDIDATE! 0 T= ;
: XC-CODE? ( ptr u8 n -- )   \ assert the captured diagnostic names this code
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;
: XC-CODE-END ( -- ) 0 0= 0= DIAG-JSON! DIAG-BUFFER-OFF ;

: XC-REG-RAW ( -- )
   s" variable XCRAW" VERIFY:SOURCE-BUF-IN-SCOPE ;

: XC-SECTION-STAGE3 ( -- )
   XC-REG-RAW
   \ the raw-variable launder now rejects, named E-EXEC-OPAQUE-XT (was -1 before the flip)
   s" L1 ( -- ) XCRAW @ execute" XC-CODE<
   s\" \"code\":\"E-EXEC-OPAQUE-XT\"" XC-CODE?
   XC-CODE-END
   \ contrast: the typed cell fit-checks the row, so a wrong-row execute rejects
   s" L2 ( -- ) HK @ execute" CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- STAGE-3 CATCH: catch of a raw-variable xt now REJECTS too --------------
\ dot habu-flip-rscatch-opaque-5da02bd5 flipped the RSCATCH T-VAR branch: catching
\ an xt fetched from untyped memory is the same launder as executing it, so it is
\ rejected with the same E-EXEC-OPAQUE-XT code, but the reason names 'catch'. The
\ typed catch routes (direct tick, quotation parameter, typed xt cell, defer) fit-
\ check the caught row and stay green.
: XC-REG-CATCH ( -- )
   s" defer XC-DF ( n -- n )" VERIFY:SOURCE-BUF-IN-SCOPE ;

: XC-SECTION-CATCH ( -- )
   XC-REG-CATCH
   \ the raw-variable catch launder rejects, named E-EXEC-OPAQUE-XT (catch sibling of L1)
   s" LC ( -- n ) XCRAW @ catch" XC-CODE<
   s\" \"code\":\"E-EXEC-OPAQUE-XT\"" XC-CODE?
   s" Catch an xt" XC-CODE?               \ the catch-specific reason fired, not the execute one
   XC-CODE-END
   \ typed catch routes stay green: direct tick / quotation parameter / typed cell / defer
   s" GC1 ( n -- n n ) ['] SP catch" CHECK-QUIET-CANDIDATE! -1 T=
   s" GC2 ( n [ n -- n ] -- n n ) catch" CHECK-QUIET-CANDIDATE! -1 T=
   s" GC3 ( n -- n n ) HK @ catch" CHECK-QUIET-CANDIDATE! -1 T=
   s" GC4 ( n -- n n ) ['] XC-DF catch" CHECK-QUIET-CANDIDATE! -1 T= ;

: RUN ( -- )
   T-RESET
   XC-SECTION-CHECKER
   XC-SECTION-LIVE
   XC-SECTION-BUFFER
   XC-SECTION-TICK-STORE
   XC-SECTION-ADMISSIBILITY
   XC-SECTION-STAGE3
   XC-SECTION-CATCH
   T-REPORT ;

RUN

;package
