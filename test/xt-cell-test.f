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

\ ---- STAGE-3 PIN: plain raw variable @ execute still LAUNDERS ----------------
\ dot habu-checker-exec-of-5923c543 owns the RSEXEC T-VAR flip (step 3). Today a
\ raw variable's stored xt @ execute is modeled as a pure ( -- ) quotation, so a
\ laundering word CERTIFIES. This fixture PINS that unchanged behavior; when stage
\ 3 flips the RSEXEC T-VAR branch the L1 candidate must REJECT (its ready red
\ fixture). The typed xt cell above is the SOUND alternative that already
\ fit-checks (L2 rejects a wrong-row execute today).
: XC-REG-RAW ( -- )
   s" variable XCRAW" VERIFY:SOURCE-BUF-IN-SCOPE ;

: XC-SECTION-STAGE3-PIN ( -- )
   XC-REG-RAW
   \ STAGE-3 TARGET (flip pending, dot 5923c543): still certifies = laundering
   s" L1 ( -- ) XCRAW @ execute" CHECK-QUIET-CANDIDATE! -1 T=
   \ contrast: the typed cell fit-checks the row, so a wrong-row execute rejects
   s" L2 ( -- ) HK @ execute" CHECK-QUIET-CANDIDATE! 0 T= ;

: RUN ( -- )
   T-RESET
   XC-SECTION-CHECKER
   XC-SECTION-LIVE
   XC-SECTION-BUFFER
   XC-SECTION-ADMISSIBILITY
   XC-SECTION-STAGE3-PIN
   T-REPORT ;

RUN

;package
