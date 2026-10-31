\ xt-cell-test.f - xt<effect> storage cells.
\ `TYPED-VARIABLE NAME [ in -- out ]` declares a code cell whose @ recovers
\ xt<E> as a T-QUOT: a typed store fit-checks W's certified effect against E,
\ and `NAME @ execute` fit-checks the row as a literal xt<E> does (RSEXEC). The
\ accessor's declared effect bakes a concrete T-QUOT into its usig scheme,
\ freshened per occurrence, so E-INST raw-var freshening cannot erase it.

\ Run: bin/hb --load src/lib/errors.f src/lib/string.f src/lib/test.f src/lib/fs.f
\   test/xt-cell-test.f

require src/lib/errors.f
require src/lib/string.f
require src/lib/test.f
require src/lib/fs.f
require src/lib/test/subject.f

package XT-CELL-TEST

\ words under test: SP / SP2 fit ( n -- n ); DBL ( n -- n n ) misfits the cell
: SP ( n -- n ) 1 + ;
: SP2 ( n -- n ) 2 + ;
: DBL ( n -- n n ) dup ;

\ the xt<( n -- n )> storage cell, plus an indexed xt<( n -- n )> buffer
TYPED-VARIABLE HK [ n -- n ]
2 TYPED-BUFFER HKB [ n -- n ]
\ a cell whose effect takes a quotation: the spelling runs to its outer closer
TYPED-VARIABLE HKN [ [ n -- n ] -- n ]

\ define-time reject verdicts (INCLUDE-EVALUATE under catch)
TYPED-VARIABLE XC-A ptr u8  variable XC-U
: XC-RUN ( -- ) XC-A @ XC-U @ INCLUDE-EVALUATE ;
: XC-EVAL ( ptr u8 n -- n ) XC-U ! XC-A ! [: XC-RUN ;] catch ;

\ ---- checker seam: store / fetch / execute fit-checks ------------------------
: XC-SECTION-CHECKER ( -- )
   s" S1 ( -- ) [: SP ;] HK !" CHECK-QUIET-CANDIDATE! -1 T=
   \ wrong-effect store rejects: DBL is ( n -- n n )
   s" S2 ( -- ) [: DBL ;] HK !" CHECK-QUIET-CANDIDATE! 0 T=
   s" F1 ( n -- n ) HK @ execute" CHECK-QUIET-CANDIDATE! -1 T=
   \ wrong-row execute rejects: E needs an n input, none on ( -- )
   s" F2 ( -- ) HK @ execute" CHECK-QUIET-CANDIDATE! 0 T=
   \ row polymorphism: surplus depth under the arg is preserved
   s" F3 ( n n -- n n ) HK @ execute" CHECK-QUIET-CANDIDATE! -1 T=
   s" F4 ( -- ptr [ n -- n ] ) HK" CHECK-QUIET-CANDIDATE! -1 T= ;

\ ---- live round-trip through the cell ---------------------------------------
: HK-STORE ( -- ) [: SP ;] HK ! ;
: HK-STORE2 ( -- ) [: SP2 ;] HK ! ;
: HK-RUN ( n -- n ) HK @ execute ;

: XC-SECTION-LIVE ( -- )
   HK-STORE
   5 HK-RUN 6 T=
   9 HK-RUN 10 T=
   HK-STORE2
   5 HK-RUN 7 T=
   9 HK-RUN 11 T= ;

\ ---- a nested quotation effect round-trips the same way ---------------------
: APPLY3 ( [ n -- n ] -- n ) 3 swap execute ;
: HKN-RUN ( -- n ) [: APPLY3 ;] HKN !  [: SP ;] HKN @ execute ;

: XC-SECTION-NESTED ( -- )
   HKN-RUN 4 T= ;

\ ---- TYPED-BUFFER of xt<E>: the buffer machinery generalizes trivially -------
: HKB-STORE ( -- ) [: SP ;] 0 HKB !  [: SP2 ;] 1 HKB ! ;
: HKB-RUN ( n n -- n ) HKB @ execute ;

: XC-SECTION-BUFFER ( -- )
   s" B1 ( -- ) [: SP ;] 0 HKB !" CHECK-QUIET-CANDIDATE! -1 T=
   s" B2 ( -- ) [: DBL ;] 0 HKB !" CHECK-QUIET-CANDIDATE! 0 T=
   s" B3 ( n -- n ) 0 HKB @ execute" CHECK-QUIET-CANDIDATE! -1 T=
   s" B4 ( -- ) 0 HKB @ execute" CHECK-QUIET-CANDIDATE! 0 T=
   HKB-STORE
   5 0 HKB-RUN 6 T=
   5 1 HKB-RUN 7 T= ;

\ a family whose field holds whatever its argument names
STRUCTURE xbox<a> FIELD item a ;STRUCTURE

\ ---- tick store: `['] W HK !` retypes the tick so the store fit-checks W's -----
\ certified effect against the cell. BTICK-TOK types every tick of a word the
\ checker knows, so T1 certifying proves the retype fires (a plain-n erasure
\ would reject it), and T1 against T2 proves the store discriminates on the
\ effect. Candidate-path only, like every direct-tick fixture: the reconstructed
\ --load body drops the target.
: XC-SECTION-TICK-STORE ( -- )
   s" T1 ( -- ) ['] SP HK !" CHECK-QUIET-CANDIDATE! -1 T=
   s" T2 ( -- ) ['] DBL HK !" CHECK-QUIET-CANDIDATE! 0 T=
   s" T3 ( -- ) 42 HK !" CHECK-QUIET-CANDIDATE! 0 T=
   \ a tick before a scalar sink REJECTS: the quotation cannot unify with +'s n
   s" T4 ( n -- n ) ['] SP +" CHECK-QUIET-CANDIDATE! 0 T=
   \ a buffer slot store fit-checks on the effect too: the index between the
   \ tick and the accessor does not hide the sink
   s" T5 ( -- ) ['] SP 0 HKB !" CHECK-QUIET-CANDIDATE! -1 T=
   s" T6 ( -- ) ['] DBL 0 HKB !" CHECK-QUIET-CANDIDATE! 0 T=
   s" T7 ( -- ) 42 0 HKB !" CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- define-time admissibility: malformed quotation types still reject -------
\ The definer reads a quotation type to its closer or to the end of its line,
\ and what it read is the checker's to refuse.
: XC-SECTION-ADMISSIBILITY ( -- )
   \ missing -- : not a well-formed effect
   s" TYPED-VARIABLE BADQ1 [ n n ]" XC-EVAL CHECKER-REJECT-RC T=
   \ unterminated quotation: the input ends before its closer
   s" TYPED-VARIABLE BADQ2 [ n -- n" XC-EVAL CHECKER-REJECT-RC T=
   \ bogus pointee type inside the quotation body
   s" TYPED-VARIABLE BADQ3 [ n -- zzz ]" XC-EVAL CHECKER-REJECT-RC T=
   \ none of the rejected names ever reached the dictionary
   s" BADQ1" 0 search-wl 0= TTRUE
   s" BADQ2" 0 search-wl 0= TTRUE
   s" BADQ3" 0 search-wl 0= TTRUE ;

\ ---- a plain raw variable's @ execute REJECTS (E-EXEC-OPAQUE-XT) --------------
\ Executing an xt fetched from untyped memory (a raw variable or create cell) is
\ refused at RSEXEC's T-VAR branch: its stack effect is not statically known.
\ The typed xt cell above is the sound alternative.
create XC-DBUF 8192 allot
: XC-CODE< ( ptr u8 n -- )   \ run a candidate with JSON diags captured; assert reject (verdict 0)
   XC-DBUF 8192 DIAG-BUFFER!  0 0= DIAG-JSON!
   CHECK-CANDIDATE! 0 T= ;
: XC-CODE? ( ptr u8 n -- )   \ assert the captured diagnostic names this code
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;
: XC-CODE-END ( -- ) 0 0= 0= DIAG-JSON! DIAG-BUFFER-OFF ;

\ The raw cell and the defer below are compiled here, because a candidate binds
\ the record the compiler binds: a name only the source pre-scan registered has
\ no engine record, and a live candidate cannot name it.
variable XCRAW

: XC-SECTION-STAGE3 ( -- )
   \ the raw-variable launder rejects, named E-EXEC-OPAQUE-XT
   s" L1 ( -- ) XCRAW @ execute" XC-CODE<
   s\" \"code\":\"E-EXEC-OPAQUE-XT\"" XC-CODE?
   XC-CODE-END
   \ contrast: the typed cell fit-checks the row, so a wrong-row execute rejects
   s" L2 ( -- ) HK @ execute" CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- catch of a raw-variable xt REJECTS too ----------------------------------
\ RSCATCH's T-VAR branch: catching an xt fetched from untyped memory is the same
\ launder, refused with E-EXEC-OPAQUE-XT and a reason naming 'catch'. The typed
\ catch routes (direct tick, quotation parameter, typed xt cell, defer)
\ fit-check the caught row.
defer XC-DF ( n -- n )

: XC-SECTION-CATCH ( -- )
   \ the raw-variable catch launder rejects, named E-EXEC-OPAQUE-XT (catch sibling of L1)
   s" LC ( -- n ) XCRAW @ catch" XC-CODE<
   s\" \"code\":\"E-EXEC-OPAQUE-XT\"" XC-CODE?
   s" Catch an xt" XC-CODE?               \ the catch-specific reason fired, not the execute one
   XC-CODE-END
   s" GC1 ( n -- n n ) ['] SP catch" CHECK-QUIET-CANDIDATE! -1 T=
   s" GC2 ( n [ n -- n ] -- n n ) catch" CHECK-QUIET-CANDIDATE! -1 T=
   s" GC3 ( n -- n n ) HK @ catch" CHECK-QUIET-CANDIDATE! -1 T=
   s" GC4 ( n -- n n ) ['] XC-DF catch" CHECK-QUIET-CANDIDATE! -1 T= ;

\ ---- the `xt` type: the token `'` pushes for a word with no stack effect -------
\ XE declares it. A cast may take it but never yields one, and no storage holds
\ one, directly or in a field; a quotation row may name it.
: XC-EVAL< ( ptr u8 n -- )   \ evaluate with JSON diags captured; assert reject
   XC-DBUF 8192 DIAG-BUFFER!  0 0= DIAG-JSON!
   XC-EVAL CHECKER-REJECT-RC T= ;
: XC-STORE-XT? ( -- )   \ the captured refusal is the stored-xt one
   s\" \"code\":\"E-BAD-STORAGE\"" XC-CODE?
   s" xt in a stored type" XC-CODE? ;

: XC-SECTION-XT-TYPE ( -- )
   s" : XE ( -- xt ) ['] type ;" XC-EVAL 0 T=
   s" XR ( -- ) XE execute" XC-CODE<
   s\" \"code\":\"E-EXEC-OPAQUE-XT\"" XC-CODE?
   XC-CODE-END
   s" XS ( -- ) XE XCRAW !" XC-CODE<
   s\" \"code\":\"E-RAW-CELL-PTR\"" XC-CODE?
   XC-CODE-END
   s" CAST: >XT ( n -- xt )" XC-EVAL E-CAST-CLASS T=
   s" public" XC-EVAL 0 T=
   s" CAST: XT>N ( xt -- n )" XC-EVAL 0 T=
   s" : XN ( -- n ) ['] type XT>N ;" XC-EVAL 0 T=
   s" CAST: XT>Q ( xt -- [ n -- n ] )" XC-EVAL E-CAST-MINT T=
   s" private" XC-EVAL 0 T=
   s" TYPED-VARIABLE XV xt" XC-EVAL< XC-STORE-XT? XC-CODE-END
   s" TYPED-VARIABLE XW xbox<xt>" XC-EVAL< XC-STORE-XT? XC-CODE-END
   s" TYPED-VARIABLE XQ [ xt -- ]" XC-EVAL 0 T=
   \ No user type takes the name: an effect would read it as this token. The
   \ value-record refusal exits the process, so it runs as a subject, before the
   \ family whose admission would refuse it as a duplicate.
   s" VALUE-RECORD xt value n END-VALUE-RECORD"
   70 s" bad or duplicate value-record type" SUBJECT:REFUSED? TTRUE
   s" STRUCTURE xt FIELD item n ;STRUCTURE" XC-EVAL TYPE-DECL:E-TDECL-NAME T= ;

: RUN ( -- )
   T-RESET
   XC-SECTION-CHECKER
   XC-SECTION-LIVE
   XC-SECTION-NESTED
   XC-SECTION-BUFFER
   XC-SECTION-TICK-STORE
   XC-SECTION-ADMISSIBILITY
   XC-SECTION-STAGE3
   XC-SECTION-CATCH
   XC-SECTION-XT-TYPE
   T-REPORT ;

RUN

;package
