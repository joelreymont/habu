\ snap-test.f - focused tests for lib/test/snap.f stack-snapshot assertions.
\ Run: bin/hb --load lib/test.f lib/test/snap-test.f

require lib/test.f
require test/checker-assert.f

: SNT-PAIR ( -- n n )
   3 4 ;

: SNT-COUNT-NEG ( -- )
   -1 TS-EXPECT-N! ;

: SNT-COUNT-HIGH ( -- )
   TS-CAP 1+ TS-ACTUAL-N! ;

\ typed SNAP= comparator cases live in checked words (a quotation is compile-only,
\ and calling from a checked word is what verifies the two branch shapes match).
: SNT-EQ-PASS ( -- )
   [: ;] [: ;] SNAP=
   [: 1 2 + ;] [: 3 ;] SNAP=
   [: SNT-PAIR ;] [: 3 4 ;] SNAP=
   [: -5 0 7 ;] [: -5 0 7 ;] SNAP= ;

: SNT-EQ-FAIL ( -- )
   [: 1 2 + ;] [: 4 ;] SNAP= ;

: SNT-EQ-REJECTS ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! 0 T= ;

\ a labelled passing case exercises the TS-JUDGE label-clear path.
: SNT-LABELED ( -- )
   [: 5 ;] [: 5 ;] SNAP= ;

\ SNAP= forces both branch rows to the same shape, so the count-mismatch guard in
\ TS-MATCH? can only be exercised directly through the checked count setters.
: SNT-MATCH-COUNT-DIFF ( -- bool )
   2 TS-ACTUAL-N! 1 TS-EXPECT-N! TS-MATCH? ;

\ typed SNAP= comparator: matching quotation shapes compare at runtime
T-RESET
SNT-EQ-PASS
T-CASES 4 T=
T-FAILURES 0 T=

\ value mismatch under equal shapes fails at runtime as one case
T-RESET
SNT-EQ-FAIL
T-CASES 1 T=
T-FAILURES 1 T=

\ shape checks: equal branch shapes certify, a mismatch rejects at CHECK time
T-RESET
s" SNT-EQ-OK ( -- ) [: 1 2 ;] [: 3 4 ;] SNAP=" CHECK-QUIET-CANDIDATE! -1 T=
s" SNT-EQ-BAD ( -- ) [: 1 2 ;] [: 3 ;] SNAP=" SNT-EQ-REJECTS
s" SNT-EQ-BAD2 ( -- ) [: 1 ;] [: 3 4 ;] SNAP=" SNT-EQ-REJECTS
T-FAILURES 0 T=

\ labels apply to snapshot cases and clear after judging
T-RESET
s" snap-label" T-LABEL
SNT-LABELED
T-LABEL$ s" " T$=
T-FAILURES 0 T=

\ the count-mismatch comparator branch rejects unequal recorded counts
T-RESET
SNT-MATCH-COUNT-DIFF TFALSE
0 TS-EXPECT-N!
0 TS-ACTUAL-N!
T-FAILURES 0 T=

\ capacity guards fail closed
T-RESET
' SNT-COUNT-NEG E-TBL-BOUNDS TTHROWS
' SNT-COUNT-HIGH E-TBL-BOUNDS TTHROWS
0 TS-EXPECT-N!
0 TS-ACTUAL-N!
T-FAILURES 0 T=

T-REPORT
