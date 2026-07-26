\ cast-suite.f - positive behavior contract for the CAST: checked retype declarer
\ (src/core/roles.f plus the checker.f CAST-PEND certification window). Run BY THE
\ ENGINE over stdin, like test/deftype-suite.f:
\     bin/hb < test/cast-suite.f
\ Registered as a positive gate case in test/candidate-validation.f.
\
\ CAST: is the converter form that ends per-declaration TRUSTED growth: the
\ checker certifies the body under the identity row ( in -- in ) and publishes the
\ declared ( in -- out ), so the retype is CHECKED, not trusted. This suite pins:
\   - an empty-body cast retypes n <-> an arity-0 family scalar, both directions
\   - the value passes through UNCHANGED at runtime (identity data flow)
\   - a guard body throws out of range and passes in-range values through
\   - a parametric cell family round-trips, and its generic projection
\     ( family<e> -- n ) certifies both generically and at a concrete instance
\   - a checked caller certifies against the published ( in -- out ) row, and the
\     published output is genuinely nominal (demanding a plain n there rejects)
\ A failure prints F<index> + detail; REPORT exits 1 on any fail.

require test/checker-assert.f

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want . s" got " type got . cr
   then ;

\ silence expected rejection diagnostics (verdicts are asserted, not printed).
create CSDIAG-BUF 8192 allot
CSDIAG-BUF 8192 DIAG-BUFFER!

9001 constant E-CS-RANGE

\ two arity-0 family scalars and one parametric cell family.
NEWTYPE csrole 0
NEWTYPE csbnd 0
NEWTYPE csix 1

\ 1. empty-body cast: n <-> arity-0 family scalar, both directions.
CAST: >CSROLE ( n -- csrole ) ;
CAST: CSROLE>N ( csrole -- n ) ;

\ 2. guarded cast: identity data flow plus a range guard that throws at runtime.
CAST: >CSBND ( n -- csbnd ) dup 0 < over 128 >= or if E-CS-RANGE throw then ;
CAST: CSBND>N ( csbnd -- n ) ;

\ 3. parametric family: a concrete-arg injection to build a value, plus the
\    generic projection ( family<e> -- n ).
CAST: >CSIXN ( n -- csix<n> ) ;
CAST: CSIX>N ( csix<e> -- n ) ;

\ --- runtime: the value passes through unchanged (identity data flow). --------
5 >CSROLE CSROLE>N 5 T=
0 >CSROLE CSROLE>N 0 T=

\ --- runtime: the guard passes in-range values and throws out of range. -------
9 >CSBND CSBND>N 9 T=
127 >CSBND CSBND>N 127 T=
variable CS-RC
: CS-OOR-HI ( -- ) 200 >CSBND CSBND>N drop ;
: CS-OOR-LO ( -- ) -1 >CSBND CSBND>N drop ;
' CS-OOR-HI catch CS-RC ! CS-RC @ E-CS-RANGE T=
' CS-OOR-LO catch CS-RC ! CS-RC @ E-CS-RANGE T=

\ --- runtime: parametric round-trip through the projection. -------------------
7 >CSIXN CSIX>N 7 T=

\ --- checked callers certify against the published ( in -- out ) rows. --------
\ round trip both directions certifies on n.
s" CC-RT ( n -- n ) >CSROLE CSROLE>N"        CHECK-QUIET-CANDIDATE! -1 T=
\ the caller sees the nominal output row.
s" CC-OUT ( n -- csrole ) >CSROLE"           CHECK-QUIET-CANDIDATE! -1 T=
\ and the published output is genuinely the family: demanding n there rejects.
s" CC-WRONG ( n -- n ) >CSROLE"              CHECK-QUIET-CANDIDATE!  0 T=
\ the projection certifies generically and at a concrete instance.
s" CC-PROJ ( csix<e> -- n ) CSIX>N"          CHECK-QUIET-CANDIDATE! -1 T=
s" CC-PROJ-N ( csix<n> -- n ) CSIX>N"        CHECK-QUIET-CANDIDATE! -1 T=

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" cast-suite: failures" 1 die ;
REPORT
