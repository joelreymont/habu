\ cast-suite.f - positive behavior contract for the CAST: checked retype declarer
\ (the engine's `cast:` reader keyword plus checker.f CHECKER-DEFCAST). Run BY
\ THE ENGINE over stdin, like test/deftype-suite.f:
\     bin/hb < test/cast-suite.f
\ Registered as a positive gate case in test/candidate-validation.f.
\
\ CAST: is the converter form that ends per-declaration TRUSTED growth: the
\ checker proves the declared retype legal by its five structural refusals and
\ publishes ( in -- out ), so the retype is CHECKED, not trusted. This suite pins:
\   - a cast retypes n <-> an arity-0 family scalar, both directions
\   - the value passes through UNCHANGED at runtime (identity data flow)
\   - a guarded conversion (a checked word plus a cast) throws out of range and
\     passes in-range values through
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
CAST: >CSROLE ( n -- csrole )
CAST: CSROLE>N ( csrole -- n )

\ 2. a guarded conversion is TWO things now — the retype, and the checked word
\    that refuses an out-of-range value before applying it. The runtime cases
\    below are unchanged, which is the point: the guard still throws and the
\    in-range value still passes through untouched.
CAST: N>CSBND ( n -- csbnd )
package CS-BND
public
: >CSBND ( n -- csbnd ) dup 0 < over 128 >= or if E-CS-RANGE throw then N>CSBND ;
;package
CAST: CSBND>N ( csbnd -- n )

\ 3. parametric cell family: its type argument is phantom, so the generic
\    projection is structurally non-owning.
CAST: >CSIXN ( n -- csix<n> )
CAST: CSIX>N ( csix<e> -- n )

\ --- runtime: the value passes through unchanged (identity data flow). --------
5 >CSROLE CSROLE>N 5 T=
0 >CSROLE CSROLE>N 0 T=

\ --- runtime: the guard passes in-range values and throws out of range. -------
9 CS-BND:>CSBND CSBND>N 9 T=
127 CS-BND:>CSBND CSBND>N 127 T=
variable CS-RC
package CS-OOR
public
: HI ( -- ) 200 CS-BND:>CSBND CSBND>N drop ;
: LO ( -- ) -1 CS-BND:>CSBND CSBND>N drop ;
;package
' CS-OOR:HI catch CS-RC ! CS-RC @ E-CS-RANGE T=
' CS-OOR:LO catch CS-RC ! CS-RC @ E-CS-RANGE T=

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

\ --- the declaration publishes a REAL word, not just a checker row. -----------
\ A cast the checker knows but the dictionary does not would certify every caller
\ in this file and then die at run time on the first call, so findable and
\ callable are asserted apart: the record is in the live wordlist, it has an
\ execution token, and a COMPILED caller reaches it and gets its value back.
package CS-LINK
s" >CSROLE" 0 search-wl 0= 0 T=          \ declared at global scope, so wordlist 0 owns it
s" CSROLE>N" 0 search-wl 0= 0 T=
' >CSROLE 0 <> -1 T=
' CSROLE>N 0 <> -1 T=
: CALL-COMPILED ( n -- n ) >CSROLE CSROLE>N ;
11 CALL-COMPILED 11 T=
;package

\ --- a call to a cast emits ZERO instructions. -------------------------------
\ Not a memory of a measurement: the code pointer is read around three
\ definitions that differ only in how many casts they call, and the three spans
\ must be equal. An empty body is 16 bytes (CP - entry - 4), which is exactly the
\ inliner's prologue-span window, so the copied span is empty. Make the published
\ body one instruction longer and this fails.
package CS-BYTES
cp@ constant B0
: NOCAST ( n -- n ) ;
cp@ constant B1
: ONECAST ( n -- n ) >CSROLE CSROLE>N ;
cp@ constant B2
: THREECAST ( n -- n ) >CSROLE CSROLE>N >CSROLE CSROLE>N >CSROLE CSROLE>N ;
cp@ constant B3
B2 B1 -  B1 B0 -  T=
B3 B2 -  B1 B0 -  T=
;package

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" cast-suite: failures" 1 die ;
REPORT
