\ cast-negative-suite.f - reject contract for the CAST: checked retype declarer.
\ Run BY THE ENGINE over stdin, like test/deftype-suite.f:
\     bin/hb < test/cast-negative-suite.f
\ Registered as a positive gate case in test/candidate-validation.f: it asserts
\ every reject in-process and prints ok, so the process exits 0 with clean stderr.
\
\ Each illegal cast is rejected by its NAMED reject:
\   - E-CAST-ARITY : more than one input term, or more than one output term
\   - E-CAST-CLASS : in/out is not a single retype-eligible machine cell (a
\                    pointer is a class/width reinterpret, not a cell retype)
\   - E-CAST-FAM   : in/out names an undeclared family
\   - E-CAST-LINEAR: in/out transitively contains linear ownership
\   - verdict 0    : a net-stack-effect body, or a body that drops the input —
\                    both fail the ( in -- in ) identity certification
\   - verdict 0    : the cast: declarer used inside a checked body (unsafe token)
\ Legality violations throw their named code; body-certification
\ failures surface as verdict 0. A failure prints F<index>; REPORT exits 1 on any.

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
create CNDIAG-BUF 8192 allot
CNDIAG-BUF 8192 DIAG-BUFFER!

NEWTYPE cnfam 0
package CAST-NEG
public
DEFLINEAR CAST-NEG:lease
STRUCTURE nested 0 FIELD owner CAST-NEG:lease ;STRUCTURE
;package

\ Arm the one-shot cast window with the declaration name, then certify the
\ declaration as a candidate. A legality violation throws its named code (caught
\ here); a body-certification failure returns verdict 0. Result: an E-CAST-* code,
\ or the verdict. The window is single-shot, cleared by CHECK on every match.
variable CN-A
variable CN-U
variable CN-VERD
: CN-RUN ( -- ) CN-A @ CN-U @ CHECK-QUIET-CANDIDATE! CN-VERD ! ;
: CN-CAST ( ptr u8 n ptr u8 n -- n )   \ ( arm-name decl-source -- result )
   CN-U ! CN-A !
   CAST-PEND!
   0 CN-VERD !
   [: CN-RUN ;] catch
   dup 0 <> if exit then
   drop CN-VERD @ ;
\ arity: more than one input, or more than one output term.
s" CNA1"  s" CNA1 ( n n -- cnfam )"      CN-CAST E-CAST-ARITY T=
s" CNA2"  s" CNA2 ( n -- cnfam cnfam )"  CN-CAST E-CAST-ARITY T=
\ class: a pointer is a reinterpret, not a single-cell retype — either side.
s" CNC1"  s" CNC1 ( n -- ptr a )"        CN-CAST E-CAST-CLASS T=
s" CNC2"  s" CNC2 ( ptr a -- n )"        CN-CAST E-CAST-CLASS T=
\ undeclared family in the signature.
s" CNF1"  s" CNF1 ( n -- neverdecl )"    CN-CAST E-CAST-FAM T=
\ Neither direction, linear-to-linear, nor transitive containment may cross
\ CAST:, even when both sides occupy one machine cell.
s" CNL1"  s" CNL1 ( n -- CAST-NEG:lease )" CN-CAST E-CAST-LINEAR T=
s" CNL2"  s" CNL2 ( CAST-NEG:lease -- n )" CN-CAST E-CAST-LINEAR T=
s" CNL3"  s" CNL3 ( CAST-NEG:lease -- CAST-NEG:lease )" CN-CAST E-CAST-LINEAR T=
s" CNL4"  s" CNL4 ( n -- CAST-NEG:nested )" CN-CAST E-CAST-LINEAR T=
s" CNL5"  s" CNL5 ( CAST-NEG:nested -- n )" CN-CAST E-CAST-LINEAR T=
\ The production declarer path rejects the same foreign-package forgeries and
\ rolls every failed word back out of the dictionary.
package CAST-FOREIGN
variable CN-SRC-A
variable CN-SRC-U
: CN-EVAL-SOURCE ( -- )
   CN-SRC-A @ CN-SRC-U @ INCLUDE-EVALUATE ;
: CN-EVAL-CATCH ( ptr u8 n -- n )
   CN-SRC-U ! CN-SRC-A !
   [: CN-EVAL-SOURCE ;] catch ;
: CN-ABSENT? ( ptr u8 n -- bool )
   get-current search-wl 0= ;
: CN-PRIVATE-PROBE ( -- ) ;
\ WID 0 cannot observe a current-package private word, so it cannot prove
\ rollback here. The exact current private WID sees the probe and no failed cast.
s" CN-PRIVATE-PROBE" 0 search-wl 0= -1 T=
s" CN-PRIVATE-PROBE" CN-ABSENT? 0 T=
s" CAST: CNP1 ( n -- CAST-NEG:lease ) ;" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CAST: CNP2 ( CAST-NEG:lease -- n ) ;" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CAST: CNP3 ( CAST-NEG:lease -- CAST-NEG:lease ) ;" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CAST: CNP4 ( n -- CAST-NEG:nested ) ;" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CAST: CNP5 ( CAST-NEG:nested -- n ) ;" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP1" CN-ABSENT? -1 T=
s" CNP2" CN-ABSENT? -1 T=
s" CNP3" CN-ABSENT? -1 T=
s" CNP4" CN-ABSENT? -1 T=
s" CNP5" CN-ABSENT? -1 T=
;package
\ net-stack-effect body: the ( in -- in ) identity certification rejects.
s" CNS1"  s" CNS1 ( n -- cnfam ) dup"    CN-CAST 0 T=
\ a body that drops the input: identity certification rejects.
s" CND1"  s" CND1 ( n -- cnfam ) drop"   CN-CAST 0 T=

\ the cast: declarer used inside a checked body is rejected unsafe (verdict 0);
\ no window is armed — the reject is the bare token, before any name is parsed.
s" CNO1 ( n -- cnfam ) cast:"            CHECK-QUIET-CANDIDATE! 0 T=

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" cast-negative-suite: failures" 1 die ;
REPORT
