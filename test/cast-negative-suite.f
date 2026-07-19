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
\   - verdict 0    : a net-stack-effect body, or a body that drops the input —
\                    both fail the ( in -- in ) identity certification
\   - verdict 0    : the cast: declarer used inside a checked body (unsafe token)
\ Legality violations throw their named code (7129-7131); body-certification
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

TYPEFAMILY cnfam 0

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
