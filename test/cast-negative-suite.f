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
\   - E-CAST-OWNER : scalar-cell family output is outside its declaring package
\   - verdict 0    : a net-stack-effect body, or a body that drops the input —
\                    both fail the ( in -- in ) identity certification
\   - verdict 0    : the cast: declarer used inside a checked body (unsafe token)
\ Legality violations throw their named code (7129-7131, 7135); body-certification
\ failures surface as verdict 0. A failure prints F<index>; REPORT exits 1 on any.

require test/checker-assert.f
require src/habu/verify-source.f

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

package CN
public
NEWTYPE cnfam 0
NEWTYPE cncell 1
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
s" CNA1"  s" CNA1 ( n n -- CN:cnfam )"              CN-CAST E-CAST-ARITY T=
s" CNA2"  s" CNA2 ( n -- CN:cnfam CN:cnfam )"       CN-CAST E-CAST-ARITY T=
\ class: a pointer is a reinterpret, not a single-cell retype — either side.
s" CNC1"  s" CNC1 ( n -- ptr a )"        CN-CAST E-CAST-CLASS T=
s" CNC2"  s" CNC2 ( ptr a -- n )"        CN-CAST E-CAST-CLASS T=
\ undeclared family in the signature.
s" CNF1"  s" CNF1 ( n -- neverdecl )"    CN-CAST E-CAST-FAM T=
\ introduction into an arity-0 or parametric cell family belongs to its
\ declaring package. Same-owner introduction works; another package cannot mint
\ either shape, while projections out remain unrestricted.
package CN
s" CNO0"  s" CNO0 ( n -- CN:cnfam )"                 CN-CAST -1 T=
s" CNG0"  s" CNG0 ( n -- CN:cncell<n> )"             CN-CAST -1 T=
;package
package CN-HIR
s" CNO1"  s" CNO1 ( n -- CN:cnfam )"                 CN-CAST E-CAST-OWNER T=
s" CNG1"  s" CNG1 ( n -- CN:cncell<n> )"             CN-CAST E-CAST-OWNER T=
s" CNP1"  s" CNP1 ( CN:cnfam -- n )"                 CN-CAST -1 T=
s" CNP2"  s" CNP2 ( CN:cncell<n> -- n )"             CN-CAST -1 T=
;package
\ CHECKER-PACKAGE is a callable parser mirror, not package authority. Spoofing
\ its supported mutator while the engine remains in the global wordlist rejects.
s" CN" CHECKER-PACKAGE
s" CNSP1" s" CNSP1 ( n -- CN:cnfam )"                 CN-CAST E-CAST-OWNER T=
CHECKER-END-PACKAGE
\ Direct mutation of every name/mode mirror cell rejects for the same reason.
99 CHECKER-PACKAGE-NAME c!
110 CHECKER-PACKAGE-NAME 1 + c!
2 CHECKER-PACKAGE-U !
CHECKER-PACKAGE-PRIVATE CHECKER-PACKAGE-MODE !
s" CNSP2" s" CNSP2 ( n -- CN:cnfam )"                 CN-CAST E-CAST-OWNER T=
CHECKER-END-PACKAGE

\ A forged parser package cannot own a declaration. The family, visibility,
\ symbols, and later CAST authorization all follow the live engine namespace.
package CN-SPOOF-A
s" CN-SPOOF-B" CHECKER-PACKAGE
CHECKER-PUBLIC
public
NEWTYPE spoof 0
s" CNSA0" s" CNSA0 ( n -- CN-SPOOF-A:spoof )" CN-CAST -1 T=
CHECKER-END-PACKAGE
;package
package CN-SPOOF-B
s" CNSB0" s" CNSB0 ( n -- CN-SPOOF-A:spoof )" CN-CAST E-CAST-OWNER T=
s" CNSB1" s" CNSB1 ( n -- CN-SPOOF-B:spoof )" CN-CAST E-CAST-FAM T=
;package

\ Visibility comes from the real current WID. A mirror-public declaration in a
\ private engine section stays private; only its owner resolves it.
package CN-VIS-PRI
CHECKER-PUBLIC
NEWTYPE hidden 0
s" CN-VIS-FAKE" CHECKER-PACKAGE
s" CNVP0" s" CNVP0 ( n -- hidden )" CN-CAST -1 T=
CHECKER-END-PACKAGE
;package
package CN-VIS-FOREIGN
s" CNVP1" s" CNVP1 ( n -- CN-VIS-PRI:hidden )" CN-CAST E-CAST-FAM T=
s" CNVP2" s" CNVP2 ( n -- hidden )" CN-CAST E-CAST-FAM T=
;package

\ A mirror-private declaration in a public engine section stays public. Foreign
\ code resolves it but still cannot introduce the owner-only cell family.
package CN-VIS-PUB
public
CHECKER-PRIVATE
NEWTYPE shown 0
CHECKER-END-PACKAGE
;package
package CN-VIS-FOREIGN
s" CNVU0" s" CNVU0 ( CN-VIS-PUB:shown -- n )" CN-CAST -1 T=
s" CNVU1" s" CNVU1 ( n -- CN-VIS-PUB:shown )" CN-CAST E-CAST-OWNER T=
;package

\ EXPORT records the actual package target even when the parser mirror names a
\ different package.
package CN-EXP-SRC-PKG
public
: CN-EXP-SRC ( n -- n ) ;
;package
package CN-EXP-A
public
s" CN-EXP-B" CHECKER-PACKAGE
EXPORT CN-EXP-SRC-PKG:CN-EXP-SRC
CHECKER-END-PACKAGE
;package
s" CNEX0 ( n -- n ) CN-EXP-A:CN-EXP-SRC" CHECK-QUIET-CANDIDATE! -1 T=
s" CNEX1 ( n -- n ) CN-EXP-B:CN-EXP-SRC" CHECK-QUIET-CANDIDATE! 1 T=

\ Offline verification is the sole scoped mirror authority. A normal simulated
\ package verifies, restores the live provider, and leaves no family behind.
package CN-CAST-TEST
public
: CN-VRF-VERIFY ( -- )
   s" package CN-VRF public NEWTYPE vfam 0 ;package : CN-VRF-USE ( CN-VRF:vfam -- CN-VRF:vfam ) ;"
   VERIFY:SOURCE-BUF ;

: CN-VRF-FAIL ( -- )
   s" package CN-VRF-ERR public NEWTYPE efam 0 NEWTYPE efam 0 ;package"
   VERIFY:SOURCE-BUF ;
;package

' CN-CAST-TEST:CN-VRF-VERIFY catch 0 T=
s" CNVR0" s" CNVR0 ( n -- CN-VRF:vfam )" CN-CAST E-CAST-FAM T=

' CN-CAST-TEST:CN-VRF-FAIL catch 7102 T=
package CN-VRF-LIVE
NEWTYPE live 0
s" CN-VRF-POISON" CHECKER-PACKAGE
s" CNVR1" s" CNVR1 ( n -- CN-VRF-LIVE:live )" CN-CAST -1 T=
CHECKER-END-PACKAGE
;package

\ net-stack-effect body: the ( in -- in ) identity certification rejects.
package CN
s" CNS1"  s" CNS1 ( n -- CN:cnfam ) dup"             CN-CAST 0 T=
\ a body that drops the input: identity certification rejects.
s" CND1"  s" CND1 ( n -- CN:cnfam ) drop"            CN-CAST 0 T=

\ the cast: declarer used inside a checked body is rejected unsafe (verdict 0);
\ no window is armed — the reject is the bare token, before any name is parsed.
s" CNU1 ( n -- CN:cnfam ) cast:"                      CHECK-QUIET-CANDIDATE! 0 T=
;package

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" cast-negative-suite: failures" 1 die ;
REPORT
