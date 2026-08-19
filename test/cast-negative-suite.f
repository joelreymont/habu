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
\   - E-CAST-OWNER : scalar-cell family output is outside its declaring package
\   - verdict 0    : the cast: declarer used inside a checked body (unsafe token)
\   - underdepth   : a bare call at an empty interpret stack, refused by name
\ Every case runs the PRODUCTION declarer: the engine's own `cast:` reader
\ keyword, evaluated as source, so what is asserted is what a file gets. A
\ refusal throws its named code and leaves NO name behind — the record is not
\ counted until the checker has accepted the row. A failure prints F<index>;
\ REPORT exits 1 on any.

require test/checker-assert.f
require src/habu/verify-source.f
require lib/test/subject.f

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

\ Silence expected rejection diagnostics (verdicts are asserted, not printed).
\ The production declarer emits a real diagnostic per refusal where the old
\ candidate path was quiet, so the buffer holds all of them.
package CN-DIAG
create BUF 65536 allot
BUF 65536 DIAG-BUFFER!
;package

package CN
public
NEWTYPE cnfam 0
NEWTYPE cncell 1
;package
s" STRUCTURE cnpbox 1 FIELD value a ;STRUCTURE" INCLUDE-EVALUATE
package CAST-NEG
public
DEFLINEAR CAST-NEG:lease
STRUCTURE nested 0 FIELD owner CAST-NEG:lease ;STRUCTURE
;package

\ Run one declaration through the PRODUCTION path: the text is evaluated, so the
\ engine's `cast:` keyword reads it exactly as it reads a file, and the checker
\ it calls is the live one. Result: the E-CAST-* code a refusal threw, or 0 when
\ the cast was accepted and published. There is no window to arm and no
\ candidate scope in front of it — a refusal here is the refusal a file gets.
\ Each fixture carries its whole source, keyword included, so what the case
\ shows is exactly what a file would contain.
package CN-RUN
public
variable SRC-A
variable SRC-U
: EVAL ( -- ) SRC-A @ SRC-U @ INCLUDE-EVALUATE ;
: DECL ( ptr u8 n -- n )               \ ( decl-source -- thrown-code | 0 )
   SRC-U !  SRC-A !
   [: EVAL ;] catch ;
;package
\ arity: more than one input, or more than one output term.
s" cast: CNA1 ( n n -- CN:cnfam )"              CN-RUN:DECL E-CAST-ARITY T=
s" cast: CNA2 ( n -- CN:cnfam CN:cnfam )"       CN-RUN:DECL E-CAST-ARITY T=
\ class: a pointer is a reinterpret, not a single-cell retype — either side.
s" cast: CNC1 ( n -- ptr a )"        CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNC2 ( ptr a -- n )"        CN-RUN:DECL E-CAST-CLASS T=
\ undeclared family in the signature.
s" cast: CNF1 ( n -- neverdecl )"    CN-RUN:DECL E-CAST-FAM T=
\ Neither direction, linear-to-linear, nor transitive containment may cross
\ CAST:, even when both sides occupy one machine cell.
s" cast: CNL1 ( n -- CAST-NEG:lease )" CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL2 ( CAST-NEG:lease -- n )" CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL3 ( CAST-NEG:lease -- CAST-NEG:lease )" CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL4 ( n -- CAST-NEG:nested )" CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL5 ( CAST-NEG:nested -- n )" CN-RUN:DECL E-CAST-LINEAR T=
\ The production declarer path rejects the same foreign-package forgeries and
\ rolls every failed word back out of the dictionary.
package CAST-FOREIGN
variable CN-WID
get-current CN-WID !
\ The same runner, with the declaration landing in the captured WID so the
\ absence assertions below can see a package-private name.
: CN-EVAL-CATCH ( ptr u8 n -- n )
   CN-WID @ set-current
   CN-RUN:DECL ;
: CN-ABSENT? ( ptr u8 n -- bool )
   CN-WID @ search-wl 0= ;
: CN-PRIVATE-PROBE ( -- ) ;
\ WID 0 cannot observe a package-private word, so it cannot prove rollback.
\ The captured private WID sees the probe and must see no failed cast.
s" CN-PRIVATE-PROBE" 0 search-wl 0= -1 T=
s" CN-PRIVATE-PROBE" CN-ABSENT? 0 T=
s" CAST: CNP1 ( n -- CAST-NEG:lease )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP1" CN-ABSENT? -1 T=  s" CNP1" 0 search-wl 0= -1 T=
s" CAST: CNP2 ( CAST-NEG:lease -- n )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP2" CN-ABSENT? -1 T=  s" CNP2" 0 search-wl 0= -1 T=
s" CAST: CNP3 ( CAST-NEG:lease -- CAST-NEG:lease )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP3" CN-ABSENT? -1 T=  s" CNP3" 0 search-wl 0= -1 T=
s" CAST: CNP4 ( n -- CAST-NEG:nested )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP4" CN-ABSENT? -1 T=  s" CNP4" 0 search-wl 0= -1 T=
s" CAST: CNP5 ( CAST-NEG:nested -- n )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP5" CN-ABSENT? -1 T=  s" CNP5" 0 search-wl 0= -1 T=
s" CAST: CNP6 ( n -- cnpbox<CAST-NEG:lease> )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP6" CN-ABSENT? -1 T=  s" CNP6" 0 search-wl 0= -1 T=
s" CAST: CNP7 ( cnpbox<cnpbox<CAST-NEG:lease>> -- n )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP7" CN-ABSENT? -1 T=  s" CNP7" 0 search-wl 0= -1 T=
s" CAST: CNP8 ( n -- a )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP8" CN-ABSENT? -1 T=  s" CNP8" 0 search-wl 0= -1 T=
s" CAST: CNP9 ( n -- cnpbox<cnpbox<a>> )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP9" CN-ABSENT? -1 T=  s" CNP9" 0 search-wl 0= -1 T=
s" CN-PRIVATE-PROBE" 0 search-wl 0= -1 T=
s" CN-PRIVATE-PROBE" CN-ABSENT? 0 T=
;package

\ introduction into an arity-0 or parametric cell family belongs to its
\ declaring package. Same-owner introduction works; another package cannot mint
\ either shape, while projections out remain unrestricted.
package CN
s" cast: CNO0 ( n -- CN:cnfam )"                 CN-RUN:DECL 0 T=
s" cast: CNG0 ( n -- CN:cncell<n> )"             CN-RUN:DECL 0 T=
;package
package CN-HIR
s" cast: CNO1 ( n -- CN:cnfam )"                 CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNG1 ( n -- CN:cncell<n> )"             CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNP1 ( CN:cnfam -- n )"                 CN-RUN:DECL 0 T=
s" cast: CNP2 ( CN:cncell<n> -- n )"             CN-RUN:DECL 0 T=
;package

\ The families the ENGINE registers (src/core/type-family.f) are declared in the
\ global, empty package, so the global scope is their owner and no package may
\ mint one. This is the shape the live regression had: maki/extent.f declared
\ `CAST: >RED ( ix<e> -- redx<e> )` inside `package MAKI`, minting the engine's
\ `redx`, and every load of the file died with this reject. The repair moved the
\ declaration to global scope (lib/type/extent-role.f) and moved `ix` into the
\ same engine registration as its `extprod` and `redx` siblings.
\ `redx` is a parametric engine family; `attn-stage-q` is an arity-0 one.
s" cast: CNE0 ( n -- redx<n> )"                  CN-RUN:DECL 0 T=
s" cast: CNE1 ( n -- attn-stage-q )"             CN-RUN:DECL 0 T=
package CN-ENG
s" cast: CNE2 ( n -- redx<n> )"                  CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNE3 ( n -- attn-stage-q )"             CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNE4 ( redx<n> -- n )"                  CN-RUN:DECL 0 T=
s" cast: CNE5 ( attn-stage-q -- n )"             CN-RUN:DECL 0 T=
\ ...and the parser mirror cannot buy that ownership back. Ending the mirror's
\ package makes it claim top level - the engine's own definition wordlist is
\ still CN-ENG's, so the engine family stays out of reach.
CHECKER-END-PACKAGE
s" cast: CNE6 ( n -- redx<n> )"                  CN-RUN:DECL E-CAST-OWNER T=
CHECKER-PUBLIC
;package
\ CHECKER-PACKAGE is a callable parser mirror, not package authority. Spoofing
\ its supported mutator while the engine remains in the global wordlist rejects.
s" CN" CHECKER-PACKAGE
s" cast: CNSP1 ( n -- CN:cnfam )"                 CN-RUN:DECL E-CAST-OWNER T=
CHECKER-END-PACKAGE
\ Direct mutation of every name/mode mirror cell rejects for the same reason.
99 CHECKER-PACKAGE-NAME c!
110 CHECKER-PACKAGE-NAME 1 + c!
2 CHECKER-PACKAGE-U !
CHECKER-PACKAGE-PRIVATE CHECKER-PACKAGE-MODE !
s" cast: CNSP2 ( n -- CN:cnfam )"                 CN-RUN:DECL E-CAST-OWNER T=
CHECKER-END-PACKAGE

\ A forged parser package cannot own a declaration. The family, visibility,
\ symbols, and later CAST authorization all follow the live engine namespace.
package CN-SPOOF-A
s" CN-SPOOF-B" CHECKER-PACKAGE
CHECKER-PUBLIC
public
NEWTYPE spoof 0
s" cast: CNSA0 ( n -- CN-SPOOF-A:spoof )" CN-RUN:DECL 0 T=
CHECKER-END-PACKAGE
;package
package CN-SPOOF-B
s" cast: CNSB0 ( n -- CN-SPOOF-A:spoof )" CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNSB1 ( n -- CN-SPOOF-B:spoof )" CN-RUN:DECL E-CAST-FAM T=
;package

\ Visibility comes from the real current WID. A mirror-public declaration in a
\ private engine section stays private; only its owner resolves it.
package CN-VIS-PRI
CHECKER-PUBLIC
NEWTYPE hidden 0
s" CN-VIS-FAKE" CHECKER-PACKAGE
s" cast: CNVP0 ( n -- hidden )" CN-RUN:DECL 0 T=
CHECKER-END-PACKAGE
;package
package CN-VIS-FOREIGN
s" cast: CNVP1 ( n -- CN-VIS-PRI:hidden )" CN-RUN:DECL E-CAST-FAM T=
s" cast: CNVP2 ( n -- hidden )" CN-RUN:DECL E-CAST-FAM T=
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
s" cast: CNVU0 ( CN-VIS-PUB:shown -- n )" CN-RUN:DECL 0 T=
s" cast: CNVU1 ( n -- CN-VIS-PUB:shown )" CN-RUN:DECL E-CAST-OWNER T=
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
s" cast: CNVR0 ( n -- CN-VRF:vfam )" CN-RUN:DECL E-CAST-FAM T=

' CN-CAST-TEST:CN-VRF-FAIL catch 7102 T=
package CN-VRF-LIVE
NEWTYPE live 0
s" CN-VRF-POISON" CHECKER-PACKAGE
s" cast: CNVR1 ( n -- CN-VRF-LIVE:live )" CN-RUN:DECL 0 T=
CHECKER-END-PACKAGE
;package
\ A cast carries no body, and this is how that is enforced rather than merely
\ intended. The declaration ENDS at its closing paren: whatever follows is read
\ by the interpreter as its own token, not swallowed as a body.
package CN
s" cast: CNS1 ( n -- CN:cnfam ) 42"              CN-RUN:DECL 0 T=
42 T=                                         \ the trailing token ran, and left its value
s" CNS1" get-current search-wl 0= 0 T=        \ ... and the cast published anyway
;package

\ The last two refusals belong to the ENGINE, not the checker: it writes them to
\ fd 2 and dies, which this suite's clean-stderr contract cannot host. A child
\ is the better assertion anyway — it pins the exit status AND the message a
\ user actually sees.
package CN-CHILD
public
$400 constant CAP
10000 constant CHILD-MS
70 constant UNDEF-RC
$4A constant NO-NAME-RC
create OUT CAP allot
create ERR CAP allot
variable ERR-U
: RUN ( ptr u8 n -- n )   \ source -> child exit status (-1 = signal or timeout)
   OUT CAP >LEN ERR CAP >LEN CHILD-MS >MS SUBJECT:RUN
   MATCH outcome
     exited OF ENDOF
     signaled OF drop -1 ENDOF
     timeout OF -1 ENDOF
   ;MATCH
   {: rc:n :}
   LEN>N ERR-U !
   LEN>N drop
   rc ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;
;package

\ A cast's record carries its certified minimum input arity, so a bare call at an
\ empty interpret stack is refused BY NAME instead of reading the cell below the
\ base. The bits are poked by the cast's own publish tail; drop that step from
\ the engine and this case is the one that notices (measured).
s" NEWTYPE cnmrole 0  cast: >CNMROLE ( n -- cnmrole )  >CNMROLE" CN-CHILD:RUN CN-CHILD:UNDEF-RC T=
CN-CHILD:ERR$ s" hb: interpret stack underdepth: >CNMROLE" CONTAINS? -1 T=

\ The OLD spelling is dead, and that is a fact worth failing on: the trailing
\ `;` every converted site used to carry is now an undefined token at interpret
\ state — which is exactly why the conversion had to be one commit.
s" cast: CNZ1 ( n -- n ) ;" CN-CHILD:RUN CN-CHILD:UNDEF-RC T=
CN-CHILD:ERR$ s" E-UNDEFINED: ;" CONTAINS? -1 T=
\ A reader keyword that needs a name and reaches the end of its stream fails
\ closed, and says which keyword and where.
s" cast:" CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
CN-CHILD:ERR$ s" hb: cast: missing name after" CONTAINS? -1 T=
package CN

\ the cast: declarer used inside a checked body is rejected unsafe (verdict 0);
\ no window is armed — the reject is the bare token, before any name is parsed.
s" CNU1 ( n -- CN:cnfam ) cast:"                      CHECK-QUIET-CANDIDATE! 0 T=
;package

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" cast-negative-suite: failures" 1 die ;
REPORT
