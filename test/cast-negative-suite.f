\ cast-negative-suite.f - reject contract for the CAST: checked retype declarer.
\ Run by the engine over stdin:
\     bin/hb < test/cast-negative-suite.f
\ The native registry runs it as a positive case: every reject is asserted
\ in-process, so the process exits 0 with clean stderr.

require src/habu/verify-source.f
require src/lib/string.f
require src/lib/test/subject.f
require src/lib/test.f

T-RESET

\ Silences the expected rejection diagnostics: the production declarer emits one
\ per refusal.
package CN-DIAG
create BUF 65536 allot
BUF 65536 DIAG-BUFFER!
;package

package CN
public
NEWTYPE cnfam
NEWTYPE cncell<a>
;package
s" STRUCTURE cnpbox<a> FIELD value a ;STRUCTURE" INCLUDE-EVALUATE
package CAST-NEG
public
DEFLINEAR CAST-NEG:lease
STRUCTURE nested FIELD owner CAST-NEG:lease ;STRUCTURE
STRUCTURE wide FIELD lo n FIELD hi n ;STRUCTURE
;package

\ Evaluates one declaration through the engine's `cast:` keyword and the live
\ checker; the answer is the E-CAST-* code a refusal threw, or 0 when the cast
\ was published.
\ Each case evaluates the production `cast:` keyword as source; a refusal throws
\ its named code and leaves no name behind.
package CN-RUN
public
TYPED-VARIABLE SRC-A ptr u8
variable SRC-U
: EVAL ( -- ) SRC-A @ SRC-U @ INCLUDE-EVALUATE ;
: DECL ( ptr u8 n -- n )               \ ( decl-source -- thrown-code | 0 )
   SRC-U !  SRC-A !
   [: EVAL ;] catch ;
;package
\ arity: more than one input, or more than one output term.
s" cast: CNA1 ( n n -- CN:cnfam )"              CN-RUN:DECL E-CAST-ARITY T=
s" cast: CNA2 ( n -- CN:cnfam CN:cnfam )"       CN-RUN:DECL E-CAST-ARITY T=
\ class: an atom is a phantom index, not a cell, and a pointer to one names no
\ storage. A layout value wider than a cell stands for one term per cell, so it
\ is refused as arity before its class is asked.
s" cast: CNC1 ( n -- extent-a )"     CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNC2 ( n -- ptr extent-a )" CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNC3 ( n -- CAST-NEG:wide )" CN-RUN:DECL E-CAST-ARITY T=
\ Nor does the destination introduce an atom where a call, a read or a field
\ projection yields a value: a produced row, a pointee and a layout argument
\ refuse one as the bare term does, even in a private section, where the mint
\ itself certifies. A quotation that only consumes an atom introduces none
\ unless it is a pointee.
package CN-ATOM
s" cast: CNC4 ( n -- [ -- extent-a ] )"     CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNC5 ( n -- ptr [ -- extent-a ] )" CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNC6 ( n -- cnpbox<extent-a> )"    CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNC7 ( n -- [ extent-a -- ] )"     CN-RUN:DECL 0 T=
;package
\ No cast yields a width<t>, bare or behind a pointer, even in a private
\ section. A cast out of one reads its bytes.
s" cast: CNWD1 ( n -- width<n> )"     CN-RUN:DECL E-CAST-CLASS T=
package CN-WIDTH
s" cast: CNWD2 ( n -- width<n> )"     CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNWD3 ( n -- ptr width<n> )" CN-RUN:DECL E-CAST-CLASS T=
;package
s" cast: CNWD4 ( width<n> -- n )"     CN-RUN:DECL 0 T=
\ A quotation's consumed rows are fed by the cast's result: none is handed an
\ atom or a width. A width the source produces is read, not introduced.
package CN-WIDTH
private
s" cast: CNWD5 ( [ width<n> -- n ] -- [ n -- n ] )" CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNWD6 ( [ xt -- ] -- [ n -- ] )"           CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNWD7 ( [ extent-a -- ] -- [ n -- ] )"     CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNWD8 ( [ n -- width<n> ] -- [ n -- n ] )" CN-RUN:DECL 0 T=
\ A pointee is written as well as read: every position in it is checked.
s" cast: CNWD9 ( ptr [ -- width<n> ] -- ptr [ -- n ] )"   CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNWD10 ( ptr [ n -- ] -- ptr [ width<n> -- ] )"  CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNWD12 ( ptr [ n -- ] -- ptr [ xt -- ] )"        CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNWD13 ( ptr [ -- xt ] -- ptr [ -- n ] )"        CN-RUN:DECL E-CAST-CLASS T=
;package
\ undeclared family in the signature.
s" cast: CNF1 ( n -- neverdecl )"    CN-RUN:DECL E-CAST-FAM T=
\ A family applied to the wrong number of arguments is malformed the same way:
\ the declaration is refused by name, not by a later death.
s" cast: CNF2 ( n -- cnpbox )"       CN-RUN:DECL E-CAST-FAM T=
s" cast: CNF3 ( n -- ptr cnpbox )"   CN-RUN:DECL E-CAST-FAM T=
s" cast: CNF4 ( cnpbox<n,n> -- n )"  CN-RUN:DECL E-CAST-FAM T=
\ Any other signature fault, here a bare `ptr`, is refused before a walk reads
\ the row, as a stored signature that does not parse is. The first fault names
\ the class, so CNB1's wrong-arity `cnpbox` never reaches a walk.
s" cast: CNB1 ( ptr -- cnpbox )"     CN-RUN:DECL E-BAD-STORED-SIGNATURE T=
s" cast: CNB2 ( n -- ptr )"          CN-RUN:DECL E-BAD-STORED-SIGNATURE T=
\ Neither direction, linear-to-linear, nor transitive containment may cross
\ CAST:, even when both sides occupy one machine cell.
s" cast: CNL1 ( n -- CAST-NEG:lease )" CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL2 ( CAST-NEG:lease -- n )" CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL3 ( CAST-NEG:lease -- CAST-NEG:lease )" CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL4 ( n -- CAST-NEG:nested )" CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL5 ( CAST-NEG:nested -- n )" CN-RUN:DECL E-CAST-LINEAR T=
\ A variable only behind a pointer binds no linear value, so the cast admits it;
\ one a quotation produces may bind one. `( n -- ptr a )` mints a pointer.
s" cast: CNL6 ( n -- ptr a )"        CN-RUN:DECL E-CAST-MINT T=
s" cast: CNL7 ( ptr a -- n )"        CN-RUN:DECL 0 T=
s" cast: CNL8 ( n -- [ -- a ] )"     CN-RUN:DECL E-CAST-LINEAR T=
\ A private section admits the mint under a bare name only.
package CN-STORED
s" cast: CNL18 ( n -- ptr a )"       CN-RUN:DECL 0 T=
s" cast: CNQ:FORGE ( n -- ptr u8 )"  CN-RUN:DECL E-CAST-MINT T=
;package
\ A linear type behind a pointer, or in either of a quotation's rows, is
\ carried ownership too, on either side of the cast.
s" cast: CNL9 ( n -- ptr ptr CAST-NEG:lease )"      CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL10 ( n -- [ CAST-NEG:lease -- ] )"      CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL12 ( [ -- CAST-NEG:nested ] -- n )"     CN-RUN:DECL E-CAST-LINEAR T=
\ A layout's fields are what its projection yields, so a field holding a linear
\ value behind a pointer or in a quotation row carries it through the cast as
\ well, on either side, and a private section does not change that. STRUCTURE
\ refuses the pointer field itself; PRODUCT admits it.
PRODUCT lpfbox FIELD p ptr CAST-NEG:lease ;PRODUCT
STRUCTURE lqfbox FIELD q [ -- CAST-NEG:lease ] ;STRUCTURE
package CN-LIN
s" cast: CNL13 ( n -- lpfbox )"      CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL14 ( n -- lqfbox )"      CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL15 ( n -- ptr lqfbox )"  CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL16 ( lpfbox -- n )"      CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL17 ( lqfbox -- n )"      CN-RUN:DECL E-CAST-LINEAR T=
;package
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
s" CN-PRIVATE-PROBE" 0 search-wl 0= TTRUE
s" CN-PRIVATE-PROBE" CN-ABSENT? TFALSE
s" CAST: CNP1 ( n -- CAST-NEG:lease )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP1" CN-ABSENT? TTRUE  s" CNP1" 0 search-wl 0= TTRUE
s" CAST: CNP2 ( CAST-NEG:lease -- n )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP2" CN-ABSENT? TTRUE  s" CNP2" 0 search-wl 0= TTRUE
s" CAST: CNP3 ( CAST-NEG:lease -- CAST-NEG:lease )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP3" CN-ABSENT? TTRUE  s" CNP3" 0 search-wl 0= TTRUE
s" CAST: CNP4 ( n -- CAST-NEG:nested )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP4" CN-ABSENT? TTRUE  s" CNP4" 0 search-wl 0= TTRUE
s" CAST: CNP5 ( CAST-NEG:nested -- n )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP5" CN-ABSENT? TTRUE  s" CNP5" 0 search-wl 0= TTRUE
s" CAST: CNP6 ( n -- cnpbox<CAST-NEG:lease> )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP6" CN-ABSENT? TTRUE  s" CNP6" 0 search-wl 0= TTRUE
s" CAST: CNP7 ( cnpbox<cnpbox<CAST-NEG:lease>> -- n )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP7" CN-ABSENT? TTRUE  s" CNP7" 0 search-wl 0= TTRUE
s" CAST: CNP8 ( n -- a )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP8" CN-ABSENT? TTRUE  s" CNP8" 0 search-wl 0= TTRUE
s" CAST: CNP9 ( n -- cnpbox<cnpbox<a>> )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP9" CN-ABSENT? TTRUE  s" CNP9" 0 search-wl 0= TTRUE
\ A view's element is a cast term: the unpack hands out whatever t binds.
s" CAST: CNP10 ( read-view<p,q,t> -- ptr u8 n )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP10" CN-ABSENT? TTRUE  s" CNP10" 0 search-wl 0= TTRUE
s" CAST: CNP11 ( read-view<p,q,CAST-NEG:nested> -- ptr u8 n )" CN-EVAL-CATCH E-CAST-LINEAR T=
s" CNP11" CN-ABSENT? TTRUE  s" CNP11" 0 search-wl 0= TTRUE
s" CN-PRIVATE-PROBE" 0 search-wl 0= TTRUE
s" CN-PRIVATE-PROBE" CN-ABSENT? TFALSE
;package

package CN-HIR
s" cast: CNP1 ( CN:cnfam -- n )"                 CN-RUN:DECL 0 T=
s" cast: CNP2 ( CN:cncell<n> -- n )"             CN-RUN:DECL 0 T=
;package
package CN-FO
public
NEWTYPE fofam
;package
package CN-FQ
public
STRUCTURE fqbox FIELD v CN-FO:fofam ;STRUCTURE
ENUM fqlist VARIANT more FIELD v CN-FO:fofam FIELD next ptr fqlist ;VARIANT VARIANT last ;VARIANT ;ENUM
private
s" cast: CNW9 ( fqbox -- n )"            CN-RUN:DECL 0 T=
\ An ENUM variant may point at its own family. The walk reads each instance once
\ per path, so a cast into a list or out of one terminates.
s" cast: CNW13 ( ptr fqlist -- n )"      CN-RUN:DECL 0 T=
s" cast: CNW17 ( n -- ptr fqlist )"      CN-RUN:DECL 0 T=
;package
\ CN-OH's public section casts a value into CN's `cnfam`; its private section
\ casts bytes into a pointer to CN-OQ's `oqbox` and a value into its own `ohold`
\ that holds one. The pointer is a class mint, refused in a public section. Any
\ package projects out.
package CN-OQ
public
STRUCTURE oqbox OPAQUE FIELD v n ;STRUCTURE
;package
package CN-OH
public
STRUCTURE ohold FIELD b CN-OQ:oqbox ;STRUCTURE
s" cast: CNQ10 ( ptr u8 -- ptr CN-OQ:oqbox )"    CN-RUN:DECL E-CAST-MINT T=
s" cast: CNQ13 ( n -- CN:cnfam )"                CN-RUN:DECL 0 T=
private
s" cast: CNQ11 ( ptr u8 -- ptr CN-OQ:oqbox )"    CN-RUN:DECL 0 T=
s" cast: CNQ12 ( n -- ohold )"                   CN-RUN:DECL 0 T=
s" cast: CNQ4 ( CN-OQ:oqbox -- n )"              CN-RUN:DECL 0 T=
s" cast: CNQ5 ( ptr CN-OQ:oqbox -- ptr u8 )"     CN-RUN:DECL 0 T=
;package
\ The families the engine registers (src/core/type-family.f) are declared in the
\ global, empty package. `redx` is a parametric engine family; `attn-stage-q`
\ is an arity-0 one.
package CN-ENG
s" cast: CNE4 ( redx<n> -- n )"                  CN-RUN:DECL 0 T=
s" cast: CNE5 ( attn-stage-q -- n )"             CN-RUN:DECL 0 T=
;package

\ mint: a pointer or quotation destination is a class mint - it asserts that an
\ integer is an address or code, which nothing checks - so it is declared only
\ in a package's private section, where the package's own words are its only
\ callers. Top level and a public section refuse it and leave no name behind.
s" cast: CNM1 ( n -- ptr u8 )"                    CN-RUN:DECL E-CAST-MINT T=
s" CNM1" 0 search-wl 0= TTRUE
s" cast: CNM2 ( n -- [ n -- n ] )"                CN-RUN:DECL E-CAST-MINT T=
s" cast: CNM3 ( ptr u8 -- ptr n )"                CN-RUN:DECL E-CAST-MINT T=
\ A layout argument is what a field projection yields, so an instance whose
\ argument is a pointer or a quotation, at any depth, is the same mint.
s" cast: CNM11 ( n -- cnpbox<ptr u8> )"           CN-RUN:DECL E-CAST-MINT T=
s" cast: CNM12 ( n -- cnpbox<[ n -- n ]> )"       CN-RUN:DECL E-CAST-MINT T=
s" cast: CNM13 ( n -- cnpbox<cnpbox<ptr u8>> )"   CN-RUN:DECL E-CAST-MINT T=
\ A layout's own field is projected the same way, so a field of pointer or
\ quotation type, at any depth of nesting, is the same mint.
STRUCTURE pfbox FIELD p ptr u8 ;STRUCTURE
STRUCTURE qfbox FIELD q [ n -- n ] ;STRUCTURE
STRUCTURE pfnest FIELD inner pfbox ;STRUCTURE
s" cast: CNM17 ( n -- pfbox )"                   CN-RUN:DECL E-CAST-MINT T=
s" cast: CNM18 ( n -- qfbox )"                   CN-RUN:DECL E-CAST-MINT T=
s" cast: CNM19 ( n -- pfnest )"                  CN-RUN:DECL E-CAST-MINT T=
package CN-MINT
public
s" cast: CNM4 ( n -- ptr u8 )"                    CN-RUN:DECL E-CAST-MINT T=
s" cast: CNM5 ( n -- [ -- ] )"                    CN-RUN:DECL E-CAST-MINT T=
s" cast: CNM14 ( n -- cnpbox<ptr u8> )"           CN-RUN:DECL E-CAST-MINT T=
s" cast: CNM15 ( n -- cnpbox<[ n -- n ]> )"       CN-RUN:DECL E-CAST-MINT T=
s" cast: CNM20 ( n -- pfbox )"                   CN-RUN:DECL E-CAST-MINT T=
private
s" cast: CNM6 ( n -- ptr u8 )"                    CN-RUN:DECL 0 T=
s" cast: CNM7 ( n -- [ -- ] )"                    CN-RUN:DECL 0 T=
\ A pointee carries no width rule: a layout of any width is one pointer away.
s" cast: CNM10 ( n -- ptr CAST-NEG:wide )"        CN-RUN:DECL 0 T=
s" cast: CNM21 ( n -- pfbox )"                   CN-RUN:DECL 0 T=
s" cast: CNM22 ( n -- qfbox )"                   CN-RUN:DECL 0 T=
s" cast: CNM23 ( n -- pfnest )"                  CN-RUN:DECL 0 T=
;package
\ Projections out of a pointer, a quotation or a layout holding one mint
\ nothing: any scope.
s" cast: CNM8 ( ptr u8 -- n )"                    CN-RUN:DECL 0 T=
s" cast: CNM9 ( [ n -- n ] -- n )"                CN-RUN:DECL 0 T=
s" cast: CNM16 ( cnpbox<ptr u8> -- n )"           CN-RUN:DECL 0 T=
s" cast: CNM24 ( pfbox -- n )"                   CN-RUN:DECL 0 T=

\ scope: a cast with a scoped term on either side is E-CAST-SCOPE, asked before
\ the arity rule, which would read a view's two cells as two terms, except the
\ view casts VIEW-CAST-CERTIFY admits. Each refusal leaves no name in the
\ private wordlist, which sees the private probe.
package CN-VIEW
variable CV-WID
get-current CV-WID !
: CV-DECL ( ptr u8 n -- n )
   CV-WID @ set-current
   CN-RUN:DECL ;
: CV-ABSENT? ( ptr u8 n -- bool )
   CV-WID @ search-wl 0= ;
: CV-PRIVATE-PROBE ( -- ) ;
s" CV-PRIVATE-PROBE" CV-ABSENT? TFALSE
s" cast: CNV1 ( ptr u8 n -- read-view<p,q,u8> )"        CV-DECL E-CAST-SCOPE T=
s" CNV1" CV-ABSENT? TTRUE
s" cast: CNV2 ( ptr u8 n -- mut-view<p,q,a,u8> )"       CV-DECL E-CAST-SCOPE T=
s" CNV2" CV-ABSENT? TTRUE
s" cast: CNV3 ( mut-view<p,l,a,u8> -- ptr u8 n )"       CV-DECL E-CAST-SCOPE T=
s" CNV3" CV-ABSENT? TTRUE
s" cast: CNV4 ( mut-view<p,l,a,u8> -- read-view<p,l,u8> )" CV-DECL E-CAST-SCOPE T=
s" CNV4" CV-ABSENT? TTRUE
s" cast: CNV5 ( read-view<p,q,u8> -- n )"               CV-DECL E-CAST-SCOPE T=
s" CNV5" CV-ABSENT? TTRUE
s" cast: CNV6 ( [ mut-view<p,p,fresh-region-b,u8> -- ] -- [ ptr u8 n -- ] )" CV-DECL E-CAST-SCOPE T=
s" CNV6" CV-ABSENT? TTRUE
s" cast: CNV10 ( [ read-view<p,q,u8> -- ] -- [ ptr u8 n -- ] )" CV-DECL E-CAST-SCOPE T=
s" CNV10" CV-ABSENT? TTRUE
s" cast: CNV7 ( n -- init<i,n> )"                       CV-DECL E-CAST-SCOPE T=
s" CNV7" CV-ABSENT? TTRUE
\ A qualified name selects that package's public wordlist, so the read-view
\ unpack this section admits is refused under one, and once the package closes
\ that wordlist holds no such name.
s" cast: CN-VIEW:CNV9 ( read-view<p,q,u8> -- ptr u8 n )" CV-DECL E-CAST-SCOPE T=
public
get-current constant CV-PUBLIC-WID
s" cast: CNV8 ( read-view<p,q,u8> -- ptr u8 n )"        CN-RUN:DECL E-CAST-SCOPE T=
;package
s" CNV9" CN-VIEW:CV-PUBLIC-WID search-wl 0= TTRUE
\ The stored-signature intake still refuses an output variable no input
\ supplies; a view whose variables its input supplies is a well-formed row.
s" defer CNVD1 ( ptr u8 n -- read-view<p,q,u8> )"       CN-RUN:DECL E-BAD-STORED-SIGNATURE T=
s" defer CNVD2 ( read-view<p,q,u8> -- read-view<p,q,u8> )" CN-RUN:DECL 0 T=
\ The scope is the engine's live one: a parser mirror claiming a package's
\ private section while the engine compiles at top level still refuses.
s" CN" CHECKER-PACKAGE
s" cast: CNSP3 ( n -- ptr u8 )"                   CN-RUN:DECL E-CAST-MINT T=
CHECKER-END-PACKAGE

\ A forged parser package cannot own a declaration. The family, visibility and
\ symbols all follow the live engine namespace.
package CN-SPOOF-A
s" CN-SPOOF-B" CHECKER-PACKAGE
CHECKER-PUBLIC
public
NEWTYPE spoof
s" cast: CNSA0 ( n -- CN-SPOOF-A:spoof )" CN-RUN:DECL 0 T=
CHECKER-END-PACKAGE
;package
package CN-SPOOF-B
s" cast: CNSB1 ( n -- CN-SPOOF-B:spoof )" CN-RUN:DECL E-CAST-FAM T=
;package

\ Visibility comes from the real current WID. A mirror-public declaration in a
\ private engine section stays private; only its owner resolves it.
package CN-VIS-PRI
CHECKER-PUBLIC
NEWTYPE hidden
s" CN-VIS-FAKE" CHECKER-PACKAGE
s" cast: CNVP0 ( n -- hidden )" CN-RUN:DECL 0 T=
CHECKER-END-PACKAGE
;package
package CN-VIS-FOREIGN
s" cast: CNVP1 ( n -- CN-VIS-PRI:hidden )" CN-RUN:DECL E-CAST-FAM T=
s" cast: CNVP2 ( n -- hidden )" CN-RUN:DECL E-CAST-FAM T=
;package

\ A mirror-private declaration in a public engine section stays public: foreign
\ code resolves it.
package CN-VIS-PUB
public
CHECKER-PRIVATE
NEWTYPE shown
CHECKER-END-PACKAGE
;package
package CN-VIS-FOREIGN
s" cast: CNVU0 ( CN-VIS-PUB:shown -- n )" CN-RUN:DECL 0 T=
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
s" CNEX1 ( n -- n ) CN-EXP-B:CN-EXP-SRC" CHECK-QUIET-CANDIDATE! 0 T=

\ Offline verification replays a package in the verifier window's own scope. A
\ normal simulated package verifies, restores the live provider, and leaves no
\ family behind.
package CN-CAST-TEST
public
: CN-VRF-VERIFY ( -- )
   s" package CN-VRF public NEWTYPE vfam ;package : CN-VRF-USE ( CN-VRF:vfam -- CN-VRF:vfam ) ;"
   VERIFY:SOURCE-BUF ;

: CN-VRF-FAIL ( -- )
   s" package CN-VRF-ERR public NEWTYPE efam NEWTYPE efam ;package"
   VERIFY:SOURCE-BUF ;

\ The pre-pass reads public and private as it replays, so a mint certifies in a
\ private section, with a checked caller beside it, and refuses in a public one.
: CN-VRF-MINT ( -- )
   s" package CN-VRF-M CAST: CVM0 ( n -- ptr u8 ) : CVM1 ( n -- u8 ) CVM0 c@ ; ;package"
   VERIFY:SOURCE-BUF ;

: CN-VRF-PUBMINT ( -- )
   s" package CN-VRF-P public CAST: CVP0 ( n -- ptr u8 ) ;package"
   VERIFY:SOURCE-BUF ;
;package

' CN-CAST-TEST:CN-VRF-VERIFY catch 0 T=
s" cast: CNVR0 ( n -- CN-VRF:vfam )" CN-RUN:DECL E-CAST-FAM T=
' CN-CAST-TEST:CN-VRF-MINT catch 0 T=
' CN-CAST-TEST:CN-VRF-PUBMINT catch E-CAST-MINT T=

' CN-CAST-TEST:CN-VRF-FAIL catch 7102 T=
package CN-VRF-LIVE
NEWTYPE live
s" CN-VRF-POISON" CHECKER-PACKAGE
s" cast: CNVR1 ( n -- CN-VRF-LIVE:live )" CN-RUN:DECL 0 T=
CHECKER-END-PACKAGE
;package
\ A cast carries no body: the declaration ends at its closing paren, so the
\ trailing 42 is read as its own token and left as residue, which
\ INCLUDE-EVALUATE, a closed evaluation, refuses by name.
package CN
s" cast: CNS1 ( n -- CN:cnfam ) 42"              CN-RUN:DECL E-EVAL-RESIDUE T=
s" CNS1" get-current search-wl 0= TFALSE        \ ... and the cast published anyway
;package

\ The engine, not the checker, makes the last refusals: it writes them to fd 2
\ and dies, which this suite's clean-stderr contract cannot host, so a child
\ pins the exit status and the message.
package CN-CHILD
public
$400 constant CAP
10000 constant CHILD-MS
70 constant UNDEF-RC
$4A constant NO-NAME-RC
create OUT CAP allot
create ERR CAP allot
variable ERR-U
: RUN ( ptr u8 n -- n ) {: src:ptr u:n :}   \ source -> child exit status (-1 = signal)
   src u OUT CAP >LEN ERR CAP >LEN CHILD-MS >MS SUBJECT:RUN {: outu:len erru:len oc :}
   erru LEN>N ERR-U !
   oc MATCH PROCESS:ending
     exited OF ENDOF
     signaled OF drop -1 ENDOF
     timeout OF src u OUT outu LEN>N ERR erru LEN>N SUBJECT:TIMED-OUT ENDOF
   ;MATCH ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;
;package

\ A cast's record carries its certified minimum input arity, so a bare call at
\ an empty interpret stack is refused by name instead of reading the cell below
\ the base. The cast's publish tail pokes the bits; this case is the one that
\ notices that step missing (measured).
s" NEWTYPE cnmrole  cast: >CNMROLE ( n -- cnmrole )  >CNMROLE" CN-CHILD:RUN CN-CHILD:UNDEF-RC T=
CN-CHILD:ERR$ s" hb: interpret stack underdepth: >CNMROLE" CONTAINS? TTRUE

\ The trailing `;` of the retired spelling is an undefined token at interpret
\ state.
s" cast: CNZ1 ( n -- n ) ;" CN-CHILD:RUN CN-CHILD:UNDEF-RC T=
CN-CHILD:ERR$ s" E-UNDEFINED: ;" CONTAINS? TTRUE
\ A reader keyword that needs a name and reaches the end of its stream fails
\ closed, and says which keyword and where.
s" cast:" CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
CN-CHILD:ERR$ s" hb: cast: missing name after" CONTAINS? TTRUE

\ Required reader operands fail before stale token bytes can be consumed.
\ These are child checks because the reader exits after its diagnostic.
s" create"   CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
CN-CHILD:ERR$ s" hb: reader keyword needs a name: create" CONTAINS? TTRUE
s" trusted:" CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
CN-CHILD:ERR$ s" hb: reader keyword needs a name: trusted:" CONTAINS? TTRUE
s" variable" CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
\ `variable` shares the CREATE emitter, so its stable diagnostic names the
\ implementation keyword that owns the common name reader.
CN-CHILD:ERR$ s" hb: reader keyword needs a name: create" CONTAINS? TTRUE
s" constant" CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
CN-CHILD:ERR$ s" hb: reader keyword needs a name: constant" CONTAINS? TTRUE
s" char"     CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
CN-CHILD:ERR$ s" hb: reader keyword needs a name: char" CONTAINS? TTRUE
\ The interpreter reads a whole definition before it binds the body's tokens,
\ so at EOF this unfinished one is refused before [char] or ['] reads an operand.
s" : x [char]" CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
CN-CHILD:ERR$ s" hb: source ended inside definition: x" CONTAINS? TTRUE
s" '"        CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
CN-CHILD:ERR$ s" hb: reader keyword needs a name: '" CONTAINS? TTRUE
s" : x [']"   CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
CN-CHILD:ERR$ s" hb: source ended inside definition: x" CONTAINS? TTRUE
package CN

\ the cast: declarer used inside a checked body is rejected unsafe (verdict 0);
\ no window is armed — the reject is the bare token, before any name is parsed.
s" CNU1 ( n -- CN:cnfam ) cast:"                      CHECK-QUIET-CANDIDATE! 0 T=
;package

T-REPORT
