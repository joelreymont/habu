\ cast-negative-suite.f - reject contract for the CAST: checked retype declarer.
\ Run BY THE ENGINE over stdin, like test/deftype-suite.f:
\     bin/hb < test/cast-negative-suite.f
\ The native registry runs it as a positive case: it asserts every reject
\ in-process and prints ok, so the process exits 0 with clean stderr.
\
\ Each illegal cast is rejected by its NAMED reject:
\   - E-CAST-ARITY : more than one input term, or more than one output term (a
\                    layout value wider than a cell is one term per cell)
\   - E-CAST-CLASS : in/out is not one machine cell a cast may retype: an atom,
\                    or a pointer to one; or the destination introduces an atom
\   - E-CAST-FAM   : in/out names an undeclared family, or applies one to the
\                    wrong number of arguments
\   - CHECKER-REJECT-RC (70): any other signature fault, bad syntax or a bare
\                    `ptr`, with the bad-signature diagnostic
\   - E-CAST-LINEAR: a linear type or a type variable anywhere in in/out,
\                    pointees, quotation rows and layout fields included
\   - E-CAST-OWNER : the destination introduces a scalar-cell family outside its
\                    declaring package
\   - E-CAST-MINT  : the destination introduces a pointer or a quotation, a
\                    class mint, outside a package's private section
\ The destination introduces what sits in an introduction position: the term, a
\ pointee, a layout family's arguments, its fields and its variants' payloads,
\ and a quotation's produced rows, recursively, with a quotation's consumed
\ rows flipping the direction.
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
require lib/fmt.f                        \ FMT:.INT - one-line number text

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ .
   #FAIL @ 1 + #FAIL ! ;
: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" assert: expected " type want FMT:.INT s"  got " type got FMT:.INT cr
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
STRUCTURE wide 0 FIELD lo n FIELD hi n ;STRUCTURE
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
\ itself certifies. A quotation that only consumes an atom introduces none.
package CN-ATOM
s" cast: CNC4 ( n -- [ -- extent-a ] )"     CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNC5 ( n -- ptr [ -- extent-a ] )" CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNC6 ( n -- cnpbox<extent-a> )"    CN-RUN:DECL E-CAST-CLASS T=
s" cast: CNC7 ( n -- [ extent-a -- ] )"     CN-RUN:DECL 0 T=
;package
\ undeclared family in the signature.
s" cast: CNF1 ( n -- neverdecl )"    CN-RUN:DECL E-CAST-FAM T=
\ A family applied to the wrong number of arguments is malformed the same way:
\ the declaration is refused by name, not by a later death.
s" cast: CNF2 ( n -- cnpbox )"       CN-RUN:DECL E-CAST-FAM T=
s" cast: CNF3 ( n -- ptr cnpbox )"   CN-RUN:DECL E-CAST-FAM T=
s" cast: CNF4 ( cnpbox<n,n> -- n )"  CN-RUN:DECL E-CAST-FAM T=
\ Any other signature fault, here a bare `ptr`, is refused before a walk reads
\ the row, as a definition with that signature is: CHECKER-REJECT-RC. The first
\ fault names the class, so CNB1's wrong-arity `cnpbox` never reaches a walk.
s" cast: CNB1 ( ptr -- cnpbox )"     CN-RUN:DECL CHECKER-REJECT-RC T=
s" cast: CNB2 ( n -- ptr )"          CN-RUN:DECL CHECKER-REJECT-RC T=
\ Neither direction, linear-to-linear, nor transitive containment may cross
\ CAST:, even when both sides occupy one machine cell.
s" cast: CNL1 ( n -- CAST-NEG:lease )" CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL2 ( CAST-NEG:lease -- n )" CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL3 ( CAST-NEG:lease -- CAST-NEG:lease )" CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL4 ( n -- CAST-NEG:nested )" CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL5 ( CAST-NEG:nested -- n )" CN-RUN:DECL E-CAST-LINEAR T=
\ A type variable may bind linear, so one anywhere in either term is refused the
\ same way: `( n -- ptr a )` would forge a pointer to any nominal,
\ `( ptr a -- n )` reads one, and a quotation producing `a` forges the value
\ itself.
s" cast: CNL6 ( n -- ptr a )"        CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL7 ( ptr a -- n )"        CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL8 ( n -- [ -- a ] )"     CN-RUN:DECL E-CAST-LINEAR T=
\ A linear type behind a pointer, or in any of a quotation's four rows, is
\ carried ownership too, on either side of the cast.
s" cast: CNL9 ( n -- ptr ptr CAST-NEG:lease )"      CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL10 ( n -- [ CAST-NEG:lease -- ] )"      CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL11 ( n -- [ -- | -- CAST-NEG:lease ] )" CN-RUN:DECL E-CAST-LINEAR T=
s" cast: CNL12 ( [ -- CAST-NEG:nested ] -- n )"     CN-RUN:DECL E-CAST-LINEAR T=
\ A layout's fields are what its projection yields, so a field holding a linear
\ value behind a pointer or in a quotation row carries it through the cast as
\ well, on either side, and a private section does not change that. STRUCTURE
\ refuses the pointer field itself; PRODUCT admits it.
PRODUCT lpfbox 0 FIELD p ptr CAST-NEG:lease ;PRODUCT
STRUCTURE lqfbox 0 FIELD q [ -- CAST-NEG:lease ] ;STRUCTURE
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
s" cast: CNOP ( n -- ptr CN:cnfam )"             CN-RUN:DECL 0 T=
s" cast: CNOQ ( n -- [ -- CN:cncell<n> ] )"      CN-RUN:DECL 0 T=
;package
package CN-HIR
s" cast: CNO1 ( n -- CN:cnfam )"                 CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNG1 ( n -- CN:cncell<n> )"             CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNP1 ( CN:cnfam -- n )"                 CN-RUN:DECL 0 T=
s" cast: CNP2 ( CN:cncell<n> -- n )"             CN-RUN:DECL 0 T=
\ Reading through a minted pointer, or calling a minted quotation, yields what
\ its pointee or produced rows name, so those are introduction positions too,
\ even in this package's private section. A quotation handed in flips the
\ direction: a consumer passed to the minted quotation is fed by it.
s" cast: CNG2 ( n -- ptr CN:cnfam )"             CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNG3 ( n -- ptr ptr CN:cncell<n> )"     CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNG4 ( n -- [ -- CN:cnfam ] )"          CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNG5 ( n -- [ -- | -- CN:cnfam ] )"     CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNG6 ( n -- [ [ CN:cnfam -- ] -- ] )"   CN-RUN:DECL E-CAST-OWNER T=
\ A layout family's arguments are introduction positions as well: projecting
\ the field of a minted `cnpbox` yields a value of its argument family.
s" cast: CNG10 ( n -- cnpbox<CN:cnfam> )"        CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNG11 ( n -- ptr cnpbox<CN:cncell<n>> )" CN-RUN:DECL E-CAST-OWNER T=
\ A quotation that only consumes the family projects it, a producer handed in
\ is its caller's own, and a source term never introduces.
s" cast: CNG7 ( n -- [ CN:cnfam -- n ] )"        CN-RUN:DECL 0 T=
s" cast: CNG8 ( n -- [ [ -- CN:cnfam ] -- ] )"   CN-RUN:DECL 0 T=
s" cast: CNG9 ( ptr CN:cnfam -- n )"             CN-RUN:DECL 0 T=
s" cast: CNG12 ( n -- [ cnpbox<CN:cnfam> -- ] )" CN-RUN:DECL 0 T=
;package
\ A layout's fields and its variants' payloads are introduction positions too:
\ projecting a field or matching a variant yields the value. A layout whose
\ field carries another package's family hands that family out, directly,
\ through a nested layout, a generic instance, a pointer field, a pointer to a
\ variant of either payload form, a field quotation that produces it, or a
\ produced quotation, even in the layout owner's own private section. A field
\ quotation that only consumes the family hands out none. The family's owner
\ casts into the same layouts.
package CN-FO
public
NEWTYPE fofam 0
;package
package CN-FQ
public
STRUCTURE fqbox 0 FIELD v CN-FO:fofam ;STRUCTURE
STRUCTURE fqnest 0 FIELD inner fqbox ;STRUCTURE
STRUCTURE fqapp 0 FIELD b cnpbox<CN-FO:fofam> ;STRUCTURE
STRUCTURE fqptr 0 FIELD p ptr fqbox ;STRUCTURE
ENUM fqsum 0 VARIANT has FIELD v CN-FO:fofam ;VARIANT VARIANT none ;VARIANT ;ENUM
SUMTYPE fqleg 0 VARIANT has CN-FO:fofam ;VARIANT VARIANT none ;VARIANT ;SUMTYPE
ENUM fqlist 0 VARIANT more FIELD v CN-FO:fofam FIELD next ptr fqlist ;VARIANT VARIANT last ;VARIANT ;ENUM
STRUCTURE fqprod 0 FIELD q [ -- CN-FO:fofam ] ;STRUCTURE
STRUCTURE fqcons 0 FIELD q [ CN-FO:fofam -- ] ;STRUCTURE
s" cast: CNW1 ( n -- fqbox )"            CN-RUN:DECL E-CAST-OWNER T=
private
s" cast: CNW2 ( n -- fqnest )"           CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNW3 ( n -- fqapp )"            CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNW4 ( n -- fqptr )"            CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNW5 ( n -- ptr fqsum )"        CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNW6 ( n -- ptr fqleg )"        CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNW7 ( n -- [ -- fqbox ] )"     CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNW8 ( n -- [ fqbox -- ] )"     CN-RUN:DECL 0 T=
s" cast: CNW9 ( fqbox -- n )"            CN-RUN:DECL 0 T=
s" cast: CNW10 ( n -- fqprod )"          CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNW11 ( n -- fqcons )"          CN-RUN:DECL 0 T=
\ An ENUM variant may point at its own family. The walk reads each instance once
\ per path, so a list of foreign values is refused and its projection is not.
s" cast: CNW12 ( n -- ptr fqlist )"      CN-RUN:DECL E-CAST-OWNER T=
s" cast: CNW13 ( ptr fqlist -- n )"      CN-RUN:DECL 0 T=
;package
package CN-FO
s" cast: CNW14 ( n -- CN-FQ:fqbox )"     CN-RUN:DECL 0 T=
s" cast: CNW15 ( n -- CN-FQ:fqptr )"     CN-RUN:DECL 0 T=
s" cast: CNW16 ( n -- ptr CN-FQ:fqsum )" CN-RUN:DECL 0 T=
s" cast: CNW17 ( n -- ptr CN-FQ:fqlist )" CN-RUN:DECL 0 T=
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

\ mint: a pointer or quotation destination is a class mint - it asserts that an
\ integer is an address or code, which nothing checks - so it is declared only
\ in a package's private section, where the package's own words are its only
\ callers. Top level and a public section refuse it and leave no name behind.
s" cast: CNM1 ( n -- ptr u8 )"                    CN-RUN:DECL E-CAST-MINT T=
s" CNM1" 0 search-wl 0= -1 T=
s" cast: CNM2 ( n -- [ n -- n ] )"                CN-RUN:DECL E-CAST-MINT T=
s" cast: CNM3 ( ptr u8 -- ptr n )"                CN-RUN:DECL E-CAST-MINT T=
\ A layout argument is what a field projection yields, so an instance whose
\ argument is a pointer or a quotation, at any depth, is the same mint.
s" cast: CNM11 ( n -- cnpbox<ptr u8> )"           CN-RUN:DECL E-CAST-MINT T=
s" cast: CNM12 ( n -- cnpbox<[ n -- n ]> )"       CN-RUN:DECL E-CAST-MINT T=
s" cast: CNM13 ( n -- cnpbox<cnpbox<ptr u8>> )"   CN-RUN:DECL E-CAST-MINT T=
\ A layout's own field is projected the same way, so a field of pointer or
\ quotation type, at any depth of nesting, is the same mint.
STRUCTURE pfbox 0 FIELD p ptr u8 ;STRUCTURE
STRUCTURE qfbox 0 FIELD q [ n -- n ] ;STRUCTURE
STRUCTURE pfnest 0 FIELD inner pfbox ;STRUCTURE
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
\ The scope is the engine's live one: a parser mirror claiming a package's
\ private section while the engine compiles at top level still refuses.
s" CN" CHECKER-PACKAGE
s" cast: CNSP3 ( n -- ptr u8 )"                   CN-RUN:DECL E-CAST-MINT T=
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

\ Offline verification replays a package in the verifier window's own scope. A
\ normal simulated package verifies, restores the live provider, and leaves no
\ family behind.
package CN-CAST-TEST
public
: CN-VRF-VERIFY ( -- )
   s" package CN-VRF public NEWTYPE vfam 0 ;package : CN-VRF-USE ( CN-VRF:vfam -- CN-VRF:vfam ) ;"
   VERIFY:SOURCE-BUF ;

: CN-VRF-FAIL ( -- )
   s" package CN-VRF-ERR public NEWTYPE efam 0 NEWTYPE efam 0 ;package"
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
NEWTYPE live 0
s" CN-VRF-POISON" CHECKER-PACKAGE
s" cast: CNVR1 ( n -- CN-VRF-LIVE:live )" CN-RUN:DECL 0 T=
CHECKER-END-PACKAGE
;package
\ A cast carries no body, and this is how that is enforced rather than merely
\ intended. The declaration ENDS at its closing paren: whatever follows is read
\ by the interpreter as its own token, not swallowed as a body. The trailing 42
\ runs and is left as the text's residue, which INCLUDE-EVALUATE, a closed
\ evaluation, refuses by name.
package CN
s" cast: CNS1 ( n -- CN:cnfam ) 42"              CN-RUN:DECL E-EVAL-RESIDUE T=
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
: RUN ( ptr u8 n -- n ) {: src:ptr u:n :}   \ source -> child exit status (-1 = signal)
   src u OUT CAP >LEN ERR CAP >LEN CHILD-MS >MS SUBJECT:RUN {: outu:len erru:len oc :}
   erru LEN>N ERR-U !
   oc MATCH outcome
     exited OF ENDOF
     signaled OF drop -1 ENDOF
     timeout OF src u OUT outu LEN>N ERR erru LEN>N SUBJECT:TIMED-OUT ENDOF
   ;MATCH ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

\ Tier 1 records a whole definition before binding its body tokens. At EOF,
\ this unfinished definition is refused before [char] or ['] reads an operand.
\ The default ARM tier 0 reads that operand immediately.
: BODY-EOF? ( ptr u8 n -- bool )
   HB-TARGET-LINUX-X86-64? if
      2drop ERR$ s" hb: source ended inside definition: x" CONTAINS?
   else
      ERR$ 2swap CONTAINS?
   then ;
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

\ Required reader operands fail before stale token bytes can be consumed.
\ These are child checks because the reader exits after its diagnostic.
s" create"   CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
CN-CHILD:ERR$ s" hb: reader keyword needs a name: create" CONTAINS? -1 T=
s" trusted:" CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
CN-CHILD:ERR$ s" hb: reader keyword needs a name: trusted:" CONTAINS? -1 T=
s" variable" CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
\ `variable` shares the CREATE emitter, so its stable diagnostic names the
\ implementation keyword that owns the common name reader.
CN-CHILD:ERR$ s" hb: reader keyword needs a name: create" CONTAINS? -1 T=
s" constant" CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
CN-CHILD:ERR$ s" hb: reader keyword needs a name: constant" CONTAINS? -1 T=
s" char"     CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
CN-CHILD:ERR$ s" hb: reader keyword needs a name: char" CONTAINS? -1 T=
s" : x [char]" CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
s" hb: reader keyword needs a name: [char]" CN-CHILD:BODY-EOF? -1 T=
s" '"        CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
CN-CHILD:ERR$ s" hb: reader keyword needs a name: '" CONTAINS? -1 T=
s" : x [']"   CN-CHILD:RUN CN-CHILD:NO-NAME-RC T=
s" hb: reader keyword needs a name: [']" CN-CHILD:BODY-EOF? -1 T=
package CN

\ the cast: declarer used inside a checked body is rejected unsafe (verdict 0);
\ no window is armed — the reject is the bare token, before any name is parsed.
s" CNU1 ( n -- CN:cnfam ) cast:"                      CHECK-QUIET-CANDIDATE! 0 T=
;package

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" cast-negative-suite: failures" 1 die ;
REPORT
