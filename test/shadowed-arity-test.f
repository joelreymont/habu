\ shadowed-arity-test.f - a package public whose own bare tail a private word of
\ the same package claims (dot habu-refuse-an-arity-5affb6b7).
\
\ A bare tail binds the open package's PRIVATE wordlist first, so such a public
\ definition is not what its own name binds to, and the native compiler reads a
\ definition's contract from that binding (compiler.f KEEP-ARITY asks
\ NDICT:SPELL-ARITY with the bare name). The pair itself is the deliberate
\ forwarder pattern - lib/task.f publishes `: PREPARE ( ptr n -- ) PREPARE ;`
\ over its private PREPARE - and stays legal; what ncomp cannot survive is the
\ two effects moving DIFFERENT numbers of cells. Before this rule that pair
\ passed the source run and failed the native build minutes later with -8303
\ E-NELAB-ARITY (the Tender backend's report); it is now refused at the second
\ definition with E-SHADOWED-ARITY.
\
\ WHY EVERY CASE RUNS THROUGH INCLUDE-EVALUATE. The rule is about WHEN the
\ refusal happens: at the definition, on the ordinary load path, not at the
\ build. One source string per case makes that observable - the code comes back
\ from the string that carried the second definition.

require lib/prelude.f
require lib/fmt.f                        \ FMT:.INT - one-line number text

\ The harness owns a package because this suite shares an in-process slice with
\ others carrying the same test vocabulary; a global `T=` here is a duplicate
\ definition (throw 78) before the first case runs. The CASES stay at top level:
\ each opens a package inside the evaluated source, which has to happen in
\ top-level scope to mean what it says.
package SHADOWED-ARITY
private

variable #FAIL
variable #CASE

: T-FAIL ( -- )
   [char] F emit #CASE @ . ;

TYPED-VARIABLE SAE-A ptr u8   variable SAE-U
: SAE-GO ( -- )  SAE-A @ SAE-U @ INCLUDE-EVALUATE ;

variable SAVED-HOOK

public

\ A definition the checker never sees, bracketed at top level.
TRUSTED: UNCHECKED+ ( -- ) check@ SAVED-HOOK ! 0 set-check ;
TRUSTED: UNCHECKED- ( -- ) SAVED-HOOK @ set-check ;

: T= ( n n -- ) {: got:n want:n :}
   #CASE @ 1 + #CASE !
   got want <> if
      T-FAIL s" shadowed-arity-test: expected " type want FMT:.INT s"  got " type got FMT:.INT cr
      #FAIL @ 1 + #FAIL !
   then ;

\ Evaluate one source string, returning its throw code (0 = accepted).
: SAE-CATCH ( ptr u8 n -- n )  SAE-U ! SAE-A !  [: SAE-GO ;] catch ;

\ The codes the refused cases expect. They live in the package because a global
\ `E-*` constant is lib/errors.f's surface alone.
7145 constant E-SHADOW-ARITY   \ E-SHADOWED-ARITY: src/core/checker.f SHADOW-ARITY-CK
70 constant E-REJECT           \ E-UNDEFINED: the checker's compile reject (habu2.f RC-REJECT)

: REPORT ( -- )
   #FAIL @ 0 = if s" ok" type cr exit then
   #FAIL @ . s" shadowed-arity-test: failures" 1 die ;

;package

\ === refused: the private twin's effect moves a different number of cells =====
\ Fewer inputs than the private word. This is the reported shape: ncomp used to
\ elaborate the public body against the private word's two inputs and reach the
\ return with one value too many (-8303).
s\" package SAT-A\n: F ( n n -- n ) + ;\npublic\n: F ( n -- n ) 1 + ;\n;package"
   SHADOWED-ARITY:SAE-CATCH SHADOWED-ARITY:E-SHADOW-ARITY SHADOWED-ARITY:T=

\ More inputs than the private word: the same rule from the other side (ncomp
\ reached it as -8304 E-NELAB-UNDER, one value short of what `+` consumes).
s\" package SAT-B\n: G ( n -- n ) 1 + ;\npublic\n: G ( n n -- n ) + ;\n;package"
   SHADOWED-ARITY:SAE-CATCH SHADOWED-ARITY:E-SHADOW-ARITY SHADOWED-ARITY:T=

\ The OUTPUT side counts too: the inputs agree and the results do not.
s\" package SAT-C\n: H ( n -- n ) 1 + ;\npublic\n: H ( n -- n n ) dup ;\n;package"
   SHADOWED-ARITY:SAE-CATCH SHADOWED-ARITY:E-SHADOW-ARITY SHADOWED-ARITY:T=

\ CELLS, not terms: `ptr u8 n` is two terms and two cells, and the public tail
\ here moves one. A rule written over term counts would accept this.
s\" package SAT-D\n: K ( ptr u8 n -- n ) drop c@ ;\npublic\n: K ( n -- n ) 1 + ;\n;package"
   SHADOWED-ARITY:SAE-CATCH SHADOWED-ARITY:E-SHADOW-ARITY SHADOWED-ARITY:T=

\ The private twin may be a definer-made word: what the rule judges is the PUBLIC
\ colon definition, whose body ncomp elaborates against whatever the bare tail
\ binds - here a constant that leaves one cell where the body declares two.
s\" package SAT-M\n10 constant W\npublic\n: W ( n -- n ) 1 + ;\n;package"
   SHADOWED-ARITY:SAE-CATCH SHADOWED-ARITY:E-SHADOW-ARITY SHADOWED-ARITY:T=

\ === accepted: every neighbouring shape that must stay legal =================
\ A DEFINER-MADE PUBLIC is not judged by this rule: `constant` compiles no body
\ for ncomp to elaborate, so the private twin's arity never reaches it. This is
\ test/ndict-binding.f's NDB-LEFT shape (private SHARED 10, public SHARED 20),
\ and the record a definer-made public presents to the checker is the DEFINER
\ CALL's effect - `20 constant CV` presents ( n -- ) - not the created word's,
\ which is why the rule is asked at the colon-definition intake alone.
s\" package SAT-K\n10 constant CV\npublic\n20 constant CV\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" : SAT-K-CK ( -- ) SAT-K:CV 20 <> if s\q public constant lost its own value\q 1 die then ;\nSAT-K-CK"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=

\ The same narrowing from the other side: a definer-made public over a private
\ COLON word of another width. The private word binds its own name, the public
\ constant has no body, and nothing here can reach E-NELAB-ARITY.
s\" package SAT-L\n: CW ( n -- ) drop ;\npublic\n30 constant CW\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=

\ THE FORWARDER PATTERN ITSELF. Same tail, same effect, and the bare call in the
\ public body is the private word - which is the point of the pattern.
s\" package SAT-E\n: P ( n -- n ) 1 + ;\npublic\n: P ( n -- n ) P ;\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" : SAT-E-CK ( -- ) 41 SAT-E:P 42 <> if s\q forwarder missed the private word\q 1 die then ;\nSAT-E-CK"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=

\ Same cells through different types: two cells either way, so the compiler
\ reads an arity that fits. Measured as accepted today and kept that way.
s\" package SAT-F\n: Q ( ptr u8 n -- n ) drop c@ ;\npublic\n: Q ( n n -- n ) + ;\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=

\ Different tails in one package: nothing shadows anything.
s\" package SAT-G\n: R ( n -- n ) 1 + ;\npublic\n: S ( n n -- n ) + ;\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=

\ A PACKAGE WORD SHADOWING A GLOBAL of another arity, which is legal (docs/forth.md
\ Packages) and is what a rule asked of the bare-name resolver instead of the
\ record's own key would have refused.
s" : SAT-SHADOWED ( n n -- n ) + ;" SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" package SAT-H\n: SAT-SHADOWED ( n -- n ) 1 + ;\npublic\n: U ( n -- n ) SAT-SHADOWED ;\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" : SAT-H-CK ( -- ) 7 SAT-H:U 8 <> if s\q package word did not shadow the global\q 1 die then ;\nSAT-H-CK"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=

\ The public one FIRST and the private one after it: the public definition binds
\ its own name when it is made, which is when its contract is read, so the pair
\ is sound whatever the arities are.
s\" package SAT-I\npublic\n: V ( n -- n ) 1 + ;\nprivate\n: V ( n n -- n ) + ;\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=

\ A GLOBAL redefinition with another arity, which `undefine` allows and this rule
\ does not touch: the global wordlist has one entry, so the name still binds the
\ definition that owns it.
s" : SAT-W ( n -- n ) 1 + ;" SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s" undefine SAT-W" SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s" : SAT-W ( n n -- n ) + ;" SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=

\ A second definition of the same tail in ONE wordlist is the older rule and
\ keeps its own code (78), ahead of this one.
s\" package SAT-J\n: X ( n -- n ) 1 + ;\n: X ( n n -- n ) + ;\n;package"
   SHADOWED-ARITY:SAE-CATCH 78 SHADOWED-ARITY:T=

\ === nothing without a live record binds a name ===============================
\ A REFUSED public leaves nothing behind. Its symbol used to be interned before
\ this rule threw, and a later `using` of the package then refused the bare tail
\ as E-USING-SHADOW-GLOBAL against a public the engine never defined. The engine
\ binds the global there, so the checked word does too: SAT-N-USE answers 7.
\ The refusal is caught INSIDE the package, by a nested `evaluate`, so that
\ `;package` still closes SAT-N with its private word for the `using` to read.
s" : SAT-T ( -- n ) 7 ;" SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" package SAT-N\n: SAT-T ( n n -- n ) + ;\npublic\ns\q : SAT-T ( n -- n ) 1 + ;\q ' evaluate catch SHADOWED-ARITY:E-SHADOW-ARITY SHADOWED-ARITY:T=\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" using SAT-N\n: SAT-N-USE ( -- n ) SAT-T ;\n;using"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" : SAT-N-CK ( -- ) SAT-N-USE 7 <> if s\q the refused public bound its tail\q 1 die then ;\nSAT-N-CK"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=

\ An `undefine`d private word's tombstone binds nothing either: the engine binds
\ the global again once the private word is gone, and so does the checker.
s" : SAT-U ( -- n ) 7 ;" SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" package SAT-UP\n: SAT-U ( -- n ) 1 ;\nundefine SAT-U\npublic\n: SAT-UP-USE ( -- n ) SAT-U ;\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" : SAT-UP-CK ( -- ) SAT-UP:SAT-UP-USE 7 <> if s\q the tombstone bound its tail\q 1 die then ;\nSAT-UP-CK"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=

\ Nor does a symbol that source makes through a checker primitive, which carries
\ no record: CHECKER-DEFER at a package's top level and from a checked body, and
\ CHECKER-UNDEFINE of a name the package never defined. The engine defines no
\ SAT-D in these packages, so each bare SAT-D is the global.
s" : SAT-D ( -- n ) 7 ;" SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" package SAT-DA\ns\q SAT-D\q CHECKER-DEFER\npublic\n: SAT-DA-USE ( -- n ) SAT-D ;\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" package SAT-DB\n: SAT-DB-HIDE ( -- ) s\q SAT-D\q CHECKER-DEFER ;\nSAT-DB-HIDE\npublic\n: SAT-DB-USE ( -- n ) SAT-D ;\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" package SAT-DC\ns\q SAT-D\q CHECKER-UNDEFINE\npublic\n: SAT-DC-USE ( -- n ) SAT-D ;\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" : SAT-D-CK ( -- ) SAT-DA:SAT-DA-USE SAT-DB:SAT-DB-USE + SAT-DC:SAT-DC-USE + 21 <> if s\q a recordless symbol bound its tail\q 1 die then ;\nSAT-D-CK"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=

\ Nor does a public twin bind past a private word the engine still holds. The
\ engine binds the private word first, so one the checker has no live record
\ for stops the walk before the public leg: a CHECKER-UNDEFINE deleted only the
\ checker's record (SAT-DT), and a `0 set-check` word never had one (SAT-DU).
\ Each use below was certified against the public ( ptr u8 -- n ) and ran the
\ private ( n -- n ) on a pointer; it is refused. The engine's own `undefine`
\ retires the private word too, and then the public twin binds (SAT-DV).
s\" package SAT-DT\n: SAT-DT-X ( n -- n ) 1 + ;\npublic\n: SAT-DT-X ( ptr u8 -- n ) drop 5 ;\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" package SAT-DT\ns\q SAT-DT-X\q CHECKER-UNDEFINE\n: SAT-DT-USE ( -- n ) s\q ab\q drop SAT-DT-X ;\n;package"
   SHADOWED-ARITY:SAE-CATCH SHADOWED-ARITY:E-REJECT SHADOWED-ARITY:T=
s\" package SAT-DU\nSHADOWED-ARITY:UNCHECKED+\n: SAT-DU-X ( n -- n ) 1 + ;\nSHADOWED-ARITY:UNCHECKED-\npublic\n: SAT-DU-X ( ptr u8 -- n ) drop 5 ;\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" package SAT-DU\n: SAT-DU-USE ( -- n ) s\q ab\q drop SAT-DU-X ;\n;package"
   SHADOWED-ARITY:SAE-CATCH SHADOWED-ARITY:E-REJECT SHADOWED-ARITY:T=
s\" package SAT-DV\n: SAT-DV-X ( n -- n ) 1 + ;\npublic\n: SAT-DV-X ( ptr u8 -- n ) drop 5 ;\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" package SAT-DV\nundefine SAT-DV-X\npublic\n: SAT-DV-USE ( -- n ) s\q ab\q drop SAT-DV-X ;\n;package"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=
s\" : SAT-DV-CK ( -- ) SAT-DV:SAT-DV-USE 5 <> if s\q the retired private twin bound its tail\q 1 die then ;\nSAT-DV-CK"
   SHADOWED-ARITY:SAE-CATCH 0 SHADOWED-ARITY:T=

SHADOWED-ARITY:REPORT
