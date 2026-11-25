\ using-test.f - `using NAME ... ;using`: package NAME's public wordlist is
\ visible to bare lookup until the matching `;using`, the enclosing `;package`
\ or the end of the load file. Privates stay invisible, definitions never land
\ in NAME, and a bare tail resolving in two used packages is a hard error. Cases
\ answer the throw code of their source (0 = accepted).

require src/lib/prelude.f
require src/habu/verify-source.f
require src/lib/test.f
require src/lib/span.f

T-RESET

TYPED-VARIABLE UCE-A ^u8   variable UCE-U
: UCE-GO ( -- )  UCE-A @ UCE-U @ >str evaluate-closed ;
: UCE-CATCH ( str -- n )  dup STR:LEN UCE-U ! STR:DATA UCE-A !  [: UCE-GO ;] catch ;

\ the same source through the production source verifier: the checker resolves the
\ bare tail itself there, with no engine body lookup in front of it.
: VS-GO ( -- )   UCE-A @ UCE-U @ >str VERIFY:SOURCE-BUF ;
: VS-CATCH ( str -- n )  dup STR:LEN UCE-U ! STR:DATA UCE-A !  [: VS-GO ;] catch ;

\ The captured rejection record contains the text.
: UCE-SAID ( str -- )  {: s:str :}  REJECTION:BUFFER$ s STR:CONTAINS? TTRUE ;

package UA public : AW ( -- n ) 11 ; ;package
package UB public : BW ( -- n ) 22 ; ;package
package UC public : AW ( -- n ) 33 ; ;package   \ same tail AW as UA -> ambiguity source
package UP  : SECP ( -- n ) 99 ; public : PUBW ( -- n ) SECP ; ;package   \ SECP is private
package UMM public : MM ( n -- n ) 7 + ; ;package

\ Shadow fixtures: a used package whose publics collide with globals of the same
\ name. GW's global effect differs from the public's; MW's coincides (the silent
\ wrong-bind case: no mismatch would surface). NC has no global; SW's global is
\ defined later inside the using scope (the order-swapped variant).
package USG public
   : GW ( -- n ) 1 ;
   : MW ( -- n ) 2 ;
   : SW ( -- n ) 3 ;
   : NC ( -- n ) 4 ;
;package
: GW ( n n -- n ) + ;              \ global GW (2 -- 1): differs from USG:GW (0 -- 1)
: MW ( -- n ) 111 ;                \ global MW (0 -- 1): coincides with USG:MW (0 -- 1)

\ === positives (accepted) ===
\ top-level using: bare public resolves
s" using UA AW drop ;using" UCE-CATCH 0 T=
\ two concurrent usings both resolve; ;using pops the most recent
s" using UA using UB AW drop BW drop ;using ;using" UCE-CATCH 0 T=
\ qualified name is always available regardless of using
s" UA:AW drop" UCE-CATCH 0 T=
\ the same package used twice is not ambiguous (one WID)
s" using UA using UA AW drop ;using ;using" UCE-CATCH 0 T=
\ checked body resolves a used public (proves the checker resolution)
s" using UA : UT-CB1 ( -- n ) AW 1 + ; UT-CB1 drop ;using" UCE-CATCH 0 T=
\ Engine DATA offsets do not claim an application's imported word names.
s" package URF public : REFUSAL-CELL ( n n -- ) 2drop ; ;package using URF : URF-CONSUME ( n n -- ) REFUSAL-CELL ; 1 2 URF-CONSUME ;using" UCE-CATCH 0 T=
\ using inside a package block; the used public resolves in the public body
s" package UH using UA public : UT-H1 ( -- n ) AW ; ;package UH:UT-H1 drop" UCE-CATCH 0 T=
\ inner scope wins silently: the open package's own tail shadows a used tail
s" package UI using UA : AW ( -- n ) 7 ; public : UT-I1 ( -- n ) AW ; ;package UI:UT-I1 drop" UCE-CATCH 0 T=
\ a call resolved under a using stays compiled after ;using (compile-time resolution)
s" using UA : UT-CB2 ( -- n ) AW ; ;using UT-CB2 drop" UCE-CATCH 0 T=
\ A public MM is callable bare through using, including in a checked body: the
\ checker's global match-mode cell must not read it as a global shadow.
s" using UMM : UT-MM ( -- n ) 60 MM ; ;using UT-MM 67 T=" UCE-CATCH 0 T=
s" using UMM : UT-MM-V ( n -- n ) MM ; ;using" VS-CATCH 0 T=

\ === negatives (rejected, fail closed) ===
\ used package's PRIVATE word does not resolve bare
s" using UP SECP drop ;using" UCE-CATCH 70 T=
\ a private word is not reachable in a checked body under using either
s" using UP : UT-BADP ( -- n ) SECP ; ;using" UCE-CATCH 70 T=
\ a bare public is not visible without a using
s" AW drop" UCE-CATCH 70 T=
\ ambiguous: AW resolves in two used packages (interpret)
s" using UA using UC AW drop ;using ;using" UCE-CATCH 70 T=
\ ambiguous in a compiled body: the checker refuses it (checker.f LIVE-BIND)
REJECTION:CAPTURE
s" using UA using UC : UT-AMB ( -- n ) AW ; ;using ;using" UCE-CATCH 70 T=
S\" error[E-USING-AMBIGUOUS]: `AW` is public in more than one used package\n" UCE-SAID
S\" ^^^^^^^^^ evaluates the text below\n" UCE-SAID
S\" = evaluated: using UA using UC : UT-AMB ( -- n ) AW ; ;using ;using\n" UCE-SAID
S\"  ^^ ambiguous here\n" UCE-SAID
S\" = packages: UA, UC\n" UCE-SAID
S\" = help: write the package's name before the word, as `UA:AW` or `UC:AW`\n" UCE-SAID  REJECTION:BUFFER-OFF
\ a word public in two used parts
REJECTION:CAPTURE
s" package UPT private package UPU public : AW ( -- n ) 1 ; ;package package UPV public : AW ( -- n ) 2 ; ;package using UPU using UPV : UT-PAMB ( -- n ) AW ; ;using ;using ;package" UCE-CATCH 70 T=
S\" = packages: UPT:UPU, UPT:UPV\n" UCE-SAID
S\" = help: write the package's name before the word, as `UPU:AW` or `UPV:AW`\n" UCE-SAID  REJECTION:BUFFER-OFF
\ a word public in a used top-level package and in the used part that hides it
REJECTION:CAPTURE
s" package UHB public : HW ( -- n ) 9 ; ;package using UHB package UHA private package UHB public : HW ( -- n ) 7 ; ;package using UHB : UHA-C ( -- n ) HW ; ;using ;package ;using" UCE-CATCH 70 T=
S\" = packages: UHB, UHA:UHB\n" UCE-SAID
S\" = help: write the package's name before the word, as `UHB:HW`, or rename the part `UHA:UHB`, which hides package `UHB` here\n" UCE-SAID  REJECTION:BUFFER-OFF
REJECTION:CAPTURE
s" using UA using UC : UT-AMB2 ( -- n ) AW ; ;using ;using" VS-CATCH 70 T=
S\" error[E-USING-AMBIGUOUS]: `AW` is public in more than one used package\n" UCE-SAID
S\"  ^^ ambiguous here\n" UCE-SAID
S\" = packages: UA, UC\n" UCE-SAID
S\" = help: write the package's name before the word, as `UA:AW` or `UC:AW`\n" UCE-SAID  REJECTION:BUFFER-OFF
\ a top-level `target` where a word of that name is in scope, and where none is
REJECTION:CAPTURE
s" package UT-CLASH : TARGET ( -- ) ; target ;package" VS-CATCH 70 T=
S\" error[E-SECTION-WORD]: `target` is the section word, and a word of that name is in scope\n" UCE-SAID
S\"  ^^^^^^ section word or word in scope\n" UCE-SAID
S\" = help: rename the word, or call it by its qualified name if it is public\n" UCE-SAID  REJECTION:BUFFER-OFF
s" package UT-SECT : UT-SECT-W ( -- ) ; target ;package" VS-CATCH 0 T=
\ unknown package
s" using NOPE-PKG" UCE-CATCH 70 T=
\ a colon-bearing name is not a package name
s" using UA:AW" UCE-CATCH 70 T=
\ ;using with no using open is unbalanced
s" ;using" UCE-CATCH 70 T=
\ a define under a using lands in the current scope, never in the used package
s" using UA : UT-NEW ( -- n ) 5 ; ;using UA:UT-NEW drop" UCE-CATCH 70 T=
\ overflow: more than USE-MAX (16) concurrent usings
s" using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA using UA" UCE-CATCH 70 T=

\ === scope end: using does not leak ===
\ ;using ends the scope: AW is unresolved after it
s" using UA AW drop ;using AW drop" UCE-CATCH 70 T=
\ ;package clears a using opened inside the package
s" package UJ using UA public : UT-J1 ( -- n ) AW ; ;package AW drop" UCE-CATCH 70 T=

\ === a package's ;using closes only the package's own usings ===
\ `;package` restores the using depth the package opened at, so a `;using` inside
\ the package that closes a using opened before it is refused by name instead of
\ coming undone at `;package` and leaving AW resolving past it.
s" using UA package UQ ;using ;package AW drop ;using" UCE-CATCH 70 T=
\ the production source verifier refuses the same nesting
REJECTION:CAPTURE
s" using UA package UQ ;using ;package" VS-CATCH 70 T=
S\" error[E-USING-OUTER]: `;using` would close a using opened outside this package\n" UCE-SAID
S\"  ^^^^^^ using opened outside\n" UCE-SAID
S\" = help: close each using in the package that opens it\n" UCE-SAID  REJECTION:BUFFER-OFF
\ a using opened and closed inside the package, and one opened before `package`
\ and closed after `;package`, are unchanged on both routes
s" package UQ using UA AW drop ;using ;package" UCE-CATCH 0 T=
s" using UA package UQ AW drop ;package AW drop ;using" UCE-CATCH 0 T=
s" package UQ using UA public : UT-Q2 ( -- n ) AW ; ;using ;package" VS-CATCH 0 T=
s" using UA package UQ public : UT-Q3 ( -- n ) AW ; ;package : UT-Q4 ( -- n ) AW ; ;using" VS-CATCH 0 T=
\ a buffer lowers the package floor at its end: `;package` closes UQF, not UA
s" using UA package UQF" UCE-CATCH 0 T=
;package
s" AW drop" UCE-CATCH 70 T=
\ A throw closes the package its buffer opened, and the checker's mirror follows
\ before the throw reaches its handler: one compiled word catches the buffer and
\ replays at once.
package USING-TEST
public
: RECLOSE ( -- n n )  s" package UQT AW drop" UCE-CATCH  s" 1 drop" VS-CATCH ;
;package
USING-TEST:RECLOSE 0 T= 70 T=

\ === global-vs-used-public shadow ===
\ A bare tail inside a using scope that resolves to a global and a used public
\ of the same name is ambiguous and fails closed at the reference site. global
\ defined by an earlier load, then using-import of the same name -> reject
REJECTION:CAPTURE
s" using USG : USG-R1 ( -- n ) GW ; ;using" UCE-CATCH 70 T=
S\" error[E-USING-SHADOW-GLOBAL]: `GW` is both a global word and a public of used package `USG`\n" UCE-SAID
S\" ^^^^^^^^^ evaluates the text below\n" UCE-SAID
S\" = evaluated: using USG : USG-R1 ( -- n ) GW ; ;using\n" UCE-SAID
S\"  ^^ ambiguous here\n" UCE-SAID
S\" = global: GW\n" UCE-SAID  S\" = package: USG:GW\n" UCE-SAID
S\" = help: write `USG:GW` to call the package's word, or rename one of the two: the global word has no qualified form\n" UCE-SAID  REJECTION:BUFFER-OFF
\ a used part's public that is also a global word
REJECTION:CAPTURE
s" package UPG private package UPGW public : GW ( -- n ) 1 ; ;package using UPGW : UPG-R1 ( -- n ) GW ; ;using ;package" UCE-CATCH 70 T=
S\" error[E-USING-SHADOW-GLOBAL]: `GW` is both a global word and a public of used package `UPG:UPGW`\n" UCE-SAID
S\" = package: UPG:UPGW:GW\n" UCE-SAID
S\" = help: write `UPGW:GW` to call the package's word, or rename one of the two: the global word has no qualified form\n" UCE-SAID  REJECTION:BUFFER-OFF
\ a used top-level package's public that is also a global word, where a part hides the package
REJECTION:CAPTURE
s" : UHX ( -- n ) 1 ; package UHS public : UHX ( -- n ) 9 ; ;package using UHS package UHT private package UHS public : ZW ( -- n ) 7 ; ;package : UHT-C ( -- n ) UHX ; ;package ;using" UCE-CATCH 70 T=
S\" error[E-USING-SHADOW-GLOBAL]: `UHX` is both a global word and a public of used package `UHS`\n" UCE-SAID
S\" = package: UHS:UHX\n" UCE-SAID
S\" = help: rename one of the two: the global word has no qualified form, and the part `UHT:UHS` hides package `UHS` here\n" UCE-SAID  REJECTION:BUFFER-OFF
\ order-swapped: using first, global defined later in the same scope, then bareword
REJECTION:CAPTURE
s" using USG : SW ( -- n ) 9 ; : USG-R2 ( -- n ) SW ; ;using" UCE-CATCH 70 T=
S\" error[E-USING-SHADOW-GLOBAL]: `SW` is both a global word and a public of used package `USG`\n" UCE-SAID
S\" ^^^^^^^^^ evaluates the text below\n" UCE-SAID
S\" = evaluated: using USG : SW ( -- n ) 9 ; : USG-R2 ( -- n ) SW ; ;using\n" UCE-SAID
S\"  ^^ ambiguous here\n" UCE-SAID
S\" = global: SW\n" UCE-SAID  S\" = package: USG:SW\n" UCE-SAID
S\" = help: write `USG:SW` to call the package's word, or rename one of the two: the global word has no qualified form\n" UCE-SAID  REJECTION:BUFFER-OFF
\ effects coincide: a silent bind to the global would certify; must reject
REJECTION:CAPTURE
s" using USG : USG-R3 ( -- n ) MW ; ;using" UCE-CATCH 70 T=
S\" error[E-USING-SHADOW-GLOBAL]: `MW` is both a global word and a public of used package `USG`\n" UCE-SAID
S\" ^^^^^^^^^ evaluates the text below\n" UCE-SAID
S\" = evaluated: using USG : USG-R3 ( -- n ) MW ; ;using\n" UCE-SAID
S\"  ^^ ambiguous here\n" UCE-SAID
S\" = global: MW\n" UCE-SAID  S\" = package: USG:MW\n" UCE-SAID
S\" = help: write `USG:MW` to call the package's word, or rename one of the two: the global word has no qualified form\n" UCE-SAID  REJECTION:BUFFER-OFF
\ qualified access is the escape: PKG:WORD reaches the used public and certifies
s" using USG : USG-R4 ( -- n ) USG:GW ; ;using" UCE-CATCH 0 T=
\ a used import with no global collision keeps resolving (purely-additive
\ contract)
s" using USG : USG-R5 ( -- n ) NC ; ;using" UCE-CATCH 0 T=
\ no using in scope: a bare global is never a shadow error (resolves to the global)
s" MW drop" UCE-CATCH 0 T=

\ The interpreter refuses the same collision at top level and as the operand of
\ `'`, by name, a throw its evaluate's caller catches. A qualified name, a tail
\ no used package exports, a word the open package defines itself and a read
\ after `;using` are unchanged. A buffer evaluated under its includer's using
\ resolves through it, so it refuses too.
s" using USG MW drop ;using" UCE-CATCH 70 T=
s" using USG ' MW drop ;using" UCE-CATCH 70 T=
s" using USG USG:MW drop ;using" UCE-CATCH 0 T=
s" using USG NC drop ;using" UCE-CATCH 0 T=
s" package USM : MW ( -- n ) 5 ; using USG MW drop ' MW drop ;using ;package" UCE-CATCH 0 T=
using USG
s" MW drop" UCE-CATCH 70 T=
;using
s" using USG ;using MW drop ' MW drop" UCE-CATCH 0 T=

\ An already compiled import keeps its binding when a global is defined later.
package USH public
   : SOLO ( -- n ) 7 ;
   : LATER ( n -- n ) 2 * ;
   : SOONER ( -- n ) 13 ;
;package
s" using USH : USH-R3 ( -- n ) SOLO ; ;using" UCE-CATCH 0 T=
USH-R3 7 T=
REJECTION:CAPTURE
s" using USH : LATER ( -- n ) 3 ; : USH-R4 ( -- n ) LATER ; ;using" UCE-CATCH 70 T=
S\" error[E-USING-SHADOW-GLOBAL]: `LATER` is both a global word and a public of used package `USH`\n" UCE-SAID
S\" ^^^^^^^^^ evaluates the text below\n" UCE-SAID
S\" = evaluated: using USH : LATER ( -- n ) 3 ; : USH-R4 ( -- n ) LATER ; ;using\n" UCE-SAID
S\"  ^^^^^ ambiguous here\n" UCE-SAID
S\" = global: LATER\n" UCE-SAID  S\" = package: USH:LATER\n" UCE-SAID
S\" = help: write `USH:LATER` to call the package's word, or rename one of the two: the global word has no qualified form\n" UCE-SAID  REJECTION:BUFFER-OFF
s" using USH : USH-R5 ( -- n ) SOONER ; ;using" UCE-CATCH 0 T=
USH-R5 13 T=
s" : SOONER ( -- n ) 99 ;" UCE-CATCH 0 T=
USH-R5 13 T=

\ An open package's own definition wins over an imported tail. The different
\ effect makes a wrong binding observable through rejection and execution.
package USP public : USPW ( n -- n ) 1 + ; ;package
s" package USQ using USP : USPW ( -- n ) 5 ; ;using ;package" UCE-CATCH 0 T=
s" package USQ using USP public : USQ-R1 ( -- n ) 41 USPW ; ;using ;package" UCE-CATCH 70 T=
s" package USQ using USP public : USQ-R2 ( -- n ) 41 USP:USPW ; ;using ;package" UCE-CATCH 0 T=
USQ:USQ-R2 42 T=

\ Package-private and public definitions both win over a same-named global.
\ These are ordinary compiled declarations with their real checked effects.
s" : OPK-G ( -- n ) 7 ;" UCE-CATCH 0 T=
s" package OPKA : OPK-G ( -- n ) 41 ; ;package" UCE-CATCH 0 T=
s" package OPKA public : OPKA-R ( -- n ) OPK-G ; ;package" UCE-CATCH 0 T=
OPKA:OPKA-R 41 T=
\ Another package still reaches the global and ordinary primitives.
s" package OPKB public : OPKB-R ( -- n ) OPK-G ; ;package" UCE-CATCH 0 T=
OPKB:OPKB-R 7 T=
s" package OPKB public : OPKB-P ( n -- n ) dup + ; ;package" UCE-CATCH 0 T=
7 OPKB:OPKB-P 14 T=

s" : OPK-H ( -- n ) 7 ;" UCE-CATCH 0 T=
s" package OPKC public : OPK-H ( -- n ) 41 ; ;package" UCE-CATCH 0 T=
s" package OPKC public : OPKC-R ( -- n ) OPK-H ; ;package" UCE-CATCH 0 T=
OPKC:OPKC-R 41 T=

\ Type imports use the same explicit package scope. A compiler-owned public
\ cell-id must not prevent an application from importing its own cell-id.
package UTA public NEWTYPE cell-id
NEWTYPE import-id
;package
package UTB public NEWTYPE import-id
;package
package UTP NEWTYPE secret-id
public : VISIBLE ( -- ) ; ;package
s" package UTC using UTA public : CELL ( cell-id -- cell-id ) ; : KEEP ( import-id -- import-id ) ; ;package" UCE-CATCH 0 T=
\ Compiled effects retain the imported family after its scope closes.
s" : UT-SAME ( UTA:cell-id -- UTA:cell-id ) UTC:CELL ;" UCE-CATCH 0 T=
s" : UT-WRONG ( UTB:import-id -- UTB:import-id ) UTC:KEEP ;" UCE-CATCH 70 T=
s" using UTA using UTA : UT-REPEAT ( import-id -- import-id ) ; ;using ;using" UCE-CATCH 0 T=
s" using UTA using UTB : UT-TAMB ( import-id -- import-id ) ; ;using ;using" UCE-CATCH 70 T=
s" using UTP : UT-SECRET ( secret-id -- secret-id ) ; ;using" UCE-CATCH 70 T=
\ A closed import no longer names its types.
s" using UTA ;using : UT-CLOSED ( import-id -- import-id ) ;" UCE-CATCH 70 T=
\ A public family no other package declares still needs its package imported
\ or named: bare outside it is refused, under `using` it resolves.
package UTD public NEWTYPE solo-id
;package
s" : UT-SOLO-BARE ( solo-id -- solo-id ) ;" UCE-CATCH 70 T=
s" using UTD : UT-SOLO-USED ( solo-id -- solo-id ) ; ;using" UCE-CATCH 0 T=
\ The current package still wins before imported families.
s" package UTB using UTA public : OWN ( import-id -- import-id ) ; ;package" UCE-CATCH 0 T=
s" : UT-OWN ( UTB:import-id -- UTB:import-id ) UTB:OWN ;" UCE-CATCH 0 T=

\ Binding to the package's different effect must reject the mismatched caller.
s" : OPK-K ( -- n ) 7 ;" UCE-CATCH 0 T=
s" package OPKD : OPK-K ( n -- n ) drop 41 ; ;package" UCE-CATCH 0 T=
s" package OPKD public : OPKD-R ( -- n ) OPK-K ; ;package" UCE-CATCH 70 T=

\ === a buffer closes only the usings it opens ===
\ A load file is a using scope: a buffer's `;using` that would close a using its
\ includer opened is refused by name, as inside a package. Closing UA for the
\ buffer would let its `using UB` take UA's slot, and the depth restored at the
\ buffer's end would leave the includer resolving BW where AW had been, on the
\ clean and the throwing path alike. A using the buffer opened closes, and the
\ verifier's replay refuses the inherited one the same way.
using UA
s" ;using using UB" UCE-CATCH 70 T=
s" ;using using UB NO-SUCH-WORDX" UCE-CATCH 70 T=
s" using UB ;using ;using" UCE-CATCH 70 T=
s" AW drop" UCE-CATCH 0 T=
s" BW drop" UCE-CATCH 70 T=
s" using UB BW drop ;using AW drop" UCE-CATCH 0 T=
REJECTION:CAPTURE
s" ;using" VS-CATCH 70 T=
S\" error[E-USING-OUTER]: `;using` would close a using opened outside this file\n" UCE-SAID
S\"  ^^^^^^ using opened outside\n" UCE-SAID
S\" = help: close each using in the file that opens it\n" UCE-SAID  REJECTION:BUFFER-OFF
s" using UB ;using" VS-CATCH 0 T=
;using

\ === a buffer closes only the packages it opens ===
package UQK
REJECTION:CAPTURE
s" ;package" UCE-CATCH 70 T=
S\" error[E-PKG-CONTEXT]: `;package` closes `UQK`, which this file or string did not open\n" UCE-SAID
s" ;package" VS-CATCH 70 T=
REJECTION:BUFFER-OFF
;package

\ === a retired used public is no candidate ===
\ `undefine` retires a used package's public. The engine's used-search skips it
\ and binds the global or the one live public, else nothing; the checker binds
\ through that search (src/core/checker.f LIVE-BIND), so a checked body
\ certifies against the word the engine binds and runs it, and a tail with
\ nothing left to bind is undefined. The retired publics' effects differ from
\ the words bound in their place, so a body certified against a retired one is
\ refused. A checker that found the retired symbol would refuse the first two
\ shapes as E-USING-SHADOW-GLOBAL and E-USING-AMBIGUOUS.
package URS public : drop ( n n -- ) 2drop ; undefine drop ;package
package URA public : URW ( n -- n ) 1 + ; ;package
package URB public : URW ( n -- ) drop ; undefine URW ;package
package URN public : URNW ( -- n ) 3 ; undefine URNW ;package
package USING-TEST public variable URS-V ;package
\ a retired public shadowing global drop: the body's drop is the global ( x -- )
s" using URS : URS-R1 ( n n -- n ) drop ; ;using 5 7 URS-R1 USING-TEST:URS-V !" UCE-CATCH 0 T=
USING-TEST:URS-V @ 5 T=
\ two used publics of one tail, one retired: URW is URA's ( n -- n )
s" using URA using URB : URA-R1 ( n -- n ) URW ; ;using ;using 41 URA-R1 USING-TEST:URS-V !" UCE-CATCH 0 T=
USING-TEST:URS-V @ 42 T=
\ a retired public with no global: undefined on both routes
s" using URN : URN-R1 ( -- n ) URNW ; ;using" UCE-CATCH 70 T=
s" using URN : URN-R2 ( -- n ) URNW ; ;using" VS-CATCH 70 T=

\ === a part of the open package ===
\ Inside package UPA, `using UPB` names UPA's part UPB: its publics are bare
\ until `;using`, on the engine and in the verifier's replay. At top level no
\ name reaches the part.
s" package UPA package UPB public : UPX ( -- n ) 6 ; ;package ;package" UCE-CATCH 0 T=
package USING-TEST public variable UPT-V ;package
s" package UPA using UPB : UPR ( -- n ) UPX ; ;using UPR USING-TEST:UPT-V ! ;package" UCE-CATCH 0 T=
USING-TEST:UPT-V @ 6 T=
s" package UPA using UPB ;using UPX drop ;package" UCE-CATCH 70 T=
s" package UPA using UPB : UPV ( -- n ) UPX ; ;using ;package" VS-CATCH 0 T=
s" using UPB" UCE-CATCH 70 T=

\ === an owned primitive is no global ===
\ STR's `len` is an engine primitive only STR binds: under `using SPAN` a bare
\ LEN is SPAN's.
package USING-TEST public : SPAN4 ( -- SPAN:span<u8> ) s" abcd" dup STR:DATA swap STR:LEN SPAN:MAKE ; ;package
s" an owned primitive leaves a used public's bare tail to it" T-LABEL
s" package USPN using SPAN public : USPN-R ( -- n ) USING-TEST:SPAN4 LEN ; ;using ;package" UCE-CATCH 0 T=
USPN:USPN-R 4 T=  T-LABEL-CLEAR

\ === a definition reusing an ambiguous used name ===
\ A definition binds its own pending name first, so a global AW is admitted
\ while UA and UC both export AW, at each available tier, and runs as itself. A
\ reference to the bare AW stays ambiguous, in a checked body and as a
\ top-level token: both are E-USING-AMBIGUOUS. This section comes last
\ because it defines a global AW, which the cases above need absent.
package USING-TEST public variable UAD-V ;package
s" using UA using UC : AW ( -- n ) 1 ; ;using ;using AW USING-TEST:UAD-V !" UCE-CATCH 0 T=
USING-TEST:UAD-V @ 1 T=
REJECTION:CAPTURE
s" using UA using UC : UAD-R1 ( -- n ) AW ; ;using ;using" UCE-CATCH 70 T=
S\" error[E-USING-AMBIGUOUS]: `AW` is public in more than one used package\n" UCE-SAID
S\" ^^^^^^^^^ evaluates the text below\n" UCE-SAID
S\" = evaluated: using UA using UC : UAD-R1 ( -- n ) AW ; ;using ;using\n" UCE-SAID
S\"  ^^ ambiguous here\n" UCE-SAID
S\" = packages: UA, UC\n" UCE-SAID
S\" = help: write the package's name before the word, as `UA:AW` or `UC:AW`\n" UCE-SAID  REJECTION:BUFFER-OFF
s" using UA using UC AW drop ;using ;using" UCE-CATCH 70 T=
s" undefine AW" UCE-CATCH 0 T=

T-REPORT
