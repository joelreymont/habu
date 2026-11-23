\ cast-negative-suite.f - reject contract for the CAST: checked retype declarer.
\ Run by the engine over stdin:
\     bin/hb < test/cast-negative-suite.f
\ The native registry runs it as a positive case: every reject is asserted
\ in-process, so the process exits 0 with clean stderr.

require src/habu/verify-source.f
require src/lib/string.f
require src/lib/test/subject.f
require src/lib/test.f
require src/lib/test/rejection-text.f

T-RESET

\ Silences the expected rejection diagnostics: the production declarer emits one
\ per refusal.
REJECTION:CAPTURE

package CN
public
NEWTYPE cnfam
NEWTYPE cncell<a>
;package
s" STRUCTURE cnpbox<a> FIELD value a ;STRUCTURE" evaluate-closed
package CAST-NEG
public
DEFLINEAR CAST-NEG:lease
STRUCTURE nested FIELD owner CAST-NEG:lease ;STRUCTURE
STRUCTURE wide FIELD lo n FIELD hi n ;STRUCTURE
;package

\ Evaluates one declaration through the engine's `cast:` keyword and the live
\ checker; the answer is the status a rejection stopped with, its record left
\ in the capture, or 0 when the cast was published. A rejection leaves no name
\ behind.
package CN-RUN
public
TYPED-VARIABLE SRC-A ^u8
variable SRC-U
: EVAL ( -- ) SRC-A @ SRC-U @ >str evaluate-closed ;
: DECL ( str -- n )               \ ( decl-source -- status | 0 )
   str> SRC-U !  SRC-A !
   REJECTION:CAPTURE
   [: EVAL ;] catch ;
\ The status was the checker's and the captured record carries the code and
\ the word.
: REJECTS ( n str str -- ) {: rc:n c w :}
   rc 70 T=
   REJECTION:BUFFER$ c CONTAINS? TTRUE
   REJECTION:BUFFER$ w CONTAINS? TTRUE ;
;package
\ arity: more than one input, more than one output term, or a return-stack
\ effect. A record is placed at the word the line ran.
s" cast: CNA1 ( n n -- CN:cnfam )"              CN-RUN:DECL s" error[E-CAST-ARITY]" s" `cast: CNA1`" CN-RUN:REJECTS
REJECTION:BUFFER$ s" ^^^^ input not one term" CONTAINS? TTRUE
s" cast: CNA2 ( n -- CN:cnfam CN:cnfam )"       CN-RUN:DECL s" error[E-CAST-ARITY]" s" `cast: CNA2`" CN-RUN:REJECTS
REJECTION:BUFFER$ s" error[E-CAST-ARITY]: in `cast: CNA2`, the output is not one term over the stack below" CONTAINS? TTRUE
REJECTION:BUFFER$ s" cast-negative-suite.f" 49 s" cast: CNA2 ( n -- CN:cnfam CN:cnfam )" T-PLACED
REJECTION:BUFFER$ s\" ^^^^^^^^^^^ evaluates the text below\n= evaluated: cast: CNA2 ( n -- CN:cnfam CN:cnfam )\n                   ^^^^ output not one term\n= signature: n -- CN:cnfam CN:cnfam\n= help: give exactly one term as the output, as `( n -- u8 )`, with one declaration per term\n" REJECTION-TEXT:HAS? TTRUE
s" cast: CNA3 ( n | U -- n | U )"               CN-RUN:DECL s" error[E-CAST-ARITY]" s" `cast: CNA3`" CN-RUN:REJECTS
REJECTION:BUFFER$ s" error[E-CAST-ARITY]: in `cast: CNA3`, the signature declares a return-stack effect" CONTAINS? TTRUE
REJECTION:BUFFER$ s" cast-negative-suite.f" 49 s" cast: CNA3 ( n | U -- n | U )" T-PLACED
REJECTION:BUFFER$ s\" ^^^^^^^^^^^ evaluates the text below\n= evaluated: cast: CNA3 ( n | U -- n | U )\n                   ^^^^ a return-stack effect\n= signature: n | U -- n | U\n= help: write the signature with no `|` clause\n" REJECTION-TEXT:HAS? TTRUE
\ class: an atom is a phantom index, not a cell, and a pointer to one names no
\ storage. A layout value wider than a cell is one term; a cast between values
\ of different widths is refused as arity before its class is asked.
s" cast: CNC1 ( n -- extent-a )"     CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNC1`" CN-RUN:REJECTS
s" cast: CNC2 ( n -- ^extent-a )" CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNC2`" CN-RUN:REJECTS
s" cast: CNC3 ( n -- CAST-NEG:wide )" CN-RUN:DECL s" error[E-CAST-ARITY]" s" `cast: CNC3`" CN-RUN:REJECTS
REJECTION:BUFFER$ s" the input and output differ in width" CONTAINS? TTRUE
REJECTION:BUFFER$ s" ^^^^ `n` is 1 cell, `cast-neg:wide` is 2" CONTAINS? TTRUE
REJECTION:BUFFER$ s" = help: give both sides the same width in cells: `n` is 1 cell, `cast-neg:wide` is 2" CONTAINS? TTRUE
\ Nor does the destination introduce an atom where a call, a read or a field
\ projection yields a value: a produced row, a pointee and a layout argument
\ refuse one as the bare term does, even in a private section, where the mint
\ itself certifies. A quotation that only consumes an atom introduces none
\ unless it is a pointee.
package CN-ATOM
s" cast: CNC4 ( n -- [ -- extent-a ] )"     CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNC4`" CN-RUN:REJECTS
s" cast: CNC5 ( n -- ^[ -- extent-a ] )" CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNC5`" CN-RUN:REJECTS
s" cast: CNC6 ( n -- cnpbox<extent-a> )"    CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNC6`" CN-RUN:REJECTS
s" cast: CNC7 ( n -- [ extent-a -- ] )"     CN-RUN:DECL 0 T=
;package
\ No cast yields a width<t>, bare or behind a pointer, even in a private
\ section. A cast out of one reads its bytes.
s" cast: CNWD1 ( n -- width<n> )"     CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNWD1`" CN-RUN:REJECTS
package CN-WIDTH
s" cast: CNWD2 ( n -- width<n> )"     CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNWD2`" CN-RUN:REJECTS
s" cast: CNWD3 ( n -- ^width<n> )" CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNWD3`" CN-RUN:REJECTS
;package
s" cast: CNWD4 ( width<n> -- n )"     CN-RUN:DECL 0 T=
\ A quotation's consumed rows are fed by the cast's result: none is handed an
\ atom or a width. A width the source produces is read, not introduced.
package CN-WIDTH
private
s" cast: CNWD5 ( [ width<n> -- n ] -- [ n -- n ] )" CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNWD5`" CN-RUN:REJECTS
s" cast: CNWD6 ( [ xt -- ] -- [ n -- ] )"           CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNWD6`" CN-RUN:REJECTS
s" cast: CNWD7 ( [ extent-a -- ] -- [ n -- ] )"     CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNWD7`" CN-RUN:REJECTS
s" cast: CNWD8 ( [ n -- width<n> ] -- [ n -- n ] )" CN-RUN:DECL 0 T=
\ A pointee is written as well as read: every position in it is checked.
s" cast: CNWD9 ( ^[ -- width<n> ] -- ^[ -- n ] )"   CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNWD9`" CN-RUN:REJECTS
s" cast: CNWD10 ( ^[ n -- ] -- ^[ width<n> -- ] )"  CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNWD10`" CN-RUN:REJECTS
s" cast: CNWD12 ( ^[ n -- ] -- ^[ xt -- ] )"        CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNWD12`" CN-RUN:REJECTS
s" cast: CNWD13 ( ^[ -- xt ] -- ^[ -- n ] )"        CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNWD13`" CN-RUN:REJECTS
;package
\ undeclared family in the signature.
s" cast: CNF1 ( n -- neverdecl )"    CN-RUN:DECL s" error[E-CAST-FAM]" s" `neverdecl`" CN-RUN:REJECTS
\ A family applied to the wrong number of arguments is malformed the same way:
\ the declaration is refused by name, not by a later death.
s" cast: CNF2 ( n -- cnpbox )"       CN-RUN:DECL s" error[E-CAST-FAM]" s" `cnpbox`" CN-RUN:REJECTS
s" cast: CNF3 ( n -- ^cnpbox )"   CN-RUN:DECL s" error[E-CAST-FAM]" s" `cnpbox`" CN-RUN:REJECTS
s" cast: CNF4 ( cnpbox<n,n> -- n )"  CN-RUN:DECL s" error[E-CAST-FAM]" s" `cnpbox`" CN-RUN:REJECTS
REJECTION:BUFFER$ s" = help: give `cnpbox` 1 type argument" CONTAINS? TTRUE
\ A cast's help is a definition's: `ptr` is told how to spell a pointer.
s" cast: CNF5 ( ptr -- n )"          CN-RUN:DECL s" error[E-CAST-FAM]" s" `ptr`" CN-RUN:REJECTS
REJECTION:BUFFER$ s" = help: spell a pointer `^T`, `^` joined to the type it points at, as in `^u8`" CONTAINS? TTRUE
\ Any other signature fault, here a bare `^`, is refused before a walk reads
\ the row, as a stored signature that does not parse is. The first fault names
\ the class, so CNB1's wrong-arity `cnpbox` never reaches a walk.
s" cast: CNB1 ( ^ -- cnpbox )"     CN-RUN:DECL s" error[E-BAD-STORED-SIGNATURE]" s" `CNB1`" CN-RUN:REJECTS
s" cast: CNB2 ( n -- ^ )"          CN-RUN:DECL s" error[E-BAD-STORED-SIGNATURE]" s" `CNB2`" CN-RUN:REJECTS
\ Neither direction, linear-to-linear, nor transitive containment may cross
\ CAST:, even when both sides occupy one machine cell.
s" cast: CNL1 ( n -- CAST-NEG:lease )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL1`" CN-RUN:REJECTS
s" cast: CNL2 ( CAST-NEG:lease -- n )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL2`" CN-RUN:REJECTS
s" cast: CNL3 ( CAST-NEG:lease -- CAST-NEG:lease )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL3`" CN-RUN:REJECTS
s" cast: CNL4 ( n -- CAST-NEG:nested )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL4`" CN-RUN:REJECTS
s" cast: CNL5 ( CAST-NEG:nested -- n )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL5`" CN-RUN:REJECTS
\ A variable only behind a pointer binds no linear value, so the cast admits it;
\ one a quotation produces may bind one. `( n -- ^a )` mints a pointer.
s" cast: CNL6 ( n -- ^a )"        CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNL6`" CN-RUN:REJECTS
s" cast: CNL7 ( ^a -- n )"        CN-RUN:DECL 0 T=
s" cast: CNL8 ( n -- [ -- a ] )"     CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL8`" CN-RUN:REJECTS
\ A quotation whose rows end in different tails hands out values it never took;
\ a cast's input only reads it.
s" cast: CNL19 ( n -- [ R -- S ] )"  CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL19`" CN-RUN:REJECTS
s" cast: CNL20 ( [ R -- S ] -- n )"  CN-RUN:DECL 0 T=
\ A private section admits the mint under a bare name only.
package CN-STORED
s" cast: CNL18 ( n -- ^a )"       CN-RUN:DECL 0 T=
s" cast: CNQ:FORGE ( n -- ^u8 )"  CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNQ:FORGE`" CN-RUN:REJECTS
s" cast: CNL21 ( ^u8 -- ^[ R -- S ] )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL21`" CN-RUN:REJECTS
s" cast: CNL22 ( ^u8 -- ^[ R -- R n ] )" CN-RUN:DECL 0 T=
;package
\ A linear type behind a pointer, or in either of a quotation's rows, is
\ carried ownership too, on either side of the cast.
s" cast: CNL9 ( n -- ^^CAST-NEG:lease )"      CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL9`" CN-RUN:REJECTS
s" cast: CNL10 ( n -- [ CAST-NEG:lease -- ] )"      CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL10`" CN-RUN:REJECTS
s" cast: CNL12 ( [ -- CAST-NEG:nested ] -- n )"     CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL12`" CN-RUN:REJECTS
\ A layout's fields and a sum's variants are what its projection yields, so one
\ holding a linear value behind a pointer or in a quotation row carries it
\ through the cast as well, on either side, and a private section does not
\ change that.
SUMTYPE lpvbox VARIANT hold ^CAST-NEG:lease ;VARIANT ;SUMTYPE
STRUCTURE lqfbox FIELD q [ -- CAST-NEG:lease ] ;STRUCTURE
package CN-LIN
s" cast: CNL13 ( n -- ^lpvbox )"  CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL13`" CN-RUN:REJECTS
s" cast: CNL14 ( n -- lqfbox )"      CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL14`" CN-RUN:REJECTS
s" cast: CNL15 ( n -- ^lqfbox )"  CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL15`" CN-RUN:REJECTS
s" cast: CNL16 ( ^lpvbox -- n )"  CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL16`" CN-RUN:REJECTS
s" cast: CNL17 ( lqfbox -- n )"      CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNL17`" CN-RUN:REJECTS
;package
\ The production declarer path rejects the same foreign-package forgeries and
\ rolls every failed word back out of the dictionary.
package CAST-FOREIGN
variable CN-WID
get-current CN-WID !
: CN-ABSENT? ( str -- bool )
   CN-WID @ search-wl 0= ;
: CN-PRIVATE-PROBE ( -- ) ;
\ WID 0 cannot observe a package-private word, so it cannot prove rollback.
\ The captured private WID sees the probe and must see no failed cast.
s" CN-PRIVATE-PROBE" 0 search-wl 0= TTRUE
s" CN-PRIVATE-PROBE" CN-ABSENT? TFALSE
s" CAST: CNP1 ( n -- CAST-NEG:lease )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNP1`" CN-RUN:REJECTS
s" CNP1" CN-ABSENT? TTRUE  s" CNP1" 0 search-wl 0= TTRUE
s" CAST: CNP2 ( CAST-NEG:lease -- n )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNP2`" CN-RUN:REJECTS
s" CNP2" CN-ABSENT? TTRUE  s" CNP2" 0 search-wl 0= TTRUE
s" CAST: CNP3 ( CAST-NEG:lease -- CAST-NEG:lease )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNP3`" CN-RUN:REJECTS
s" CNP3" CN-ABSENT? TTRUE  s" CNP3" 0 search-wl 0= TTRUE
s" CAST: CNP4 ( n -- CAST-NEG:nested )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNP4`" CN-RUN:REJECTS
s" CNP4" CN-ABSENT? TTRUE  s" CNP4" 0 search-wl 0= TTRUE
s" CAST: CNP5 ( CAST-NEG:nested -- n )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNP5`" CN-RUN:REJECTS
s" CNP5" CN-ABSENT? TTRUE  s" CNP5" 0 search-wl 0= TTRUE
s" CAST: CNP6 ( n -- cnpbox<CAST-NEG:lease> )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNP6`" CN-RUN:REJECTS
s" CNP6" CN-ABSENT? TTRUE  s" CNP6" 0 search-wl 0= TTRUE
s" CAST: CNP7 ( cnpbox<cnpbox<CAST-NEG:lease>> -- n )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNP7`" CN-RUN:REJECTS
s" CNP7" CN-ABSENT? TTRUE  s" CNP7" 0 search-wl 0= TTRUE
s" CAST: CNP8 ( n -- a )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNP8`" CN-RUN:REJECTS
s" CNP8" CN-ABSENT? TTRUE  s" CNP8" 0 search-wl 0= TTRUE
s" CAST: CNP9 ( n -- cnpbox<cnpbox<a>> )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNP9`" CN-RUN:REJECTS
s" CNP9" CN-ABSENT? TTRUE  s" CNP9" 0 search-wl 0= TTRUE
\ A view's element is a cast term: the unpack hands out whatever t binds.
s" CAST: CNP10 ( read-view<p,q,t> -- ^u8 n )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNP10`" CN-RUN:REJECTS
s" CNP10" CN-ABSENT? TTRUE  s" CNP10" 0 search-wl 0= TTRUE
s" CAST: CNP11 ( read-view<p,q,CAST-NEG:nested> -- ^u8 n )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNP11`" CN-RUN:REJECTS
s" CNP11" CN-ABSENT? TTRUE  s" CNP11" 0 search-wl 0= TTRUE
s" CAST: CNP12 ( read-view<p,q,[ R -- S ]> -- ^u8 n )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNP12`" CN-RUN:REJECTS
s" CNP12" CN-ABSENT? TTRUE  s" CNP12" 0 search-wl 0= TTRUE
\ Where the output mints, an input's row variable keeps its place; an
\ output's own is the linear rule's.
s" CAST: CNP13 ( [ -- n ] -- [ -- S ] )" CN-RUN:DECL s" error[E-CAST-LINEAR]" s" `cast: CNP13`" CN-RUN:REJECTS
s" CNP13" CN-ABSENT? TTRUE
s" CAST: CNP14 ( [ R -- S ] -- [ S -- R ] )" CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNP14`" CN-RUN:REJECTS
s" CNP14" CN-ABSENT? TTRUE
s" CAST: CNP15 ( [ R -- S ] -- [ n -- ] )" CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNP15`" CN-RUN:REJECTS
s" CNP15" CN-ABSENT? TTRUE
s" CAST: CNP17 ( [ R -- R ] -- [ R n -- R ] )" CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNP17`" CN-RUN:REJECTS
s" CNP17" CN-ABSENT? TTRUE
s" CAST: CNP18 ( [ R n -- R ] -- [ R ^n -- R ] )" CN-RUN:DECL 0 T=
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
ENUM fqlist VARIANT more FIELD v CN-FO:fofam FIELD next ^fqlist ;VARIANT VARIANT last ;VARIANT ;ENUM
private
s" cast: CNW9 ( fqbox -- n )"            CN-RUN:DECL 0 T=
\ An ENUM variant may point at its own family. The walk reads each instance once
\ per path, so a cast into a list or out of one terminates.
s" cast: CNW13 ( ^fqlist -- n )"      CN-RUN:DECL 0 T=
s" cast: CNW17 ( n -- ^fqlist )"      CN-RUN:DECL 0 T=
;package
\ No package makes an enum or sum value from a cell, its owner included, nor
\ hands out a type variable a caller may bind to one; any projects out, a view of storage
\ is the mint rule's, and a layout argument counts through its members.
package CN-EN
public
ENUM cnshade DERIVE eq dark light ;ENUM
SUMTYPE cnopt VARIANT some n ;VARIANT VARIANT none ;VARIANT ;SUMTYPE
STRUCTURE cnrec FIELD s cnshade ;STRUCTURE
STRUCTURE cnqb<a> FIELD f [ -- a ] ;STRUCTURE
STRUCTURE cnqc<a> FIELD f [ a -- ] ;STRUCTURE
STRUCTURE cnph<a> FIELD x n ;STRUCTURE
STRUCTURE cnpr<a> FIELD p ^a ;STRUCTURE
ENUM cnnul<a> POLICY niche-null VARIANT none ;VARIANT VARIANT some FIELD value ^a ;VARIANT ;ENUM
private
s" cast: CNN2 ( n -- cnshade )"          CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNN2`" CN-RUN:REJECTS
s" cast: CNN4 ( n -- ^cnopt )"        CN-RUN:DECL 0 T=
s" cast: CNN5 ( n -- [ -- cnshade ] )"   CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNN5`" CN-RUN:REJECTS
s" cast: CNN6 ( [ cnshade -- ] -- [ n -- ] )" CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNN6`" CN-RUN:REJECTS
s" cast: CNN7 ( [ cnshade -- ] -- n )"   CN-RUN:DECL 0 T=
s" cast: CNN8 ( n -- [ -- cnopt ] )"     CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNN8`" CN-RUN:REJECTS
s" cast: CNN9 ( n -- cnrec )"            CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNN9`" CN-RUN:REJECTS
s" cast: CNN10 ( n -- cnqb<t> )"         CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNN10`" CN-RUN:REJECTS
s" cast: CNN11 ( cnqc<t> -- [ n -- ] )"  CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNN11`" CN-RUN:REJECTS
s" cast: CNN12 ( n -- cnph<cnshade> )"   CN-RUN:DECL 0 T=
s" cast: CNN13 ( ^u8 -- cnpr<cnshade> )" CN-RUN:DECL 0 T=
s" cast: CNN14 ( ^u8 -- cnnul<u8> )"  CN-RUN:DECL 0 T=
\ A pointer output, even to a quotation, feeds the source quotation nothing.
s" cast: CNN15 ( [ cnshade -- ] -- ^[ n -- ] )" CN-RUN:DECL 0 T=
\ A consumed row inside a layout argument keeps the argument bit.
s" cast: CNN16 ( n -- cnph<[ [ cnshade -- ] -- ]> )" CN-RUN:DECL 0 T=
\ A layout argument hands out no quotation, and its consumed rows introduce nothing.
s" cast: CNN17 ( [ cnshade -- ] -- cnph<[ extent-a -- ]> )" CN-RUN:DECL 0 T=
\ A caller binds a stored variable to a two-cell type: the walks leave its kind.
s" cast: CNK1 ( n -- ^cnqb<t> ) : CNK1-USE ( ^cnqb<CAST-NEG:wide> -- ) drop ; : CNK1-GO ( -- ) 0 CNK1 CNK1-USE ;" CN-RUN:DECL 0 T=
;package
package CN-EZ
s" cast: CNN1 ( n -- CN-EN:cnshade )"    CN-RUN:DECL s" error[E-CAST-CLASS]" s" `cast: CNN1`" CN-RUN:REJECTS
s" cast: CNN3 ( CN-EN:cnshade -- n )"    CN-RUN:DECL 0 T=
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
s" cast: CNQ10 ( ^u8 -- ^CN-OQ:oqbox )"    CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNQ10`" CN-RUN:REJECTS
s" cast: CNQ13 ( n -- CN:cnfam )"                CN-RUN:DECL 0 T=
private
s" cast: CNQ11 ( ^u8 -- ^CN-OQ:oqbox )"    CN-RUN:DECL 0 T=
s" cast: CNQ12 ( n -- ohold )"                   CN-RUN:DECL 0 T=
s" cast: CNQ4 ( CN-OQ:oqbox -- n )"              CN-RUN:DECL 0 T=
s" cast: CNQ5 ( ^CN-OQ:oqbox -- ^u8 )"     CN-RUN:DECL 0 T=
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
s" cast: CNM1 ( n -- ^u8 )"                    CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNM1`" CN-RUN:REJECTS
s" CNM1" 0 search-wl 0= TTRUE
s" cast: CNM2 ( n -- [ n -- n ] )"                CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNM2`" CN-RUN:REJECTS
s" cast: CNM3 ( ^u8 -- ^n )"                CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNM3`" CN-RUN:REJECTS
\ A layout argument is what a field projection yields, so an instance whose
\ argument is a pointer or a quotation, at any depth, is the same mint.
s" cast: CNM11 ( n -- cnpbox<^u8> )"           CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNM11`" CN-RUN:REJECTS
s" cast: CNM12 ( n -- cnpbox<[ n -- n ]> )"       CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNM12`" CN-RUN:REJECTS
s" cast: CNM13 ( n -- cnpbox<cnpbox<^u8>> )"   CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNM13`" CN-RUN:REJECTS
\ A layout's own field is projected the same way, so a field of pointer or
\ quotation type, at any depth of nesting, is the same mint.
STRUCTURE pfbox FIELD p ^u8 ;STRUCTURE
STRUCTURE qfbox FIELD q [ n -- n ] ;STRUCTURE
STRUCTURE pfnest FIELD inner pfbox ;STRUCTURE
s" cast: CNM17 ( n -- pfbox )"                   CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNM17`" CN-RUN:REJECTS
s" cast: CNM18 ( n -- qfbox )"                   CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNM18`" CN-RUN:REJECTS
s" cast: CNM19 ( n -- pfnest )"                  CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNM19`" CN-RUN:REJECTS
package CN-MINT
public
s" cast: CNM4 ( n -- ^u8 )"                    CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNM4`" CN-RUN:REJECTS
s" cast: CNM5 ( n -- [ -- ] )"                    CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNM5`" CN-RUN:REJECTS
s" cast: CNM14 ( n -- cnpbox<^u8> )"           CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNM14`" CN-RUN:REJECTS
s" cast: CNM15 ( n -- cnpbox<[ n -- n ]> )"       CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNM15`" CN-RUN:REJECTS
s" cast: CNM20 ( n -- pfbox )"                   CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNM20`" CN-RUN:REJECTS
private
s" cast: CNM6 ( n -- ^u8 )"                    CN-RUN:DECL 0 T=
s" cast: CNM7 ( n -- [ -- ] )"                    CN-RUN:DECL 0 T=
\ A pointee carries no width rule: a layout of any width is one pointer away.
s" cast: CNM10 ( n -- ^CAST-NEG:wide )"        CN-RUN:DECL 0 T=
s" cast: CNM21 ( n -- pfbox )"                   CN-RUN:DECL 0 T=
s" cast: CNM22 ( n -- qfbox )"                   CN-RUN:DECL 0 T=
s" cast: CNM23 ( n -- pfnest )"                  CN-RUN:DECL 0 T=
;package
\ Projections out of a pointer, a quotation or a layout holding one mint
\ nothing: any scope.
s" cast: CNM8 ( ^u8 -- n )"                    CN-RUN:DECL 0 T=
s" cast: CNM9 ( [ n -- n ] -- n )"                CN-RUN:DECL 0 T=
s" cast: CNM16 ( cnpbox<^u8> -- n )"           CN-RUN:DECL 0 T=
s" cast: CNM24 ( pfbox -- n )"                   CN-RUN:DECL 0 T=

\ scope: a cast with a scoped term on either side is E-CAST-SCOPE, asked before
\ the arity rule, which would read a view's two cells as two terms, except the
\ view casts VIEW-CAST-CERTIFY admits. Each refusal leaves no name in the
\ private wordlist, which sees the private probe.
package CN-VIEW
variable CV-WID
get-current CV-WID !
: CV-ABSENT? ( str -- bool )
   CV-WID @ search-wl 0= ;
: CV-PRIVATE-PROBE ( -- ) ;
s" CV-PRIVATE-PROBE" CV-ABSENT? TFALSE
s" cast: CNV1 ( ^u8 n -- read-view<p,q,u8> )"        CN-RUN:DECL s" error[E-CAST-SCOPE]" s" `cast: CNV1`" CN-RUN:REJECTS
s" CNV1" CV-ABSENT? TTRUE
s" cast: CNV2 ( ^u8 n -- mut-view<p,q,a,u8> )"       CN-RUN:DECL s" error[E-CAST-SCOPE]" s" `cast: CNV2`" CN-RUN:REJECTS
s" CNV2" CV-ABSENT? TTRUE
s" cast: CNV3 ( mut-view<p,l,a,u8> -- ^u8 n )"       CN-RUN:DECL s" error[E-CAST-SCOPE]" s" `cast: CNV3`" CN-RUN:REJECTS
s" CNV3" CV-ABSENT? TTRUE
s" cast: CNV4 ( mut-view<p,l,a,u8> -- read-view<p,l,u8> )" CN-RUN:DECL s" error[E-CAST-SCOPE]" s" `cast: CNV4`" CN-RUN:REJECTS
s" CNV4" CV-ABSENT? TTRUE
s" cast: CNV5 ( read-view<p,q,u8> -- n )"               CN-RUN:DECL s" error[E-CAST-SCOPE]" s" `cast: CNV5`" CN-RUN:REJECTS
s" CNV5" CV-ABSENT? TTRUE
s" cast: CNV6 ( [ mut-view<p,p,fresh-region-b,u8> -- ] -- [ ^u8 n -- ] )" CN-RUN:DECL s" error[E-CAST-SCOPE]" s" `cast: CNV6`" CN-RUN:REJECTS
s" CNV6" CV-ABSENT? TTRUE
s" cast: CNV10 ( [ read-view<p,q,u8> -- ] -- [ ^u8 n -- ] )" CN-RUN:DECL s" error[E-CAST-SCOPE]" s" `cast: CNV10`" CN-RUN:REJECTS
s" CNV10" CV-ABSENT? TTRUE
s" cast: CNV7 ( n -- init<i,n> )"                       CN-RUN:DECL s" error[E-CAST-SCOPE]" s" `cast: CNV7`" CN-RUN:REJECTS
s" CNV7" CV-ABSENT? TTRUE
\ A qualified name selects that package's public wordlist, so the read-view
\ unpack this section admits is refused under one, and once the package closes
\ that wordlist holds no such name.
s" cast: CN-VIEW:CNV9 ( read-view<p,q,u8> -- ^u8 n )" CN-RUN:DECL s" error[E-CAST-SCOPE]" s" `cast: CN-VIEW:CNV9`" CN-RUN:REJECTS
public
get-current constant CV-PUBLIC-WID
s" cast: CNV8 ( read-view<p,q,u8> -- ^u8 n )"        CN-RUN:DECL s" error[E-CAST-SCOPE]" s" `cast: CNV8`" CN-RUN:REJECTS
;package
s" CNV9" CN-VIEW:CV-PUBLIC-WID search-wl 0= TTRUE
\ The stored-signature intake still refuses an output variable no input
\ supplies; a view whose variables its input supplies is a well-formed row.
s" defer CNVD1 ( ^u8 n -- read-view<p,q,u8> )"       CN-RUN:DECL s" error[E-BAD-STORED-SIGNATURE]" s" `CNVD1`" CN-RUN:REJECTS
s" defer CNVD2 ( read-view<p,q,u8> -- read-view<p,q,u8> )" CN-RUN:DECL 0 T=
\ The scope is the engine's live one: a cast at top level is rejected.
s" cast: CNSP3 ( n -- ^u8 )"                   CN-RUN:DECL s" error[E-CAST-MINT]" s" `cast: CNSP3`" CN-RUN:REJECTS

\ The family follows the live engine namespace.
package CN-SPOOF-A
public
NEWTYPE spoof
s" cast: CNSA0 ( n -- CN-SPOOF-A:spoof )" CN-RUN:DECL 0 T=
;package

\ Visibility comes from the real current WID. A declaration in a private
\ section stays private; only its owner resolves it.
package CN-VIS-PRI
NEWTYPE hidden
s" cast: CNVP0 ( n -- hidden )" CN-RUN:DECL 0 T=
;package
package CN-VIS-FOREIGN
s" cast: CNVP1 ( n -- CN-VIS-PRI:hidden )" CN-RUN:DECL s" error[E-CAST-FAM]" s" `CN-VIS-PRI:hidden`" CN-RUN:REJECTS
s" cast: CNVP2 ( n -- hidden )" CN-RUN:DECL s" error[E-CAST-FAM]" s" `hidden`" CN-RUN:REJECTS
;package

\ A declaration in a public section is public: foreign code resolves it.
package CN-VIS-PUB
public
NEWTYPE shown
;package
package CN-VIS-FOREIGN
s" cast: CNVU0 ( CN-VIS-PUB:shown -- n )" CN-RUN:DECL 0 T=
;package

\ EXPORT records the package it runs in.
package CN-EXP-SRC-PKG
public
: CN-EXP-SRC ( n -- n ) ;
;package
package CN-EXP-A
public
EXPORT CN-EXP-SRC-PKG:CN-EXP-SRC
;package
s" CNEX0 ( n -- n ) CN-EXP-A:CN-EXP-SRC" CHECK-QUIET-CANDIDATE! -1 T=

\ Offline verification replays a package in the verifier window's own scope. A
\ normal simulated package verifies, restores the live provider, and leaves no
\ family behind.
package CN-CAST-TEST
public
: CN-VRF-VERIFY ( -- )
   s\" package CN-VRF public NEWTYPE vfam\n;package : CN-VRF-USE ( CN-VRF:vfam -- CN-VRF:vfam ) ;"
   VERIFY:SOURCE-BUF ;

: CN-VRF-FAIL ( -- )
   s\" package CN-VRF-ERR public NEWTYPE efam\nNEWTYPE efam\n;package"
   VERIFY:SOURCE-BUF ;

\ The pre-pass reads public and private as it replays, so a mint certifies in a
\ private section, with a checked caller beside it, and refuses in a public one.
: CN-VRF-MINT ( -- )
   s" package CN-VRF-M CAST: CVM0 ( n -- ^u8 ) : CVM1 ( n -- u8 ) CVM0 c@ ; ;package"
   VERIFY:SOURCE-BUF ;

: CN-VRF-PUBMINT ( -- )
   s" package CN-VRF-P public CAST: CVP0 ( n -- ^u8 ) ;package"
   VERIFY:SOURCE-BUF ;
;package

' CN-CAST-TEST:CN-VRF-VERIFY catch 0 T=
s" cast: CNVR0 ( n -- CN-VRF:vfam )" CN-RUN:DECL s" error[E-CAST-FAM]" s" `CN-VRF:vfam`" CN-RUN:REJECTS
' CN-CAST-TEST:CN-VRF-MINT catch 0 T=
REJECTION:CAPTURE ' CN-CAST-TEST:CN-VRF-PUBMINT catch
   s" error[E-CAST-MINT]" s" `cast: CVP0`" CN-RUN:REJECTS

REJECTION:CAPTURE ' CN-CAST-TEST:CN-VRF-FAIL catch
   s" error[E-BAD-DECLARATION]" s" `efam`" CN-RUN:REJECTS
package CN-VRF-LIVE
NEWTYPE live
s" cast: CNVR1 ( n -- CN-VRF-LIVE:live )" CN-RUN:DECL 0 T=
;package
\ A cast carries no body: the declaration ends at its closing paren, so the
\ trailing 42 is read as its own token and left as residue, which
\ evaluate-closed rejects: the kernel writes that record to fd 2, past the
\ capture.
package CN
s" cast: CNS1 ( n -- CN:cnfam ) 42"              CN-RUN:DECL 70 T=
s" CNS1" get-current search-wl 0= TFALSE        \ ... and the cast published anyway
;package

\ The engine, not the checker, makes the last refusals: it writes them to fd 2
\ and dies, which this suite's clean-stderr contract cannot host, so a child
\ pins the exit status and the message.
package CN-CHILD
public
$400 constant CAP
create OUT CAP allot
create ERR CAP allot
variable ERR-U
: RUN ( str -- n ) {: src :}   \ source -> child exit status (-1 = signal)
   src OUT CAP >LEN ERR CAP >LEN PROCESS:WAIT-FOREVER SUBJECT:RUN {: outu:len erru:len oc :}
   erru LEN>N ERR-U !
   oc MATCH PROCESS:ending
     exited OF ENDOF
     signaled OF drop -1 ENDOF
     timeout OF src OUT outu LEN>N >str ERR erru LEN>N >str SUBJECT:TIMED-OUT ENDOF
   ;MATCH ;
: ERR$ ( -- str ) ERR ERR-U @ >str ;
\ The child's record carries the code and the word.
: SAYS ( str str -- ) {: c w :}
   ERR$ c CONTAINS? TTRUE
   ERR$ w CONTAINS? TTRUE ;
: HAS ( str -- ) {: s :}   ERR$ s CONTAINS? TTRUE ;
;package

\ A cast's bare call on an empty stack is rejected at its place before it reads
\ the cell below the base.
s\" NEWTYPE cnmrole\ncast: >CNMROLE ( n -- cnmrole )  >CNMROLE" CN-CHILD:RUN 70 T=
s" error[E-INPUT-UNDERFLOW]" CN-CHILD:HAS  CN-CHILD:ERR$ s" cast-negative-suite.f" 65 s\" s\\\" NEWTYPE cnmrole\\ncast: >CNMROLE ( n -- cnmrole )  >CNMROLE\" CN-CHILD:RUN" T-PLACED
s" >CNMROLE" CN-CHILD:HAS

\ The trailing `;` of the retired spelling ends no definition.
s" cast: CNZ1 ( n -- n ) ;" CN-CHILD:RUN 70 T=
s" error[E-NO-DEFINITION]" s" `;`" CN-CHILD:SAYS
\ A reader keyword that needs a name and reaches the end of its stream fails
\ closed, and says which keyword and where.
s" cast:" CN-CHILD:RUN 70 T=
s" error[E-MISSING-NAME]" s" `cast:`" CN-CHILD:SAYS

\ Required reader operands fail before stale token bytes can be consumed.
\ These are child checks because the reader exits after its diagnostic.
s" create"   CN-CHILD:RUN 70 T=
s" error[E-MISSING-NAME]" s" `create`" CN-CHILD:SAYS
s" trusted:" CN-CHILD:RUN 70 T=
s" error[E-MISSING-NAME]" s" `trusted:`" CN-CHILD:SAYS
s" variable" CN-CHILD:RUN 70 T=
s" error[E-MISSING-NAME]" s" `variable`" CN-CHILD:SAYS
s" constant" CN-CHILD:RUN 70 T=
s" error[E-MISSING-NAME]" s" `constant`" CN-CHILD:SAYS
s" char"     CN-CHILD:RUN 70 T=
s" error[E-MISSING-NAME]" s" `char`" CN-CHILD:SAYS
\ The interpreter reads a whole definition before it binds the body's tokens,
\ so at EOF this unfinished one is refused before [char] or ['] reads an operand.
s" : x [char]" CN-CHILD:RUN 70 T=
s" error[E-VS-UNTERMINATED-DEFINITION]" s" `x`" CN-CHILD:SAYS
s" '"        CN-CHILD:RUN 70 T=
s" error[E-MISSING-NAME]" s" `'`" CN-CHILD:SAYS
s" : x [']"   CN-CHILD:RUN 70 T=
s" error[E-VS-UNTERMINATED-DEFINITION]" s" `x`" CN-CHILD:SAYS
package CN

\ the cast: declarer used inside a checked body is rejected unsafe (verdict 0);
\ no window is armed — the reject is the bare token, before any name is parsed.
s" CNU1 ( n -- CN:cnfam ) cast:"                      CHECK-QUIET-CANDIDATE! 0 T=
;package

\ A cast retypes one value of any width, to a value of the same width only: a
\ str is two cells, and n is one.
package CN-STR
: WIDTH? ( -- bool )
   REJECTION:BUFFER$ s" the input and output differ in width" CONTAINS? ;
s" cast: CNS1 ( str -- n )" CN-RUN:DECL s" error[E-CAST-ARITY]" s" `cast: CNS1`" CN-RUN:REJECTS
WIDTH? TTRUE
REJECTION:BUFFER$ s" ^^^^ `str` is 2 cells, `n` is 1" CONTAINS? TTRUE
REJECTION:BUFFER$ s" = help: give both sides the same width in cells: `str` is 2 cells, `n` is 1" CONTAINS? TTRUE
s" cast: CNS2 ( n -- str )" CN-RUN:DECL s" error[E-CAST-ARITY]" s" `cast: CNS2`" CN-RUN:REJECTS
WIDTH? TTRUE
REJECTION:BUFFER$ s" ^^^^ `n` is 1 cell, `str` is 2" CONTAINS? TTRUE
REJECTION:BUFFER$ s" = help: give both sides the same width in cells: `n` is 1 cell, `str` is 2" CONTAINS? TTRUE
\ A package retypes a str as its own role and back in its private section.
public
STRUCTURE path OPAQUE FIELD v str ;STRUCTURE
private
s" cast: >PATH ( str -- path )" CN-RUN:DECL 0 T=
s" cast: PATH> ( path -- str )" CN-RUN:DECL 0 T=
public
: MK ( str -- path ) >PATH ;
: UN ( path -- str ) PATH> ;
;package
s" abc" CN-STR:MK CN-STR:UN str> nip 3 T=

T-REPORT
