\ base-pointer-arith-refusals.f - the rejected programs for "a pointer derived
\ from a base address holds a plain value" (dots habu-bound-ptr-arithmetic-8bf6b54a
\ and habu-fence-a-base-c6c1d71d).
\
\ WHAT WAS WRONG. `data-base` and `NULL-PTR` are the two pointers in the language
\ that are not the address OF a declared element, and both published `-- ptr a`
\ with an ordinary polymorphic pointee. Pointer arithmetic is
\ pointee-polymorphic (`ptr a n + -- ptr a`), so the CALLER chose what the cell
\ under the derived address contained: `NEWTYPE thing 0  : F ( n -- thing )
\ NULL-PTR + @ ;` certified and forged a nominal identity out of an integer, the
\ same program with `( n -- ptr n )` forged an address, and `data-base + @` did
\ both from the engine's own DATA. `data-base @` and `NULL-PTR @` did it at
\ offset zero with no arithmetic at all. The raw-storage rule
\ (habu-refuse-a-ptr-5ad2734e) did not reach any of it: neither base is a raw
\ storage cell, and `data-base` must stay ordinary because DATA-CELL@ and the
\ AOT linker read scalars through it.
\
\ WHAT IS PINNED HERE. Two pointee kinds, because the two bases do not have the
\ same honest uses.
\
\ TVK-NULL is the null. It is fenced in VALUE position and permissive inside a
\ POINTEE, because every honest use of a null binds its pointee inside a `ptr`
\ and means this one literal address: `ptr thing NULL-PTR =`, `NULL-PTR X !`,
\ `NULL-PTR -`, `NULL-PTR 0=`. What it refuses is a value read THROUGH it.
\
\ TVK-DBASE is the DATA region's base, and it is fenced at EVERY pointee depth.
\ The first cut stopped at the first pointee -- NOMPTR-BLOCK? catches a nominal
\ there -- so a declaration that put the element one `ptr` deeper walked around
\ it: `( -- ptr ptr bpathing ) data-base 8 +` then minted the nominal with
\ `F @ @`, and `( -- ptr ptr ptr n )` forged an ADDRESS out of whatever integer
\ the DATA word held (`1 W  F @ @ @` certified and SIGSEGVed). The same kind
\ reaching a declared quantifier through an INPUT-only position walked around it
\ a second way, so the input-only excuse is the null's alone.
\
\ Both refusals answer E-RAW-CELL-PTR, each with its own reason, and a wrapper
\ that publishes a base-derived pointer under a type variable is refused at its
\ own declaration instead of at each of its callers.
\
\ The controls beside them matter as much as the refusals: the scalar cell reads
\ the engine and the AOT linker make through `data-base`, the `ptr-field` door
\ that reaches a DATA cell holding a real address, the byte view the linker
\ takes before it does any arithmetic at all, and every null store, compare,
\ distance and test -- at depth 2 as well -- each keep certifying. A rule that
\ also refuses those is not this rule.
\
\ These run against the live engine's own checker through CHECK-CANDIDATE!, the
\ public probing verdict word, so no `evaluate` boundary and no trust row stands
\ between a fixture and the checker that answers it.
require lib/test.f
require lib/string.f

\ The nominal identity the forgeries mint, and a declared pointer cell for the
\ null store. Both are file-global because the candidates below are checked in
\ the scope a real program writes them in.
NEWTYPE bpathing 0
PTR-VARIABLE BPA-SLOT
TYPED-VARIABLE BPA-PP ptr ptr n     \ a declared cell that really holds an address
TYPED-VARIABLE BPA-HK [ -- n ]      \ a declared CODE cell, for the executable-value controls
variable BPA-RAW                    \ an undeclared raw storage cell, for the RAW controls

\ A stepper written in ordinary checked Habu. Its own certification is half the
\ point: the offset mark rides its published quantifier, so a null handed to it
\ comes out a DATA-base pointer at the CALLER, and no wrapper launders the step.
: BPA-STEPPER ( ptr a n -- ptr a ) + ;

package BASE-PTR-ARITH-TEST

private

create DIAG-BUF 8192 allot
8192 constant DIAG-CAP

: ARM ( -- )   \ capture the next diagnostic as JSON instead of printing it
   DIAG-BUF DIAG-CAP DIAG-BUFFER!  true DIAG-JSON! ;

: DISARM ( -- )
   false DIAG-JSON!  DIAG-BUFFER-OFF ;

: CODE$ ( -- ptr u8 n )
   S\" \"code\":\"E-RAW-CELL-PTR\"" ;

: DB-REASON$ ( -- ptr u8 n )
   S\" \"reason\":\"base address: a cell reached from data-base, or from a pointer computed off NULL-PTR, holds a plain value at every pointee depth, never a nominal type or a pointer\"" ;

: NULL-REASON$ ( -- ptr u8 n )
   S\" \"reason\":\"null address: nothing is read through NULL-PTR, so it is never a nominal type and never the address of one\"" ;

: REPAIR$ ( -- ptr u8 n )
   S\" \"repair_class\":\"declare_pointer_cell\"" ;

\ The raw-storage rule's own reason. No fixture here may answer it: these
\ programs name no `variable`, `create`, `constant` or `here`, and a rule that
\ borrowed the other one's prose would say the wrong thing about them.
: RAW-REASON$ ( -- ptr u8 n )
   S\" \"reason\":\"raw storage cell: a pointer cannot be stored in or fetched from an undeclared cell\"" ;

\ An EXECUTABLE value reached through either base answers the same code with the
\ raw rule's own reason and its own repair class: the answer is the same at every
\ base and at an undeclared cell -- declare the cell that holds the xt (dot
\ habu-refuse-an-executable-e8834546).
: EXEC-REASON$ ( -- ptr u8 n )
   S\" \"reason\":\"raw storage cell: an undeclared cell cannot hold an execution token / a quotation\"" ;

: EXEC-REPAIR$ ( -- ptr u8 n )
   S\" \"repair_class\":\"declare_xt_cell\"" ;

: NP-CODE$ ( -- ptr u8 n )
   S\" \"code\":\"E-NONPARAMETRIC-EFFECT\"" ;

: NP-SUGGEST$ ( -- ptr u8 n )
   S\" Declare the pointee this base address really reaches" ;

: HAS? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TTRUE ;

: LACKS? ( ptr u8 n -- )
   DIAG-BUFFER$ 2swap CONTAINS? TFALSE ;

\ A candidate the checker must REFUSE, with the diagnostic it must answer.
\ Asserting the reason and not only the code is what keeps this rule's prose
\ from silently becoming the raw-cell rule's prose -- or the other base's.
: REFUSED-CODE ( ptr u8 n -- )   \ leaves the diagnostic armed for the reason assertions
   ARM
   CHECK-CANDIDATE! 0 T=
   CODE$ HAS?  REPAIR$ HAS?  RAW-REASON$ LACKS? ;

: DB-REFUSED ( ptr u8 n -- )
   REFUSED-CODE  DB-REASON$ HAS?  NULL-REASON$ LACKS?  DISARM ;

: NULL-REFUSED ( ptr u8 n -- )
   REFUSED-CODE  NULL-REASON$ HAS?  DB-REASON$ LACKS?  DISARM ;

\ The raw-cell rule answers its own prose for the same shape over an undeclared
\ cell, and neither base rule may borrow it.
: RAW-REFUSED ( ptr u8 n -- )
   ARM
   CHECK-CANDIDATE! 0 T=
   CODE$ HAS?  RAW-REASON$ HAS?  DB-REASON$ LACKS?  NULL-REASON$ LACKS?
   DISARM ;

: EXEC-REFUSED ( ptr u8 n -- )
   ARM
   CHECK-CANDIDATE! 0 T=
   CODE$ HAS?  EXEC-REPAIR$ HAS?  EXEC-REASON$ HAS?
   REPAIR$ LACKS?  DB-REASON$ LACKS?  NULL-REASON$ LACKS?  RAW-REASON$ LACKS?
   DISARM ;

: CERTIFIES ( ptr u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

\ ---- the forgery: a nominal identity out of a base address -------------------

\ An OFFSET null is a DATA-base pointer, so it answers the DATA base's reason:
\ what the pointer addresses is the caller's arithmetic, not the one literal
\ address the permissive pointee arm is for. The LITERAL null keeps that arm and
\ its own reason (CASE-NULL-ZERO below, and every control at the end).
: CASE-NULL-NOMINAL ( -- )
   s" a null plus an offset does not answer a nominal identity" T-LABEL
   s" BPA-F ( n -- bpathing ) NULL-PTR + @" DB-REFUSED ;

: CASE-DATA-NOMINAL ( -- )
   s" and neither does an offset into the engine's own DATA" T-LABEL
   s" BPA-D ( n -- bpathing ) data-base + @" DB-REFUSED ;

\ Offset zero is the same forgery with the arithmetic left out, which is why the
\ fence rides the pointee rather than the `+` token.
: CASE-NULL-ZERO ( -- )
   s" the same refusal with no arithmetic at all: NULL-PTR @" T-LABEL
   s" BPA-NZ ( -- bpathing ) NULL-PTR @" NULL-REFUSED ;

: CASE-DATA-ZERO ( -- )
   s" and data-base @, the first cell of the region" T-LABEL
   s" BPA-DZ ( -- bpathing ) data-base @" DB-REFUSED ;

\ ---- the third door: an EXECUTION TOKEN out of a base address -----------------
\ `: QCELL ( -- ptr [ -- n ] ) data-base 8 + ;` certified before this rule, and
\ so did `: FIRE ( -- n ) QCELL @ execute ;`: the accessor handed its caller a
\ certified code cell and the fetched value carried a DECLARED quotation type,
\ so the opaque-execute rule -- which judges opacity, not provenance -- never
\ fired. `1 W !  FIRE` then branched to address 1 (measured: SIGBUS). Neither
\ base addresses a declared element, so neither can hand out code.

: CASE-DATA-XT-CELL ( -- )
   s" a DATA offset cannot be declared to hold an execution token" T-LABEL
   s" BPA-Q ( -- ptr [ -- n ] ) data-base 8 +" EXEC-REFUSED ;

: CASE-DATA-XT-ZERO ( -- )
   s" the same at offset zero, with no arithmetic at all" T-LABEL
   s" BPA-QZ ( -- ptr [ -- n ] ) data-base" EXEC-REFUSED ;

\ The null's PERMISSIVE pointee arm is where a code cell declared over the null
\ lands, so the executable value is fenced ahead of that arm. The price is
\ CASE-NULL-XT-COMPARE below.
: CASE-NULL-XT-CELL ( -- )
   s" and a null-derived address cannot hold one either" T-LABEL
   s" BPA-NQ ( -- ptr [ -- n ] ) NULL-PTR 8 +" EXEC-REFUSED ;

: CASE-NULL-XT-ZERO ( -- )
   s" NULL-PTR itself is no code cell" T-LABEL
   s" BPA-NQZ ( -- ptr [ -- n ] ) NULL-PTR" EXEC-REFUSED ;

\ A quotation READ out of a base-derived cell is the same refusal, which is what
\ the caller of such an accessor would write if the accessor itself were legal.
: CASE-DATA-XT-FETCH ( -- )
   s" nor is one fetched from a DATA cell" T-LABEL
   s" BPA-QF ( -- [ -- n ] ) data-base 8 + @" EXEC-REFUSED ;

\ ---- the other half: an ADDRESS out of a base address ------------------------

: CASE-NULL-POINTER ( -- )
   s" a pointer fetched through a null-derived address is refused too" T-LABEL
   s" BPA-G ( n -- ptr n ) NULL-PTR + @" DB-REFUSED ;

: CASE-DATA-POINTER ( -- )
   s" a DATA cell answers a number, not the address it was never declared to hold" T-LABEL
   s" BPA-DG ( n -- ptr n ) data-base + @" DB-REFUSED ;

\ The other direction. Refusing the fetch alone would leave the cell holding an
\ address for some other mention to read back.
: CASE-POINTER-IN ( -- )
   s" and a real pointer cannot be put into an undeclared DATA cell either" T-LABEL
   s" BPA-PS ( ptr n n -- ) data-base + !" DB-REFUSED ;

\ ---- the ways around it ------------------------------------------------------
\ The fence rides the POINTEE VARIABLE, not the `+` token, so every route that
\ keeps that variable is refused at whatever token finally reads through it.

: CASE-OTHER-STEPS ( -- )
   s" cell+ steps the same pointee, and answers the same refusal" T-LABEL
   s" BPA-CP ( -- bpathing ) data-base cell+ @" DB-REFUSED ;

: CASE-REVERSED-ADD ( -- )
   s" and so does the offset-first spelling of +" T-LABEL
   s" BPA-RA ( -- bpathing ) 8 data-base + @" DB-REFUSED ;

\ A quotation's inferred effect carries the pointee like any other, so the
\ refusal lands on `execute` rather than on the fetch inside the brackets.
: CASE-THROUGH-QUOTATION ( -- )
   s" a quotation does not launder it either" T-LABEL
   ARM
   s" BPA-Q ( -- bpathing ) [: data-base 8 + @ ;] execute" CHECK-CANDIDATE! 0 T=
   CODE$ HAS?  DB-REASON$ HAS?
   DISARM ;

\ Stored into a DECLARED pointer cell and read back. PTR-VARIABLE seals its
\ pointee TVK-RAW, so the meet is RAW -- the stricter kind, by the
\ ANY < NULL < DBASE < RAW lattice -- and the RAW rule answers first. The refusal is what is pinned
\ here; the prose belongs to habu-refuse-a-ptr-5ad2734e, not to this rule.
: CASE-THROUGH-DECLARED-CELL ( -- )
   s" a round trip through a declared pointer cell is refused by the raw rule" T-LABEL
   s" BPA-RT ( -- bpathing ) data-base 8 + BPA-SLOT ! BPA-SLOT @ @" CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- the same forgery, landing on the RETURN row ------------------------------
\ `>r` moves any one cell, so it accepts the value the base cell hands out and
\ only the declared RETURN row refuses it. That row is unified by the same
\ unifier and fenced by the same pointee kind, so it has to answer the same
\ reason; until the return row had a first-failure capture of its own these two
\ answered a bare return-stack imbalance (dot habu-name-a-raw-09fe04d0).

: CASE-RETURN-NOMINAL ( -- )
   s" the declared return row refuses the nominal forgery, and names it" T-LABEL
   s" BPA-RN ( | -- | bpathing ) data-base 8 + @ >r" DB-REFUSED ;

: CASE-RETURN-POINTER ( -- )
   s" and the address half of it on the return row" T-LABEL
   s" BPA-RP ( | -- | ptr n ) data-base 8 + @ >r" DB-REFUSED ;

\ ---- the pointee CHAIN: the fence follows it all the way down -----------------
\ NOMPTR-BLOCK? already stops a nominal at the FIRST pointee, which is what made
\ the first cut look complete. Put the element one `ptr` deeper and the fence
\ has to follow, or the declaration mints exactly what it was refused above.

: CASE-DEPTH-2-POINTER ( -- )
   s" a DATA cell is not the address of an address" T-LABEL
   s" BPA-P2 ( -- ptr ptr n ) data-base 8 +" DB-REFUSED ;

: CASE-DEPTH-2-NOMINAL ( -- )
   s" nor the address of a pointer to a nominal" T-LABEL
   s" BPA-N2 ( -- ptr ptr bpathing ) data-base 8 +" DB-REFUSED ;

\ The SIGSEGV shape, run here only as a CHECK refusal: with this declaration
\ certified, `: W ( n -- ) data-base 8 + ! ; : BOOM ( -- n ) BPA-P3 @ @ @ ;` and
\ `1 W BOOM` dereferenced the integer 1. The fixture pins the refusal, not the
\ crash.
: CASE-DEPTH-3 ( -- )
   s" and a three-deep chain is the same forgery with one more hop" T-LABEL
   s" BPA-P3 ( -- ptr ptr ptr n ) data-base 8 +" DB-REFUSED ;

: CASE-DEPTH-4 ( -- )
   s" the fence follows the chain however deep it is declared" T-LABEL
   s" BPA-P4 ( -- ptr ptr ptr ptr n ) data-base 8 +" DB-REFUSED ;

\ The chain is read where it lands, too, not only declared.
: CASE-DEPTH-2-FETCH ( -- )
   s" a fetch that lands two deep answers the same refusal" T-LABEL
   s" BPA-F2 ( -- ptr n ) data-base 8 + @ @" DB-REFUSED ;

\ The RAW cell refused this depth all along, and its prose stays its own. These
\ are the controls that say the two rules did not merge.
: CASE-RAW-DEPTH-2-POINTER ( -- )
   s" an undeclared raw cell still answers the raw-storage reason" T-LABEL
   s" BPA-R2 ( -- ptr ptr n ) BPA-RAW" RAW-REFUSED ;

: CASE-RAW-DEPTH-2-NOMINAL ( -- )
   s" and so does the nominal spelling of it" T-LABEL
   s" BPA-R2N ( -- ptr ptr bpathing ) BPA-RAW" RAW-REFUSED ;

\ ---- the input-only excuse is the NULL's alone --------------------------------
\ A quantifier restricted through an input-only position is excused because the
\ kind still rides the published effect and every caller's fetch is fenced where
\ it lands. That reasoning holds for the null, which is ONE address whose only
\ readable value is the trap. It does not hold for the DATA region: the caller
\ instantiates the quantifier, so the excused word WRITES an arbitrary DATA word
\ into the caller's own declared cell and the caller reads it back typed.

: CASE-EXCUSE-STORE ( -- )
   s" a DATA-derived pointer is not excused by an input-only quantifier" T-LABEL
   ARM
   s" BPA-STASH ( ptr ptr a -- ) data-base 8 + swap !" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

\ The same reach through a quotation PARAMETER: NP-INVARS-WALK descends into the
\ quotation's own rows, so `a` is input-only there as well, and the excused word
\ handed the pointer straight to the caller's consumer
\ (`[: ( ptr bpathing -- ) @ drop ;] BPA-FEED`).
: CASE-EXCUSE-QUOTATION ( -- )
   s" nor by one that is input-only inside a quotation parameter" T-LABEL
   ARM
   s" BPA-FEED ( [ ptr a -- ] -- ) data-base 8 + swap execute" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

\ ---- the wrapper, refused at its own declaration ------------------------------
\ `( -- ptr a )` over a DATA offset is the shape that carried the forgery into
\ the shipped library: `SB-LEN @` read as a nominal certified, because SB-LEN
\ handed its caller the choice of element type. It is refused where it is
\ written, not at each of its uses, and the suggestion says what to write.

: CASE-PUBLISH-VAR ( -- )
   s" a wrapper may not publish a base-derived pointer under a type variable" T-LABEL
   ARM
   s" BPA-W ( -- ptr a ) data-base 8 +" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

\ INPUT-ONLY is the exemption, and this is why it has to be input-ONLY: a
\ quantifier named on BOTH sides is still published, so excusing it let the
\ caller pick the pointee again through `ptr bpathing BPA-LEAK @`.
: CASE-PUBLISH-INOUT ( -- )
   s" a wrapper that names the variable on both sides is refused too" T-LABEL
   ARM
   s" BPA-LEAK ( ptr a -- ptr a ) drop data-base 8 +" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

\ ---- an OFFSET null is a DATA-base pointer -----------------------------------
\ The hole the two kinds left. TVK-NULL's permissive pointee arm is judged at the
\ BINDING, and the arithmetic rows (`+`, `-` with an n, `1+`, `1-`, `cell+`,
\ `char+`) keep the base's pointee var, so a COMPUTED offset from the null still
\ wore the permissive kind and could be declared two deep. Measured on the engine
\ before this rule: `variable V  777 V !` and
\ `: F ( -- ptr ptr n ) NULL-PTR <V's distance> + ;  : G ( -- n ) F @ @ ;`
\ certified and PRINTED 777 -- a read of an attacker-chosen writable cell, which
\ is exactly the reach the null was said not to have. The offset rows now mark
\ the pointee they step (src/habu/prims.f PE-PTR-A-OFF), and a null riding one
\ meets out as TVK-DBASE: fenced at every depth, with no input-only excuse.

: CASE-NULL-OFFSET-DEPTH-2 ( -- )
   s" a null plus an offset is not the address of an address" T-LABEL
   s" BPA-NO2 ( n -- ptr ptr n ) NULL-PTR +" DB-REFUSED ;

: CASE-NULL-OFFSET-NOMINAL-DEEP ( -- )
   s" nor the address of a pointer to a nominal" T-LABEL
   s" BPA-NON ( n -- ptr ptr bpathing ) NULL-PTR +" DB-REFUSED ;

\ The parametric spelling: the offset arrives as an argument and the body never
\ names data-base at all.
: CASE-NULL-OFFSET-PARAMETRIC ( -- )
   s" and neither does the parametric spelling of the same step" T-LABEL
   s" BPA-NO7 ( n -- ptr ptr n ) NULL-PTR swap +" DB-REFUSED ;

\ The typed-cell route: store the offset null into a cell declared to hold an
\ address, and the caller reads it back typed. The store is where it lands.
: CASE-NULL-OFFSET-TYPED-CELL ( -- )
   s" an offset null does not go into a cell declared to hold an address" T-LABEL
   s" BPA-NO6 ( n -- ) NULL-PTR + BPA-PP !" DB-REFUSED ;

\ Every other step of the same pointee, not only bare `+`.
: CASE-NULL-OFFSET-CELL-STEP ( -- )
   s" cell+ steps the null the same way, and is refused the same way" T-LABEL
   s" BPA-NOC ( -- ptr ptr n ) NULL-PTR cell+" DB-REFUSED ;

: CASE-NULL-OFFSET-MINUS ( -- )
   s" and so does stepping it backwards by an integer" T-LABEL
   s" BPA-NOM ( n -- ptr ptr n ) NULL-PTR swap -" DB-REFUSED ;

: CASE-NULL-OFFSET-FETCH ( -- )
   s" a fetch that lands two deep through an offset null is refused" T-LABEL
   s" BPA-NOF ( n -- ptr n ) NULL-PTR + @ @" DB-REFUSED ;

\ The wrapper, refused where it is written: the input-only excuse is the LITERAL
\ null's, and an offset null is no longer one address.
: CASE-NULL-OFFSET-PUBLISH ( -- )
   s" a wrapper may not publish an offset null under a type variable" T-LABEL
   ARM
   s" BPA-NOW ( -- ptr a ) NULL-PTR 8 +" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

: CASE-NULL-OFFSET-EXCUSE ( -- )
   s" and an input-only quantifier does not excuse one either" T-LABEL
   ARM
   s" BPA-NOS ( ptr ptr a -- ) NULL-PTR 8 + swap !" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

: CASE-NULL-OFFSET-VIA-WRAPPER ( -- )
   s" a checked stepper does not launder the offset either" T-LABEL
   s" BPA-NOWR ( n -- ptr ptr n ) NULL-PTR swap BPA-STEPPER" DB-REFUSED ;

\ The mark is PROVENANCE, not a fence: an ordinary pointer still steps, and a
\ definition that steps a pointee it was handed still preserves its quantifier.
\ A rule that refuses these is not this rule -- it is a ban on `+`.
: CASE-OFFSET-KEEPS-POINTEE ( -- )
   s" stepping a pointer to a nominal still answers that nominal" T-LABEL
   s" BPA-STEP ( ptr bpathing -- bpathing ) 8 + @" CERTIFIES ;

: CASE-OFFSET-KEEPS-QUANTIFIER ( -- )
   s" and a declared quantifier survives an offset step" T-LABEL
   s" BPA-STEPQ ( ptr a -- ptr a ) 8 +" CERTIFIES ;

: CASE-OFFSET-KEEPS-DEPTH ( -- )
   s" including a pointee that is itself an address" T-LABEL
   s" BPA-STEP2 ( ptr ptr n -- ptr ptr n ) cell+" CERTIFIES ;

: CASE-OFFSET-WRAPPER-CERTIFIES ( -- )
   s" and a stepper called on an ordinary pointer answers its pointee" T-LABEL
   s" BPA-OKWR ( ptr bpathing -- bpathing ) 8 BPA-STEPPER @" CERTIFIES ;

\ The sanctioned address-to-integer idiom, which is how the probes computed the
\ offset in the first place: both sides are byte views, so the pointee is a
\ concrete `u8` and no kind rides the distance row.
: CASE-NULL-BYTE-DISTANCE ( -- )
   s" the byte-view distance from the null is still a number" T-LABEL
   s" BPA-NBV ( ptr bpathing -- n ) BYTE-VIEW NULL-PTR BYTE-VIEW -" CERTIFIES ;

\ ---- the controls: every honest use of a base address -------------------------

\ A NULL restricted through an INPUT ONLY is not a restriction the signature has
\ to spell: src/core/dynamic-storage.f RELEASE is this word, and the null it
\ stores is the one the language's own reset code stores.
: CASE-INPUT-ONLY ( -- )
   s" a null stored through a declared parameter's own pointee certifies" T-LABEL
   s" BPA-IN ( ptr ptr a -- ) NULL-PTR swap !" CERTIFIES ;

\ And through a quotation parameter, the shape the DATA base is refused for.
: CASE-INPUT-ONLY-QUOT ( -- )
   s" and so does a null handed to a caller's own quotation" T-LABEL
   s" BPA-NQ ( [ ptr a -- ] -- ) NULL-PTR swap execute" CERTIFIES ;

\ The pointer ITSELF. Each of these binds the pointee INSIDE a `ptr`, where the
\ kind is permissive, and each is a shape the tree uses: 89 sites store a null
\ into a declared pointer cell and 11 compare one.
: CASE-NULL-STORE ( -- )
   s" a null still goes into a declared pointer cell" T-LABEL
   s" BPA-NS ( -- ) NULL-PTR BPA-SLOT !" CERTIFIES ;

: CASE-NULL-COMPARE ( -- )
   s" and is still comparable against a pointer to a nominal" T-LABEL
   s" BPA-EQ ( ptr bpathing -- bool ) NULL-PTR =" CERTIFIES ;

: CASE-NULL-DIFF ( -- )
   s" and still subtracts from one" T-LABEL
   s" BPA-SUB ( ptr bpathing -- n ) NULL-PTR -" CERTIFIES ;

: CASE-NULL-ZEROTEST ( -- )
   s" and still answers 0=" T-LABEL
   s" BPA-Z ( -- bool ) NULL-PTR 0=" CERTIFIES ;

\ Depth 2 for the null, where the DATA base is now refused. A cell declared to
\ hold an address is cleared with a null like any other, and the pointee arm is
\ what lets it: the null is ONE address, so the caller learns nothing about an
\ element from it. This is the residue the split accepts and names.
: CASE-NULL-STORE-DEEP ( -- )
   s" a null still clears a cell declared to hold an address" T-LABEL
   s" BPA-N2S ( -- ) NULL-PTR BPA-PP !" CERTIFIES ;

: CASE-NULL-COMPARE-DEEP ( -- )
   s" and still compares against a pointer to a pointer" T-LABEL
   s" BPA-N2C ( ptr ptr bpathing -- bool ) NULL-PTR =" CERTIFIES ;

\ The engine's own reads. These are the shapes the rule had to keep: a scalar
\ cell of the running task's DATA, read and written at an offset.
: CASE-DATA-SCALAR ( -- )
   s" a DATA cell still reads as the number it holds" T-LABEL
   s" BPA-RD ( n -- n ) data-base + @" CERTIFIES ;

: CASE-DATA-STORE ( -- )
   s" and still takes one" T-LABEL
   s" BPA-WR ( n n -- ) data-base + !" CERTIFIES ;

: CASE-DATA-DISTANCE ( -- )
   s" the distance between two addresses is still a number" T-LABEL
   s" BPA-DIST ( ptr n -- n ) data-base -" CERTIFIES ;

\ The declared door. A DATA cell that really holds an address is reached with
\ `ptr-field`, exactly as before this rule: the checker's own SOURCE-CELL and
\ TARGET-CELL, and the AOT span table, are written this way.
: CASE-DATA-FIELD ( -- )
   s" a DATA cell that holds an address is still reached through ptr-field" T-LABEL
   s" BPA-FLD ( -- ptr ptr u8 ) data-base 8 + 0 ptr-field @" CERTIFIES ;

\ The linker's shape: `data-base BYTE-VIEW` first, then arithmetic on a byte
\ pointer whose pointee is concrete. src/habu/aot-closure.f DATA-PTR is this
\ word, and DATA-CELL@ is DATA-PTR CELL-VIEW @.
: CASE-DATA-BYTE-VIEW ( -- )
   s" and the linker's byte view of the region still certifies" T-LABEL
   s" BPA-BV ( n -- n ) data-base BYTE-VIEW swap + CELL-VIEW @" CERTIFIES ;

\ A concrete pointee is the repair the diagnostic names, and it is what the
\ converted library words now declare.
: CASE-DECLARED-POINTEE ( -- )
   s" a wrapper that names its pointee is the repair, and it certifies" T-LABEL
   s" BPA-OK ( -- ptr n ) data-base 8 +" CERTIFIES ;

\ ---- the executable-value controls -------------------------------------------

\ `xt!` is the sanctioned mint: the one prim whose purpose is to DECLARE that
\ the cell it writes holds an execution token, for a persisted cell whose
\ address the caller works out from a table base and a row index. It keeps
\ certifying at its own token -- otherwise the engine could not install its own
\ callbacks -- and the window closes with that token.
: CASE-XT-DECLARATION ( -- )
   s" xt! declares the DATA cell it writes, so it keeps certifying" T-LABEL
   s" BPA-XTPUT ( [ -- n ] -- ) data-base 8 + xt!" CERTIFIES ;

\ A DECLARED code cell is a code cell wherever it is named: the storage word
\ mints the quotation type and neither base is involved.
: CASE-DECLARED-XT-CELL ( -- )
   s" a TYPED-VARIABLE code cell certifies, and is executed" T-LABEL
   s" BPA-HKACC ( -- ptr [ -- n ] ) BPA-HK" CERTIFIES
   s" BPA-HKRUN ( -- n ) BPA-HK @ execute" CERTIFIES ;

\ THE PRICE, MEASURED. Fencing the executable value ahead of the null's
\ permissive arm costs the null COMPARISON on a declared code cell: it certified
\ before this rule and is refused after it, with the same reason as the cell
\ shapes above. An empty code cell is read through a NUMBER-typed accessor of
\ the same address instead -- the two-accessor shape lib/genio.f already uses
\ for its device rows, where a cleared row is spelled ZERO.
: CASE-NULL-XT-COMPARE ( -- )
   s" comparing a code cell against the null is refused, and says why" T-LABEL
   s" BPA-HKNULL ( -- bool ) BPA-HK NULL-PTR =" EXEC-REFUSED ;

public

\ NULL-PTR is registered by a PRIM: row in src/core/cell-effects.f rather than a
\ TRUST declaration. The seal-time internal-word marking pass keeps a
\ checker-known name callable, so this executed definition -- ordinary test
\ source, loaded long after the seal -- is the proof that it stayed one, and not
\ only that the checker can quote its effect.
: NULL-STILL-CALLABLE ( -- )
   s" NULL-PTR is still callable from ordinary source after the seal" T-LABEL
   NULL-PTR BPA-SLOT !
   BPA-SLOT @ NULL-PTR = TTRUE
   NULL-PTR NULL-PTR - 0 T=
   NULL-PTR 0= TTRUE ;

: RUN ( -- )
   T-RESET
   CASE-NULL-NOMINAL
   CASE-DATA-NOMINAL
   CASE-NULL-ZERO
   CASE-DATA-ZERO
   CASE-NULL-POINTER
   CASE-DATA-POINTER
   CASE-POINTER-IN
   CASE-OTHER-STEPS
   CASE-REVERSED-ADD
   CASE-THROUGH-QUOTATION
   CASE-THROUGH-DECLARED-CELL
   CASE-RETURN-NOMINAL
   CASE-RETURN-POINTER
   CASE-PUBLISH-VAR
   CASE-PUBLISH-INOUT
   CASE-DEPTH-2-POINTER
   CASE-DEPTH-2-NOMINAL
   CASE-DEPTH-3
   CASE-DEPTH-4
   CASE-DEPTH-2-FETCH
   CASE-RAW-DEPTH-2-POINTER
   CASE-RAW-DEPTH-2-NOMINAL
   CASE-EXCUSE-STORE
   CASE-EXCUSE-QUOTATION
   CASE-NULL-OFFSET-DEPTH-2
   CASE-NULL-OFFSET-NOMINAL-DEEP
   CASE-NULL-OFFSET-PARAMETRIC
   CASE-NULL-OFFSET-TYPED-CELL
   CASE-NULL-OFFSET-CELL-STEP
   CASE-NULL-OFFSET-MINUS
   CASE-NULL-OFFSET-FETCH
   CASE-NULL-OFFSET-PUBLISH
   CASE-NULL-OFFSET-EXCUSE
   CASE-NULL-OFFSET-VIA-WRAPPER
   CASE-OFFSET-KEEPS-POINTEE
   CASE-OFFSET-KEEPS-QUANTIFIER
   CASE-OFFSET-KEEPS-DEPTH
   CASE-OFFSET-WRAPPER-CERTIFIES
   CASE-NULL-BYTE-DISTANCE
   CASE-INPUT-ONLY
   CASE-INPUT-ONLY-QUOT
   CASE-NULL-STORE
   CASE-NULL-COMPARE
   CASE-NULL-DIFF
   CASE-NULL-ZEROTEST
   CASE-NULL-STORE-DEEP
   CASE-NULL-COMPARE-DEEP
   CASE-DATA-SCALAR
   CASE-DATA-STORE
   CASE-DATA-DISTANCE
   CASE-DATA-FIELD
   CASE-DATA-BYTE-VIEW
   CASE-DECLARED-POINTEE
   CASE-DATA-XT-CELL
   CASE-DATA-XT-ZERO
   CASE-NULL-XT-CELL
   CASE-NULL-XT-ZERO
   CASE-DATA-XT-FETCH
   CASE-XT-DECLARATION
   CASE-DECLARED-XT-CELL
   CASE-NULL-XT-COMPARE
   NULL-STILL-CALLABLE
   T-REPORT ;

;package

BASE-PTR-ARITH-TEST:RUN
