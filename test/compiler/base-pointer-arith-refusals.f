\ base-pointer-arith-refusals.f - rejected programs and controls for "a pointer
\ derived from a base address (data-base, NULL-PTR) holds a plain value". Each
\ base refusal answers E-RAW-CELL-PTR with a reason per base. TVK-NULL is fenced
\ in value position and permissive inside a pointee; TVK-DBASE is fenced at
\ every pointee depth with no input-only excuse. Candidates go to the live
\ engine's checker through CHECK-CANDIDATE!.
require src/lib/test.f
require src/lib/string.f

\ File-global: the candidates are checked in the scope a real program writes
\ them in.
NEWTYPE bpathing
PTR-VARIABLE BPA-SLOT
TYPED-VARIABLE BPA-PP ptr ptr n     \ a declared cell that really holds an address
TYPED-VARIABLE BPA-HK [ -- n ]      \ a declared CODE cell, for the executable-value controls
TYPED-VARIABLE BPA-TV bpathing      \ a pointer to a nominal, for the inferred-operand distances
variable BPA-RAW                    \ an undeclared raw storage cell, for the RAW controls

\ Its own certification matters: the offset mark rides its published quantifier,
\ so a null handed to it is a DATA-base pointer at the caller.
: BPA-STEPPER ( ptr a n -- ptr a ) + ;

\ A distance reads no storage and leaves no kind on its pointee `a`, so a caller
\ may instantiate `a` with anything, a quotation among them.
: BPA-NDIST ( ptr a -- n ) NULL-PTR - ;
: BPA-DDIST ( ptr a -- n ) data-base - ;

\ A distance with no signature: its input is inferred from the body, never a
\ base address, so it pairs with BPA-TV's pointee (CASE-DIST-INFERRED-OPERAND).
: BPA-TVDIST BPA-TV - ;

package BASE-PTR-ARITH-TEST

private

create DIAG-BUF 8192 allot
8192 constant DIAG-CAP

: ARM ( -- )
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

\ The raw-storage rule's reason. No base fixture may answer it: they name no
\ `variable`, `create`, `constant` or `here`.
: RAW-REASON$ ( -- ptr u8 n )
   S\" \"reason\":\"raw storage cell: a pointer cannot be stored in or fetched from an undeclared cell\"" ;

\ An executable value reached through either base or an undeclared cell answers
\ the raw rule's reason and repair class: declare the cell that holds the xt.
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

\ Asserting the reason, not only the code, keeps each rule's prose its own.
: REFUSED-CODE ( ptr u8 n -- )   \ leaves the diagnostic armed for the reason assertions
   ARM
   CHECK-CANDIDATE! 0 T=
   CODE$ HAS?  REPAIR$ HAS?  RAW-REASON$ LACKS? ;

: DB-REFUSED ( ptr u8 n -- )
   REFUSED-CODE  DB-REASON$ HAS?  NULL-REASON$ LACKS?  DISARM ;

: NULL-REFUSED ( ptr u8 n -- )
   REFUSED-CODE  NULL-REASON$ HAS?  DB-REASON$ LACKS?  DISARM ;

\ The raw-cell rule answers its own prose for the same shape over an undeclared
\ cell.
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

\ An offset null is a DATA-base pointer and answers the DATA base's reason; the
\ literal null keeps its permissive pointee arm and its own reason
\ (CASE-NULL-ZERO, the controls).
: CASE-NULL-NOMINAL ( -- )
   s" a null plus an offset does not answer a nominal identity" T-LABEL
   s" BPA-F ( n -- bpathing ) NULL-PTR + @" DB-REFUSED ;

: CASE-DATA-NOMINAL ( -- )
   s" and neither does an offset into the engine's own DATA" T-LABEL
   s" BPA-D ( n -- bpathing ) data-base + @" DB-REFUSED ;

\ Offset zero: the fence rides the pointee, not the `+` token.
: CASE-NULL-ZERO ( -- )
   s" the same refusal with no arithmetic at all: NULL-PTR @" T-LABEL
   s" BPA-NZ ( -- bpathing ) NULL-PTR @" NULL-REFUSED ;

: CASE-DATA-ZERO ( -- )
   s" and data-base @, the first cell of the region" T-LABEL
   s" BPA-DZ ( -- bpathing ) data-base @" DB-REFUSED ;

\ ---- the third door: an EXECUTION TOKEN out of a base address -----------------
\ `: QCELL ( -- ptr [ -- n ] ) data-base 8 + ;` with
\ `: FIRE ( -- n ) QCELL @ execute ;` certified unfenced and `1 W !  FIRE`
\ branched to address 1 (measured: SIGBUS): the opaque-execute rule judges
\ opacity, not provenance. Neither base addresses a declared element, so neither
\ hands out code.

: CASE-DATA-XT-CELL ( -- )
   s" a DATA offset cannot be declared to hold an execution token" T-LABEL
   s" BPA-Q ( -- ptr [ -- n ] ) data-base 8 +" EXEC-REFUSED ;

: CASE-DATA-XT-ZERO ( -- )
   s" the same at offset zero, with no arithmetic at all" T-LABEL
   s" BPA-QZ ( -- ptr [ -- n ] ) data-base" EXEC-REFUSED ;

\ The executable value is fenced ahead of the null's permissive pointee arm; the
\ price is CASE-NULL-XT-COMPARE.
: CASE-NULL-XT-CELL ( -- )
   s" and a null-derived address cannot hold one either" T-LABEL
   s" BPA-NQ ( -- ptr [ -- n ] ) NULL-PTR 8 +" EXEC-REFUSED ;

: CASE-NULL-XT-ZERO ( -- )
   s" NULL-PTR itself is no code cell" T-LABEL
   s" BPA-NQZ ( -- ptr [ -- n ] ) NULL-PTR" EXEC-REFUSED ;

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

\ Refusing only the fetch would leave the address in the cell for another read.
: CASE-POINTER-IN ( -- )
   s" and a real pointer cannot be put into an undeclared DATA cell either" T-LABEL
   s" BPA-PS ( ptr n n -- ) data-base + !" DB-REFUSED ;

\ ---- the ways around it ------------------------------------------------------
\ The fence rides the pointee variable, so every route that keeps it is refused
\ at the token that reads through it.

: CASE-OTHER-STEPS ( -- )
   s" cell+ steps the same pointee, and answers the same refusal" T-LABEL
   s" BPA-CP ( -- bpathing ) data-base cell+ @" DB-REFUSED ;

: CASE-REVERSED-ADD ( -- )
   s" and so does the offset-first spelling of +" T-LABEL
   s" BPA-RA ( -- bpathing ) 8 data-base + @" DB-REFUSED ;

\ A quotation's inferred effect carries the pointee: the refusal lands on
\ `execute`, not on the fetch inside the brackets.
: CASE-THROUGH-QUOTATION ( -- )
   s" a quotation does not launder it either" T-LABEL
   ARM
   s" BPA-Q ( -- bpathing ) [: data-base 8 + @ ;] execute" CHECK-CANDIDATE! 0 T=
   CODE$ HAS?  DB-REASON$ HAS?
   DISARM ;

\ PTR-VARIABLE seals its pointee TVK-RAW, so the meet is RAW (ANY < NULL < DBASE
\ < RAW) and the raw rule answers first with its own prose; only the refusal is
\ pinned here.
: CASE-THROUGH-DECLARED-CELL ( -- )
   s" a round trip through a declared pointer cell is refused by the raw rule" T-LABEL
   s" BPA-RT ( -- bpathing ) data-base 8 + BPA-SLOT ! BPA-SLOT @ @" CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- the pointee CHAIN: the fence follows it all the way down -----------------
\ NOMPTR-BLOCK? stops a nominal only at the first pointee; the fence follows the
\ chain.

: CASE-DEPTH-2-POINTER ( -- )
   s" a DATA cell is not the address of an address" T-LABEL
   s" BPA-P2 ( -- ptr ptr n ) data-base 8 +" DB-REFUSED ;

: CASE-DEPTH-2-NOMINAL ( -- )
   s" nor the address of a pointer to a nominal" T-LABEL
   s" BPA-N2 ( -- ptr ptr bpathing ) data-base 8 +" DB-REFUSED ;

\ Pinned as a check refusal: certified, `: W ( n -- ) data-base 8 + ! ;
\ : BOOM ( -- n ) BPA-P3 @ @ @ ;` and `1 W BOOM` dereferenced the integer 1.
: CASE-DEPTH-3 ( -- )
   s" and a three-deep chain is the same forgery with one more hop" T-LABEL
   s" BPA-P3 ( -- ptr ptr ptr n ) data-base 8 +" DB-REFUSED ;

: CASE-DEPTH-4 ( -- )
   s" the fence follows the chain however deep it is declared" T-LABEL
   s" BPA-P4 ( -- ptr ptr ptr ptr n ) data-base 8 +" DB-REFUSED ;

: CASE-DEPTH-2-FETCH ( -- )
   s" a fetch that lands two deep answers the same refusal" T-LABEL
   s" BPA-F2 ( -- ptr n ) data-base 8 + @ @" DB-REFUSED ;

\ Controls: the RAW cell refuses this depth with its own prose; the rules stay
\ separate.
: CASE-RAW-DEPTH-2-POINTER ( -- )
   s" an undeclared raw cell still answers the raw-storage reason" T-LABEL
   s" BPA-R2 ( -- ptr ptr n ) BPA-RAW" RAW-REFUSED ;

: CASE-RAW-DEPTH-2-NOMINAL ( -- )
   s" and so does the nominal spelling of it" T-LABEL
   s" BPA-R2N ( -- ptr ptr bpathing ) BPA-RAW" RAW-REFUSED ;

\ ---- the input-only excuse is the NULL's alone --------------------------------
\ A quantifier restricted through an input-only position is excused because the
\ kind rides the published effect and each caller's fetch is fenced. That holds
\ for the null, one address whose only readable value is the trap; not for DATA,
\ where the caller instantiates the quantifier and the excused word writes an
\ arbitrary DATA word into the caller's declared cell.

: CASE-EXCUSE-STORE ( -- )
   s" a DATA-derived pointer is not excused by an input-only quantifier" T-LABEL
   ARM
   s" BPA-STASH ( ptr ptr a -- ) data-base 8 + swap !" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

\ NP-INVARS-WALK descends into a quotation parameter's rows, so `a` is
\ input-only there too: `[: ( ptr bpathing -- ) @ drop ;] BPA-FEED` reached the
\ caller's consumer.
: CASE-EXCUSE-QUOTATION ( -- )
   s" nor by one that is input-only inside a quotation parameter" T-LABEL
   ARM
   s" BPA-FEED ( [ ptr a -- ] -- ) data-base 8 + swap execute" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

\ ---- the wrapper, refused at its own declaration ------------------------------
\ `( -- ptr a )` over a DATA offset let the caller choose the element type
\ (`SB-LEN @` certified as a nominal). It is refused where written, and the
\ suggestion says what to write.

: CASE-PUBLISH-VAR ( -- )
   s" a wrapper may not publish a base-derived pointer under a type variable" T-LABEL
   ARM
   s" BPA-W ( -- ptr a ) data-base 8 +" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

\ A quantifier named on both sides is still published: the caller picks the
\ pointee again through `ptr bpathing BPA-LEAK @`.
: CASE-PUBLISH-INOUT ( -- )
   s" a wrapper that names the variable on both sides is refused too" T-LABEL
   ARM
   s" BPA-LEAK ( ptr a -- ptr a ) drop data-base 8 +" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

\ ---- an OFFSET null is a DATA-base pointer -----------------------------------
\ The arithmetic rows (`+`, `-` with an n, `1+`, `1-`, `cell+`, `char+`) keep
\ the base's pointee var, and the null's permissive arm is judged at the
\ binding, so an unmarked offset null could be declared two deep:
\ `: F ( -- ptr ptr n ) NULL-PTR <V's distance> + ;  : G ( -- n ) F @ @ ;`
\ printed 777 from `variable V  777 V !` (measured). The offset rows mark the
\ pointee they step (src/habu/prims.f PE-PTR-A-OFF), and a null riding one
\ meets as TVK-DBASE.

: CASE-NULL-OFFSET-DEPTH-2 ( -- )
   s" a null plus an offset is not the address of an address" T-LABEL
   s" BPA-NO2 ( n -- ptr ptr n ) NULL-PTR +" DB-REFUSED ;

: CASE-NULL-OFFSET-NOMINAL-DEEP ( -- )
   s" nor the address of a pointer to a nominal" T-LABEL
   s" BPA-NON ( n -- ptr ptr bpathing ) NULL-PTR +" DB-REFUSED ;

: CASE-NULL-OFFSET-PARAMETRIC ( -- )
   s" and neither does the parametric spelling of the same step" T-LABEL
   s" BPA-NO7 ( n -- ptr ptr n ) NULL-PTR swap +" DB-REFUSED ;

\ The refusal lands on the store into the typed cell.
: CASE-NULL-OFFSET-TYPED-CELL ( -- )
   s" an offset null does not go into a cell declared to hold an address" T-LABEL
   s" BPA-NO6 ( n -- ) NULL-PTR + BPA-PP !" DB-REFUSED ;

: CASE-NULL-OFFSET-CELL-STEP ( -- )
   s" cell+ steps the null the same way, and is refused the same way" T-LABEL
   s" BPA-NOC ( -- ptr ptr n ) NULL-PTR cell+" DB-REFUSED ;

: CASE-NULL-OFFSET-MINUS ( -- )
   s" and so does stepping it backwards by an integer" T-LABEL
   s" BPA-NOM ( n -- ptr ptr n ) NULL-PTR swap -" DB-REFUSED ;

: CASE-NULL-OFFSET-FETCH ( -- )
   s" a fetch that lands two deep through an offset null is refused" T-LABEL
   s" BPA-NOF ( n -- ptr n ) NULL-PTR + @ @" DB-REFUSED ;

\ The input-only excuse is the literal null's; an offset null is not one
\ address.
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

\ The mark is provenance, not a fence: an ordinary pointer still steps, and a
\ word that steps a pointee it was handed keeps its quantifier.
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

\ The sanctioned address-to-integer idiom: both sides are byte views, so the
\ pointee is a concrete `u8` and no kind rides the distance row.
: CASE-NULL-BYTE-DISTANCE ( -- )
   s" the byte-view distance from the null is still a number" T-LABEL
   s" BPA-NBV ( ptr bpathing -- n ) BYTE-VIEW NULL-PTR BYTE-VIEW -" CERTIFIES ;

\ ---- the controls: every honest use of a base address -------------------------

\ A null through an input-only position needs no spelled restriction:
\ src/core/dynamic-storage.f RELEASE is this shape.
: CASE-INPUT-ONLY ( -- )
   s" a null stored through a declared parameter's own pointee certifies" T-LABEL
   s" BPA-IN ( ptr ptr a -- ) NULL-PTR swap !" CERTIFIES ;

\ Through a quotation parameter, the shape the DATA base is refused for.
: CASE-INPUT-ONLY-QUOT ( -- )
   s" and so does a null handed to a caller's own quotation" T-LABEL
   s" BPA-NQ ( [ ptr a -- ] -- ) NULL-PTR swap execute" CERTIFIES ;

\ The pointer itself: each binds the pointee inside a `ptr`, where the kind is
\ permissive. The tree has 89 null stores into declared pointer cells and 11
\ compares.
: CASE-NULL-STORE ( -- )
   s" a null still goes into a declared pointer cell" T-LABEL
   s" BPA-NS ( -- ) NULL-PTR BPA-SLOT !" CERTIFIES ;

: CASE-NULL-COMPARE ( -- )
   s" and is still comparable against a pointer to a nominal" T-LABEL
   s" BPA-EQ ( ptr bpathing -- bool ) NULL-PTR =" CERTIFIES ;

: CASE-NULL-DIFF ( -- )
   s" and still subtracts from one" T-LABEL
   s" BPA-SUB ( ptr bpathing -- n ) NULL-PTR -" CERTIFIES ;

: CASE-NULL-DIST-FREE ( -- )
   s" a distance from a base address leaves the pointee free" T-LABEL
   s" BPA-NDQ ( -- n ) BPA-HK BPA-NDIST" CERTIFIES
   s" BPA-DDQ ( -- n ) BPA-HK BPA-DDIST" CERTIFIES ;

\ A base row matches only a base address's own pointee. An operand still being
\ inferred -- a quotation's input, a signature-less word's input -- is not one,
\ so a distance against a pointer to a nominal pairs both pointees, and the
\ inferred input is that pointer: it takes `ptr bpathing` and refuses the null.
: CASE-DIST-INFERRED-OPERAND ( -- )
   s" an inferred operand of a distance takes the other pointee" T-LABEL
   s" BPA-QX ( ptr bpathing -- n ) [: BPA-TV - ;] execute" CERTIFIES
   s" BPA-QD ( -- [ ptr bpathing -- n ] ) [: BPA-TV - ;]" CERTIFIES
   s" BPA-NSU ( ptr bpathing -- n ) BPA-TVDIST" CERTIFIES ;

: CASE-DIST-INFERRED-NULL ( -- )
   s" and a null is not that pointee" T-LABEL
   ARM
   s" BPA-NSN ( -- n ) NULL-PTR BPA-TVDIST" CHECK-CANDIDATE! 0 T=
   S\" \"code\":\"E-MISMATCH\"" HAS?  S\" \"expected\":\"ptr bpathing \"" HAS?
   DISARM ;

\ A null whose distance was taken is still the null: a read through it answers
\ the null's suggestion.
: CASE-NULL-DIST-READ ( -- )
   s" a read through the null a distance was taken from is still refused" T-LABEL
   ARM
   s" BPA-NDR ( ptr a -- a ) NULL-PTR dup rot - drop @" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  s" Declare the pointee this null stands in for" HAS?
   DISARM ;

: CASE-NULL-ZEROTEST ( -- )
   s" and still answers 0=" T-LABEL
   s" BPA-Z ( -- bool ) NULL-PTR 0=" CERTIFIES ;

\ Depth 2 for the null: a pointer cell may be cleared with a null because the
\ null is one address and tells the caller nothing about an element. The split
\ accepts this.
: CASE-NULL-STORE-DEEP ( -- )
   s" a null still clears a cell declared to hold an address" T-LABEL
   s" BPA-N2S ( -- ) NULL-PTR BPA-PP !" CERTIFIES ;

: CASE-NULL-COMPARE-DEEP ( -- )
   s" and still compares against a pointer to a pointer" T-LABEL
   s" BPA-N2C ( ptr ptr bpathing -- bool ) NULL-PTR =" CERTIFIES ;

\ The engine's own reads: a scalar cell of the task's DATA, read and written at
\ an offset.
: CASE-DATA-SCALAR ( -- )
   s" a DATA cell still reads as the number it holds" T-LABEL
   s" BPA-RD ( n -- n ) data-base + @" CERTIFIES ;

: CASE-DATA-STORE ( -- )
   s" and still takes one" T-LABEL
   s" BPA-WR ( n n -- ) data-base + !" CERTIFIES ;

: CASE-DATA-DISTANCE ( -- )
   s" the distance between two addresses is still a number" T-LABEL
   s" BPA-DIST ( ptr n -- n ) data-base -" CERTIFIES ;

\ A DATA cell that holds an address is reached with `ptr-field`, as the
\ checker's SOURCE-CELL and TARGET-CELL and the AOT span table do.
: CASE-DATA-FIELD ( -- )
   s" a DATA cell that holds an address is still reached through ptr-field" T-LABEL
   s" BPA-FLD ( -- ptr ptr u8 ) data-base 8 + 0 ptr-field @" CERTIFIES ;

\ The linker's shape: src/habu/aot-closure.f DATA-PTR takes `data-base
\ BYTE-VIEW` first, and DATA-CELL@ is DATA-PTR CELL-VIEW @.
: CASE-DATA-BYTE-VIEW ( -- )
   s" and the linker's byte view of the region still certifies" T-LABEL
   s" BPA-BV ( n -- n ) data-base BYTE-VIEW swap + CELL-VIEW @" CERTIFIES ;

\ A concrete pointee is the repair the diagnostic names.
: CASE-DECLARED-POINTEE ( -- )
   s" a wrapper that names its pointee is the repair, and it certifies" T-LABEL
   s" BPA-OK ( -- ptr n ) data-base 8 +" CERTIFIES ;

\ ---- the executable-value controls -------------------------------------------

\ `xt!` is the sanctioned mint: it declares that the cell it writes holds an xt,
\ for a persisted cell addressed from a table base and a row index. It certifies
\ at its own token, or the engine could not install its callbacks.
: CASE-XT-DECLARATION ( -- )
   s" xt! declares the DATA cell it writes, so it keeps certifying" T-LABEL
   s" BPA-XTPUT ( [ -- n ] -- ) data-base 8 + xt!" CERTIFIES ;

\ A declared code cell mints the quotation type wherever it is named; no base
\ involved.
: CASE-DECLARED-XT-CELL ( -- )
   s" a TYPED-VARIABLE code cell certifies, and is executed" T-LABEL
   s" BPA-HKACC ( -- ptr [ -- n ] ) BPA-HK" CERTIFIES
   s" BPA-HKRUN ( -- n ) BPA-HK @ execute" CERTIFIES ;

\ The price: the null comparison on a declared code cell is refused with the
\ cell shapes' reason. An empty code cell is read through a number-typed
\ accessor of the same address, as src/lib/genio-core.f does for device rows,
\ where a cleared row is ZERO.
: CASE-NULL-XT-COMPARE ( -- )
   s" comparing a code cell against the null is refused, and says why" T-LABEL
   s" BPA-HKNULL ( -- bool ) BPA-HK NULL-PTR =" EXEC-REFUSED ;

\ A tick of an engine primitive is an opaque xt: a primitive's effect is its
\ EPRIM: row at a call, and no row rides its xt. NULL-PTR is a primitive, so
\ `['] NULL-PTR execute` is refused like `['] dup execute`; a quotation
\ that calls it carries the row.
: OPAQUE-REFUSED ( ptr u8 n -- )
   ARM
   CHECK-CANDIDATE! 0 T=
   S\" \"code\":\"E-EXEC-OPAQUE-XT\"" HAS?
   DISARM ;

: CASE-NULL-TICK ( -- )
   s" executing a ticked NULL-PTR is refused as an opaque xt" T-LABEL
   s" BPA-TKN ( -- bool ) ['] NULL-PTR execute 0=" OPAQUE-REFUSED
   s" and a quotation that calls NULL-PTR certifies" T-LABEL
   s" BPA-QKN ( -- bool ) [: NULL-PTR ;] execute 0=" CERTIFIES ;

public

\ NULL-PTR is an engine primitive with an EPRIM: row in src/habu/prims.f, not a
\ TRUST declaration. The seal's internal-word marking keeps a checker-known name
\ callable; this executed definition, loaded after the seal, proves it stayed
\ callable.
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
   CASE-NULL-DIST-FREE
   CASE-NULL-DIST-READ
   CASE-DIST-INFERRED-OPERAND
   CASE-DIST-INFERRED-NULL
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
   CASE-NULL-TICK
   NULL-STILL-CALLABLE
   T-REPORT ;

;package

BASE-PTR-ARITH-TEST:RUN
