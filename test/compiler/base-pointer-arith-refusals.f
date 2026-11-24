split-str
\ base-pointer-arith-refusals.f - rejected programs and controls for "a pointer
\ derived from NULL-PTR holds a plain value". Each refusal answers E-RAW-CELL-PTR
\ with a reason per kind. TVK-NULL is fenced in value position and permissive
\ inside a pointee; an offset null meets as TVK-DBASE, fenced at every pointee
\ depth with no input-only excuse. Candidates go to the live engine's checker
\ through CHECK-CANDIDATE!.
require src/lib/test.f
require src/lib/string.f
require src/lib/adt/nullable.f
require src/lib/adt/option.f

\ File-global: the candidates are checked in the scope a real program writes
\ them in.
NEWTYPE bpathing
PTR-VARIABLE BPA-SLOT
TYPED-VARIABLE BPA-PP ^^n     \ a declared cell that really holds an address
TYPED-VARIABLE BPA-HK [ -- n ]      \ a declared CODE cell, for the executable-value controls
TYPED-VARIABLE BPA-TV bpathing      \ a pointer to a nominal, for the inferred-operand distances
variable BPA-RAW                    \ an undeclared raw storage cell, for the RAW controls
STRUCTURE bpa-rec FIELD x n FIELD y n ;STRUCTURE
ENUM bpa-dir north south ;ENUM
TYPED-VARIABLE BPA-RV bpa-rec       \ a pointer to a record, for the stored-pointee distances
DEFLINEAR bpa-tok                   \ a linear type, for linear-family pointees

\ Its own certification matters: the offset mark rides its published quantifier,
\ so a null handed to it meets as TVK-DBASE at the caller.
: BPA-STEPPER ( ^a n -- ^a ) + ;

\ A distance reads no storage, so a caller may instantiate its pointee `a` with
\ any stored type, a quotation or a record among them.
: BPA-NDIST ( ^a -- n ) NULL-PTR - ;

\ A distance with no signature: its input is inferred from the body, never a
\ base address, so it pairs with BPA-TV's pointee (CASE-DIST-INFERRED-OPERAND).
: BPA-TVDIST BPA-TV - ;

\ An inferred signature keeps the measured pointee open (CASE-ANY-POINTEE).
: BPA-QI PTR>N ;

\ A body that also fetches through the pointer publishes a one-cell pointee.
: BPA-QR dup PTR>N drop @ drop ;

package BASE-PTR-ARITH-TEST

private


: ARM ( -- )
   REJECTION:CAPTURE  true REJECTION:JSON ! ;

: DISARM ( -- )
   false REJECTION:JSON !  REJECTION:BUFFER-OFF ;

: CODE$ ( -- str )
   S\" \"code\":\"E-RAW-CELL-PTR\"" ;

: DB-REASON$ ( -- str )
   S\" \"message\":\"base address: a cell reached from a pointer computed off NULL-PTR, or from the engine's DATA base, holds a plain value at every pointee depth, never a nominal type or a pointer\"" ;

: NULL-REASON$ ( -- str )
   S\" \"message\":\"null address: nothing is read through NULL-PTR, so it is never a nominal type and never the address of one\"" ;

\ The raw-storage rule's reason. No base fixture may answer it: they name no
\ `variable`, `create`, `constant` or `here`.
: RAW-REASON$ ( -- str )
   S\" \"message\":\"raw storage cell: a pointer cannot be stored in or fetched from an undeclared cell\"" ;

\ An executable value reached through a base address or an undeclared cell
\ answers the raw rule's reason for code: declare the cell that holds the xt.
: EXEC-REASON$ ( -- str )
   S\" \"message\":\"raw storage cell: an undeclared cell cannot hold an execution token / a quotation\"" ;

: NP-CODE$ ( -- str )
   S\" \"code\":\"E-NONPARAMETRIC-EFFECT\"" ;

: NP-SUGGEST$ ( -- str )
   s" declare the pointee the base address reaches" ;

: HAS? ( ^u8 n -- )
   REJECTION:BUFFER$ 2swap STR:CONTAINS? TTRUE ;

: LACKS? ( str -- )
   REJECTION:BUFFER$ 2swap STR:CONTAINS? TFALSE ;

\ Asserting the reason, not only the code, keeps each rule's prose its own.
: REFUSED-CODE ( str -- )   \ leaves the diagnostic armed for the reason assertions
   ARM
   CHECK-CANDIDATE! 0 T=
   CODE$ HAS?  RAW-REASON$ LACKS? ;

: DB-REFUSED ( str -- )
   REFUSED-CODE  DB-REASON$ HAS?  NULL-REASON$ LACKS?  DISARM ;

: NULL-REFUSED ( str -- )
   REFUSED-CODE  NULL-REASON$ HAS?  DB-REASON$ LACKS?  DISARM ;

\ The raw-cell rule answers its own prose for the same shape over an undeclared
\ cell.
: RAW-REFUSED ( str -- )
   ARM
   CHECK-CANDIDATE! 0 T=
   CODE$ HAS?  RAW-REASON$ HAS?  DB-REASON$ LACKS?  NULL-REASON$ LACKS?
   DISARM ;

: EXEC-REFUSED ( str -- )
   ARM
   CHECK-CANDIDATE! 0 T=
   CODE$ HAS?  EXEC-REASON$ HAS?
   DB-REASON$ LACKS?  NULL-REASON$ LACKS?  RAW-REASON$ LACKS?
   DISARM ;

: CERTIFIES ( ^u8 n -- )
   CHECK-QUIET-CANDIDATE! -1 T= ;

\ ---- the forgery: a nominal identity out of a base address -------------------

\ An offset null meets as TVK-DBASE and answers that kind's reason; the
\ literal null keeps its permissive pointee arm and its own reason
\ (CASE-NULL-ZERO, the controls).
: CASE-NULL-NOMINAL ( -- )
   s" a null plus an offset does not answer a nominal identity" T-LABEL
   s" BPA-F ( n -- bpathing ) NULL-PTR + @" DB-REFUSED ;

\ Offset zero: the fence rides the pointee, not the `+` token.
: CASE-NULL-ZERO ( -- )
   s" the same refusal with no arithmetic at all: NULL-PTR @" T-LABEL
   s" BPA-NZ ( -- bpathing ) NULL-PTR @" NULL-REFUSED ;

\ ---- the third door: an EXECUTION TOKEN out of a base address -----------------
\ The opaque-execute rule judges opacity, not provenance, so the fence is here:
\ the null addresses no declared element, so it hands out no code.

\ The executable value is fenced ahead of the null's permissive pointee arm; the
\ price is CASE-NULL-XT-COMPARE.
: CASE-NULL-XT-CELL ( -- )
   s" a null-derived address cannot be declared to hold an execution token" T-LABEL
   s" BPA-NQ ( -- ^[ -- n ] ) NULL-PTR 8 +" EXEC-REFUSED ;

: CASE-NULL-XT-ZERO ( -- )
   s" NULL-PTR itself is no code cell" T-LABEL
   s" BPA-NQZ ( -- ^[ -- n ] ) NULL-PTR" EXEC-REFUSED ;

\ ---- the other half: an ADDRESS out of a base address ------------------------

: CASE-NULL-POINTER ( -- )
   s" a pointer fetched through a null-derived address is refused too" T-LABEL
   s" BPA-G ( n -- ^n ) NULL-PTR + @" DB-REFUSED ;

\ ptr-cell! takes `!`'s cell-width rule; its prims.f row alone admits a `^u8`.
: CASE-BYTE-PTR-CELL ( -- )
   s" ptr-cell! refuses a byte pointer as its cell, as ! does" T-LABEL
   ARM
   s" BPA-PCB ( u8 ^u8 -- ) ptr-cell!" CHECK-CANDIDATE! 0 T=
   S\" \"code\":\"E-MISMATCH\"" HAS?
   S\" \"expected\":\"^a\",\"found\":\"^u8\"" HAS?
   DISARM ;

\ Controls: an undeclared raw cell refuses a two-deep pointer with its own
\ prose; the rules stay separate.
: CASE-RAW-DEPTH-2-POINTER ( -- )
   s" an undeclared raw cell still answers the raw-storage reason" T-LABEL
   s" BPA-R2 ( -- ^^n ) BPA-RAW" RAW-REFUSED ;

: CASE-RAW-DEPTH-2-NOMINAL ( -- )
   s" and so does the nominal spelling of it" T-LABEL
   s" BPA-R2N ( -- ^^bpathing ) BPA-RAW" RAW-REFUSED ;

\ ---- an OFFSET null meets as TVK-DBASE ---------------------------------------
\ The arithmetic rows (`+`, `-` with an n, `1+`, `1-`, `cell+`, `char+`) keep
\ the base's pointee var, and the null's permissive arm is judged at the
\ binding, so an unmarked offset null could be declared two deep:
\ `: F ( -- ^^n ) NULL-PTR <V's distance> + ;  : G ( -- n ) F @ @ ;`
\ printed 777 from `variable V  777 V !` (measured). The offset rows mark the
\ pointee they step (src/habu/prims.f PE-PTR-A-OFF), and a null riding one
\ meets as TVK-DBASE.

: CASE-NULL-OFFSET-DEPTH-2 ( -- )
   s" a null plus an offset is not the address of an address" T-LABEL
   s" BPA-NO2 ( n -- ^^n ) NULL-PTR +" DB-REFUSED ;

: CASE-NULL-OFFSET-NOMINAL-DEEP ( -- )
   s" nor the address of a pointer to a nominal" T-LABEL
   s" BPA-NON ( n -- ^^bpathing ) NULL-PTR +" DB-REFUSED ;

: CASE-NULL-OFFSET-PARAMETRIC ( -- )
   s" and neither does the parametric spelling of the same step" T-LABEL
   s" BPA-NO7 ( n -- ^^n ) NULL-PTR swap +" DB-REFUSED ;

\ The refusal lands on the store into the typed cell.
: CASE-NULL-OFFSET-TYPED-CELL ( -- )
   s" an offset null does not go into a cell declared to hold an address" T-LABEL
   s" BPA-NO6 ( n -- ) NULL-PTR + BPA-PP !" DB-REFUSED ;

: CASE-NULL-OFFSET-CELL-STEP ( -- )
   s" cell+ steps the null the same way, and is refused the same way" T-LABEL
   s" BPA-NOC ( -- ^^n ) NULL-PTR cell+" DB-REFUSED ;

: CASE-NULL-OFFSET-MINUS ( -- )
   s" and so does stepping it backwards by an integer" T-LABEL
   s" BPA-NOM ( n -- ^^n ) NULL-PTR swap -" DB-REFUSED ;

: CASE-NULL-OFFSET-FETCH ( -- )
   s" a fetch that lands two deep through an offset null is refused" T-LABEL
   s" BPA-NOF ( n -- ^n ) NULL-PTR + @ @" DB-REFUSED ;

\ The input-only excuse is the literal null's; an offset null is not one
\ address.
: CASE-NULL-OFFSET-PUBLISH ( -- )
   s" a wrapper may not publish an offset null under a type variable" T-LABEL
   ARM
   s" BPA-NOW ( -- ^a ) NULL-PTR 8 +" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

: CASE-NULL-OFFSET-EXCUSE ( -- )
   s" and an input-only quantifier does not excuse one either" T-LABEL
   ARM
   s" BPA-NOS ( ^^a -- ) NULL-PTR 8 + swap !" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

: CASE-NULL-OFFSET-VIA-WRAPPER ( -- )
   s" a checked stepper does not launder the offset either" T-LABEL
   s" BPA-NOWR ( n -- ^^n ) NULL-PTR swap BPA-STEPPER" DB-REFUSED ;

\ Refusing only the fetch would leave the address in the cell for another read.
: CASE-NULL-OFFSET-POINTER-IN ( -- )
   s" a real pointer cannot be put into a cell an offset null reaches" T-LABEL
   s" BPA-PS ( ^n n -- ) NULL-PTR + !" DB-REFUSED ;

\ ptr-cell! is checked as `!`, so its declaration does not excuse the cell.
: CASE-NULL-OFFSET-POINTER-IN-PTR-CELL ( -- )
   s" nor through ptr-cell!, whose declaration does not excuse it" T-LABEL
   s" BPA-PCS ( ^n n -- ) NULL-PTR + ptr-cell!" DB-REFUSED ;

\ A quotation's inferred effect carries the pointee: the refusal lands on
\ `execute`, not on the fetch inside the brackets.
: CASE-NULL-OFFSET-THROUGH-QUOTATION ( -- )
   s" a quotation does not launder an offset null" T-LABEL
   ARM
   s" BPA-Q ( -- bpathing ) [: NULL-PTR 8 + @ ;] execute" CHECK-CANDIDATE! 0 T=
   CODE$ HAS?  DB-REASON$ HAS?
   DISARM ;

\ PTR-VARIABLE seals its pointee TVK-RAW, so the meet is RAW (ANY < NULL < DBASE
\ < RAW) and the raw rule answers first with its own prose; only the refusal is
\ pinned here.
: CASE-NULL-OFFSET-THROUGH-DECLARED-CELL ( -- )
   s" a round trip through a declared pointer cell is refused by the raw rule" T-LABEL
   s" BPA-RT ( -- bpathing ) NULL-PTR 8 + BPA-SLOT ! BPA-SLOT @ @" CHECK-QUIET-CANDIDATE! 0 T= ;

: CASE-NULL-OFFSET-DEPTH-3 ( -- )
   s" an offset null is not the address of a three-deep chain" T-LABEL
   s" BPA-P3 ( -- ^^^n ) NULL-PTR 8 +" DB-REFUSED ;

: CASE-NULL-OFFSET-DEPTH-4 ( -- )
   s" the fence follows the chain however deep it is declared" T-LABEL
   s" BPA-P4 ( -- ^^^^n ) NULL-PTR 8 +" DB-REFUSED ;

\ NP-INVARS-WALK descends into a quotation parameter's rows, so `a` is
\ input-only there too: `[: ( ^bpathing -- ) @ drop ;] BPA-FEED` reached the
\ caller's consumer.
: CASE-NULL-OFFSET-EXCUSE-QUOTATION ( -- )
   s" nor one that is input-only inside a quotation parameter" T-LABEL
   ARM
   s" BPA-FEED ( [ ^a -- ] -- ) NULL-PTR 8 + swap execute" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

\ A quantifier named on both sides is still published: the caller picks the
\ pointee again through `^bpathing BPA-LEAK @`.
: CASE-NULL-OFFSET-PUBLISH-INOUT ( -- )
   s" a wrapper that names the variable on both sides is refused too" T-LABEL
   ARM
   s" BPA-LEAK ( ^a -- ^a ) drop NULL-PTR 8 +" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  NP-SUGGEST$ HAS?
   DISARM ;

\ The mark is provenance, not a fence: an ordinary pointer still steps, and a
\ word that steps a pointee it was handed keeps its quantifier.
: CASE-OFFSET-KEEPS-POINTEE ( -- )
   s" stepping a pointer to a nominal still answers that nominal" T-LABEL
   s" BPA-STEP ( ^bpathing -- bpathing ) 8 + @" CERTIFIES ;

: CASE-OFFSET-KEEPS-QUANTIFIER ( -- )
   s" and a declared quantifier survives an offset step" T-LABEL
   s" BPA-STEPQ ( ^a -- ^a ) 8 +" CERTIFIES ;

: CASE-OFFSET-KEEPS-DEPTH ( -- )
   s" including a pointee that is itself an address" T-LABEL
   s" BPA-STEP2 ( ^^n -- ^^n ) cell+" CERTIFIES ;

: CASE-OFFSET-WRAPPER-CERTIFIES ( -- )
   s" and a stepper called on an ordinary pointer answers its pointee" T-LABEL
   s" BPA-OKWR ( ^bpathing -- bpathing ) 8 BPA-STEPPER @" CERTIFIES ;

: CASE-NULL-OFFSET-SCALAR ( -- )
   s" a cell an offset null reaches still reads as a number" T-LABEL
   s" BPA-RD ( n -- n ) NULL-PTR + @" CERTIFIES ;

: CASE-NULL-OFFSET-STORE ( -- )
   s" and still takes one" T-LABEL
   s" BPA-WR ( n n -- ) NULL-PTR + !" CERTIFIES ;

\ A cell that holds an address is reached with `ptr-field`.
: CASE-NULL-OFFSET-FIELD ( -- )
   s" an address in a cell an offset null reaches comes through ptr-field" T-LABEL
   s" BPA-FLD ( -- ^^u8 ) NULL-PTR 8 + 0 ptr-field @" CERTIFIES ;

: CASE-NULL-OFFSET-BYTE-VIEW ( -- )
   s" a byte view of an offset null steps to a cell view that certifies" T-LABEL
   s" BPA-BV ( n -- n ) NULL-PTR 8 + BYTE-VIEW swap + CELL-VIEW @" CERTIFIES ;

\ A concrete pointee is the repair the diagnostic names.
: CASE-NULL-OFFSET-DECLARED-POINTEE ( -- )
   s" a wrapper that names its pointee is the repair, and it certifies" T-LABEL
   s" BPA-OK ( -- ^n ) NULL-PTR 8 +" CERTIFIES ;

\ The sanctioned address-to-integer idiom: both sides are byte views, so the
\ pointee is a concrete `u8` and no kind rides the distance row.
: CASE-NULL-BYTE-DISTANCE ( -- )
   s" the byte-view distance from the null is still a number" T-LABEL
   s" BPA-NBV ( ^bpathing -- n ) BYTE-VIEW NULL-PTR BYTE-VIEW -" CERTIFIES ;

\ ---- the controls: every honest use of a base address -------------------------

\ A null through an input-only position needs no spelled restriction:
\ src/core/dynamic-storage.f RELEASE is this shape.
: CASE-INPUT-ONLY ( -- )
   s" a null stored through a declared parameter's own pointee certifies" T-LABEL
   s" BPA-IN ( ^^a -- ) NULL-PTR swap !" CERTIFIES ;

: CASE-INPUT-ONLY-QUOT ( -- )
   s" and so does a null handed to a caller's own quotation" T-LABEL
   s" BPA-NQ ( [ ^a -- ] -- ) NULL-PTR swap execute" CERTIFIES ;

\ The pointer itself: each binds the pointee inside a `^`, where the kind is
\ permissive. The tree has 89 null stores into declared pointer cells and 11
\ compares.
: CASE-NULL-STORE ( -- )
   s" a null still goes into a declared pointer cell" T-LABEL
   s" BPA-NS ( -- ) NULL-PTR BPA-SLOT !" CERTIFIES ;

: CASE-NULL-COMPARE ( -- )
   s" and is still comparable against a pointer to a nominal" T-LABEL
   s" BPA-EQ ( ^bpathing -- bool ) NULL-PTR =" CERTIFIES ;

: CASE-NULL-DIFF ( -- )
   s" and still subtracts from one" T-LABEL
   s" BPA-SUB ( ^bpathing -- n ) NULL-PTR -" CERTIFIES ;

: CASE-NULL-DIST-FREE ( -- )
   s" a distance from a base address leaves the pointee free" T-LABEL
   s" BPA-NDQ ( -- n ) BPA-HK BPA-NDIST" CERTIFIES ;

\ PTR>N and a distance read nothing through the pointer, so they take a pointer
\ to any type; no signature states a pointer to a scoped family or a view.
: CASE-ANY-POINTEE ( -- )
   s" a distance and PTR>N take a pointer to any type" T-LABEL
   s" BPA-RN ( ^bpa-rec -- n ) PTR>N" CERTIFIES
   s" BPA-NN ( ^nullable<u8> -- n ) PTR>N" CERTIFIES
   s" BPA-EN ( ^bpa-dir -- n ) PTR>N" CERTIFIES
   s" BPA-ON ( ^option<n> -- n ) PTR>N" CERTIFIES
   s" BPA-RR ( ^bpa-rec ^bpa-rec -- n ) -" CERTIFIES
   s" BPA-RVD ( -- n ) BPA-RV BPA-NDIST" CERTIFIES
   s" BPA-QO ( ^option<a> -- n ) PTR>N" CERTIFIES
   s" BPA-DO3 ( ^option<a> ^option<a> -- n ) -" CERTIFIES
   s" BPA-LN ( ^option<bpa-tok> -- n ) PTR>N" CERTIFIES
   s" BPA-QIO ( ^option<a> -- n ) BPA-QI" CERTIFIES ;

: MISMATCH ( ^u8 n -- )
   ARM
   CHECK-CANDIDATE! 0 T=
   S\" \"code\":\"E-MISMATCH\"" HAS?
   DISARM ;

\ A definition that declares `^a` may rely on `a` being closed and laid out,
\ so its callers still pass it no open family.
: CASE-DECLARED-POINTEE-CLOSED ( -- )
   s" a declared ^a still refuses an open family" T-LABEL
   s" BPA-D2O ( ^option<a> -- n ) BPA-NDIST" MISMATCH
   s" BPA-QRO ( ^option<a> -- ) BPA-QR" MISMATCH ;

\ A byte view of a closed record shows no more than the view of each field
\ (`rec FIELD BYTE-VIEW`). An open family has no layout yet, so the view still
\ refuses a pointer to one.
: CASE-RECORD-BYTE-VIEW ( -- )
   s" a byte view of a pointer to a closed record certifies" T-LABEL
   s" BPA-RBV ( ^bpa-rec -- ^u8 ) BYTE-VIEW" CERTIFIES
   s" and of a pointer to an open family is still refused" T-LABEL
   s" BPA-OBV ( ^option<a> -- ^u8 ) BYTE-VIEW" MISMATCH ;

\ A base row matches only a base address's own pointee. An operand still being
\ inferred -- a quotation's input, a signature-less word's input -- is not one,
\ so a distance against a pointer to a nominal pairs both pointees, and the
\ inferred input is that pointer: it takes `^bpathing` and refuses the null.
: CASE-DIST-INFERRED-OPERAND ( -- )
   s" an inferred operand of a distance takes the other pointee" T-LABEL
   s" BPA-QX ( ^bpathing -- n ) [: BPA-TV - ;] execute" CERTIFIES
   s" BPA-QD ( -- [ ^bpathing -- n ] ) [: BPA-TV - ;]" CERTIFIES
   s" BPA-NSU ( ^bpathing -- n ) BPA-TVDIST" CERTIFIES ;

: CASE-DIST-INFERRED-NULL ( -- )
   s" and a null is not that pointee" T-LABEL
   ARM
   s" BPA-NSN ( -- n ) NULL-PTR BPA-TVDIST" CHECK-CANDIDATE! 0 T=
   S\" \"code\":\"E-MISMATCH\"" HAS?  S\" \"expected\":\"^bpathing\"" HAS?
   DISARM ;

\ A null whose distance was taken is still the null: a read through it answers
\ the null's suggestion.
: CASE-NULL-DIST-READ ( -- )
   s" a read through the null a distance was taken from is still refused" T-LABEL
   ARM
   s" BPA-NDR ( ^a -- a ) NULL-PTR dup rot - drop @" CHECK-CANDIDATE! 0 T=
   NP-CODE$ HAS?  s" declare the pointee the null stands in for" HAS?
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
   s" BPA-N2C ( ^^bpathing -- bool ) NULL-PTR =" CERTIFIES ;

\ ---- the executable-value controls -------------------------------------------

\ A declared code cell mints the quotation type wherever it is named; no base
\ involved.
: CASE-DECLARED-XT-CELL ( -- )
   s" a TYPED-VARIABLE code cell certifies, and is executed" T-LABEL
   s" BPA-HKACC ( -- ^[ -- n ] ) BPA-HK" CERTIFIES
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
: OPAQUE-REFUSED ( str -- )
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

\ NULL-PTR is an engine primitive with an EPRIM: row in src/habu/prims.f. The
\ seal's internal-word marking keeps a checker-known name callable; this
\ executed definition, loaded after the seal, proves it stayed callable.
: NULL-STILL-CALLABLE ( -- )
   s" NULL-PTR is still callable from ordinary source after the seal" T-LABEL
   NULL-PTR BPA-SLOT !
   BPA-SLOT @ NULL-PTR = TTRUE
   NULL-PTR NULL-PTR - 0 T=
   NULL-PTR 0= TTRUE ;

: RUN ( -- )
   T-RESET
   CASE-NULL-NOMINAL
   CASE-NULL-ZERO
   CASE-NULL-POINTER
   CASE-BYTE-PTR-CELL
   CASE-RAW-DEPTH-2-POINTER
   CASE-RAW-DEPTH-2-NOMINAL
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
   CASE-NULL-OFFSET-POINTER-IN
   CASE-NULL-OFFSET-POINTER-IN-PTR-CELL
   CASE-NULL-OFFSET-THROUGH-QUOTATION
   CASE-NULL-OFFSET-THROUGH-DECLARED-CELL
   CASE-NULL-OFFSET-DEPTH-3
   CASE-NULL-OFFSET-DEPTH-4
   CASE-NULL-OFFSET-EXCUSE-QUOTATION
   CASE-NULL-OFFSET-PUBLISH-INOUT
   CASE-OFFSET-KEEPS-POINTEE
   CASE-OFFSET-KEEPS-QUANTIFIER
   CASE-OFFSET-KEEPS-DEPTH
   CASE-OFFSET-WRAPPER-CERTIFIES
   CASE-NULL-OFFSET-SCALAR
   CASE-NULL-OFFSET-STORE
   CASE-NULL-OFFSET-FIELD
   CASE-NULL-OFFSET-BYTE-VIEW
   CASE-NULL-OFFSET-DECLARED-POINTEE
   CASE-NULL-BYTE-DISTANCE
   CASE-INPUT-ONLY
   CASE-INPUT-ONLY-QUOT
   CASE-NULL-STORE
   CASE-NULL-COMPARE
   CASE-NULL-DIFF
   CASE-NULL-DIST-FREE
   CASE-ANY-POINTEE
   CASE-DECLARED-POINTEE-CLOSED
   CASE-RECORD-BYTE-VIEW
   CASE-NULL-DIST-READ
   CASE-DIST-INFERRED-OPERAND
   CASE-DIST-INFERRED-NULL
   CASE-NULL-ZEROTEST
   CASE-NULL-STORE-DEEP
   CASE-NULL-COMPARE-DEEP
   CASE-NULL-XT-CELL
   CASE-NULL-XT-ZERO
   CASE-DECLARED-XT-CELL
   CASE-NULL-XT-COMPARE
   CASE-NULL-TICK
   NULL-STILL-CALLABLE
   T-REPORT ;

;package

BASE-PTR-ARITH-TEST:RUN
