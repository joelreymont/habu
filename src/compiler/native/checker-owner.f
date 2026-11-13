\ checker-owner.f - the checker this compiler asks.
\ A source-loaded compiler binds the checker beside it from the same tree.
\ Missing fields refuse with E-COMP-OWNER; they never fall back by name.

require src/lib/prelude.f
require src/lib/error.f
require src/core/checker-owner-guard.f
require src/core/check-hook.f
require src/habu/layout.f

package CHECKER-OWNER

\ The live source owner. Zero until an engine publishes one.
: RECORD ( -- ptr u8 )
   NCOMP-DISPATCH:DECL-A @ ;

variable BIND-UNJUDGED

\ A retained prefix may carry these pre-hook words without native call models.
\ Bind their actual execution tokens with their existing callable contracts.
defer SOURCE-UNJUDGED ( ptr u8 n -- n )
defer SOURCE-CALL-CELLS ( n -- n n )
defer SOURCE-CALL-BINDING ( n -- ptr u8 n )
defer SOURCE-UNJUDGED-BINDING ( n -- ptr u8 n )
defer SOURCE-BINDING-WINDOW ( -- n )
defer SOURCE-CALL-GLUE ( n -- n n )
defer SOURCE-MATCH-PAYLOAD ( n -- n n )
defer SOURCE-QUOT-IN ( n n -- n n )
defer SOURCE-QUOT-OUT ( n n -- n n )
defer SOURCE-INIT-LAYOUT ( n -- n n n )
defer SOURCE-FIELD-SPAN ( n -- n n )
defer SOURCE-C2-STOW ( n -- n n )
defer SOURCE-ROWS-END ( -- n )
defer SOURCE-RETRACT-ROWS ( n -- )
defer SOURCE-WRITE-WINDOW ( n -- )
defer SOURCE-REPORT-RESET ( -- )
defer SOURCE-JSON-REPORTED ( -- bool )
defer SOURCE-DOES-UNJUDGED ( ptr u8 n ptr u8 n -- n )
defer SOURCE-DOES-CHECK ( ptr u8 n ptr u8 n -- n )
defer SOURCE-DOES-REPORT ( n ptr u8 n -- n )

: REFUSE ( ptr u8 n -- ) {: a:ptr u:n :}
   2 s" ncomp: the source owner carries no " write drop
   2 a u write drop
   2 S\" \n" write drop
   E-COMP-OWNER throw ;

\ The owner record stores raw execution tokens; each view states a field's
\ signature so native execute has a callable layout.
CAST: AS-CHECK ( n -- [ ptr u8 n -- n ] )
CAST: AS-ACTION ( n -- [ -- ] )
CAST: AS-DOES-CHECK ( n -- [ ptr u8 n ptr u8 n -- n ] )
CAST: AS-DOES-REPORT ( n -- [ n ptr u8 n -- n ] )
CAST: AS-N ( n -- [ -- n ] )
CAST: AS-BOOL ( n -- [ -- bool ] )
CAST: AS-SPAN ( n -- [ -- n n bool ] )
CAST: AS-TEXT ( n -- [ -- ptr u8 n ] )
CAST: AS-MARK-ACTION ( n -- [ n -- ] )
CAST: AS-CELLS ( n -- [ n -- n n ] )
CAST: AS-BINDING ( n -- [ n -- ptr u8 n ] )
CAST: AS-INIT-LAYOUT ( n -- [ n -- n n n ] )
CAST: AS-QUOT-CELLS ( n -- [ n n -- n n ] )

\ Checked bodies name field offsets through layout.f's NCOMP-DISPATCH:DECL-*.
\ CHECKER-OWNER-ABI loads before the checker, so build-fixpoint's stage-built
\ hb-host refuses a checked body naming CHECKER-OWNER-ABI:CHECK-OFF with
\ E-UNDEFINED, while NCOMP-DISPATCH:DECL-CHECK-OFF certifies there.

\ A field is read only after the guard bounds the record and its recorded
\ extent.
: RECORD-FIELD ( ptr u8 n ptr u8 n -- n ) {: r:ptr off:n a:ptr u:n :}
   r off CELL + CHECKER-OWNER-GUARD:VALIDATE {: rec:ptr :}
   rec off + CELL-VIEW @ {: xt:n :}
   xt 0= if a u REFUSE then
   xt ;

\ A field of the live owner, read per call.
: SOURCE-FIELD ( n ptr u8 n -- n ) {: off:n a:ptr u:n :}
   RECORD off a u RECORD-FIELD ;

\ Bound once while this source compiler and its checker are paired. The engine
\ refuses a tick of an internal pre-hook word and a checked body may not call a
\ trust-boundary one (E-CAP-TRUSTED), so the owner's
\ callable field is the binding. The record is this tree's checker's, found by
\ name: inside a build the live record is the building hb's.
: PAIRED-FIELD ( n ptr u8 n -- n ) {: off:n a:ptr u:n :}
   CHECKER-REG:DECLARATIONS BYTE-VIEW off a u RECORD-FIELD ;

: BIND-SOURCE-CALLS ( -- )
   NCOMP-DISPATCH:DECL-CHECK-UNJUDGED-OFF s" source unjudged scan" PAIRED-FIELD AS-CHECK is SOURCE-UNJUDGED
   NCOMP-DISPATCH:DECL-CALL-CELLS-OFF s" source call cells" PAIRED-FIELD AS-CELLS is SOURCE-CALL-CELLS
   NCOMP-DISPATCH:DECL-CALL-BINDING-OFF s" source call binding" PAIRED-FIELD AS-BINDING is SOURCE-CALL-BINDING
   NCOMP-DISPATCH:DECL-UNJUDGED-BINDING-OFF s" source unjudged binding" PAIRED-FIELD AS-BINDING is SOURCE-UNJUDGED-BINDING
   CHECKER-OWNER-ABI:BINDING-WINDOW-OFF s" source binding window" PAIRED-FIELD AS-N is SOURCE-BINDING-WINDOW
   NCOMP-DISPATCH:DECL-CALL-GLUE-OFF s" source call glue" PAIRED-FIELD AS-CELLS is SOURCE-CALL-GLUE
   NCOMP-DISPATCH:DECL-CALL-MATCH-OFF s" source match payload" PAIRED-FIELD AS-CELLS is SOURCE-MATCH-PAYLOAD
   NCOMP-DISPATCH:DECL-CALL-QUOT-IN-OFF s" source quotation inputs" PAIRED-FIELD AS-QUOT-CELLS is SOURCE-QUOT-IN
   NCOMP-DISPATCH:DECL-CALL-QUOT-OUT-OFF s" source quotation outputs" PAIRED-FIELD AS-QUOT-CELLS is SOURCE-QUOT-OUT
   NCOMP-DISPATCH:DECL-INIT-LAYOUT-OFF s" source init layout" PAIRED-FIELD AS-INIT-LAYOUT is SOURCE-INIT-LAYOUT
   NCOMP-DISPATCH:DECL-FIELD-SPAN-OFF s" source field span" PAIRED-FIELD AS-CELLS is SOURCE-FIELD-SPAN
   NCOMP-DISPATCH:DECL-C2-STOW-OFF s" source C2 stow" PAIRED-FIELD AS-CELLS is SOURCE-C2-STOW
   NCOMP-DISPATCH:DECL-ROWS-END-OFF s" source rows end" PAIRED-FIELD AS-N is SOURCE-ROWS-END
   NCOMP-DISPATCH:DECL-RETRACT-ROWS-OFF s" source rows retract" PAIRED-FIELD AS-MARK-ACTION is SOURCE-RETRACT-ROWS
   NCOMP-DISPATCH:DECL-WRITE-WINDOW-OFF s" source write window" PAIRED-FIELD AS-MARK-ACTION is SOURCE-WRITE-WINDOW
   NCOMP-DISPATCH:DECL-RESET-REPORT-OFF s" source report reset" PAIRED-FIELD AS-ACTION is SOURCE-REPORT-RESET
   NCOMP-DISPATCH:DECL-JSON-REPORTED-OFF s" source JSON report" PAIRED-FIELD AS-BOOL is SOURCE-JSON-REPORTED
   NCOMP-DISPATCH:DECL-DOES-UNJUDGED-OFF s" source unjudged does> split" PAIRED-FIELD AS-DOES-CHECK is SOURCE-DOES-UNJUDGED
   NCOMP-DISPATCH:DECL-DOES-CHECK-OFF s" source does> split" PAIRED-FIELD AS-DOES-CHECK is SOURCE-DOES-CHECK
   NCOMP-DISPATCH:DECL-NATIVE-DOES-REPORT-OFF s" source does> report" PAIRED-FIELD AS-DOES-REPORT is SOURCE-DOES-REPORT ;
BIND-SOURCE-CALLS

public

\ ---- the front end the checker IS: the scan, the does> split and the retract
\ of one ------------------------------------------------------------------------

: CHECK-UNJUDGED ( ptr u8 n -- n )
   SOURCE-UNJUDGED ;

: RESET-REPORT ( -- )
   SOURCE-REPORT-RESET ;

: JSON-REPORTED? ( -- bool )
   SOURCE-JSON-REPORTED ;

private

\ A clause is scanned in its parent's regime (BIND-REGIME!, set by the parent's
\ scan): unjudged under a TRUSTED: or hookless parent, so it binds and
\ authorizes calls as that parent's body does (checker.f CHECK-DOES-UNJUDGED!).
: DOES-UNJUDGED ( ptr u8 n ptr u8 n -- n )
   SOURCE-DOES-UNJUDGED ;

public

: DOES-CHECK ( ptr u8 n ptr u8 n -- n )
   BIND-UNJUDGED @ 0<> if DOES-UNJUDGED exit then
   SOURCE-DOES-CHECK ;

\ The split's scan renders nothing, so the reason for a clause it refused is
\ rendered here, under the clause name of the definer NAME (checker.f
\ CHECKER-NATIVE-DOES-REPORT), while the owner still holds that scan.
: DOES-REPORT ( n ptr u8 n -- n )
   SOURCE-DOES-REPORT ;

\ Whether the checker counted a refusal of this definition's declared row
\ (checker.f SIG-COUNTED). Read per call, not by name nor in BIND-SOURCE-CALLS:
\ a build host's checker lacks the name and its record the field.
: SIG-REFUSED? ( -- bool )
   NCOMP-DISPATCH:DECL-SIG-COUNTED-OFF s" counted signature refusal" SOURCE-FIELD
   AS-BOOL execute ;

\ The token the last scan refused: its offsets in the scanned text, equal when
\ none was pinned, and whether it is undefined (checker.f PIN-SPAN).
: REFUSED-SPAN ( -- n n bool )
   NCOMP-DISPATCH:DECL-PIN-SPAN-OFF s" refused token span" SOURCE-FIELD
   AS-SPAN execute ;

\ The text of the token the last scan refused, empty when none was pinned
\ (checker.f PIN-TOKEN). Read per call, as SIG-REFUSED? is.
: ERROR-TOKEN ( -- ptr u8 n )
   NCOMP-DISPATCH:DECL-PIN-TOKEN-OFF s" refused token text" SOURCE-FIELD
   AS-TEXT execute ;

\ The store's end when a definition begins, and a refused one's cut back to it:
\ exactly the rows it recorded go, and every row recorded before it stays
\ (checker.f CHECKER-RETRACT-ROWS).
: ROWS-END ( -- n )
   SOURCE-ROWS-END ;

: RETRACT-ROWS ( n -- )
   SOURCE-RETRACT-ROWS ;

\ Opens the compile window with the code a callback's checker write is refused
\ with, or shuts it with 0: between them only the compiler's callbacks run, so
\ the cut above takes exactly the definition's rows (checker.f WRITE-WINDOW).
: WRITE-WINDOW ( n -- )
   SOURCE-WRITE-WINDOW ;


\ ---- the finalized per-call-site facts the scan recorded ----------------

: CALL-CELLS ( n -- n n )
   SOURCE-CALL-CELLS ;

: CALL-BINDING ( n -- ptr u8 n )
   SOURCE-CALL-BINDING ;

: UNJUDGED-BINDING ( n -- ptr u8 n )
   SOURCE-UNJUDGED-BINDING ;

: BIND-REGIME! ( n -- ) BIND-UNJUDGED ! ;

: SOURCE-BINDING ( n -- ptr u8 n )
   BIND-UNJUDGED @ 0<> if UNJUDGED-BINDING else CALL-BINDING then ;

private

\ A field's execution token, or zero where a retained host's owner record is too
\ short to hold it or its cell is zero (SOURCE-FIELD refuses both).
: OPTIONAL-FIELD ( n -- n ) {: off:n :}
   RECORD 0 CHECKER-OWNER-GUARD:VALIDATE {: rec:ptr :}
   rec 8 - CELL-VIEW @ off CELL + < if 0 exit then
   rec off + CELL-VIEW @ ;

public

\ Whether this definition binds from an unjudged scan that left a construct
\ open (checker.f UNJUDGED-CLOSED?): its window answers no binding.
: UNJUDGED-OPEN? ( -- bool )
   BIND-UNJUDGED @ 0= if false exit then
   NCOMP-DISPATCH:DECL-UNJUDGED-CLOSED-OFF OPTIONAL-FIELD {: xt:n :}
   xt 0= if false exit then
   xt AS-BOOL execute 0= ;

\ The current source owner and scan identity. Its validity is checked when a
\ compiler binds or borrows rows, so a rejected tape can still be sealed.
: BINDING-WINDOW ( -- ptr u8 n bool )
   RECORD
   SOURCE-BINDING-WINDOW
   BIND-UNJUDGED @ 0<> ;

: BINDING-WINDOW-CK ( ptr u8 n bool -- )
   {: owner:ptr serial:n unjudged:bool :}
   RECORD owner <> if CHECKER-OWNER-ABI:BINDING-RC throw then
   unjudged if BIND-UNJUDGED @ 0= else BIND-UNJUDGED @ 0<> then
   if CHECKER-OWNER-ABI:BINDING-RC throw then
   SOURCE-BINDING-WINDOW
   serial <> if CHECKER-OWNER-ABI:BINDING-RC throw then ;

: INIT-LAYOUT ( n -- n n n )
   SOURCE-INIT-LAYOUT ;

: FIELD-SPAN ( n -- n n )
   SOURCE-FIELD-SPAN ;

: C2-STOW ( n -- n n )
   SOURCE-C2-STOW ;

: CALL-GLUE ( n -- n n )
   SOURCE-CALL-GLUE ;

: MATCH-PAYLOAD ( n -- n n )
   SOURCE-MATCH-PAYLOAD ;

: CALL-QUOT-IN ( n n -- n n )
   SOURCE-QUOT-IN ;

: CALL-QUOT-OUT ( n n -- n n )
   SOURCE-QUOT-OUT ;

: SCAN-INCOMPLETE? ( -- bool )
   SCAN-INCOMPLETE @ 0<> ;

;package
