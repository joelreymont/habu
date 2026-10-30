\ checker-owner.f - the checker this compiler asks, resolved per definition.
\ A source-loaded compiler binds the checker beside it from the same tree.
\ Capture switches it to dispatch through the live declaration-owner record,
\ so a baked compiler follows a replacement checker instead of its old names.
\ The scan, call facts and effect-query readers must all reach that same owner.
\ Missing fields refuse with E-NCOMP-OWNER; they never fall back by name.

require src/lib/prelude.f
require src/lib/errors.f
require src/core/checker-owner-guard.f
require src/habu/layout.f

package CHECKER-OWNER

\ The live source owner. Zero until an engine publishes one.
: RECORD ( -- ptr u8 )
   NCOMP-DISPATCH:DECL-A @ ;

\ Set by this module's load, cleared before the compiler is captured.
variable SOURCE-LOADED   -1 SOURCE-LOADED !
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
defer SOURCE-REPORT ( -- )
defer SOURCE-DECLARED-ROW ( ptr u8 n ptr u8 n -- )
defer SOURCE-ROWS-END ( -- n )
defer SOURCE-RETRACT-ROWS ( n -- )
defer SOURCE-WRITE-WINDOW ( n -- )
defer SOURCE-REPORT-RESET ( -- )
defer SOURCE-JSON-REPORTED ( -- bool )
defer SOURCE-DOES-UNJUDGED ( ptr u8 n ptr u8 n -- n )
defer SOURCE-DOES-REPORT ( n ptr u8 n -- n )

: REFUSE ( ptr u8 n -- ) {: a:ptr u:n :}
   2 s" ncomp: the source owner carries no " write drop
   2 a u write drop
   2 S\" \n" write drop
   E-NCOMP-OWNER throw ;

\ The owner record stores raw execution tokens; each view states a field's
\ signature so native execute has a callable layout.
CAST: AS-CHECK ( n -- [ ptr u8 n -- n ] )
CAST: AS-TAPE-INSTALL ( n -- [ n [ ptr u8 n -- ] [ ptr u8 n n n n n -- ] [ ptr u8 n n -- ] -- ] )
CAST: AS-ACTION ( n -- [ -- ] )
CAST: AS-DOES-CHECK ( n -- [ ptr u8 n ptr u8 n -- n ] )
CAST: AS-DOES-REPORT ( n -- [ n ptr u8 n -- n ] )
CAST: AS-DOES-FINISH ( n -- [ ptr u8 n bool -- ] )
CAST: AS-N ( n -- [ -- n ] )
CAST: AS-BOOL ( n -- [ -- bool ] )
CAST: AS-MARK-ACTION ( n -- [ n -- ] )
CAST: AS-CELLS ( n -- [ n -- n n ] )
CAST: AS-BINDING ( n -- [ n -- ptr u8 n ] )
CAST: AS-RESOLUTION-NS-INSTALL ( n -- [ [ ptr u8 n -- ] -- ] )
CAST: AS-RESOLUTION-TOP ( n -- [ ptr u8 n [ ptr u8 n -- ] -- ] )
CAST: AS-INIT-LAYOUT ( n -- [ n -- n n n ] )
CAST: AS-QUOT-CELLS ( n -- [ n n -- n n ] )
CAST: AS-DECLARATION ( n -- [ ptr u8 n ptr u8 n -- ] )
CAST: AS-NAME-PREDICATE ( n -- [ ptr u8 n -- bool ] )
CAST: AS-SLOT ( n -- [ n -- n ] )
CAST: AS-SLOT-PREDICATE ( n -- [ n -- bool ] )
CAST: AS-FINALLY-CELLS ( n -- [ n -- n n n ] )
CAST: AS-EVENT ( n -- [ n -- n n n ] )
CAST: AS-WIDTH-ROW ( n -- [ n -- n n n ] )
CAST: AS-WIDTH ( n -- [ n n -- n ] )
CAST: AS-FAMILY ( n -- [ ptr u8 n -- n bool ] )
CAST: AS-VARIANT ( n -- [ ptr u8 n n -- n bool ] )
CAST: AS-FAMILY-NAME ( n -- [ n -- ptr u8 n ] )

\ Checked bodies name field offsets through layout.f's NCOMP-DISPATCH:DECL-*.
\ CHECKER-OWNER-ABI loads before the checker, so build-fixpoint's stage-built
\ hb-host refuses a checked body naming CHECKER-OWNER-ABI:CHECK-OFF with
\ E-UNDEFINED, while NCOMP-DISPATCH:DECL-CHECK-OFF certifies there.

\ Bound once while this source compiler and its checker are paired. The engine
\ refuses a tick of an internal pre-hook word and a checked body may not call a
\ trust-boundary one (CHECKER-CHECK-REPORT, E-CAP-TRUSTED), so the owner's
\ callable field is the binding. A field is read only after the guard bounds
\ the record and its recorded extent.
: SOURCE-FIELD ( n ptr u8 n -- n ) {: off:n a:ptr u:n :}
   RECORD off CELL + CHECKER-OWNER-GUARD:VALIDATE {: rec:ptr :}
   rec off + CELL-VIEW @ {: xt:n :}
   xt 0= if a u REFUSE then
   xt ;

: BIND-SOURCE-CALLS ( -- )
   NCOMP-DISPATCH:DECL-CHECK-UNJUDGED-OFF s" source unjudged scan" SOURCE-FIELD AS-CHECK is SOURCE-UNJUDGED
   NCOMP-DISPATCH:DECL-CALL-CELLS-OFF s" source call cells" SOURCE-FIELD AS-CELLS is SOURCE-CALL-CELLS
   NCOMP-DISPATCH:DECL-CALL-BINDING-OFF s" source call binding" SOURCE-FIELD AS-BINDING is SOURCE-CALL-BINDING
   NCOMP-DISPATCH:DECL-UNJUDGED-BINDING-OFF s" source unjudged binding" SOURCE-FIELD AS-BINDING is SOURCE-UNJUDGED-BINDING
   CHECKER-OWNER-ABI:BINDING-WINDOW-OFF s" source binding window" SOURCE-FIELD AS-N is SOURCE-BINDING-WINDOW
   NCOMP-DISPATCH:DECL-CALL-GLUE-OFF s" source call glue" SOURCE-FIELD AS-CELLS is SOURCE-CALL-GLUE
   NCOMP-DISPATCH:DECL-CALL-MATCH-OFF s" source match payload" SOURCE-FIELD AS-CELLS is SOURCE-MATCH-PAYLOAD
   NCOMP-DISPATCH:DECL-CALL-QUOT-IN-OFF s" source quotation inputs" SOURCE-FIELD AS-QUOT-CELLS is SOURCE-QUOT-IN
   NCOMP-DISPATCH:DECL-CALL-QUOT-OUT-OFF s" source quotation outputs" SOURCE-FIELD AS-QUOT-CELLS is SOURCE-QUOT-OUT
   NCOMP-DISPATCH:DECL-INIT-LAYOUT-OFF s" source init layout" SOURCE-FIELD AS-INIT-LAYOUT is SOURCE-INIT-LAYOUT
   NCOMP-DISPATCH:DECL-FIELD-SPAN-OFF s" source field span" SOURCE-FIELD AS-CELLS is SOURCE-FIELD-SPAN
   NCOMP-DISPATCH:DECL-C2-STOW-OFF s" source C2 stow" SOURCE-FIELD AS-CELLS is SOURCE-C2-STOW
   NCOMP-DISPATCH:DECL-CHECK-REPORT-OFF s" source scan report" SOURCE-FIELD AS-ACTION is SOURCE-REPORT
   NCOMP-DISPATCH:DECL-DECLARED-ROW-OFF s" source declared row" SOURCE-FIELD AS-DECLARATION is SOURCE-DECLARED-ROW
   NCOMP-DISPATCH:DECL-ROWS-END-OFF s" source rows end" SOURCE-FIELD AS-N is SOURCE-ROWS-END
   NCOMP-DISPATCH:DECL-RETRACT-ROWS-OFF s" source rows retract" SOURCE-FIELD AS-MARK-ACTION is SOURCE-RETRACT-ROWS
   NCOMP-DISPATCH:DECL-WRITE-WINDOW-OFF s" source write window" SOURCE-FIELD AS-MARK-ACTION is SOURCE-WRITE-WINDOW
   NCOMP-DISPATCH:DECL-RESET-REPORT-OFF s" source report reset" SOURCE-FIELD AS-ACTION is SOURCE-REPORT-RESET
   NCOMP-DISPATCH:DECL-JSON-REPORTED-OFF s" source JSON report" SOURCE-FIELD AS-BOOL is SOURCE-JSON-REPORTED
   NCOMP-DISPATCH:DECL-DOES-UNJUDGED-OFF s" source unjudged does> split" SOURCE-FIELD AS-DOES-CHECK is SOURCE-DOES-UNJUDGED
   NCOMP-DISPATCH:DECL-NATIVE-DOES-REPORT-OFF s" source does> report" SOURCE-FIELD AS-DOES-REPORT is SOURCE-DOES-REPORT ;
BIND-SOURCE-CALLS

\ Zero selects this source compiler's by-name binding; a captured compiler
\ requires an installed operation from the live source owner.
: FIELD ( n ptr u8 n -- n ) {: off:n a:ptr u:n :}
   SOURCE-LOADED @ 0<> if 0 exit then
   off a u SOURCE-FIELD ;

public

: RECORD? ( -- bool )
   RECORD 0= 0= ;

: BY-NAME? ( -- bool )
   SOURCE-LOADED @ 0<> ;

\ A baked compiler must be told which checker owns the source: clearing the
\ latch makes the built engine refuse a missing field instead of reaching for a
\ name (called by src/compiler/native/compiler.f CAPTURE-PREPARE).
: CAPTURE-PREPARE ( -- )
   0 SOURCE-LOADED ! ;

\ ---- the front end the checker IS: the scan, the tape it fills, the does> split,
\ the declared-effect row and the retract of one ----------------

: CHECK ( ptr u8 n -- n )
   NCOMP-DISPATCH:DECL-CHECK-OFF s" scan" FIELD
   dup 0= if drop CHECK! exit then
   AS-CHECK execute ;

\ An operation, not a read of the owner's quiet counter: a baked compiler reaches
\ a counter only by name, which is the retired instance, so the suppression and
\ the render would land on different checkers.
: CHECK-UNJUDGED ( ptr u8 n -- n )
   NCOMP-DISPATCH:DECL-CHECK-UNJUDGED-OFF s" unjudged scan" FIELD
   dup 0= if drop SOURCE-UNJUDGED exit then
   AS-CHECK execute ;

\ The diagnostic that scan suppressed, rendered by the same owner while it still
\ holds the scan's state: the reason for a verdict this compiler does not
\ enforce but may refuse over (definition.f CHECK-HOOKLESS).
: REPORT ( -- )
   NCOMP-DISPATCH:DECL-CHECK-REPORT-OFF s" scan report" FIELD
   dup 0= if drop SOURCE-REPORT exit then
   AS-ACTION execute ;

\ NATIVE MULTI-ERROR RECOVERY AND DIAGNOSTICS. Fields appended after the
\ retained host's owner (src/core/checker-owner-extension.f), named through
\ layout.f like every field above and read through the live source owner so a
\ captured compiler never asks a stale checker by name.
: RESET-REPORT ( -- )
   NCOMP-DISPATCH:DECL-RESET-REPORT-OFF s" report reset" FIELD
   dup 0= if drop SOURCE-REPORT-RESET exit then
   AS-ACTION execute ;

: JSON-REPORTED? ( -- bool )
   NCOMP-DISPATCH:DECL-JSON-REPORTED-OFF s" JSON report" FIELD
   dup 0= if drop SOURCE-JSON-REPORTED exit then
   AS-BOOL execute ;

\ The source owner that counted a refusal decides whether compilation may
\ continue to the next definition.
TRUSTED: MULTI-ERROR? ( -- bool )
   NCOMP-DISPATCH:DECL-MULTI-ERROR-OFF s" multi-error mode" FIELD
   dup 0= if drop MULTI-ERR? exit then
   AS-BOOL execute ;

\ Whether the definition just checked bound a refused declaration's recovery
\ row (checker.f RECOVERY-BIND): it is checked for diagnostics, never published.
TRUSTED: RECOVERY-USED? ( -- bool )
   NCOMP-DISPATCH:DECL-RECOVERY-USED-OFF s" recovery use" FIELD
   dup 0= if drop CHECKER-EFFECT-AUTHORITY:RECOVERY-USED? exit then
   AS-BOOL execute ;

\ CHECKER-TAPE token-kind constants remain part of the shared observer ABI.
: TAPE-INSTALL ( n [ ptr u8 n -- ] [ ptr u8 n n n n n -- ] [ ptr u8 n n -- ] -- )
   NCOMP-DISPATCH:DECL-TAPE-INSTALL-OFF s" tape install" FIELD
   dup 0= if drop CHECKER-TAPE:INSTALL exit then
   AS-TAPE-INSTALL execute ;

: TAPE-ARM ( -- )
   NCOMP-DISPATCH:DECL-TAPE-ARM-OFF s" tape arm" FIELD
   dup 0= if drop CHECKER-TAPE:ARM exit then
   AS-ACTION execute ;

: TAPE-DISARM ( -- )
   NCOMP-DISPATCH:DECL-TAPE-DISARM-OFF s" tape disarm" FIELD
   dup 0= if drop CHECKER-TAPE:DISARM exit then
   AS-ACTION execute ;

: TAPE-ADVANCE ( -- )
   NCOMP-DISPATCH:DECL-TAPE-ADVANCE-OFF s" tape advance" FIELD
   dup 0= if drop CHECKER-TAPE:ADVANCE exit then
   AS-ACTION execute ;

: TAPE-EVENT ( n -- n n n )
   CHECKER-OWNER-EXT:TAPE-EVENT-OFF s" tape event" FIELD
   dup 0= if drop CHECKER-TAPE:EVENT exit then
   AS-EVENT execute ;

\ A clause is scanned in its parent's regime (BIND-REGIME!, set by the parent's
\ scan): unjudged under a TRUSTED: or hookless parent, so it binds and
\ authorizes calls as that parent's body does (checker.f CHECK-DOES-UNJUDGED!).
: DOES-UNJUDGED ( ptr u8 n ptr u8 n -- n )
   NCOMP-DISPATCH:DECL-DOES-UNJUDGED-OFF s" unjudged does> split" FIELD
   dup 0= if drop SOURCE-DOES-UNJUDGED exit then
   AS-DOES-CHECK execute ;

\ CHECK-DOES! binds CHECKER-OWNER's private row (src/core/checker.f).
: DOES-CHECK ( ptr u8 n ptr u8 n -- n )
   BIND-UNJUDGED @ 0<> if DOES-UNJUDGED exit then
   NCOMP-DISPATCH:DECL-DOES-CHECK-OFF s" does> split" FIELD
   dup 0= if drop CHECK-DOES! exit then
   AS-DOES-CHECK execute ;

\ The split's scan renders nothing, so the reason for a clause it refused is
\ rendered here, under the clause name of the definer NAME (checker.f
\ CHECKER-NATIVE-DOES-REPORT), while the owner still holds that scan.
: DOES-REPORT ( n ptr u8 n -- n )
   NCOMP-DISPATCH:DECL-NATIVE-DOES-REPORT-OFF s" does> report" FIELD
   dup 0= if drop SOURCE-DOES-REPORT exit then
   AS-DOES-REPORT execute ;

TRUSTED: DOES-FINISH ( ptr u8 n bool -- )
   CHECKER-OWNER-ABI:NATIVE-DOES-FINISH-OFF s" does> publication" FIELD
   dup 0= if drop CHECKER-NATIVE-DOES-FINISH exit then
   AS-DOES-FINISH execute ;

TRUSTED: DOES-BEGIN ( -- )
   CHECKER-OWNER-ABI:NATIVE-DOES-BEGIN-OFF s" does> rollback frame" FIELD
   dup 0= if drop CHECKER-NATIVE-DOES-BEGIN exit then
   AS-ACTION execute ;

TRUSTED: DOES-COMMIT ( -- )
   CHECKER-OWNER-ABI:NATIVE-DOES-COMMIT-OFF s" does> rollback release" FIELD
   dup 0= if drop CHECKER-NATIVE-DOES-COMMIT exit then
   AS-ACTION execute ;

: DOES-IN ( -- n )
   NCOMP-DISPATCH:DECL-DOES-IN-OFF s" does> input cells" FIELD
   dup 0= if drop CHECK-DOES-DIN-CELLS exit then
   AS-N execute ;

: DOES-OUT ( -- n )
   NCOMP-DISPATCH:DECL-DOES-OUT-OFF s" does> output cells" FIELD
   dup 0= if drop CHECK-DOES-DOUT-CELLS exit then
   AS-N execute ;

\ The cells the definition just certified takes and leaves when its inferred
\ effect was not recorded (W-EFFECT-NOT-RECORDED), else CELLS-NONE: no row
\ carries them.
: UNRECORDED-IN ( -- n )
   NCOMP-DISPATCH:DECL-UNRECORDED-IN-OFF s" unrecorded input cells" FIELD
   dup 0= if drop CHECK-UNRECORDED-DIN exit then
   AS-N execute ;

: UNRECORDED-OUT ( -- n )
   NCOMP-DISPATCH:DECL-UNRECORDED-OUT-OFF s" unrecorded output cells" FIELD
   dup 0= if drop CHECK-UNRECORDED-DOUT exit then
   AS-N execute ;

: DOES-WIDE? ( -- bool )
   NCOMP-DISPATCH:DECL-DOES-WIDE-OFF s" does> width" FIELD
   dup 0= if drop CHECK-DOES-WIDE? exit then
   AS-BOOL execute ;

\ The does> row's own value boundaries. A clause's rows are rows: the terms and
\ the per-term bundle slot are read the same way a definition's DIN-N / DIN-SLOT
\ are read, and the placer (dict.f ROW-GLUE) asks the same questions of both.
: DOES-IN-N ( -- n )
   NCOMP-DISPATCH:DECL-DOES-IN-N-OFF s" does> input terms" FIELD
   dup 0= if drop CHECK-DOES-DIN-N exit then
   AS-N execute ;

: DOES-OUT-N ( -- n )
   NCOMP-DISPATCH:DECL-DOES-OUT-N-OFF s" does> output terms" FIELD
   dup 0= if drop CHECK-DOES-DOUT-N exit then
   AS-N execute ;

: DOES-IN-SLOT ( n -- n )
   NCOMP-DISPATCH:DECL-DOES-IN-SLOT-OFF s" does> input slot" FIELD
   dup 0= if drop CHECK-DOES-DIN-SLOT exit then
   AS-SLOT execute ;

: DOES-OUT-SLOT ( n -- n )
   NCOMP-DISPATCH:DECL-DOES-OUT-SLOT-OFF s" does> output slot" FIELD
   dup 0= if drop CHECK-DOES-DOUT-SLOT exit then
   AS-SLOT execute ;

\ The store's end when a definition begins, and a refused one's cut back to it:
\ exactly the rows it recorded go, and every row recorded before it stays
\ (checker.f CHECKER-RETRACT-ROWS).
: ROWS-END ( -- n )
   NCOMP-DISPATCH:DECL-ROWS-END-OFF s" rows end" FIELD
   dup 0= if drop SOURCE-ROWS-END exit then
   AS-N execute ;

: RETRACT-ROWS ( n -- )
   NCOMP-DISPATCH:DECL-RETRACT-ROWS-OFF s" rows retract" FIELD
   dup 0= if drop SOURCE-RETRACT-ROWS exit then
   AS-MARK-ACTION execute ;

\ Opens the compile window with the code a callback's checker write is refused
\ with, or shuts it with 0: between them only the compiler's callbacks run, so
\ the cut above takes exactly the definition's rows (checker.f WRITE-WINDOW).
: WRITE-WINDOW ( n -- )
   NCOMP-DISPATCH:DECL-WRITE-WINDOW-OFF s" write window" FIELD
   dup 0= if drop SOURCE-WRITE-WINDOW exit then
   AS-MARK-ACTION execute ;


\ ---- the finalized per-call-site facts the scan recorded ----------------

: CALL-CELLS ( n -- n n )
   NCOMP-DISPATCH:DECL-CALL-CELLS-OFF s" call cells" FIELD
   dup 0= if drop SOURCE-CALL-CELLS exit then
   AS-CELLS execute ;

: CALL-BINDING ( n -- ptr u8 n )
   NCOMP-DISPATCH:DECL-CALL-BINDING-OFF s" resolved call binding" FIELD
   dup 0= if drop SOURCE-CALL-BINDING exit then
   AS-BINDING execute ;

: UNJUDGED-BINDING ( n -- ptr u8 n )
   NCOMP-DISPATCH:DECL-UNJUDGED-BINDING-OFF s" unjudged source binding" FIELD
   dup 0= if drop SOURCE-UNJUDGED-BINDING exit then
   AS-BINDING execute ;

: BIND-REGIME! ( n -- ) BIND-UNJUDGED ! ;

: SOURCE-BINDING ( n -- ptr u8 n )
   BIND-UNJUDGED @ 0<> if UNJUDGED-BINDING else CALL-BINDING then ;

\ A resolution row contains the spelling and namespace copied by the active
\ source checker. These calls are paired with the same owner as the binding
\ window; the native feed consumes them before the next scan resets the table.
: RESOLUTION-COUNT ( -- n )
   CHECKER-OWNER-ABI:RESOLUTION-COUNT-OFF s" source resolution count" FIELD
   dup 0= if drop CHECKER-RESOLUTION:COUNT exit then
   AS-N execute ;

: RESOLUTION-ROW ( n -- ptr u8 n )
   CHECKER-OWNER-ABI:RESOLUTION-ROW-OFF s" source resolution row" FIELD
   dup 0= if drop CHECKER-RESOLUTION:ROW exit then
   AS-BINDING execute ;

\ The rows a top-level token of the spelling resolves through, one per leg the
\ engine searches, each passed to the visitor.
: RESOLUTION-TOP ( ptr u8 n [ ptr u8 n -- ] -- )
   CHECKER-OWNER-ABI:RESOLUTION-TOP-OFF s" top-level resolution" FIELD
   dup 0= if drop CHECKER-RESOLUTION:TOP exit then
   AS-RESOLUTION-TOP execute ;

\ The live checker's unit-export fact count (checker.f CHECKER-UNIT-FACTS), and
\ whether the owner answers it. This is the one optional field reader: a
\ retained host's owner predates the cell, so a record too short to hold it, or
\ one whose cell is zero, answers false instead of refusing as SOURCE-FIELD does.
\ Both regimes read the field: the checker's word is a trust-boundary primitive a
\ checked body may not call by name (E-CAP-TRUSTED).
: UNIT-FACTS ( -- n bool )
   RECORD 0 CHECKER-OWNER-GUARD:VALIDATE {: rec:ptr :}
   rec 8 - CELL-VIEW @ CHECKER-OWNER-EXT:UNIT-FACTS-OFF CELL + < if 0 false exit then
   rec CHECKER-OWNER-EXT:UNIT-FACTS-OFF + CELL-VIEW @ {: xt:n :}
   xt 0= if 0 false exit then
   xt AS-N execute true ;

: RESOLUTION-NS-INSTALL ( [ ptr u8 n -- ] -- )
   CHECKER-OWNER-ABI:RESOLUTION-NS-INSTALL-OFF s" namespace observer" FIELD
   dup 0= if drop CHECKER-RESOLUTION:NS-INSTALL exit then
   AS-RESOLUTION-NS-INSTALL execute ;

: RESOLUTION-NS-DEFAULT ( -- )
   CHECKER-OWNER-ABI:RESOLUTION-NS-DEFAULT-OFF s" namespace observer reset" FIELD
   dup 0= if drop CHECKER-RESOLUTION:NS-DEFAULT exit then
   AS-ACTION execute ;

\ The current source owner and scan identity. Its validity is checked when a
\ compiler binds or borrows rows, so a rejected tape can still be sealed.
: BINDING-WINDOW ( -- ptr u8 n bool )
   RECORD
   CHECKER-OWNER-ABI:BINDING-WINDOW-OFF s" source binding window" FIELD
   dup 0= if drop SOURCE-BINDING-WINDOW else AS-N execute then
   BIND-UNJUDGED @ 0<> ;

: BINDING-WINDOW-CK ( ptr u8 n bool -- )
   {: owner:ptr serial:n unjudged:bool :}
   RECORD owner <> if CHECKER-OWNER-ABI:BINDING-RC throw then
   unjudged if BIND-UNJUDGED @ 0= else BIND-UNJUDGED @ 0<> then
   if CHECKER-OWNER-ABI:BINDING-RC throw then
   CHECKER-OWNER-ABI:BINDING-WINDOW-OFF s" source binding window" FIELD
   dup 0= if drop SOURCE-BINDING-WINDOW else AS-N execute then
   serial <> if CHECKER-OWNER-ABI:BINDING-RC throw then ;

: INIT-LAYOUT ( n -- n n n )
   NCOMP-DISPATCH:DECL-INIT-LAYOUT-OFF s" init layout" FIELD
   dup 0= if drop SOURCE-INIT-LAYOUT exit then
   AS-INIT-LAYOUT execute ;

: FIELD-SPAN ( n -- n n )
   NCOMP-DISPATCH:DECL-FIELD-SPAN-OFF s" field span" FIELD
   dup 0= if drop SOURCE-FIELD-SPAN exit then
   AS-CELLS execute ;

: C2-STOW ( n -- n n )
   NCOMP-DISPATCH:DECL-C2-STOW-OFF s" C2 stow" FIELD
   dup 0= if drop SOURCE-C2-STOW exit then
   AS-CELLS execute ;

: CALL-GLUE ( n -- n n )
   NCOMP-DISPATCH:DECL-CALL-GLUE-OFF s" call glue" FIELD
   dup 0= if drop SOURCE-CALL-GLUE exit then
   AS-CELLS execute ;

: MATCH-PAYLOAD ( n -- n n )
   NCOMP-DISPATCH:DECL-CALL-MATCH-OFF s" match payload" FIELD
   dup 0= if drop SOURCE-MATCH-PAYLOAD exit then
   AS-CELLS execute ;

: CALL-QUOT-IN ( n n -- n n )
   NCOMP-DISPATCH:DECL-CALL-QUOT-IN-OFF s" quotation inputs" FIELD
   dup 0= if drop SOURCE-QUOT-IN exit then
   AS-QUOT-CELLS execute ;

: CALL-QUOT-OUT ( n n -- n n )
   NCOMP-DISPATCH:DECL-CALL-QUOT-OUT-OFF s" quotation outputs" FIELD
   dup 0= if drop SOURCE-QUOT-OUT exit then
   AS-QUOT-CELLS execute ;


\ ---- the declared-effect rows -------------------------------------------------

TRUSTED: DECLARED-EFFECT ( ptr u8 n ptr u8 n -- )
   CHECKER-OWNER-ABI:TRUST-DECL-OFF s" declared-effect row" FIELD
   dup 0= if drop TRUST-DECL exit then
   AS-DECLARATION execute ;

\ A hook-less definition's declaration as its row, without authority, where no
\ live row states the symbol (checker.f CHECKER-DECLARED-ROW!).
: DECLARED-ROW ( ptr u8 n ptr u8 n -- )
   NCOMP-DISPATCH:DECL-DECLARED-ROW-OFF s" declared row" FIELD
   dup 0= if drop SOURCE-DECLARED-ROW exit then
   AS-DECLARATION execute ;

: PARSE-IMM? ( ptr u8 n -- bool )
   NCOMP-DISPATCH:DECL-PARSE-IMM-OFF s" parse-neutral immediate" FIELD
   dup 0= if drop NEUTRAL-PARSE-IMM? exit then
   AS-NAME-PREDICATE execute ;


\ ---- the effect-store query group ---------------------------------------------
\ QUERY resolves a name into the instance's query state and every reader below
\ reads that state, so all of them must reach the same owner.

: QUERY ( ptr u8 n -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-QUERY-OFF s" effect query" FIELD
   dup 0= if drop EFFECT-QUERY exit then
   AS-NAME-PREDICATE execute ;

: DIN-N ( -- n )
   NCOMP-DISPATCH:DECL-EFFECT-DIN-N-OFF s" din terms" FIELD
   dup 0= if drop EFFECT-DIN-N exit then
   AS-N execute ;

: DOUT-N ( -- n )
   NCOMP-DISPATCH:DECL-EFFECT-DOUT-N-OFF s" dout terms" FIELD
   dup 0= if drop EFFECT-DOUT-N exit then
   AS-N execute ;

: DIN-CELLS ( -- n )
   NCOMP-DISPATCH:DECL-EFFECT-DIN-CELLS-OFF s" din cells" FIELD
   dup 0= if drop EFFECT-DIN-CELLS exit then
   AS-N execute ;

: DOUT-CELLS ( -- n )
   NCOMP-DISPATCH:DECL-EFFECT-DOUT-CELLS-OFF s" dout cells" FIELD
   dup 0= if drop EFFECT-DOUT-CELLS exit then
   AS-N execute ;

: DIN-SLOT ( n -- n )
   NCOMP-DISPATCH:DECL-EFFECT-DIN-SLOT-OFF s" din slot" FIELD
   dup 0= if drop EFFECT-DIN-SLOT exit then
   AS-SLOT execute ;

: DOUT-SLOT ( n -- n )
   NCOMP-DISPATCH:DECL-EFFECT-DOUT-SLOT-OFF s" dout slot" FIELD
   dup 0= if drop EFFECT-DOUT-SLOT exit then
   AS-SLOT execute ;

: DIN-CON ( n -- n )
   NCOMP-DISPATCH:DECL-EFFECT-DIN-CON-OFF s" din constructor" FIELD
   dup 0= if drop EFFECT-DIN-CON exit then
   AS-SLOT execute ;

: DOUT-CON ( n -- n )
   NCOMP-DISPATCH:DECL-EFFECT-DOUT-CON-OFF s" dout constructor" FIELD
   dup 0= if drop EFFECT-DOUT-CON exit then
   AS-SLOT execute ;

: STACK-STABLE? ( -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-STACK-STABLE-OFF s" stable stack rows" FIELD
   dup 0= if drop EFFECT-STACK-STABLE? exit then
   AS-BOOL execute ;

: DIN-QUOT ( n -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-DIN-QUOT-OFF s" din quotation" FIELD
   dup 0= if drop EFFECT-DIN-QUOT exit then
   AS-SLOT-PREDICATE execute ;

: DOUT-QUOT ( n -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-DOUT-QUOT-OFF s" dout quotation" FIELD
   dup 0= if drop EFFECT-DOUT-QUOT exit then
   AS-SLOT-PREDICATE execute ;

: QUOT-UP ( -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-QUOT-UP-OFF s" quotation upward" FIELD
   dup 0= if drop EFFECT-QUOT-UP exit then
   AS-BOOL execute ;

: QUOT-SIMPLE? ( -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-QUOT-SIMPLE-OFF s" simple quotation" FIELD
   dup 0= if drop EFFECT-QUOT-SIMPLE? exit then
   AS-BOOL execute ;

: CATCH-CELLS ( n -- n n )
   NCOMP-DISPATCH:DECL-EFFECT-CATCH-CELLS-OFF s" catch cells" FIELD
   dup 0= if drop EFFECT-CATCH-CELLS exit then
   AS-CELLS execute ;

: EXEC-CELLS ( n -- n n )
   NCOMP-DISPATCH:DECL-EFFECT-EXEC-CELLS-OFF s" execute cells" FIELD
   dup 0= if drop EFFECT-EXEC-CELLS exit then
   AS-CELLS execute ;

: FINALLY-CELLS ( n -- n n n )
   NCOMP-DISPATCH:DECL-EFFECT-FINALLY-CELLS-OFF s" finally cells" FIELD
   dup 0= if drop EFFECT-FINALLY-CELLS exit then
   AS-FINALLY-CELLS execute ;

: MATCH-CELLS ( n -- n )
   NCOMP-DISPATCH:DECL-EFFECT-MATCH-CELLS-OFF s" match cells" FIELD
   dup 0= if drop EFFECT-MATCH-CELLS exit then
   AS-SLOT execute ;

: DEAD-TOKEN? ( ptr u8 n -- bool )
   NCOMP-DISPATCH:DECL-CTL-DEAD-OFF s" dead control token" FIELD
   dup 0= if drop CTL-DEAD? exit then
   AS-NAME-PREDICATE execute ;

: WIDTH-AT ( n n -- n )
   NCOMP-DISPATCH:DECL-WF-W-AT-OFF s" width fact" FIELD
   dup 0= if drop WF-W-AT exit then
   AS-WIDTH execute ;

\ A scan's facts as its end leaves them, for a compiler that keeps them past the
\ next scan (t0.f ON-DONE): its width-fact rows, each the token's offset in the
\ scanned text, the operand position and its cells; and its binds, each the
\ cells that local holds.
: WIDTH-ROWS ( -- n )
   CHECKER-OWNER-EXT:WIDTH-ROWS-OFF s" width rows" FIELD
   dup 0= if drop WF-N@ exit then
   AS-N execute ;

: WIDTH-ROW ( n -- n n n )
   CHECKER-OWNER-EXT:WIDTH-ROW-OFF s" width row" FIELD
   dup 0= if drop WF-FACT@ exit then
   AS-WIDTH-ROW execute ;

: LOCAL-COUNT ( -- n )
   CHECKER-OWNER-EXT:LOCAL-COUNT-OFF s" local count" FIELD
   dup 0= if drop LOCW-HW-N@ exit then
   AS-N execute ;

: LOCAL-WIDTH ( n -- n )
   CHECKER-OWNER-ABI:LOCAL-WIDTH-OFF s" local width" FIELD
   dup 0= if drop LOCW-HW@ exit then
   AS-SLOT execute ;


\ ---- what the record a definition publishes needs from the checker ----------------

: MIN-IN ( -- n )
   NCOMP-DISPATCH:DECL-REC-MIN-IN-OFF s" minimum input arity" FIELD
   dup 0= if drop REC-MIN-IN@ exit then
   AS-N execute ;

: WIDE-PUBLISH ( -- )
   NCOMP-DISPATCH:DECL-REC-WIDE-PUBLISH-OFF s" wide record publish" FIELD
   dup 0= if drop REC-WIDE-PUBLISH exit then
   AS-ACTION execute ;

\ Family ids, variant ids and their metadata belong to the owner's registry.
\ Name lookup alone cannot transfer an id to a different registry's reader.
: FAMILY-MATCH ( ptr u8 n -- n bool )
   NCOMP-DISPATCH:DECL-FAMILY-MATCH-OFF s" match family" FIELD
   dup 0= if drop TFL-MATCH-FAM? exit then
   AS-FAMILY execute ;

: FAMILY-CON ( ptr u8 n -- n bool )
   NCOMP-DISPATCH:DECL-FAMILY-CON-OFF s" construct family" FIELD
   dup 0= if drop TFL-CON-FAM? exit then
   AS-FAMILY execute ;

: FAMILY-VARIANT ( ptr u8 n n -- n bool )
   NCOMP-DISPATCH:DECL-FAMILY-VARIANT-OFF s" family variant" FIELD
   dup 0= if drop TFAM:TFL-VAR? exit then
   AS-VARIANT execute ;

: FAMILY-SLOTS ( n -- n )
   NCOMP-DISPATCH:DECL-FAMILY-SLOTS-OFF s" family slots" FIELD
   dup 0= if drop TFAM:TFAM-SLOTS@ exit then
   AS-SLOT execute ;

: FAMILY-VARIANTS ( n -- n )
   NCOMP-DISPATCH:DECL-FAMILY-VARIANTS-OFF s" family variants" FIELD
   dup 0= if drop TFAM:TFAM-VAR-COUNT@ exit then
   AS-SLOT execute ;

: FAMILY-NAME$ ( n -- ptr u8 n )
   NCOMP-DISPATCH:DECL-FAMILY-NAME-OFF s" family name" FIELD
   dup 0= if drop TFAM-NAME$ exit then
   AS-FAMILY-NAME execute ;

: VARIANT-TAG ( n -- n )
   NCOMP-DISPATCH:DECL-VARIANT-TAG-OFF s" variant tag" FIELD
   dup 0= if drop TFAM:SUMV-TAG@ exit then
   AS-SLOT execute ;

: VARIANT-PADS ( n n -- n )
   NCOMP-DISPATCH:DECL-VARIANT-PADS-OFF s" variant pads" FIELD
   dup 0= if drop TFAM:TFL-VPADS exit then
   AS-WIDTH execute ;

: VARIANT-PAY-CELLS ( n -- n )
   NCOMP-DISPATCH:DECL-VARIANT-PAY-CELLS-OFF s" variant payload cells" FIELD
   dup 0= if drop TFAM:SUMV-PAYCELLS@ exit then
   AS-SLOT execute ;

: VARIANT-PAY-TERMS ( n -- n )
   NCOMP-DISPATCH:DECL-VARIANT-PAY-TERMS-OFF s" variant payload terms" FIELD
   dup 0= if drop TFAM:SUMV-PAY-N exit then
   AS-SLOT execute ;

;package
