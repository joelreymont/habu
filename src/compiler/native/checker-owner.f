\ checker-owner.f - the checker this compiler asks, resolved per definition.
\
\ A source-loaded compiler binds the checker beside it from the same tree.
\ Capture switches it to dispatch through the live declaration-owner record,
\ so a baked compiler follows a replacement checker instead of its old names.
\ The scan, call facts and effect-query readers must all reach that same owner.
\ Missing fields refuse with E-NCOMP-OWNER; they never fall back by name.
\ CHECKER-TAPE token-kind constants remain part of the shared observer ABI.

require lib/prelude.f
require lib/errors.f
require src/core/checker-owner-guard.f
require src/habu/layout.f

package CHECKER-OWNER

\ The live source owner. Zero until an engine publishes one.
: RECORD ( -- ptr u8 )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;

\ Set by this module's load, cleared before the compiler is captured.
variable SOURCE-LOADED   -1 SOURCE-LOADED !

\ A retained prefix may carry these pre-hook words without native call models.
\ Bind their actual execution tokens with their existing callable contracts.
defer SOURCE-UNJUDGED ( ptr u8 n -- n )
defer SOURCE-CALL-CELLS ( n -- n n )
defer SOURCE-CALL-GLUE ( n -- n n )
defer SOURCE-MATCH-PAYLOAD ( n -- n n )
defer SOURCE-QUOT-IN ( n n -- n n )
defer SOURCE-QUOT-OUT ( n n -- n n )
defer SOURCE-INIT-LAYOUT ( n -- n n n )
defer SOURCE-FIELD-SPAN ( n -- n n )
defer SOURCE-REPORT ( -- )

: REFUSE ( ptr u8 n -- ) {: a:ptr u:n :}
   2 s" ncomp: the source owner carries no " write drop
   2 a u write drop
   2 S\" \n" write drop
   E-NCOMP-OWNER throw ;

\ The owner record stores raw execution tokens. These views state each field
\ signature at that boundary so native execute has a callable layout. They do
\ not decide which owner is live or bypass FIELD's missing-field refusal.
CAST: AS-CHECK ( n -- [ ptr u8 n -- n ] )
CAST: AS-TAPE-INSTALL ( n -- [ n [ ptr u8 n -- ] [ ptr u8 n n n n n -- ] [ ptr u8 n n -- ] -- ] )
CAST: AS-ACTION ( n -- [ -- ] )
CAST: AS-DOES-CHECK ( n -- [ ptr u8 n ptr u8 n -- n ] )
CAST: AS-DOES-FINISH ( n -- [ ptr u8 n bool -- ] )
CAST: AS-N ( n -- [ -- n ] )
CAST: AS-BOOL ( n -- [ -- bool ] )
CAST: AS-NAME-ACTION ( n -- [ ptr u8 n -- ] )
CAST: AS-CELLS ( n -- [ n -- n n ] )
CAST: AS-INIT-LAYOUT ( n -- [ n -- n n n ] )
CAST: AS-QUOT-CELLS ( n -- [ n n -- n n ] )
CAST: AS-DECLARATION ( n -- [ ptr u8 n ptr u8 n -- ] )
CAST: AS-NAME-PREDICATE ( n -- [ ptr u8 n -- bool ] )
CAST: AS-SLOT ( n -- [ n -- n ] )
CAST: AS-SLOT-PREDICATE ( n -- [ n -- bool ] )
CAST: AS-FINALLY-CELLS ( n -- [ n -- n n n ] )
CAST: AS-WIDTH ( n -- [ n n -- n ] )
CAST: AS-FAMILY ( n -- [ ptr u8 n -- n bool ] )
CAST: AS-VARIANT ( n -- [ ptr u8 n n -- n bool ] )
CAST: AS-FAMILY-NAME ( n -- [ n -- ptr u8 n ] )

\ Checked bodies name field offsets through layout.f's NCOMP-DISPATCH:DECL-*.
\ CHECKER-OWNER-ABI loads before the checker, so build-fixpoint's stage-built
\ hb-host refuses a checked body naming CHECKER-OWNER-ABI:CHECK-OFF with
\ E-UNDEFINED, while NCOMP-DISPATCH:DECL-CHECK-OFF certifies there.

\ These callbacks are fields of the owner record. Bind them once while this
\ source compiler and its checker are paired. A tick of an internal pre-hook
\ word is intentionally refused by the JIT engine, and a checked body may not
\ call a trust-boundary one (CHECKER-CHECK-REPORT, E-CAP-TRUSTED); the owner's
\ callable field is the authority for this private typed binding.
: SOURCE-FIELD ( n ptr u8 n -- n ) {: off:n a:ptr u:n :}
   RECORD {: rec:ptr :}
   rec 0= if a u REFUSE then
   rec off + CELL-VIEW @ {: xt:n :}
   xt 0= if a u REFUSE then
   xt ;

: BIND-SOURCE-CALLS ( -- )
   NCOMP-DISPATCH:DECL-CHECK-UNJUDGED-OFF s" source unjudged scan" SOURCE-FIELD AS-CHECK is SOURCE-UNJUDGED
   NCOMP-DISPATCH:DECL-CALL-CELLS-OFF s" source call cells" SOURCE-FIELD AS-CELLS is SOURCE-CALL-CELLS
   NCOMP-DISPATCH:DECL-CALL-GLUE-OFF s" source call glue" SOURCE-FIELD AS-CELLS is SOURCE-CALL-GLUE
   NCOMP-DISPATCH:DECL-CALL-MATCH-OFF s" source match payload" SOURCE-FIELD AS-CELLS is SOURCE-MATCH-PAYLOAD
   NCOMP-DISPATCH:DECL-CALL-QUOT-IN-OFF s" source quotation inputs" SOURCE-FIELD AS-QUOT-CELLS is SOURCE-QUOT-IN
   NCOMP-DISPATCH:DECL-CALL-QUOT-OUT-OFF s" source quotation outputs" SOURCE-FIELD AS-QUOT-CELLS is SOURCE-QUOT-OUT
   NCOMP-DISPATCH:DECL-INIT-LAYOUT-OFF s" source init layout" SOURCE-FIELD AS-INIT-LAYOUT is SOURCE-INIT-LAYOUT
   NCOMP-DISPATCH:DECL-FIELD-SPAN-OFF s" source field span" SOURCE-FIELD AS-CELLS is SOURCE-FIELD-SPAN
   NCOMP-DISPATCH:DECL-CHECK-REPORT-OFF s" source scan report" SOURCE-FIELD AS-ACTION is SOURCE-REPORT ;
BIND-SOURCE-CALLS

\ Zero selects this source compiler's by-name binding; a captured compiler
\ requires an installed operation from the live source owner.
: FIELD ( n ptr u8 n -- n ) {: off:n a:ptr u:n :}
   SOURCE-LOADED @ 0<> if 0 exit then
   RECORD off CELL + CHECKER-OWNER-GUARD:VALIDATE {: rec:ptr :}
   rec off + CELL-VIEW @ {: xt:n :}
   xt 0= if a u REFUSE then
   xt ;

public

\ What the live owner answers with, for a caller that wants to name the regime in
\ a diagnostic or a test rather than infer it.
: RECORD? ( -- bool )
   RECORD 0= 0= ;

\ Which regime this compiler is in, for the same reason: a test states it instead
\ of deducing it from a refusal.
: BY-NAME? ( -- bool )
   SOURCE-LOADED @ 0<> ;

\ The capture seam: what is written into an image is a BAKED compiler, and a baked
\ compiler has to be told which checker owns the source. Clearing the latch here
\ is what makes the built engine refuse a missing field instead of reaching for a
\ name (src/compiler/native/compiler.f CAPTURE-PREPARE calls this).
: CAPTURE-PREPARE ( -- )
   0 SOURCE-LOADED ! ;

\ ---- the front end the checker IS: the scan, the tape it fills, the does> split,
\ the declared-effect row and the retract of one ----------------

: CHECK ( ptr u8 n -- n )
   NCOMP-DISPATCH:DECL-CHECK-OFF s" scan" FIELD
   dup 0= if drop CHECK! exit then
   AS-CHECK execute ;

\ The same scan with the owner's diagnostic render suppressed, for a body whose
\ verdict this compiler will not enforce. It is an OPERATION rather than a read of
\ the owner's quiet counter because a baked compiler can reach a counter only by
\ name, and by name is the retired instance: the suppression then landed on one
\ checker and the render on the other, and a product-hosted build printed a
\ rejection for every trusted body in the window it was rebuilding.
: CHECK-UNJUDGED ( ptr u8 n -- n )
   NCOMP-DISPATCH:DECL-CHECK-UNJUDGED-OFF s" unjudged scan" FIELD
   dup 0= if drop SOURCE-UNJUDGED exit then
   AS-CHECK execute ;

\ The diagnostic that scan suppressed, rendered by the same owner while it still
\ holds the scan's state: the reason for a verdict this compiler does not
\ enforce but may refuse over (compiler.f CHECK-HOOKLESS).
: REPORT ( -- )
   NCOMP-DISPATCH:DECL-CHECK-REPORT-OFF s" scan report" FIELD
   dup 0= if drop SOURCE-REPORT exit then
   AS-ACTION execute ;

\ The source owner that counted a refusal decides whether compilation may
\ continue to the next definition. A captured compiler never asks a stale
\ checker instance by name.
TRUSTED: MULTI-ERROR? ( -- bool )
   NCOMP-DISPATCH:DECL-MULTI-ERROR-OFF s" multi-error mode" FIELD
   dup 0= if drop MULTI-ERR? exit then
   AS-BOOL execute ;

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

\ CHECK-DOES! binds CHECKER-OWNER's private row (src/core/checker.f).
: DOES-CHECK ( ptr u8 n ptr u8 n -- n )
   NCOMP-DISPATCH:DECL-DOES-CHECK-OFF s" does> split" FIELD
   dup 0= if drop CHECK-DOES! exit then
   AS-DOES-CHECK execute ;

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

: USIG-TRUNCATE ( ptr u8 n -- )
   NCOMP-DISPATCH:DECL-USIG-TRUNCATE-OFF s" signature retract" FIELD
   dup 0= if drop CHECKER-USIGS-TRUNCATE-FROM-RAW exit then
   AS-NAME-ACTION execute ;


\ ---- the finalized per-call-site facts the scan recorded ----------------

: CALL-CELLS ( n -- n n )
   NCOMP-DISPATCH:DECL-CALL-CELLS-OFF s" call cells" FIELD
   dup 0= if drop SOURCE-CALL-CELLS exit then
   AS-CELLS execute ;

: INIT-LAYOUT ( n -- n n n )
   NCOMP-DISPATCH:DECL-INIT-LAYOUT-OFF s" init layout" FIELD
   dup 0= if drop SOURCE-INIT-LAYOUT exit then
   AS-INIT-LAYOUT execute ;

: FIELD-SPAN ( n -- n n )
   NCOMP-DISPATCH:DECL-FIELD-SPAN-OFF s" field span" FIELD
   dup 0= if drop SOURCE-FIELD-SPAN exit then
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


\ ---- the front end the checker IS: the scan, the tape it fills, the does> split,
\ the declared-effect row and the retract of one ----------------

TRUSTED: DECLARED-EFFECT ( ptr u8 n ptr u8 n -- )
   CHECKER-OWNER-ABI:TRUST-DECL-OFF s" declared-effect row" FIELD
   dup 0= if drop TRUST-DECL exit then
   AS-DECLARATION execute ;

: PARSE-IMM? ( ptr u8 n -- bool )
   NCOMP-DISPATCH:DECL-PARSE-IMM-OFF s" parse-neutral immediate" FIELD
   dup 0= if drop NEUTRAL-PARSE-IMM? exit then
   AS-NAME-PREDICATE execute ;


\ ---- the effect-store query group. QUERY resolves a name into the instance's
\ query state and every reader below reads THAT state, so all of them have to
\ reach the same owner or a reader answers about another instance's query ----------------

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

: RET-NEUTRAL? ( -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-RET-NEUTRAL-OFF s" return neutrality" FIELD
   dup 0= if drop EFFECT-RET-NEUTRAL? exit then
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
