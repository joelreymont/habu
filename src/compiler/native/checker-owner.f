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
require src/core/checker-owner-abi.f
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

: REFUSE ( ptr u8 n -- ) {: a:ptr u:n :}
   2 s" ncomp: the source owner carries no " write drop
   2 a u write drop
   2 S\" \n" write drop
   E-NCOMP-OWNER throw ;

\ These six callbacks already belong to the original 54-cell owner record.
\ Bind them once while this source compiler and its checker are paired. A tick
\ of an internal pre-hook word is intentionally refused by the JIT engine;
\ the owner's callable field is the authority for this private typed binding.
: SOURCE-FIELD ( n ptr u8 n -- n ) {: off:n a:ptr u:n :}
   RECORD {: rec:ptr :}
   rec 0= if a u REFUSE then
   rec off + CELL-VIEW @ {: xt:n :}
   xt 0= if a u REFUSE then
   xt ;

TRUSTED: BIND-SOURCE-CALLS ( -- )
   CHECKER-OWNER-ABI:CHECK-UNJUDGED-OFF s" source unjudged scan" SOURCE-FIELD is SOURCE-UNJUDGED
   CHECKER-OWNER-ABI:CALL-CELLS-OFF s" source call cells" SOURCE-FIELD is SOURCE-CALL-CELLS
   CHECKER-OWNER-ABI:CALL-GLUE-OFF s" source call glue" SOURCE-FIELD is SOURCE-CALL-GLUE
   CHECKER-OWNER-ABI:CALL-MATCH-OFF s" source match payload" SOURCE-FIELD is SOURCE-MATCH-PAYLOAD
   CHECKER-OWNER-ABI:CALL-QUOT-IN-OFF s" source quotation inputs" SOURCE-FIELD is SOURCE-QUOT-IN
   CHECKER-OWNER-ABI:CALL-QUOT-OUT-OFF s" source quotation outputs" SOURCE-FIELD is SOURCE-QUOT-OUT ;
BIND-SOURCE-CALLS

\ Zero selects this source compiler's by-name binding; a captured compiler
\ requires an installed operation from the live source owner.
: FIELD ( n ptr u8 n -- n ) {: off:n a:ptr u:n :}
   SOURCE-LOADED @ 0<> if 0 exit then
   RECORD {: rec:ptr :}
   rec 0= if a u REFUSE then
   rec off + CELL-VIEW @ {: xt:n :}
   xt 0= if a u REFUSE then
   xt ;

\ The owner record stores raw execution tokens. These views state each field
\ signature at that boundary so native execute has a callable layout. They do
\ not decide which owner is live or bypass FIELD's missing-field refusal.
TRUSTED: AS-CHECK ( n -- [ ptr u8 n -- n ] ) ;
TRUSTED: AS-TAPE-INSTALL ( n -- [ n [ ptr u8 n -- ] [ ptr u8 n n n n n -- ] [ ptr u8 n n -- ] -- ] ) ;
TRUSTED: AS-ACTION ( n -- [ -- ] ) ;
TRUSTED: AS-DOES-CHECK ( n -- [ ptr u8 n ptr u8 n -- n ] ) ;
TRUSTED: AS-N ( n -- [ -- n ] ) ;
TRUSTED: AS-BOOL ( n -- [ -- bool ] ) ;
TRUSTED: AS-NAME-ACTION ( n -- [ ptr u8 n -- ] ) ;
TRUSTED: AS-CELLS ( n -- [ n -- n n ] ) ;
TRUSTED: AS-QUOT-CELLS ( n -- [ n n -- n n ] ) ;
TRUSTED: AS-DECLARATION ( n -- [ ptr u8 n ptr u8 n -- ] ) ;
TRUSTED: AS-NAME-PREDICATE ( n -- [ ptr u8 n -- bool ] ) ;
TRUSTED: AS-SLOT ( n -- [ n -- n ] ) ;
TRUSTED: AS-SLOT-PREDICATE ( n -- [ n -- bool ] ) ;
TRUSTED: AS-FINALLY-CELLS ( n -- [ n -- n n n ] ) ;
TRUSTED: AS-WIDTH ( n -- [ n n -- n ] ) ;
TRUSTED: AS-FAMILY ( n -- [ ptr u8 n -- n bool ] ) ;
TRUSTED: AS-VARIANT ( n -- [ ptr u8 n n -- n bool ] ) ;
TRUSTED: AS-FAMILY-NAME ( n -- [ n -- ptr u8 n ] ) ;

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

TRUSTED: CHECK ( ptr u8 n -- n )
   CHECKER-OWNER-ABI:CHECK-OFF s" scan" FIELD
   dup 0= if drop CHECK! exit then
   AS-CHECK execute ;

\ The same scan with the owner's diagnostic render suppressed, for a body whose
\ verdict this compiler will not enforce. It is an OPERATION rather than a read of
\ the owner's quiet counter because a baked compiler can reach a counter only by
\ name, and by name is the retired instance: the suppression then landed on one
\ checker and the render on the other, and a product-hosted build printed a
\ rejection for every trusted body in the window it was rebuilding.
TRUSTED: CHECK-UNJUDGED ( ptr u8 n -- n )
   CHECKER-OWNER-ABI:CHECK-UNJUDGED-OFF s" unjudged scan" FIELD
   dup 0= if drop SOURCE-UNJUDGED exit then
   AS-CHECK execute ;

TRUSTED: TAPE-INSTALL ( n [ ptr u8 n -- ] [ ptr u8 n n n n n -- ] [ ptr u8 n n -- ] -- )
   CHECKER-OWNER-ABI:TAPE-INSTALL-OFF s" tape install" FIELD
   dup 0= if drop CHECKER-TAPE:INSTALL exit then
   AS-TAPE-INSTALL execute ;

TRUSTED: TAPE-ARM ( -- )
   CHECKER-OWNER-ABI:TAPE-ARM-OFF s" tape arm" FIELD
   dup 0= if drop CHECKER-TAPE:ARM exit then
   AS-ACTION execute ;

TRUSTED: TAPE-DISARM ( -- )
   CHECKER-OWNER-ABI:TAPE-DISARM-OFF s" tape disarm" FIELD
   dup 0= if drop CHECKER-TAPE:DISARM exit then
   AS-ACTION execute ;

TRUSTED: TAPE-ADVANCE ( -- )
   CHECKER-OWNER-ABI:TAPE-ADVANCE-OFF s" tape advance" FIELD
   dup 0= if drop CHECKER-TAPE:ADVANCE exit then
   AS-ACTION execute ;

TRUSTED: DOES-CHECK ( ptr u8 n ptr u8 n -- n )
   CHECKER-OWNER-ABI:DOES-CHECK-OFF s" does> split" FIELD
   dup 0= if drop CHECK-DOES! exit then
   AS-DOES-CHECK execute ;

TRUSTED: DOES-IN ( -- n )
   CHECKER-OWNER-ABI:DOES-IN-OFF s" does> input cells" FIELD
   dup 0= if drop CHECK-DOES-DIN-CELLS exit then
   AS-N execute ;

TRUSTED: DOES-OUT ( -- n )
   CHECKER-OWNER-ABI:DOES-OUT-OFF s" does> output cells" FIELD
   dup 0= if drop CHECK-DOES-DOUT-CELLS exit then
   AS-N execute ;

TRUSTED: DOES-WIDE? ( -- bool )
   CHECKER-OWNER-ABI:DOES-WIDE-OFF s" does> width" FIELD
   dup 0= if drop CHECK-DOES-WIDE? exit then
   AS-BOOL execute ;

TRUSTED: USIG-TRUNCATE ( ptr u8 n -- )
   CHECKER-OWNER-ABI:USIG-TRUNCATE-OFF s" signature retract" FIELD
   dup 0= if drop CHECKER-USIGS-TRUNCATE-FROM-RAW exit then
   AS-NAME-ACTION execute ;


\ ---- the finalized per-call-site facts the scan recorded ----------------

TRUSTED: CALL-CELLS ( n -- n n )
   CHECKER-OWNER-ABI:CALL-CELLS-OFF s" call cells" FIELD
   dup 0= if drop SOURCE-CALL-CELLS exit then
   AS-CELLS execute ;

TRUSTED: CALL-GLUE ( n -- n n )
   CHECKER-OWNER-ABI:CALL-GLUE-OFF s" call glue" FIELD
   dup 0= if drop SOURCE-CALL-GLUE exit then
   AS-CELLS execute ;

TRUSTED: MATCH-PAYLOAD ( n -- n n )
   CHECKER-OWNER-ABI:CALL-MATCH-OFF s" match payload" FIELD
   dup 0= if drop SOURCE-MATCH-PAYLOAD exit then
   AS-CELLS execute ;

TRUSTED: CALL-QUOT-IN ( n n -- n n )
   CHECKER-OWNER-ABI:CALL-QUOT-IN-OFF s" quotation inputs" FIELD
   dup 0= if drop SOURCE-QUOT-IN exit then
   AS-QUOT-CELLS execute ;

TRUSTED: CALL-QUOT-OUT ( n n -- n n )
   CHECKER-OWNER-ABI:CALL-QUOT-OUT-OFF s" quotation outputs" FIELD
   dup 0= if drop SOURCE-QUOT-OUT exit then
   AS-QUOT-CELLS execute ;


\ ---- the front end the checker IS: the scan, the tape it fills, the does> split,
\ the declared-effect row and the retract of one ----------------

TRUSTED: DECLARED-EFFECT ( ptr u8 n ptr u8 n -- )
   CHECKER-OWNER-ABI:TRUST-DECL-OFF s" declared-effect row" FIELD
   dup 0= if drop TRUST-DECL exit then
   AS-DECLARATION execute ;

TRUSTED: PARSE-IMM? ( ptr u8 n -- bool )
   CHECKER-OWNER-ABI:PARSE-IMM-OFF s" parse-neutral immediate" FIELD
   dup 0= if drop NEUTRAL-PARSE-IMM? exit then
   AS-NAME-PREDICATE execute ;


\ ---- the effect-store query group. QUERY resolves a name into the instance's
\ query state and every reader below reads THAT state, so all of them have to
\ reach the same owner or a reader answers about another instance's query ----------------

TRUSTED: QUERY ( ptr u8 n -- bool )
   CHECKER-OWNER-ABI:EFFECT-QUERY-OFF s" effect query" FIELD
   dup 0= if drop EFFECT-QUERY exit then
   AS-NAME-PREDICATE execute ;

TRUSTED: DIN-N ( -- n )
   CHECKER-OWNER-ABI:EFFECT-DIN-N-OFF s" din terms" FIELD
   dup 0= if drop EFFECT-DIN-N exit then
   AS-N execute ;

TRUSTED: DOUT-N ( -- n )
   CHECKER-OWNER-ABI:EFFECT-DOUT-N-OFF s" dout terms" FIELD
   dup 0= if drop EFFECT-DOUT-N exit then
   AS-N execute ;

TRUSTED: DIN-CELLS ( -- n )
   CHECKER-OWNER-ABI:EFFECT-DIN-CELLS-OFF s" din cells" FIELD
   dup 0= if drop EFFECT-DIN-CELLS exit then
   AS-N execute ;

TRUSTED: DOUT-CELLS ( -- n )
   CHECKER-OWNER-ABI:EFFECT-DOUT-CELLS-OFF s" dout cells" FIELD
   dup 0= if drop EFFECT-DOUT-CELLS exit then
   AS-N execute ;

TRUSTED: DIN-SLOT ( n -- n )
   CHECKER-OWNER-ABI:EFFECT-DIN-SLOT-OFF s" din slot" FIELD
   dup 0= if drop EFFECT-DIN-SLOT exit then
   AS-SLOT execute ;

TRUSTED: DOUT-SLOT ( n -- n )
   CHECKER-OWNER-ABI:EFFECT-DOUT-SLOT-OFF s" dout slot" FIELD
   dup 0= if drop EFFECT-DOUT-SLOT exit then
   AS-SLOT execute ;

TRUSTED: DIN-QUOT ( n -- bool )
   CHECKER-OWNER-ABI:EFFECT-DIN-QUOT-OFF s" din quotation" FIELD
   dup 0= if drop EFFECT-DIN-QUOT exit then
   AS-SLOT-PREDICATE execute ;

TRUSTED: DOUT-QUOT ( n -- bool )
   CHECKER-OWNER-ABI:EFFECT-DOUT-QUOT-OFF s" dout quotation" FIELD
   dup 0= if drop EFFECT-DOUT-QUOT exit then
   AS-SLOT-PREDICATE execute ;

TRUSTED: QUOT-UP ( -- bool )
   CHECKER-OWNER-ABI:EFFECT-QUOT-UP-OFF s" quotation upward" FIELD
   dup 0= if drop EFFECT-QUOT-UP exit then
   AS-BOOL execute ;

TRUSTED: RET-NEUTRAL? ( -- bool )
   CHECKER-OWNER-ABI:EFFECT-RET-NEUTRAL-OFF s" return neutrality" FIELD
   dup 0= if drop EFFECT-RET-NEUTRAL? exit then
   AS-BOOL execute ;

TRUSTED: QUOT-SIMPLE? ( -- bool )
   CHECKER-OWNER-ABI:EFFECT-QUOT-SIMPLE-OFF s" simple quotation" FIELD
   dup 0= if drop EFFECT-QUOT-SIMPLE? exit then
   AS-BOOL execute ;

TRUSTED: CATCH-CELLS ( n -- n n )
   CHECKER-OWNER-ABI:EFFECT-CATCH-CELLS-OFF s" catch cells" FIELD
   dup 0= if drop EFFECT-CATCH-CELLS exit then
   AS-CELLS execute ;

TRUSTED: EXEC-CELLS ( n -- n n )
   CHECKER-OWNER-ABI:EFFECT-EXEC-CELLS-OFF s" execute cells" FIELD
   dup 0= if drop EFFECT-EXEC-CELLS exit then
   AS-CELLS execute ;

TRUSTED: FINALLY-CELLS ( n -- n n n )
   CHECKER-OWNER-ABI:EFFECT-FINALLY-CELLS-OFF s" finally cells" FIELD
   dup 0= if drop EFFECT-FINALLY-CELLS exit then
   AS-FINALLY-CELLS execute ;

TRUSTED: MATCH-CELLS ( n -- n )
   CHECKER-OWNER-ABI:EFFECT-MATCH-CELLS-OFF s" match cells" FIELD
   dup 0= if drop EFFECT-MATCH-CELLS exit then
   AS-SLOT execute ;

TRUSTED: DEAD-TOKEN? ( ptr u8 n -- bool )
   CHECKER-OWNER-ABI:CTL-DEAD-OFF s" dead control token" FIELD
   dup 0= if drop CTL-DEAD? exit then
   AS-NAME-PREDICATE execute ;

TRUSTED: WIDTH-AT ( n n -- n )
   CHECKER-OWNER-ABI:WF-W-AT-OFF s" width fact" FIELD
   dup 0= if drop WF-W-AT exit then
   AS-WIDTH execute ;


\ ---- what the record a definition publishes needs from the checker ----------------

TRUSTED: MIN-IN ( -- n )
   CHECKER-OWNER-ABI:REC-MIN-IN-OFF s" minimum input arity" FIELD
   dup 0= if drop REC-MIN-IN@ exit then
   AS-N execute ;

TRUSTED: WIDE-PUBLISH ( -- )
   CHECKER-OWNER-ABI:REC-WIDE-PUBLISH-OFF s" wide record publish" FIELD
   dup 0= if drop REC-WIDE-PUBLISH exit then
   AS-ACTION execute ;

\ Family ids, variant ids and their metadata belong to the owner's registry.
\ Name lookup alone cannot transfer an id to a different registry's reader.
TRUSTED: FAMILY-MATCH ( ptr u8 n -- n bool )
   CHECKER-OWNER-ABI:FAMILY-MATCH-OFF s" match family" FIELD
   dup 0= if drop TFL-MATCH-FAM? exit then
   AS-FAMILY execute ;

TRUSTED: FAMILY-CON ( ptr u8 n -- n bool )
   CHECKER-OWNER-ABI:FAMILY-CON-OFF s" construct family" FIELD
   dup 0= if drop TFL-CON-FAM? exit then
   AS-FAMILY execute ;

TRUSTED: FAMILY-VARIANT ( ptr u8 n n -- n bool )
   CHECKER-OWNER-ABI:FAMILY-VARIANT-OFF s" family variant" FIELD
   dup 0= if drop TFAM:TFL-VAR? exit then
   AS-VARIANT execute ;

TRUSTED: FAMILY-SLOTS ( n -- n )
   CHECKER-OWNER-ABI:FAMILY-SLOTS-OFF s" family slots" FIELD
   dup 0= if drop TFAM:TFAM-SLOTS@ exit then
   AS-SLOT execute ;

TRUSTED: FAMILY-VARIANTS ( n -- n )
   CHECKER-OWNER-ABI:FAMILY-VARIANTS-OFF s" family variants" FIELD
   dup 0= if drop TFAM:TFAM-VAR-COUNT@ exit then
   AS-SLOT execute ;

TRUSTED: FAMILY-NAME$ ( n -- ptr u8 n )
   CHECKER-OWNER-ABI:FAMILY-NAME-OFF s" family name" FIELD
   dup 0= if drop TFAM-NAME$ exit then
   AS-FAMILY-NAME execute ;

TRUSTED: VARIANT-TAG ( n -- n )
   CHECKER-OWNER-ABI:VARIANT-TAG-OFF s" variant tag" FIELD
   dup 0= if drop TFAM:SUMV-TAG@ exit then
   AS-SLOT execute ;

TRUSTED: VARIANT-PADS ( n n -- n )
   CHECKER-OWNER-ABI:VARIANT-PADS-OFF s" variant pads" FIELD
   dup 0= if drop TFAM:TFL-VPADS exit then
   AS-WIDTH execute ;

TRUSTED: VARIANT-PAY-CELLS ( n -- n )
   CHECKER-OWNER-ABI:VARIANT-PAY-CELLS-OFF s" variant payload cells" FIELD
   dup 0= if drop TFAM:SUMV-PAYCELLS@ exit then
   AS-SLOT execute ;

TRUSTED: VARIANT-PAY-TERMS ( n -- n )
   CHECKER-OWNER-ABI:VARIANT-PAY-TERMS-OFF s" variant payload terms" FIELD
   dup 0= if drop TFAM:SUMV-PAY-N exit then
   AS-SLOT execute ;

;package
