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
require src/habu/layout.f

package CHECKER-OWNER

\ The live source owner. Zero until an engine publishes one.
: RECORD ( -- ptr u8 )
   data-base NCOMP-DISPATCH:DECL-CELL + 0 ptr-field @ ;

\ Set by this module's load, cleared before the compiler is captured.
variable SOURCE-LOADED   -1 SOURCE-LOADED !

: REFUSE ( ptr u8 n -- ) {: a:ptr u:n :}
   2 s" ncomp: the source owner carries no " write drop
   2 a u write drop
   2 S\" \n" write drop
   E-NCOMP-OWNER throw ;

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
   NCOMP-DISPATCH:DECL-CHECK-OFF s" scan" FIELD
   dup 0= if drop CHECK! exit then
   AS-CHECK execute ;

\ The same scan with the owner's diagnostic render suppressed, for a body whose
\ verdict this compiler will not enforce. It is an OPERATION rather than a read of
\ the owner's quiet counter because a baked compiler can reach a counter only by
\ name, and by name is the retired instance: the suppression then landed on one
\ checker and the render on the other, and a product-hosted build printed a
\ rejection for every trusted body in the window it was rebuilding.
TRUSTED: CHECK-UNJUDGED ( ptr u8 n -- n )
   NCOMP-DISPATCH:DECL-CHECK-UNJUDGED-OFF s" unjudged scan" FIELD
   dup 0= if drop CHECK-UNJUDGED! exit then
   AS-CHECK execute ;

TRUSTED: TAPE-INSTALL ( n [ ptr u8 n -- ] [ ptr u8 n n n n n -- ] [ ptr u8 n n -- ] -- )
   NCOMP-DISPATCH:DECL-TAPE-INSTALL-OFF s" tape install" FIELD
   dup 0= if drop CHECKER-TAPE:INSTALL exit then
   AS-TAPE-INSTALL execute ;

TRUSTED: TAPE-ARM ( -- )
   NCOMP-DISPATCH:DECL-TAPE-ARM-OFF s" tape arm" FIELD
   dup 0= if drop CHECKER-TAPE:ARM exit then
   AS-ACTION execute ;

TRUSTED: TAPE-DISARM ( -- )
   NCOMP-DISPATCH:DECL-TAPE-DISARM-OFF s" tape disarm" FIELD
   dup 0= if drop CHECKER-TAPE:DISARM exit then
   AS-ACTION execute ;

TRUSTED: TAPE-ADVANCE ( -- )
   NCOMP-DISPATCH:DECL-TAPE-ADVANCE-OFF s" tape advance" FIELD
   dup 0= if drop CHECKER-TAPE:ADVANCE exit then
   AS-ACTION execute ;

TRUSTED: DOES-CHECK ( ptr u8 n ptr u8 n -- n )
   NCOMP-DISPATCH:DECL-DOES-CHECK-OFF s" does> split" FIELD
   dup 0= if drop CHECK-DOES! exit then
   AS-DOES-CHECK execute ;

TRUSTED: DOES-IN ( -- n )
   NCOMP-DISPATCH:DECL-DOES-IN-OFF s" does> input cells" FIELD
   dup 0= if drop CHECK-DOES-DIN-CELLS exit then
   AS-N execute ;

TRUSTED: DOES-OUT ( -- n )
   NCOMP-DISPATCH:DECL-DOES-OUT-OFF s" does> output cells" FIELD
   dup 0= if drop CHECK-DOES-DOUT-CELLS exit then
   AS-N execute ;

TRUSTED: DOES-WIDE? ( -- bool )
   NCOMP-DISPATCH:DECL-DOES-WIDE-OFF s" does> width" FIELD
   dup 0= if drop CHECK-DOES-WIDE? exit then
   AS-BOOL execute ;

TRUSTED: USIG-TRUNCATE ( ptr u8 n -- )
   NCOMP-DISPATCH:DECL-USIG-TRUNCATE-OFF s" signature retract" FIELD
   dup 0= if drop CHECKER-USIGS-TRUNCATE-FROM-RAW exit then
   AS-NAME-ACTION execute ;


\ ---- the finalized per-call-site facts the scan recorded ----------------

TRUSTED: CALL-CELLS ( n -- n n )
   NCOMP-DISPATCH:DECL-CALL-CELLS-OFF s" call cells" FIELD
   dup 0= if drop CWIN-CELLS exit then
   AS-CELLS execute ;

TRUSTED: CALL-GLUE ( n -- n n )
   NCOMP-DISPATCH:DECL-CALL-GLUE-OFF s" call glue" FIELD
   dup 0= if drop CWIN-GLUE exit then
   AS-CELLS execute ;

TRUSTED: MATCH-PAYLOAD ( n -- n n )
   NCOMP-DISPATCH:DECL-CALL-MATCH-OFF s" match payload" FIELD
   dup 0= if drop CWIN-MATCH-PAYLOAD exit then
   AS-CELLS execute ;

TRUSTED: CALL-QUOT-IN ( n n -- n n )
   NCOMP-DISPATCH:DECL-CALL-QUOT-IN-OFF s" quotation inputs" FIELD
   dup 0= if drop CWIN-QUOT-IN exit then
   AS-QUOT-CELLS execute ;

TRUSTED: CALL-QUOT-OUT ( n n -- n n )
   NCOMP-DISPATCH:DECL-CALL-QUOT-OUT-OFF s" quotation outputs" FIELD
   dup 0= if drop CWIN-QUOT-OUT exit then
   AS-QUOT-CELLS execute ;


\ ---- the front end the checker IS: the scan, the tape it fills, the does> split,
\ the declared-effect row and the retract of one ----------------

TRUSTED: DECLARED-EFFECT ( ptr u8 n ptr u8 n -- )
   NCOMP-DISPATCH:DECL-TRUST-DECL-OFF s" declared-effect row" FIELD
   dup 0= if drop TRUST-DECL exit then
   AS-DECLARATION execute ;

TRUSTED: PARSE-IMM? ( ptr u8 n -- bool )
   NCOMP-DISPATCH:DECL-PARSE-IMM-OFF s" parse-neutral immediate" FIELD
   dup 0= if drop NEUTRAL-PARSE-IMM? exit then
   AS-NAME-PREDICATE execute ;


\ ---- the effect-store query group. QUERY resolves a name into the instance's
\ query state and every reader below reads THAT state, so all of them have to
\ reach the same owner or a reader answers about another instance's query ----------------

TRUSTED: QUERY ( ptr u8 n -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-QUERY-OFF s" effect query" FIELD
   dup 0= if drop EFFECT-QUERY exit then
   AS-NAME-PREDICATE execute ;

TRUSTED: DIN-N ( -- n )
   NCOMP-DISPATCH:DECL-EFFECT-DIN-N-OFF s" din terms" FIELD
   dup 0= if drop EFFECT-DIN-N exit then
   AS-N execute ;

TRUSTED: DOUT-N ( -- n )
   NCOMP-DISPATCH:DECL-EFFECT-DOUT-N-OFF s" dout terms" FIELD
   dup 0= if drop EFFECT-DOUT-N exit then
   AS-N execute ;

TRUSTED: DIN-CELLS ( -- n )
   NCOMP-DISPATCH:DECL-EFFECT-DIN-CELLS-OFF s" din cells" FIELD
   dup 0= if drop EFFECT-DIN-CELLS exit then
   AS-N execute ;

TRUSTED: DOUT-CELLS ( -- n )
   NCOMP-DISPATCH:DECL-EFFECT-DOUT-CELLS-OFF s" dout cells" FIELD
   dup 0= if drop EFFECT-DOUT-CELLS exit then
   AS-N execute ;

TRUSTED: DIN-SLOT ( n -- n )
   NCOMP-DISPATCH:DECL-EFFECT-DIN-SLOT-OFF s" din slot" FIELD
   dup 0= if drop EFFECT-DIN-SLOT exit then
   AS-SLOT execute ;

TRUSTED: DOUT-SLOT ( n -- n )
   NCOMP-DISPATCH:DECL-EFFECT-DOUT-SLOT-OFF s" dout slot" FIELD
   dup 0= if drop EFFECT-DOUT-SLOT exit then
   AS-SLOT execute ;

TRUSTED: DIN-QUOT ( n -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-DIN-QUOT-OFF s" din quotation" FIELD
   dup 0= if drop EFFECT-DIN-QUOT exit then
   AS-SLOT-PREDICATE execute ;

TRUSTED: DOUT-QUOT ( n -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-DOUT-QUOT-OFF s" dout quotation" FIELD
   dup 0= if drop EFFECT-DOUT-QUOT exit then
   AS-SLOT-PREDICATE execute ;

TRUSTED: QUOT-UP ( -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-QUOT-UP-OFF s" quotation upward" FIELD
   dup 0= if drop EFFECT-QUOT-UP exit then
   AS-BOOL execute ;

TRUSTED: RET-NEUTRAL? ( -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-RET-NEUTRAL-OFF s" return neutrality" FIELD
   dup 0= if drop EFFECT-RET-NEUTRAL? exit then
   AS-BOOL execute ;

TRUSTED: QUOT-SIMPLE? ( -- bool )
   NCOMP-DISPATCH:DECL-EFFECT-QUOT-SIMPLE-OFF s" simple quotation" FIELD
   dup 0= if drop EFFECT-QUOT-SIMPLE? exit then
   AS-BOOL execute ;

TRUSTED: CATCH-CELLS ( n -- n n )
   NCOMP-DISPATCH:DECL-EFFECT-CATCH-CELLS-OFF s" catch cells" FIELD
   dup 0= if drop EFFECT-CATCH-CELLS exit then
   AS-CELLS execute ;

TRUSTED: EXEC-CELLS ( n -- n n )
   NCOMP-DISPATCH:DECL-EFFECT-EXEC-CELLS-OFF s" execute cells" FIELD
   dup 0= if drop EFFECT-EXEC-CELLS exit then
   AS-CELLS execute ;

TRUSTED: FINALLY-CELLS ( n -- n n n )
   NCOMP-DISPATCH:DECL-EFFECT-FINALLY-CELLS-OFF s" finally cells" FIELD
   dup 0= if drop EFFECT-FINALLY-CELLS exit then
   AS-FINALLY-CELLS execute ;

TRUSTED: MATCH-CELLS ( n -- n )
   NCOMP-DISPATCH:DECL-EFFECT-MATCH-CELLS-OFF s" match cells" FIELD
   dup 0= if drop EFFECT-MATCH-CELLS exit then
   AS-SLOT execute ;

TRUSTED: DEAD-TOKEN? ( ptr u8 n -- bool )
   NCOMP-DISPATCH:DECL-CTL-DEAD-OFF s" dead control token" FIELD
   dup 0= if drop CTL-DEAD? exit then
   AS-NAME-PREDICATE execute ;

TRUSTED: WIDTH-AT ( n n -- n )
   NCOMP-DISPATCH:DECL-WF-W-AT-OFF s" width fact" FIELD
   dup 0= if drop WF-W-AT exit then
   AS-WIDTH execute ;


\ ---- what the record a definition publishes needs from the checker ----------------

TRUSTED: MIN-IN ( -- n )
   NCOMP-DISPATCH:DECL-REC-MIN-IN-OFF s" minimum input arity" FIELD
   dup 0= if drop REC-MIN-IN@ exit then
   AS-N execute ;

TRUSTED: WIDE-PUBLISH ( -- )
   NCOMP-DISPATCH:DECL-REC-WIDE-PUBLISH-OFF s" wide record publish" FIELD
   dup 0= if drop REC-WIDE-PUBLISH exit then
   AS-ACTION execute ;

;package
