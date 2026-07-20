\ maki/db/action-test.f - checked acceptance for the machine-facing action-schema
\ registry (maki/db/action.f, dot habu-v2-machine-action-a7357409). Proves the plan
\ § 23.9 acceptance, each item by a named test:
\   AT-INCOMPLETE      : a declaration missing a required field -> REGISTER incomplete (typed)
\   AT-COMPLETE-OK     : a complete declaration -> REGISTER ok
\   AT-IDEMPOTENT      : re-registering an IDENTICAL declaration is idempotent (same id)
\   AT-CONFLICT        : re-registering a name with a DIFFERENT declaration -> conflict
\   AD-K-OK / AD-K-BAD : STATIC kind safety - a non-kind cannot reach DISPATCH (verdict 0),
\                        a real art-kind can (verdict -1); the cad-kinds verdict pattern
\   AT-WRONG-KIND      : DYNAMIC - a well-typed WRONG-VARIANT input kind -> wrong-kind
\   AT-RIGHT-KIND      : the declared input kind + full grants -> accepted
\   AT-UNAUTH-EFFECT   : an ungranted declared EFFECT -> unauthorized (before execution)
\   AT-UNAUTH-CAP      : an ungranted declared CAPABILITY bit -> unauthorized
\   AT-STAGING-*       : a `declared` action -> unsupported; an `implemented` one -> accepted
\   AT-UNKNOWN         : a stale id after RESET -> unknown-action
\   AT-REPLAY          : the same set registered in ANY order digests identically (replay)
\   AT-ENUM-SORTED     : the enumeration is canonical (name-ascending)
\   AT-COMMIT-* / AT-PASS-* / AT-SCHEMA-* : seeded declarations reflect the landed surfaces
\   capacity / fail-closed throws
\
\ The test reopens package ACTION (a friend) so DISPATCH / the builder / the private SEED
\ words and ordinal helpers read bare; the static fixtures use the shared checker-assert.

require lib/test.f
require lib/string.f
require test/checker-assert.f
require maki/db/action.f
require maki/db/obligation.f
require maki/db/diagnostic.f

package ACTION

create BUF   8192 allot
create DA-BUF  64 allot
create DB-BUF  64 allot
variable ASC
variable EA-OK                                  \ ENUM-AT bounds property accumulator
variable EA-K                                   \ bad index handed to a catch xt (xts are not closures)

\ ---- checker verdict wrappers (the maki/cad-kinds-test precedent) ---------------
: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO ( ptr u8 n -- )    CHECK-QUIET-CANDIDATE! 0 T= ;

\ ---- typed-outcome -> ordinal decoders -----------------------------------------
: RCODE ( register-result<CAD-KIND:action-id> -- n )
   MATCH register-result
      ok         OF drop 0 ENDOF
      incomplete OF 1 ENDOF
      conflict   OF 2 ENDOF
   ;MATCH ;

: REG-ID ( ptr u8 n -- CAD-KIND:action-id )     \ REGISTER and take the id (throw on reject)
   REGISTER MATCH register-result
      ok         OF ENDOF
      incomplete OF E-ACTION-SEED throw ENDOF
      conflict   OF E-ACTION-SEED throw ENDOF
   ;MATCH ;

: DCODE ( dispatch-result -- n )
   MATCH dispatch-result
      accepted       OF 0 ENDOF
      unknown-action OF 1 ENDOF
      wrong-kind     OF 2 ENDOF
      unauthorized   OF 3 ENDOF
      unsupported    OF 4 ENDOF
   ;MATCH ;

63 constant ALL-EFF                             \ all six effect bits granted
-1 constant ALL-CAP                             \ every capability bit granted

\ ---- reference declarations for the registration acceptance --------------------
: DECL-COMPLETE ( -- )                          \ every required scalar set
   DECLARE
   ACTION-ART--KIND:TRANSACTION INPUT!
   ACTION-ART--KIND:REVISION OUTPUT!
   true DETERMINISTIC!  false CACHEABLE!
   OBLIG-VERIFIER:STATIC-CHECKER VERIFIER!
   ACTION-STAGING:IMPLEMENTED STAGING! ;

: DECL-NO-VERIFIER ( -- )                        \ DECL-COMPLETE minus the required verifier
   DECLARE
   ACTION-ART--KIND:TRANSACTION INPUT!
   ACTION-ART--KIND:REVISION OUTPUT!
   true DETERMINISTIC!  false CACHEABLE!
   ACTION-STAGING:IMPLEMENTED STAGING! ;

: DECL-DIFFERENT ( -- )                          \ DECL-COMPLETE with a DIFFERENT output kind
   DECLARE
   ACTION-ART--KIND:TRANSACTION INPUT!
   ACTION-ART--KIND:DIFF OUTPUT!
   true DETERMINISTIC!  false CACHEABLE!
   OBLIG-VERIFIER:STATIC-CHECKER VERIFIER!
   ACTION-STAGING:IMPLEMENTED STAGING! ;

: DECL-CAP-GATE ( -- )                           \ requires an abstract capability bit (4)
   DECLARE
   ACTION-ART--KIND:TRANSACTION INPUT!
   ACTION-ART--KIND:NONE OUTPUT!
   true DETERMINISTIC!  false CACHEABLE!
   OBLIG-VERIFIER:STATIC-CHECKER VERIFIER!
   ACTION-STAGING:IMPLEMENTED STAGING!
   4 CAP+ ;

\ ---- acceptance 1: missing declaration fields REJECT registration --------------
: AT-INCOMPLETE ( -- n )   DECL-NO-VERIFIER s" TEST:INCOMPLETE" REGISTER RCODE ;
: AT-COMPLETE-OK ( -- n )  DECL-COMPLETE s" TEST:COMPLETE" REGISTER RCODE ;
: AT-IDEMPOTENT ( -- bool )
   DECL-COMPLETE s" TEST:COMPLETE" REG-ID
   DECL-COMPLETE s" TEST:COMPLETE" REG-ID  EQUAL? ;
: AT-CONFLICT ( -- n )
   DECL-COMPLETE s" TEST:COMPLETE" REGISTER RCODE drop      \ ensure it exists (DECL-COMPLETE)
   DECL-DIFFERENT s" TEST:COMPLETE" REGISTER RCODE ;

\ ---- action fixtures (resolved by name; robust to registration order) ----------
: TX-COMMIT-ID ( -- CAD-KIND:action-id )   s" TX:COMMIT" ID-OF ;
: TX-BEGIN-ID ( -- CAD-KIND:action-id )    s" TX:BEGIN" ID-OF ;
: PASS-RUN-ID ( -- CAD-KIND:action-id )    s" PASS:RUN" ID-OF ;
: CAP-GATE-ID ( -- CAD-KIND:action-id )    DECL-CAP-GATE s" TEST:CAP-GATE" REG-ID ;

\ ---- acceptance 2 (dynamic): wrong input variant cannot dispatch ---------------
: AT-WRONG-KIND ( -- n )                        \ TX:COMMIT wants transaction; supply artifact
   TX-COMMIT-ID ACTION-ART--KIND:ARTIFACT ALL-EFF ALL-CAP DISPATCH DCODE ;
: AT-RIGHT-KIND ( -- n )
   TX-COMMIT-ID ACTION-ART--KIND:TRANSACTION ALL-EFF ALL-CAP DISPATCH DCODE ;

\ ---- acceptance 3: unauthorized effects / capabilities reject before execution -
: AT-UNAUTH-EFFECT ( -- n )                     \ grant only read-store (1); need write+publish
   TX-COMMIT-ID ACTION-ART--KIND:TRANSACTION 1 ALL-CAP DISPATCH DCODE ;
: AT-AUTH-EFFECT ( -- n )                        \ grant read|write|publish (7)
   TX-COMMIT-ID ACTION-ART--KIND:TRANSACTION 7 ALL-CAP DISPATCH DCODE ;
: AT-UNAUTH-CAP ( -- n )                         \ need cap bit 4; grant none
   CAP-GATE-ID ACTION-ART--KIND:TRANSACTION ALL-EFF 0 DISPATCH DCODE ;
: AT-AUTH-CAP ( -- n )                           \ grant cap bit 4
   CAP-GATE-ID ACTION-ART--KIND:TRANSACTION ALL-EFF 4 DISPATCH DCODE ;

\ ---- staging: declared -> unsupported; implemented -> accepted -----------------
: AT-STAGING-DECLARED ( -- n )
   PASS-RUN-ID ACTION-ART--KIND:PASS-OBJ ALL-EFF ALL-CAP DISPATCH DCODE ;
: AT-STAGING-IMPL ( -- n )
   TX-BEGIN-ID ACTION-ART--KIND:REVISION ALL-EFF ALL-CAP DISPATCH DCODE ;

\ ---- canonical enumeration + replay (acceptance 4) -----------------------------
: RESEED-A ( -- )   RESET SEED-ALL ;             \ the load-time registration order
: RESEED-B ( -- )                                \ the SAME set, reversed
   RESET
   SEED-PASS-RUN SEED-TX-ABORT SEED-TX-COMMIT SEED-TX-VALIDATE SEED-TX-APPLY
   SEED-TX-BEGIN SEED-REVISION-DIFF SEED-ARTIFACT-GET SEED-SCHEMA-LIST ;

: DIG-EQ? ( ptr u8 ptr u8 -- bool ) {: pa:ptr pb:ptr :}
   0 begin dup 32 < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;

: AT-REPLAY ( -- bool )
   RESEED-A  DA-BUF 64 DIGEST drop
   RESEED-B  DB-BUF 64 DIGEST drop
   DA-BUF DB-BUF DIG-EQ? ;

: AT-ENUM-SORTED ( -- bool )                     \ ENUM-AT[k-1] name < ENUM-AT[k] name for all k
   RESEED-A
   true ASC !
   1 begin dup COUNT < while
      dup {: k:n :}
      k 1- ENUM-AT NAME$  k ENUM-AT NAME$  BYTES< 0= if false ASC ! then
      1+
   repeat drop
   ASC @ ;

\ ---- unknown action (a stale id after RESET), then restore the registry --------
: AT-UNKNOWN ( -- n )
   TX-BEGIN-ID {: id:CAD-KIND:action-id :}
   RESET
   id ACTION-ART--KIND:REVISION ALL-EFF ALL-CAP DISPATCH DCODE
   RESEED-A ;

\ ---- seeded declarations reflect the landed surfaces ---------------------------
: AT-COUNT ( -- n )            RESEED-A COUNT ;
: AT-COMMIT-INPUT ( -- n )     TX-COMMIT-ID INPUT-KIND@ KIND>N ;     \ transaction = 4
: AT-COMMIT-OUTPUT ( -- n )    TX-COMMIT-ID OUTPUT-KIND@ KIND>N ;    \ revision = 3
: AT-COMMIT-EFFECTS ( -- n )   TX-COMMIT-ID EFFECT-MASK@ ;           \ read|write|publish = 7
: AT-COMMIT-STAGED ( -- n )    TX-COMMIT-ID STAGING@ STAGING>N ;     \ implemented = 0
: AT-PASS-STAGED ( -- n )      PASS-RUN-ID STAGING@ STAGING>N ;      \ declared = 1
: AT-PASS-DET ( -- bool )      PASS-RUN-ID DETERMINISTIC@ ;
: AT-PASS-OBLIG ( -- n )       PASS-RUN-ID OBLIGATION-MASK@ ;        \ semantic-equiv = 1
: AT-SCHEMA-INPUT ( -- n )     s" SCHEMA:LIST" ID-OF INPUT-KIND@ KIND>N ;   \ none = 0
: AT-SCHEMA-OUTPUT ( -- n )    s" SCHEMA:LIST" ID-OF OUTPUT-KIND@ KIND>N ;  \ schema = 1

\ ---- capacity / fail-closed throws ---------------------------------------------
: AT-DIGEST-SMALL ( -- )   BUF 16 DIGEST drop ;             \ cap 16 < 32 -> E-ACTION-BUF
: AT-EMPTY-NAME ( -- )     DECL-COMPLETE s" " REGISTER drop ;  \ empty name -> E-ACTION-KEY
: AT-UNKNOWN-NAME ( -- )   s" NOPE:NONE" ID-OF drop ;       \ unknown name -> E-ACTION-ID

\ ---- ENUM-AT bounds (dot habu-bounds-check-action-39819fc1) ---------------------
\ ENUM-AT builds the canonical order then read ORD[k] and minted it as a nominal action-id
\ with no k check: k<0 read before ORD, k>=COUNT read stale/OOB cells, and a large k whose
\ `cells` (k<<3) wraps the pointer back into ORD escaped a forged id. The guard now rejects
\ every out-of-range k with E-ACTION-ID before ORD-BUILD, the pointer math, and the mint.
$2000000000000000 constant EA-WRAP              \ EA-WRAP cells = 2^64 -> wraps ORD+0 if unguarded
$DEAD1D constant EA-POISON                       \ a distinctive raw an OOB read would have minted

: EA-AT-RAW ( -- )   EA-K @ ENUM-AT drop ;       \ bad-index call; caller owns registry + ORD canaries

\ exact out-of-range fixtures (each reseeds so COUNT / ACT-CAP read the live registry)
: EA-NEG ( -- )      RESEED-A -1 ENUM-AT drop ;          \ k = -1 (before ORD)
: EA-COUNT ( -- )    RESEED-A COUNT ENUM-AT drop ;       \ k = COUNT (first stale slot)
: EA-COUNT+1 ( -- )  RESEED-A COUNT 1+ ENUM-AT drop ;    \ k = COUNT+1
: EA-CAP ( -- )      RESEED-A ACT-CAP ENUM-AT drop ;     \ k = ACT-CAP (one past the ORD allocation)
: EA-WRAP-AT ( -- )  RESEED-A EA-WRAP ENUM-AT drop ;     \ k whose *cells wraps ORD+0

\ canary: a rejected call runs neither ORD-BUILD nor any read/mint, and writes nothing
: EA-CANARY ( -- bool )
   RESEED-A
   EA-POISON 0 cells ORD + !                     \ ORD[0]: a slot ORD-BUILD WOULD overwrite if it ran
   EA-POISON COUNT cells ORD + !                 \ ORD[COUNT]: a stale slot a bad read WOULD mint from
   COUNT EA-K !                                  \ reject index = COUNT
   [: EA-AT-RAW ;] catch E-ACTION-ID =             \ (a) rejected with the bounds code -> nothing escaped
   0 cells ORD + @ EA-POISON = and               \ (b) ORD-BUILD did not run (ORD[0] canary intact)
   COUNT cells ORD + @ EA-POISON = and ;         \ (c) no OOB write; stale cell was never read-then-minted

\ property: every valid index returns a VALIDATE-ID-clean id whose raw is a registered slot
: EA-INRANGE-OK ( -- bool )
   RESEED-A  true EA-OK !
   0 begin dup COUNT < while
      dup {: k:n :}
      k ENUM-AT VALIDATE-ID ACTION-ID>RAW {: r:n :}   \ VALIDATE-ID throws on an out-of-range id
      r 0 < r COUNT >= or if false EA-OK ! then
      1+
   repeat drop  EA-OK @ ;

\ property: every k in [COUNT, ACT-CAP] rejects with the bounds code (stale-in-alloc and one-past)
: EA-OOR-ALL ( -- bool )
   RESEED-A  true EA-OK !
   COUNT begin dup ACT-CAP <= while
      dup EA-K !
      [: EA-AT-RAW ;] catch E-ACTION-ID <> if false EA-OK ! then
      1+
   repeat drop  EA-OK @ ;

\ property: a band of negative indices all reject
: EA-NEG-ALL ( -- bool )
   RESEED-A  true EA-OK !
   -8 begin dup 0 < while
      dup EA-K !
      [: EA-AT-RAW ;] catch E-ACTION-ID <> if false EA-OK ! then
      1+
   repeat drop  EA-OK @ ;

T-RESET

\ acceptance 2 (STATIC, the cad-kinds verdict pattern): a real art-kind reaches DISPATCH;
\ an artifact-id in the kind slot cannot. Effects use only nominal TYPEFAMILY inputs; the
\ kind is built in the body and the dispatch-result dropped, so the fixture never depends
\ on composite-type resolution.
s" AD-K-OK ( CAD-KIND:action-id -- ) ACTION-ART--KIND:TRANSACTION 63 -1 ACTION:DISPATCH drop" YES
s" AD-K-BAD ( CAD-KIND:action-id CAD-KIND:artifact-id -- ) 63 -1 ACTION:DISPATCH drop" NO

\ acceptance 1
AT-INCOMPLETE 1 T=
AT-COMPLETE-OK 0 T=
AT-IDEMPOTENT TTRUE
AT-CONFLICT 2 T=

\ acceptance 2 (dynamic)
AT-WRONG-KIND 2 T=
AT-RIGHT-KIND 0 T=

\ acceptance 3
AT-UNAUTH-EFFECT 3 T=
AT-AUTH-EFFECT 0 T=
AT-UNAUTH-CAP 3 T=
AT-AUTH-CAP 0 T=

\ staging
AT-STAGING-DECLARED 4 T=
AT-STAGING-IMPL 0 T=

\ unknown action
AT-UNKNOWN 1 T=

\ acceptance 4
AT-REPLAY TTRUE
AT-ENUM-SORTED TTRUE

\ seeded declarations reflect the landed surfaces
AT-COUNT 9 T=
AT-COMMIT-INPUT 4 T=
AT-COMMIT-OUTPUT 3 T=
AT-COMMIT-EFFECTS 7 T=
AT-COMMIT-STAGED 0 T=
AT-PASS-STAGED 1 T=
AT-PASS-DET TTRUE
AT-PASS-OBLIG 1 T=
AT-SCHEMA-INPUT 0 T=
AT-SCHEMA-OUTPUT 1 T=

\ capacity / fail-closed throws
' AT-DIGEST-SMALL E-ACTION-BUF TTHROWS
' AT-EMPTY-NAME E-ACTION-KEY TTHROWS
' AT-UNKNOWN-NAME E-ACTION-ID TTHROWS

\ ENUM-AT bounds (dot habu-bounds-check-action-39819fc1)
' EA-NEG     E-ACTION-ID TTHROWS                \ k = -1
' EA-COUNT   E-ACTION-ID TTHROWS                \ k = COUNT
' EA-COUNT+1 E-ACTION-ID TTHROWS                \ k = COUNT+1
' EA-CAP     E-ACTION-ID TTHROWS                \ k = ACT-CAP (one past the ORD allocation)
' EA-WRAP-AT E-ACTION-ID TTHROWS                \ k whose *cells wraps ORD+0
EA-CANARY     TTRUE                             \ rejected: no ORD-BUILD, no OOB write, no nominal escape
EA-INRANGE-OK TTRUE                             \ every 0<=k<COUNT returns a validated, in-range id
EA-OOR-ALL    TTRUE                             \ every k in [COUNT, ACT-CAP] rejects
EA-NEG-ALL    TTRUE                             \ every k in [-8, -1] rejects

T-REPORT

;package
