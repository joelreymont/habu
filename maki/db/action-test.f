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

\ ---- same-shape twin for the register outcome -----------------------------------
\ regr-twin is ACTION:register-result's SHAPE under a different name: same arity, same
\ three variants in the same order, same named payload field. It exists only so the
\ negatives below can prove the register outcome is NOMINAL - two identically shaped ENUM
\ families never unify, in either direction. It lives in its OWN package, not in the
\ reopened package ACTION, because a test must not add public words to the production
\ package's surface; and it must be public, because a private family publishes no
\ constructors at all, which would let the negatives pass by being unresolvable rather
\ than ill-typed. The tail is kept short on purpose: the generated constructor package is
\ ACTION--TEST-REGR--TWIN at 23 characters, clear of the 32-character readability cap
\ above which a generated name falls back to an unreadable hash spelling.
using TFAM

package ACTION-TEST
public

ENUM regr-twin 1
   VARIANT ok FIELD id a ;VARIANT
   VARIANT incomplete ;VARIANT
   VARIANT conflict ;VARIANT
;ENUM

;package

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

\ ---- how the two outcome families are DECLARED ---------------------------------
\ register-result is a full-mode payload ENUM and dispatch-result is a COMPACT ENUM (one
\ bare token per payloadless case) under the wave ruling. Both spellings are one cell wide
\ and both give the same MATCH surface, so no consumer can tell the declaration form apart
\ by behaviour - which is exactly why the recorded kind is pinned here, read live out of
\ the family registry by REFLECT (test/checker-assert.f). Writing either declaration back
\ as SUMTYPE, or writing dispatch-result in the arity-headed full form, changes the
\ recorded kind and turns this suite red. The same probe pins the generated constructor
\ package that the DR-* and RR-* wrappers compile against, so a constructor rename cannot
\ pass unnoticed either. Each family is named by its tail plus that constructor package,
\ the (package, tail) pair that owns family identity, so REFLECT:FAMS = 1 below is also
\ the proof that the identity resolves exactly one registered family.
: DR$ ( -- ptr u8 n ptr u8 n )   s" dispatch-result" s" ACTION-DISPATCH--RESULT" ;
: RR$ ( -- ptr u8 n ptr u8 n )   s" register-result" s" ACTION-REGISTER--RESULT" ;

\ ---- hostile decoy: identity is by NAME, not by shape --------------------------
\ drother repeats dispatch-result's five case names in the same order at the same one-cell
\ width. It is a DIFFERENT type in both directions. Declared private here, so it publishes
\ no constructors of its own and the MATCH negatives cannot pass by being unresolvable.
ENUM drother
   accepted unknown-action wrong-kind unauthorized unsupported
;ENUM

\ ---- both outcome families construct and dispatch through MATCH ----------------
\ REGISTER and DISPATCH reach these arms only through a real registration or gate run.
\ These construct each variant DIRECTLY through the production wrappers and match it
\ straight back, so the register payload FIELD is proven to bind in declaration order and
\ every dispatch case is proven to keep its own tag.
: NOWORD ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  1 T= ;

: TT-MK-RR-OK ( CAD-KIND:action-id -- register-result<CAD-KIND:action-id> )  RR-OK ;
: TT-MK-RR-INC ( -- register-result<CAD-KIND:action-id> )   RR-INCOMPLETE ;
: TT-MK-RR-CON ( -- register-result<CAD-KIND:action-id> )   RR-CONFLICT ;

: TT-RR-RAW ( register-result<CAD-KIND:action-id> -- n )    \ ok payload's registry index, else -1
   MATCH register-result
      ok         OF {: got:CAD-KIND:action-id :} got ACTION-ID>RAW ENDOF
      incomplete OF -1 ENDOF
      conflict   OF -1 ENDOF
   ;MATCH ;

\ TT-REG-2ND registers an action whose registry index is at least one: RESEED-A leaves the
\ nine seeded actions occupying indices 0..8, so the test-owned name below can only land at
\ 9 or beyond. Index 0 is a legitimate index and is also what a zeroed payload reads back
\ as, so a payload comparison riding index 0 would pass on a dropped payload; TT-RR-NZ pins
\ the compared index non-zero.
: TT-REG-2ND ( -- CAD-KIND:action-id )
   RESEED-A                                     \ the nine seeded actions take indices 0..8
   DECL-COMPLETE s" TEST:C1-PAYLOAD" REG-ID ;

: TT-RR-ARM ( -- n )      TT-REG-2ND TT-MK-RR-OK RCODE ;
: TT-RR-RT ( -- n )                             \ 0 = the interned id came back unchanged
   TT-REG-2ND dup ACTION-ID>RAW {: want:n :}
   TT-MK-RR-OK TT-RR-RAW want = if 0 else 1 then ;
: TT-RR-NZ ( -- bool )    TT-REG-2ND ACTION-ID>RAW 0 > ;
: TT-RR-INC-ARM ( -- n )  TT-MK-RR-INC RCODE ;
: TT-RR-CON-ARM ( -- n )  TT-MK-RR-CON RCODE ;
: TT-RR-INC-RAW ( -- n )  TT-MK-RR-INC TT-RR-RAW ;   \ a payloadless arm carries no index

\ every dispatch case round-trips: constructor -> MATCH -> its own ordinal
: TT-DR-ACC ( -- n )   DR-ACCEPTED DCODE ;
: TT-DR-UNK ( -- n )   DR-UNKNOWN DCODE ;
: TT-DR-WK ( -- n )    DR-WRONG-KIND DCODE ;
: TT-DR-UNA ( -- n )   DR-UNAUTHORIZED DCODE ;
: TT-DR-UNS ( -- n )   DR-UNSUPPORTED DCODE ;

: TT-RESTORE ( -- n )   RESEED-A COUNT ;        \ drop the test-owned registration again

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
\ an artifact-id in the kind slot cannot. Effects use only nominal NEWTYPE inputs; the
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

\ ---- the recorded declaration kind (read live from the family registry) --------
TK-ENUM TK-SUM = 0 T=                           \ the two kinds are distinct, so the pins below bite
DR$ REFLECT:FAMS 1 T=                           \ the compact family is registered, exactly once ...
DR$ REFLECT:KIND TK-ENUM T=                     \ ... as an enum family (the pinned ruling) ...
DR$ REFLECT:KIND TK-SUM = 0 T=                  \ ... and not as a general sum
DR$ REFLECT:ARITY 0 T=                          \ compact form declares no type parameters
DR$ REFLECT:WIDTH 1 T=                          \ one cell, the same width the sum form had
DR$ REFLECT:VIS 1 T=                            \ public, so the constructors are generated
DR$ REFLECT:VARS 5 T=
DR$ 0 REFLECT:ARM$ s" accepted" T$=             \ case order fixes the tags
DR$ 1 REFLECT:ARM$ s" unknown-action" T$=
DR$ 2 REFLECT:ARM$ s" wrong-kind" T$=
DR$ 3 REFLECT:ARM$ s" unauthorized" T$=
DR$ 4 REFLECT:ARM$ s" unsupported" T$=
DR$ 0 REFLECT:ARM-CTOR$ s" ACTION-DISPATCH--RESULT" T$=   \ constructor spelling
DR$ 4 REFLECT:ARM-CTOR$ s" ACTION-DISPATCH--RESULT" T$=
\ The payload family is pinned the other way round, and that asymmetry is the point.
\ Only the COMPACT form registers a family as an enum; a full-mode declaration - the
\ arity-headed form with named payload FIELDs - is still recorded as a general SUM, the
\ same kind the legacy SUMTYPE spelling produced. Pinning TK-SUM here means a
\ later rewrite of register-result into the compact form - which would silently drop its
\ ok payload - flips this to TK-ENUM and turns the suite red.
RR$ REFLECT:FAMS 1 T=                           \ the payload family is registered, exactly once ...
RR$ REFLECT:KIND TK-SUM T=                      \ ... as a general sum, unchanged by full mode ...
RR$ REFLECT:KIND TK-ENUM = 0 T=                 \ ... and NOT as a compact enum family
RR$ REFLECT:ARITY 1 T=                          \ full mode keeps the one type parameter
RR$ REFLECT:VARS 3 T=
RR$ 0 REFLECT:ARM$ s" ok" T$=
RR$ 1 REFLECT:ARM$ s" incomplete" T$=
RR$ 2 REFLECT:ARM$ s" conflict" T$=
RR$ 0 REFLECT:ARM-CTOR$ s" ACTION-REGISTER--RESULT" T$=

\ ---- both families construct and dispatch through MATCH ------------------------
TT-RR-ARM 0 T=                                  \ a constructed ok reaches the ok arm
TT-RR-RT 0 T=                                   \ and carries its payload through unchanged
TT-RR-NZ TTRUE                                  \ against a non-zero index, so a zeroed payload fails
TT-RR-INC-ARM 1 T=                              \ incomplete reaches its own arm
TT-RR-CON-ARM 2 T=                              \ conflict reaches its own arm
TT-RR-INC-RAW -1 T=                             \ the payloadless arms of TT-RR-RAW are live
TT-DR-ACC 0 T=                                  \ each dispatch case keeps its own tag
TT-DR-UNK 1 T=
TT-DR-WK 2 T=
TT-DR-UNA 3 T=
TT-DR-UNS 4 T=
TT-RESTORE 9 T=                                 \ the test-owned registration is dropped again

\ ---- the generated constructors: exact spelling + exact effect -----------------
\ The SPELLING is load-bearing: the checker answers 1 (uncheckable) for a name it cannot
\ resolve, and YES demands -1, so a -1 means it resolved EXACTLY this constructor name; NO
\ demands 0, which it can only reach after resolving the name and refusing the types. The
\ NOWORD rows are the controls that prove that split.
s" TC-RR-OK ( CAD-KIND:action-id -- register-result<CAD-KIND:action-id> ) ACTION-REGISTER--RESULT:OK" YES
s" TC-RR-INC ( -- register-result<CAD-KIND:action-id> ) ACTION-REGISTER--RESULT:INCOMPLETE" YES
s" TC-RR-CON ( -- register-result<CAD-KIND:action-id> ) ACTION-REGISTER--RESULT:CONFLICT" YES
s" TC-RR-SPELL ( CAD-KIND:action-id -- register-result<CAD-KIND:action-id> ) ACTION-REGISTER--RESULTX:OK" NOWORD
s" TC-RR-RAW ( n -- register-result<CAD-KIND:action-id> ) ACTION-REGISTER--RESULT:OK" NO
s" TC-RR-BARE ( CAD-KIND:action-id -- n ) ACTION-REGISTER--RESULT:OK" NO
s" TC-RR-NONE ( -- register-result<CAD-KIND:action-id> ) ACTION-REGISTER--RESULT:OK" NO
s" TC-RR-FGN ( CAD-KIND:artifact-id -- register-result<CAD-KIND:action-id> ) ACTION-REGISTER--RESULT:OK" NO
\ the compact family's constructors are nullary and yield the family, never a bare cell
s" TC-DR-ACC ( -- dispatch-result ) ACTION-DISPATCH--RESULT:ACCEPTED" YES
s" TC-DR-UNS ( -- dispatch-result ) ACTION-DISPATCH--RESULT:UNSUPPORTED" YES
s" TC-DR-SPELL ( -- dispatch-result ) ACTION-DISPATCH--RESULTX:ACCEPTED" NOWORD
s" TC-DR-BARE ( -- n ) ACTION-DISPATCH--RESULT:ACCEPTED" NO
s" TC-DR-ARG ( n -- dispatch-result ) ACTION-DISPATCH--RESULT:ACCEPTED" NO
\ a bare cell cannot be MATCHed as either family, and the families do not cross
s" TC-DR-MATCH-N ( n -- ) MATCH dispatch-result accepted OF ENDOF unknown-action OF ENDOF wrong-kind OF ENDOF unauthorized OF ENDOF unsupported OF ENDOF ;MATCH" NO
s" TC-X-DR-RR ( -- register-result<CAD-KIND:action-id> ) ACTION-DISPATCH--RESULT:ACCEPTED" NO
s" TC-X-RR-DR ( CAD-KIND:action-id -- dispatch-result ) ACTION-REGISTER--RESULT:OK" NO

\ ---- identity is by name, not by shape ----------------------------------------
\ The decoy repeats every case name of dispatch-result in the same order at the same width:
\ a positive control MATCHes it in its own right, then neither family can be MATCHed as the
\ other. The register twin gets the same treatment through its own generated constructor.
s" TC-DEC-OK ( drother -- ) MATCH drother accepted OF ENDOF unknown-action OF ENDOF wrong-kind OF ENDOF unauthorized OF ENDOF unsupported OF ENDOF ;MATCH" YES
s" TC-DEC-X1 ( drother -- ) MATCH dispatch-result accepted OF ENDOF unknown-action OF ENDOF wrong-kind OF ENDOF unauthorized OF ENDOF unsupported OF ENDOF ;MATCH" NO
s" TC-DEC-X2 ( dispatch-result -- ) MATCH drother accepted OF ENDOF unknown-action OF ENDOF wrong-kind OF ENDOF unauthorized OF ENDOF unsupported OF ENDOF ;MATCH" NO
s" TC-DEC-X3 ( -- drother ) ACTION-DISPATCH--RESULT:ACCEPTED" NO
s" TC-TWIN-RR ( CAD-KIND:action-id -- ACTION-TEST:regr-twin<CAD-KIND:action-id> ) ACTION--TEST-REGR--TWIN:OK" YES
s" TC-TWIN-X1 ( CAD-KIND:action-id -- ACTION-TEST:regr-twin<CAD-KIND:action-id> ) ACTION-REGISTER--RESULT:OK" NO
s" TC-TWIN-X2 ( CAD-KIND:action-id -- register-result<CAD-KIND:action-id> ) ACTION--TEST-REGR--TWIN:OK" NO
EA-OOR-ALL    TTRUE                             \ every k in [COUNT, ACT-CAP] rejects
EA-NEG-ALL    TTRUE                             \ every k in [-8, -1] rejects

T-REPORT

;package

;using
