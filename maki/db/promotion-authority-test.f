\ maki/db/promotion-authority-test.f - acceptance for the folded discharge-AUTHORITY gate
\ (maki/db/promotion-authority.f; dot habu-v2-evidence-promotion-f8312ebe).
\
\ Proves the three folded legs of AUTHORIZED-DISCHARGE, each by a named test:
\   DA-OK     : discharges AND the verifier identity is on the allowlist -> ok
\   DA-UNAUTH : discharges but the verifier is NOT on the allowlist -> unauthorized (the new leg)
\   DA-SUBJ   : wrong subject -> not-discharged (folds a DISCHARGE named-field refusal)
\   DA-VC     : wrong verifier class -> not-discharged (the CLASS leg, even when authorized)
\   DA-INDEP  : verifier == producer under an INDEPENDENT policy -> not-discharged (the
\               INDEPENDENCE leg), proving the discharge refusal WINS over authorization
\               (the producer is on the allowlist, yet independence still refuses).
\ All sum / product / enum values are produced and consumed inside colon words.

require lib/test.f
require lib/string.f
require test/checker-assert.f     \ CHECK-QUIET-CANDIDATE! + the shared REFLECT reflection set
require maki/db/promotion-authority.f
require maki/db/obligation.f
require maki/artifact.f
require maki/config.f
require maki/producer.f

package PROMO-AUTH-TEST

\ ---- shared identities (test-owned names; REGISTER interns by content) ----------
: SUBJ ( -- CAD-KIND:artifact-id )        s" da-test/subj-1" ARTIFACT:REGISTER ;
: OTHER-SUBJ ( -- CAD-KIND:artifact-id )  s" da-test/subj-2" ARTIFACT:REGISTER ;
: ENV ( -- CAD-KIND:config-id )           s" da-test/env" CONFIG:REGISTER ;
: PROD ( -- CAD-KIND:producer-id )        s" da-test/agent-search" PRODUCER:REGISTER ;
: VERIF ( -- CAD-KIND:producer-id )       s" da-test/verifier-diff" PRODUCER:REGISTER ;
: VERIF2 ( -- CAD-KIND:producer-id )      s" da-test/verifier-other" PRODUCER:REGISTER ;

\ An INDEPENDENT semantic-equivalence obligation in the exact domain requiring a
\ differential-exec verifier, in ENV, on SUBJ, proposed by PROD.
: OBL-CANON ( -- OBLIG:obligation )
   OBLIG:NEW
   SUBJ OBLIG:SUBJECT
   OBLIG-RELATION:SEMANTIC-EQUIV OBLIG:RELATION
   OBLIG-DOMAIN:EXACT OBLIG:DOMAIN
   OBLIG-INDEPENDENCE:INDEPENDENT OBLIG:POLICY
   OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:VERIFIER-CLASS
   ENV OBLIG:ENVIRONMENT
   PROD OBLIG:PRODUCER
   OBLIG:SEAL ;

\ Evidence builders ( subject domain relation env verifier verifier-class ).
: EV-MATCH ( -- OBLIG:evidence )       \ verifier VERIF (authorized), discharges
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;
: EV-VERIF2 ( -- OBLIG:evidence )      \ verifier VERIF2 (NOT authorized), discharges
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF2 OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;
: EV-WRONGSUBJ ( -- OBLIG:evidence )
   OTHER-SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;
: EV-WRONGVC ( -- OBLIG:evidence )     \ static-checker cannot discharge a differential-exec obligation
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF OBLIG-VERIFIER:STATIC-CHECKER OBLIG:EVIDENCE ;
: EV-SELF ( -- OBLIG:evidence )        \ verifier IS the producer PROD (violates independence)
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV PROD OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG:EVIDENCE ;

\ An authority authorizing BOTH VERIF and PROD (so the independence refusal is shown to win
\ over an otherwise-authorized producer).
: AUTH-FULL ( -- DAUTH:authority )
   DAUTH:NEW  VERIF DAUTH:AUTHORIZE+  PROD DAUTH:AUTHORIZE+  DAUTH:SEAL ;

\ ---- authz-result decoder ( 0 ok / 1 not-discharged / 2 unauthorized ) -----------
: AR-CODE ( DAUTH:authz-result -- n )
   MATCH DAUTH:authz-result
      ok             OF drop 0 ENDOF
      not-discharged OF 1 ENDOF
      unauthorized   OF 2 ENDOF
   ;MATCH ;

: DA-OK ( -- n )      OBL-CANON EV-MATCH     AUTH-FULL DAUTH:AUTHORIZED-DISCHARGE AR-CODE ;
: DA-UNAUTH ( -- n )  OBL-CANON EV-VERIF2    AUTH-FULL DAUTH:AUTHORIZED-DISCHARGE AR-CODE ;
: DA-SUBJ ( -- n )    OBL-CANON EV-WRONGSUBJ AUTH-FULL DAUTH:AUTHORIZED-DISCHARGE AR-CODE ;
: DA-VC ( -- n )      OBL-CANON EV-WRONGVC   AUTH-FULL DAUTH:AUTHORIZED-DISCHARGE AR-CODE ;
: DA-INDEP ( -- n )   OBL-CANON EV-SELF      AUTH-FULL DAUTH:AUTHORIZED-DISCHARGE AR-CODE ;

\ ---- named-payload projection + round-trip through the production gate -----------
\ The accepting arm carries one named cell `ev`, the accepted evidence. An evidence is
\ multi-cell, so it is never bound as a typed local; the arm projects the field the
\ verdict actually depends on - the evidence's verifier identity - and reports whether
\ it is the identity that was authorized. A dropped or zeroed payload could not answer
\ that: it would not be VERIF, and VERIF is a registered identity rather than a zero.
: AR-EV-VERIF? ( DAUTH:authz-result -- bool )   \ the ok arm's evidence names VERIF
   MATCH DAUTH:authz-result
      ok             OF OBLIG:EV-VERIFIER@ VERIF PRODUCER:EQUAL? ENDOF
      not-discharged OF false ENDOF
      unauthorized   OF false ENDOF
   ;MATCH ;
: AR-EV-VERIF2? ( DAUTH:authz-result -- bool )  \ ...and is NOT the unauthorized identity
   MATCH DAUTH:authz-result
      ok             OF OBLIG:EV-VERIFIER@ VERIF2 PRODUCER:EQUAL? ENDOF
      not-discharged OF false ENDOF
      unauthorized   OF false ENDOF
   ;MATCH ;

: DA-GATE ( -- DAUTH:authz-result )   OBL-CANON EV-MATCH AUTH-FULL DAUTH:AUTHORIZED-DISCHARGE ;
: DA-EV-IS-VERIF? ( -- bool )   DA-GATE AR-EV-VERIF? ;
: DA-EV-IS-VERIF2? ( -- bool )  DA-GATE AR-EV-VERIF2? ;
\ the refusing arms carry no evidence at all
: DA-UNAUTH-EV? ( -- bool )
   OBL-CANON EV-VERIF2 AUTH-FULL DAUTH:AUTHORIZED-DISCHARGE AR-EV-VERIF? ;
: DA-NOTDISCH-EV? ( -- bool )
   OBL-CANON EV-WRONGSUBJ AUTH-FULL DAUTH:AUTHORIZED-DISCHARGE AR-EV-VERIF? ;

\ ---- a foreign proof token, for the unforgeability negatives ---------------------
\ Its only purpose is to be a DIFFERENT arity-0 nominal cell family of the same width
\ as DAUTH:auth-proof, which is exactly what a forger would bring to the tok slot.
NEWTYPE other-proof 0

\ ---- the identities these pins are about (tail + constructor package, REFLECT's R7 key)
: AUTH$ ( -- ptr u8 n ptr u8 n )   s" authority" s" DAUTH-AUTHORITY" ;
: AZR$ ( -- ptr u8 n ptr u8 n )    s" authz-result" s" DAUTH-AUTHZ--RESULT" ;

T-RESET

DA-OK 0 T=                             \ discharges + authorized verifier -> ok
DA-UNAUTH 2 T=                         \ discharges but verifier not authorized -> unauthorized
DA-SUBJ 1 T=                           \ wrong subject -> not-discharged
DA-VC 1 T=                             \ wrong verifier class -> not-discharged (class leg)
DA-INDEP 1 T=                          \ independence refusal wins over an authorized producer

\ ==== the sealed authority as a STRUCTURE =======================================
\ The generated pair keeps its exact spelling and effect, so SEAL and
\ AUTHORIZED-DISCHARGE do not move. A -1 means the checker resolved EXACTLY this
\ name; it answers 1 for a name it cannot resolve.
s" DA-P-MK ( n DAUTH:auth-proof -- DAUTH:authority ) DAUTH-AUTHORITY:MAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" DA-P-UN ( DAUTH:authority -- n DAUTH:auth-proof ) DAUTH-AUTHORITY:UNMAKE"
   CHECK-QUIET-CANDIDATE! -1 T=

\ ---- unforgeability: the proof slot admits nothing but a real auth-proof ---------
\ This is what makes an authority unable to silently WIDEN who may discharge. A raw
\ cell cannot fill the proof slot; a foreign nominal cell family of the same width
\ cannot either; nor can a producer identity, which is also one cell; the slot cannot
\ be dropped; and the record never UNMAKEs its proof out as a raw cell, which would
\ hand the token to any caller.
s" DA-F-RAWTOK ( n n -- DAUTH:authority ) DAUTH-AUTHORITY:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" DA-F-FGNTOK ( n other-proof -- DAUTH:authority ) DAUTH-AUTHORITY:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" DA-F-IDTOK ( n CAD-KIND:producer-id -- DAUTH:authority ) DAUTH-AUTHORITY:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" DA-F-NOTOK ( n -- DAUTH:authority ) DAUTH-AUTHORITY:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" DA-F-UNRAW ( DAUTH:authority -- n n ) DAUTH-AUTHORITY:UNMAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" DA-F-UNFGN ( DAUTH:authority -- n other-proof ) DAUTH-AUTHORITY:UNMAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
\ the two cells cannot trade places, and the record is not a bare scalar.
s" DA-F-SWAP ( DAUTH:auth-proof n -- DAUTH:authority ) DAUTH-AUTHORITY:MAKE"
   CHECK-QUIET-CANDIDATE! 0 T=
s" DA-F-BARE ( n -- DAUTH:authority ) "
   CHECK-QUIET-CANDIDATE! 0 T=
\ the mint is PRIVATE to DAUTH: unresolvable from here (verdict 1), qualified or bare,
\ so no caller outside the owner can produce a proof and seal an authority around SEAL.
s" DA-F-MINT ( n -- DAUTH:authority ) DAUTH:MINT-AUTH-PROOF DAUTH-AUTHORITY:MAKE"
   CHECK-QUIET-CANDIDATE! 1 T=
s" DA-F-MINT2 ( n -- DAUTH:authority ) MINT-AUTH-PROOF DAUTH-AUTHORITY:MAKE"
   CHECK-QUIET-CANDIDATE! 1 T=

AUTH$ REFLECT:FAMS 1 T=
AUTH$ REFLECT:KIND TK-PRODUCT T=
AUTH$ REFLECT:ARITY 0 T=
AUTH$ REFLECT:WIDTH 2 T=
AUTH$ REFLECT:VIS 1 T=
AUTH$ REFLECT:FLDS 2 T=                 \ exactly two named fields, no more
AUTH$ s" slot" REFLECT:SLOT 0 T=
AUTH$ s" tok" REFLECT:SLOT 1 T=
AUTH$ s" slot" REFLECT:CELLS 1 T=
AUTH$ s" tok" REFLECT:CELLS 1 T=
AUTH$ s" ev" REFLECT:SLOT -1 T=         \ an undeclared name has no slot
AUTH$ 0 REFLECT:ARM-FLDS 0 T=           \ a record owns no per-case rows

\ ==== the gate outcome as a full-mode payload ENUM ===============================
s" AZ-P-OK ( OBLIG:evidence -- DAUTH:authz-result ) DAUTH-AUTHZ--RESULT:OK"
   CHECK-QUIET-CANDIDATE! -1 T=
s" AZ-P-ND ( -- DAUTH:authz-result ) DAUTH-AUTHZ--RESULT:NOT-DISCHARGED"
   CHECK-QUIET-CANDIDATE! -1 T=
s" AZ-P-UN ( -- DAUTH:authz-result ) DAUTH-AUTHZ--RESULT:UNAUTHORIZED"
   CHECK-QUIET-CANDIDATE! -1 T=
\ the evidence payload is mandatory on the accepting arm and forbidden on the two
\ refusal arms, it is not a raw cell, an obligation cannot stand in for evidence even
\ though both are records from the same package, and the result is not a bare scalar.
s" AZ-F-OK-NOPAY ( -- DAUTH:authz-result ) DAUTH-AUTHZ--RESULT:OK"
   CHECK-QUIET-CANDIDATE! 0 T=
s" AZ-F-OK-RAW ( n -- DAUTH:authz-result ) DAUTH-AUTHZ--RESULT:OK"
   CHECK-QUIET-CANDIDATE! 0 T=
s" AZ-F-OK-OBL ( OBLIG:obligation -- DAUTH:authz-result ) DAUTH-AUTHZ--RESULT:OK"
   CHECK-QUIET-CANDIDATE! 0 T=
s" AZ-F-ND-PAY ( OBLIG:evidence -- DAUTH:authz-result ) DAUTH-AUTHZ--RESULT:NOT-DISCHARGED"
   CHECK-QUIET-CANDIDATE! 0 T=
s" AZ-F-UN-PAY ( OBLIG:evidence -- DAUTH:authz-result ) DAUTH-AUTHZ--RESULT:UNAUTHORIZED"
   CHECK-QUIET-CANDIDATE! 0 T=
s" AZ-F-OK-BARE ( OBLIG:evidence -- n ) DAUTH-AUTHZ--RESULT:OK"
   CHECK-QUIET-CANDIDATE! 0 T=
\ MATCH binds on exactly the accepting arm: binding a refusal arm rejects.
s" AZ-M-OK ( DAUTH:authz-result -- n ) MATCH DAUTH:authz-result ok OF drop 0 ENDOF not-discharged OF 1 ENDOF unauthorized OF 2 ENDOF ;MATCH"
   CHECK-QUIET-CANDIDATE! -1 T=
s" AZ-M-OVERBIND ( DAUTH:authz-result -- n ) MATCH DAUTH:authz-result ok OF drop 0 ENDOF not-discharged OF drop 1 ENDOF unauthorized OF 2 ENDOF ;MATCH"
   CHECK-QUIET-CANDIDATE! 0 T=

AZR$ REFLECT:FAMS 1 T=
AZR$ REFLECT:KIND TK-SUM T=
AZR$ REFLECT:ARITY 0 T=
AZR$ REFLECT:WIDTH 2 T=                 \ one payload cell plus one tag cell
AZR$ REFLECT:VIS 1 T=                   \ public: commit-store.f matches on it
AZR$ REFLECT:VARS 3 T=
AZR$ 0 REFLECT:ARM$ s" ok" T$=
AZR$ 1 REFLECT:ARM$ s" not-discharged" T$=
AZR$ 2 REFLECT:ARM$ s" unauthorized" T$=
AZR$ 0 REFLECT:ARM-CTOR$ s" DAUTH-AUTHZ--RESULT" T$=
AZR$ 0 REFLECT:ARM-FLDS 1 T=
AZR$ 1 REFLECT:ARM-FLDS 0 T=
AZR$ 2 REFLECT:ARM-FLDS 0 T=
AZR$ 0 s" ev" REFLECT:ARM-SLOT 0 T=
AZR$ 1 s" ev" REFLECT:ARM-SLOT -1 T=    \ the name is per-arm
AZR$ 2 s" ev" REFLECT:ARM-SLOT -1 T=
AZR$ 0 s" evidence" REFLECT:ARM-SLOT -1 T=   \ an undeclared name has no slot
AZR$ 3 s" ev" REFLECT:ARM-SLOT -1 T=         \ and no fourth arm exists

\ ==== the accepting arm carries the evidence the gate accepted ====================
\ Driven through the real gate, not a fabricated value: the ok arm's evidence must be
\ the one whose verifier was on the allowlist, and must not be the identity the
\ unauthorized fixture uses. The refusal arms carry no evidence to project.
DA-EV-IS-VERIF? TTRUE                  \ the accepted evidence names the authorized verifier
DA-EV-IS-VERIF2? TFALSE                \ and not the unauthorized one
DA-UNAUTH-EV? TFALSE                   \ the unauthorized arm carries no evidence
DA-NOTDISCH-EV? TFALSE                 \ nor does the not-discharged arm

public

\ auth-twin and azr-twin are the migrated families' SHAPES under different names -
\ same arity, same fields and arms in the same order, same named payload. They exist
\ only so the negatives below can prove identity is NOMINAL: an identically shaped
\ record does not unify with the sealed authority in either direction, which is what
\ stops a look-alike record from being accepted as authority. They must be public - a
\ private family publishes no constructor package at all - and each positive control
\ builds through the twin's own constructor, so no negative can pass by being
\ unresolvable rather than ill-typed.
STRUCTURE auth-twin 0
   FIELD slot n
   FIELD tok other-proof
;STRUCTURE

ENUM azr-twin 0
   VARIANT azr-twin-ok FIELD ev OBLIG:evidence ;VARIANT
   VARIANT azr-twin-nd ;VARIANT
   VARIANT azr-twin-un ;VARIANT
;ENUM

private

s" DA-TW ( n other-proof -- auth-twin ) PROMO--AUTH--TEST-AUTH--TWIN:MAKE"
   CHECK-QUIET-CANDIDATE! -1 T=
s" DA-TW-X1 ( auth-twin -- DAUTH:authority ) "
   CHECK-QUIET-CANDIDATE! 0 T=
s" DA-TW-X2 ( DAUTH:authority -- auth-twin ) "
   CHECK-QUIET-CANDIDATE! 0 T=
s" AZ-TW ( OBLIG:evidence -- azr-twin ) PROMO--AUTH--TEST-AZR--TWIN:AZR-TWIN-OK"
   CHECK-QUIET-CANDIDATE! -1 T=
s" AZ-TW-X1 ( OBLIG:evidence -- azr-twin ) DAUTH-AUTHZ--RESULT:OK"
   CHECK-QUIET-CANDIDATE! 0 T=
s" AZ-TW-X2 ( OBLIG:evidence -- DAUTH:authz-result ) PROMO--AUTH--TEST-AZR--TWIN:AZR-TWIN-OK"
   CHECK-QUIET-CANDIDATE! 0 T=
\ REFLECT reads each identity's OWN shape rather than whichever family loaded first.
s" azr-twin" s" PROMO--AUTH--TEST-AZR--TWIN" REFLECT:FAMS 1 T=
s" azr-twin" s" PROMO--AUTH--TEST-AZR--TWIN" 0 REFLECT:ARM$ s" azr-twin-ok" T$=
AZR$ 0 REFLECT:ARM$ s" ok" T$=

T-REPORT

;package
