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

T-RESET

DA-OK 0 T=                             \ discharges + authorized verifier -> ok
DA-UNAUTH 2 T=                         \ discharges but verifier not authorized -> unauthorized
DA-SUBJ 1 T=                           \ wrong subject -> not-discharged
DA-VC 1 T=                             \ wrong verifier class -> not-discharged (class leg)
DA-INDEP 1 T=                          \ independence refusal wins over an authorized producer

T-REPORT

;package
