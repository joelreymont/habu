\ maki/db/obligation-test.f - acceptance for the typed proof-obligation schema
\ (maki/db/obligation.f; dot habu-v2-proof-obligation-6cf70b4f).
\
\ Proves the plan:3737-3755 acceptance, each item by a named test word (all sum /
\ product / enum values are produced and consumed INSIDE colon words, never on the
\ interpret-mode stack):
\   A1-* : ACCEPTANCE 1 - wrong-domain evidence cannot discharge. The DYNAMIC leg is a
\          typed `wrong-domain` reject (A1-DEVICE); the STATIC leg is the cad-kinds
\          verdict-fixture pattern via CHECK-CANDIDATE! - a proof `domain` is a nominal
\          type the checker guards, so passing a `relation` or a raw int where
\          DOMAIN-COERCIBLE? needs a `domain` is REJECTED, paired with a certifying control.
\   A2-* : ACCEPTANCE 2 - subject or environment (and relation / verifier-class) mismatch rejects.
\   A3-* : ACCEPTANCE 3 - the producer cannot be the sole verifier under an INDEPENDENT
\          policy (not-independent); a SELF-VERIFY policy permits it.
\   INV-* : ACCEPTANCE 4 - a changed dependency invalidates exactly the affected
\          obligation; an UNRELATED obligation survives (INV-OTHER-X).
\   NP-*  : the documented one-way NPOL:dom -> domain projection.
\   OB-RT-* / RT-* : the obligation round-trips byte-identically and field-for-field.
\   OB-DEC-* : the decode-result reject taxonomy (malformed, noncanonical, duplicate,
\          bounds, unknown-required) and the foreign-id fold (wrong-width -> malformed,
\          unknown -> bounds) are reachable and typed.
\
\ The test reopens package OBLIG (a friend) to reach the private wire constants and
\ slot helpers; happy-path calls use the public API. Identity fixtures mint real ids
\ through their owner constructors (ARTIFACT/CONFIG/PRODUCER:REGISTER, NPOL-DOM:*) -
\ never a raw cast.

require lib/test.f
require lib/string.f
require maki/db/obligation.f
require maki/numpolicy.f
require maki/artifact.f
require maki/config.f
require maki/producer.f

package OBLIG

create OB-A 1024 allot
create OB-B 1024 allot
create OB-TB 512 allot
variable OB-TB-U
variable OB-D-SLOT                 \ last round-tripped obligation slot

\ ---- result inspectors --------------------------------------------------------
\ discharge-result: 0 ok, else the reject-arm ordinal.
: DR-CODE ( discharge-result -- n )
   MATCH discharge-result
      ok OF EV> drop 0 ENDOF
      wrong-subject OF 1 ENDOF
      wrong-domain OF 2 ENDOF
      wrong-relation OF 3 ENDOF
      wrong-environment OF 4 ENDOF
      wrong-verifier-class OF 5 ENDOF
      not-independent OF 6 ENDOF
   ;MATCH ;

\ decode-result: 0 ok, else the taxonomy ordinal.
: DC-CODE ( ptr u8 n -- n )
   DECODE MATCH decode-result
      ok OF OBL> drop 0 ENDOF
      malformed OF 1 ENDOF
      noncanonical OF 2 ENDOF
      bounds OF 3 ENDOF
      duplicate OF 4 ENDOF
      unknown-required OF 5 ENDOF
   ;MATCH ;

: DC-SLOT ( ptr u8 n -- n )        \ decoded slot on ok, else -1
   DECODE MATCH decode-result
      ok OF OBL> ENDOF
      malformed OF -1 ENDOF
      noncanonical OF -1 ENDOF
      bounds OF -1 ENDOF
      duplicate OF -1 ENDOF
      unknown-required OF -1 ENDOF
   ;MATCH ;

: BYTES= ( ptr u8 n ptr u8 n -- bool ) {: a:ptr an:n b:ptr bn:n :}
   an bn <> if false exit then
   0 begin dup an < while
      dup {: k:n :}
      a k + c@  b k + c@  <> if drop false exit then
      1+
   repeat drop true ;

\ ---- shared identities (registered once; REGISTER interns by content) ----------
\ Names carry the OBLIG-TEST prefix (docs/forth.md § "fixtures use unique test-owned
\ names") so a shared process-global registry count is not perturbed for a later
\ suite that asserts count deltas over fresh names of its own.
: SUBJ ( -- CAD-KIND:artifact-id )        s" oblig-test/art-subject-1" ARTIFACT:REGISTER ;
: OTHER-SUBJ ( -- CAD-KIND:artifact-id )  s" oblig-test/art-subject-2" ARTIFACT:REGISTER ;
: ENV ( -- CAD-KIND:config-id )           s" oblig-test/env-sm87" CONFIG:REGISTER ;
: OTHER-ENV ( -- CAD-KIND:config-id )     s" oblig-test/env-orin" CONFIG:REGISTER ;
: PROD ( -- CAD-KIND:producer-id )        s" oblig-test/agent-search" PRODUCER:REGISTER ;
: VERIF ( -- CAD-KIND:producer-id )       s" oblig-test/verifier-diff" PRODUCER:REGISTER ;
: DEPX ( -- CAD-KIND:artifact-id )        s" oblig-test/art-dep-x" ARTIFACT:REGISTER ;
: DEPY ( -- CAD-KIND:artifact-id )        s" oblig-test/art-dep-y" ARTIFACT:REGISTER ;

\ ---- the canonical obligation + a matching (independent) evidence --------------
\ An INDEPENDENT semantic-equivalence obligation in the exact domain, requiring a
\ differential-exec verifier, in the sm87 environment, on subject art-subject-1,
\ over deps {art-dep-x, art-dep-y}, proposed by agent/search.
: OBL-CANON ( -- obligation )
   NEW
   SUBJ SUBJECT
   OBLIG-RELATION:SEMANTIC-EQUIV RELATION
   OBLIG-DOMAIN:EXACT DOMAIN
   OBLIG-INDEPENDENCE:INDEPENDENT POLICY
   OBLIG-VERIFIER:DIFFERENTIAL-EXEC VERIFIER-CLASS
   ENV ENVIRONMENT
   PROD PRODUCER
   DEPX DEP+  DEPY DEP+
   SEAL ;

\ A self-verify variant (same axes, permits producer==verifier).
: OBL-SELFPOL ( -- obligation )
   NEW
   SUBJ SUBJECT
   OBLIG-RELATION:SEMANTIC-EQUIV RELATION
   OBLIG-DOMAIN:EXACT DOMAIN
   OBLIG-INDEPENDENCE:SELF-VERIFY POLICY
   OBLIG-VERIFIER:DIFFERENTIAL-EXEC VERIFIER-CLASS
   ENV ENVIRONMENT
   PROD PRODUCER
   DEPX DEP+  DEPY DEP+
   SEAL ;

\ A second obligation with a different subject depending only on art-dep-y.
: OBL-OTHER ( -- obligation )
   NEW
   OTHER-SUBJ SUBJECT
   OBLIG-RELATION:SEMANTIC-EQUIV RELATION
   OBLIG-DOMAIN:EXACT DOMAIN
   OBLIG-INDEPENDENCE:INDEPENDENT POLICY
   OBLIG-VERIFIER:DIFFERENTIAL-EXEC VERIFIER-CLASS
   ENV ENVIRONMENT
   PROD PRODUCER
   DEPY DEP+
   SEAL ;

\ Evidence builders: subject domain relation environment verifier verifier-class.
: EV-MATCH ( -- evidence )
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF OBLIG-VERIFIER:DIFFERENTIAL-EXEC EVIDENCE ;
: EV-DEVICE ( -- evidence )            \ wrong DOMAIN (a device measurement)
   SUBJ OBLIG-DOMAIN:DEVICE OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF OBLIG-VERIFIER:DIFFERENTIAL-EXEC EVIDENCE ;
: EV-WRONG-SUBJ ( -- evidence )
   OTHER-SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF OBLIG-VERIFIER:DIFFERENTIAL-EXEC EVIDENCE ;
: EV-WRONG-ENV ( -- evidence )
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV OTHER-ENV VERIF OBLIG-VERIFIER:DIFFERENTIAL-EXEC EVIDENCE ;
: EV-WRONG-REL ( -- evidence )
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:RESOURCE-BOUND ENV VERIF OBLIG-VERIFIER:DIFFERENTIAL-EXEC EVIDENCE ;
: EV-WRONG-VC ( -- evidence )          \ static-checker cannot discharge a differential-exec obligation
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV VERIF OBLIG-VERIFIER:STATIC-CHECKER EVIDENCE ;
: EV-SELF ( -- evidence )              \ verifier IS the producer (agent/search)
   SUBJ OBLIG-DOMAIN:EXACT OBLIG-RELATION:SEMANTIC-EQUIV ENV PROD OBLIG-VERIFIER:DIFFERENTIAL-EXEC EVIDENCE ;

\ ---- ACCEPTANCE 1: wrong-domain evidence cannot discharge ----------------------
: A1-OK ( -- n )       OBL-CANON EV-MATCH  DISCHARGE DR-CODE ;
: A1-DEVICE ( -- n )   OBL-CANON EV-DEVICE DISCHARGE DR-CODE ;
: A1-COERCE-EQ ( -- bool )   OBLIG-DOMAIN:EXACT OBLIG-DOMAIN:EXACT DOMAIN-COERCIBLE? ;
: A1-COERCE-NE ( -- bool )   OBLIG-DOMAIN:EXACT OBLIG-DOMAIN:APPROXIMATE DOMAIN-COERCIBLE? ;

\ ---- ACCEPTANCE 2: subject / environment (and relation / verifier-class) mismatch
: A2-SUBJ ( -- n )   OBL-CANON EV-WRONG-SUBJ DISCHARGE DR-CODE ;
: A2-ENV ( -- n )    OBL-CANON EV-WRONG-ENV  DISCHARGE DR-CODE ;
: A2-REL ( -- n )    OBL-CANON EV-WRONG-REL  DISCHARGE DR-CODE ;
: A2-VC ( -- n )     OBL-CANON EV-WRONG-VC   DISCHARGE DR-CODE ;

\ ---- ACCEPTANCE 3: producer cannot be sole verifier under an INDEPENDENT policy -
: A3-SELF-INDEP ( -- n )  OBL-CANON  EV-SELF DISCHARGE DR-CODE ;
: A3-SELF-OK ( -- n )     OBL-SELFPOL EV-SELF DISCHARGE DR-CODE ;

\ ---- ACCEPTANCE 4: a changed dependency invalidates exactly the affected obligation
: INV-CANON-X ( -- bool )         OBL-CANON DEPX INVALIDATED-BY? ;
: INV-OTHER-X ( -- bool )         OBL-OTHER DEPX INVALIDATED-BY? ;
: INV-OTHER-Y ( -- bool )         OBL-OTHER DEPY INVALIDATED-BY? ;
: INV-CANON-SUBJ ( -- bool )      OBL-CANON SUBJ INVALIDATED-BY? ;
: INV-CANON-OTHERSUBJ ( -- bool ) OBL-CANON OTHER-SUBJ INVALIDATED-BY? ;

\ ---- NPOL:dom -> domain projection (documented one-way bridge) -----------------
: NP-EXACT ( -- bool )  NPOL-DOM:EXACT     NPOL>DOMAIN OBLIG-DOMAIN:EXACT       OBLIG-DOMAIN:EQ ;
: NP-ULP ( -- bool )    NPOL-DOM:ULP       NPOL>DOMAIN OBLIG-DOMAIN:APPROXIMATE OBLIG-DOMAIN:EQ ;
: NP-REL ( -- bool )    NPOL-DOM:RELATIVE  NPOL>DOMAIN OBLIG-DOMAIN:APPROXIMATE OBLIG-DOMAIN:EQ ;
: NP-EMP ( -- bool )    NPOL-DOM:EMPIRICAL NPOL>DOMAIN OBLIG-DOMAIN:EMPIRICAL   OBLIG-DOMAIN:EQ ;

\ ---- CHECK-CANDIDATE! verdict wrapper (the cad-kinds/maki-eval harness) ---------
\ Drives the checker over a candidate "NAME ( effect ) body" string; verdict is
\ -1 certified / 0 rejected / 1 uncheckable (checker.f CHECK-CANDIDATE!). A negative
\ fixture's reject diagnostic is captured into VDIAG (the lib/ptx/neg-test-lib
\ DIAG-BUFFER! idiom) so it does not print during a passing suite.
create VDIAG 4096 allot
: VCHECK ( ptr u8 n -- n )
   VDIAG 4096 DIAG-BUFFER!
   CHECK-CANDIDATE!
   DIAG-BUFFER-OFF ;

\ ---- round-trip (byte-identical + field-for-field) -----------------------------
: D ( -- obligation )   OB-D-SLOT @ >OBL ;
: RT! ( -- )   OBL-CANON OB-A 1024 ENCODE {: l:n :}  OB-A l DC-SLOT OB-D-SLOT ! ;

: OB-RT-BYTES ( -- bool )              \ encode, decode, re-encode, compare
   OBL-CANON OB-A 1024 ENCODE {: l1:n :}
   OB-A l1 DC-SLOT >OBL OB-B 1024 ENCODE {: l2:n :}
   OB-A l1 OB-B l2 BYTES= ;

: RT-DOMAIN? ( -- bool )   D DOMAIN@ OBLIG-DOMAIN:EXACT OBLIG-DOMAIN:EQ ;
: RT-RELATION? ( -- bool ) D RELATION@ OBLIG-RELATION:SEMANTIC-EQUIV OBLIG-RELATION:EQ ;
: RT-POLICY? ( -- bool )   D POLICY@ OBLIG-INDEPENDENCE:INDEPENDENT OBLIG-INDEPENDENCE:EQ ;
: RT-VC? ( -- bool )       D VERIFIER-CLASS@ OBLIG-VERIFIER:DIFFERENTIAL-EXEC OBLIG-VERIFIER:EQ ;
: RT-SUBJ? ( -- bool )     s" oblig-test/art-subject-1" D SUBJECT@ ARTIFACT:KEY$ STR= ;
: RT-ENV? ( -- bool )      s" oblig-test/env-sm87" D ENVIRONMENT@ CONFIG:FACTS$ STR= ;
: RT-PROD? ( -- bool )     s" oblig-test/agent-search" D PRODUCER@ PRODUCER:NAME$ STR= ;
: RT-DEPN ( -- n )         D DEP-COUNT ;
: RT-DEP0? ( -- bool )     s" oblig-test/art-dep-x" D 0 DEP@ ARTIFACT:KEY$ STR= ;
: RT-DEP1? ( -- bool )     s" oblig-test/art-dep-y" D 1 DEP@ ARTIFACT:KEY$ STR= ;

\ ---- test byte builder (private wire constants) --------------------------------
: TBB-RST ( -- )   0 OB-TB-U ! ;
: TBB-U8 ( n -- ) {: c:n :}   c OB-TB OB-TB-U @ + c!  OB-TB-U @ 1+ OB-TB-U ! ;
: TBB-LE ( n n -- ) {: v:n w:n :}   v OB-TB OB-TB-U @ + w LE-PUT  OB-TB-U @ w + OB-TB-U ! ;
: TBB-U64F ( n n n -- ) {: tag:n flags:n v:n :}   \ a length-8 scalar field
   tag TBB-U8  flags TBB-U8  U64W U32W TBB-LE  v U64W TBB-LE ;
: TBB-FILL ( n n -- ) {: byte:n cnt:n :}          \ cnt copies of byte
   0 begin dup cnt < while  byte TBB-U8  1+  repeat drop ;
: TBB$ ( -- ptr u8 n )   OB-TB OB-TB-U @ ;

\ ---- decode reject taxonomy ---------------------------------------------------
: OB-DEC-MALFORMED ( -- n )            \ truncate a valid envelope by one byte
   OBL-CANON OB-A 1024 ENCODE {: len:n :}
   OB-A len 1-  DC-CODE ;

: OB-DEC-UNKNOWN-REQ ( -- n )          \ valid envelope + an unknown REQUIRED field
   OBL-CANON OB-A 1024 ENCODE {: len:n :}
   200 OB-A len + c!
   FLAG-REQUIRED OB-A len 1+ + c!
   0 OB-A len 2 + + U32W LE-PUT
   OB-A len HDR-W + U32W + DC-CODE ;

: OB-DEC-NONCANON ( -- n )             \ TAG-DOMAIN before TAG-RELATION (descending)
   TBB-RST
   TAG-DOMAIN   FLAG-REQUIRED 0 TBB-U64F
   TAG-RELATION FLAG-REQUIRED 0 TBB-U64F
   TBB$ DC-CODE ;

: OB-DEC-DUP ( -- n )                  \ TAG-DOMAIN twice
   TBB-RST
   TAG-DOMAIN FLAG-REQUIRED 0 TBB-U64F
   TAG-DOMAIN FLAG-REQUIRED 0 TBB-U64F
   TBB$ DC-CODE ;

: OB-DEC-BOUNDS ( -- n )               \ a scalar field with a non-8 declared length
   TBB-RST
   TAG-DOMAIN TBB-U8  FLAG-REQUIRED TBB-U8
   4 U32W TBB-LE
   0 U32W TBB-LE
   TBB$ DC-CODE ;

: OB-DEC-ENUM-OOR ( -- n )             \ an enum ordinal outside its closed domain
   TBB-RST
   TAG-DOMAIN FLAG-REQUIRED 6 TBB-U64F
   TBB$ DC-CODE ;

: OB-DEC-ENV-WIDTH ( -- n )            \ ENV content key of the wrong width -> malformed
   TBB-RST
   TAG-ENV TBB-U8  FLAG-REQUIRED TBB-U8  16 U32W TBB-LE
   0 16 TBB-FILL
   TBB$ DC-CODE ;

: OB-DEC-ENV-UNKNOWN ( -- n )          \ well-formed but UNREGISTERED 32-byte ENV key -> bounds
   TBB-RST
   TAG-ENV TBB-U8  FLAG-REQUIRED TBB-U8  CK-BYTES U32W TBB-LE
   $AB CK-BYTES TBB-FILL
   TBB$ DC-CODE ;

T-RESET

\ ---- ACCEPTANCE 1: wrong-domain evidence cannot discharge ----------------------
A1-OK 0 T=                             \ matching independent evidence discharges
A1-DEVICE 2 T=                         \ device-domain evidence -> wrong-domain
A1-COERCE-EQ TTRUE                     \ equal domains coerce
A1-COERCE-NE TFALSE                    \ distinct domains do not coerce (no lattice)
\ static leg: the proof-domain axis is a nominal type the checker guards.
s" POBOK ( -- bool ) OBLIG-DOMAIN:APPROXIMATE OBLIG-DOMAIN:EXACT OBLIG:DOMAIN-COERCIBLE?" VCHECK -1 T=
s" POBREL ( -- bool ) OBLIG-RELATION:SEMANTIC-EQUIV OBLIG-DOMAIN:EXACT OBLIG:DOMAIN-COERCIBLE?" VCHECK 0 T=
s" POBRAW ( -- bool ) 5 OBLIG-DOMAIN:EXACT OBLIG:DOMAIN-COERCIBLE?" VCHECK 0 T=

\ ---- ACCEPTANCE 2: subject / environment mismatch rejects ----------------------
A2-SUBJ 1 T=                           \ wrong-subject
A2-ENV 4 T=                            \ wrong-environment
A2-REL 3 T=                            \ wrong-relation
A2-VC 5 T=                             \ wrong-verifier-class

\ ---- ACCEPTANCE 3: independence ------------------------------------------------
A3-SELF-INDEP 6 T=                     \ verifier==producer under independent -> not-independent
A3-SELF-OK 0 T=                        \ self-verify policy permits producer==verifier
A1-OK 0 T=                             \ an independent verifier discharges (positive control)

\ ---- ACCEPTANCE 4: invalidation ------------------------------------------------
INV-CANON-X TTRUE                      \ affected: art-dep-x is a dependency of OBL-CANON
INV-OTHER-X TFALSE                     \ UNRELATED obligation survives an unrelated change
INV-OTHER-Y TTRUE                      \ OBL-OTHER's own dependency does invalidate it
INV-CANON-SUBJ TTRUE                   \ a subject change invalidates
INV-CANON-OTHERSUBJ TFALSE             \ an unrelated artifact does not

\ ---- NPOL bridge ---------------------------------------------------------------
NP-EXACT TTRUE
NP-ULP TTRUE
NP-REL TTRUE
NP-EMP TTRUE

\ ---- round-trip ----------------------------------------------------------------
OB-RT-BYTES TTRUE
RT!
RT-DOMAIN? TTRUE
RT-RELATION? TTRUE
RT-POLICY? TTRUE
RT-VC? TTRUE
RT-SUBJ? TTRUE
RT-ENV? TTRUE
RT-PROD? TTRUE
RT-DEPN 2 T=
RT-DEP0? TTRUE
RT-DEP1? TTRUE

\ ---- decode reject taxonomy ----------------------------------------------------
OB-DEC-MALFORMED 1 T=
OB-DEC-UNKNOWN-REQ 5 T=
OB-DEC-NONCANON 2 T=
OB-DEC-DUP 4 T=
OB-DEC-BOUNDS 3 T=
OB-DEC-ENUM-OOR 3 T=
OB-DEC-ENV-WIDTH 1 T=
OB-DEC-ENV-UNKNOWN 3 T=

T-REPORT

;package
