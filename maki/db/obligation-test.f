\ maki/db/obligation-test.f - acceptance for the typed proof-obligation schema
\ (maki/db/obligation.f; dot habu-v2-proof-obligation-6cf70b4f).
\
\ Proves the plan:3737-3755 acceptance, each item by a named test word (all sum /
\ structure / enum values are produced and consumed INSIDE colon words, never on the
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
require test/checker-assert.f
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

\ ---- obligation-id registry (reopened OBLIG friend reaches the private raw) -----
1024 constant OBID-WCAP
create OBID-WBUF OBID-WCAP allot
create OBID-SHA CK-BYTES allot

\ Equal obligations (byte-identical ENCODE) intern to ONE id; distinct obligations do not.
: OBID-INTERN-EQ ( -- bool )   OBL-CANON INTERN  OBL-CANON INTERN  ID-EQUAL? ;
: OBID-INTERN-NE ( -- bool )   OBL-CANON INTERN  OBL-OTHER INTERN  ID-EQUAL? ;

: OBID-WIRE-RT ( CAD-KIND:obligation-id -- n )   \ 0 = ID>WIRE/WIRE>ID round-trips to an EQUAL? id
   dup {: orig:CAD-KIND:obligation-id :}
   OBID-WBUF OBID-WCAP ID>WIRE {: len:n :}
   OBID-WBUF len WIRE>ID
   MATCH id-result
      ok OF orig ID-EQUAL? if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown OF 3 ENDOF
   ;MATCH ;

: OBID-WIRE-ALL ( -- n )                         \ 0 iff EVERY interned id round-trips (raw)
   OBLID-N @ 0 ?do
      i RAW>OBLIGATION-ID OBID-WIRE-RT 0<> if 1 unloop exit then
   loop 0 ;

: OBID-WIRE-WIDTH ( -- n )   OBID-WBUF 4 WIRE>ID
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: OBID-WIRE-UNKNOWN ( -- n )                     \ an out-of-range raw -> unknown
   OBLID-N @ 100 +  OBID-WBUF U64W LE-PUT
   OBID-WBUF U64W WIRE>ID
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: OBID-CKEY-RT ( CAD-KIND:obligation-id -- n )   \ 0 = KEY>WIRE/WIRE>KEY round-trips to an EQUAL? id
   dup {: orig:CAD-KIND:obligation-id :}
   OBID-WBUF OBID-WCAP KEY>WIRE {: len:n :}
   OBID-WBUF len WIRE>KEY
   MATCH id-result
      ok OF orig ID-EQUAL? if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown OF 3 ENDOF
   ;MATCH ;

: OBID-CKEY-ALL ( -- n )                         \ 0 iff EVERY interned id round-trips (content key)
   OBLID-N @ 0 ?do
      i RAW>OBLIGATION-ID OBID-CKEY-RT 0<> if 1 unloop exit then
   loop 0 ;

: OBID-CKEY-WIDTH ( -- n )   OBID-WBUF 8 WIRE>KEY
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: OBID-FILL-FF ( -- )                            \ 32 bytes no interned obligation can hash to
   0 begin dup CK-BYTES < while
      dup {: k:n :}  $FF OBID-WBUF k + c!  1+
   repeat drop ;

: OBID-CKEY-UNKNOWN ( -- n )                     \ a non-registered 32-byte key -> unknown
   OBID-FILL-FF  OBID-WBUF CK-BYTES WIRE>KEY
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: OBID-CKEY-IS-SHA ( -- n )                      \ 0 iff KEY>WIRE == SHA-256(ENCODE(obligation))
   OBL-CANON {: o:obligation :}
   o INTERN {: id:CAD-KIND:obligation-id :}
   o OB-A 1024 ENCODE {: len:n :}
   OB-A len OBID-SHA SHA256
   id OBID-WBUF OBID-WCAP KEY>WIRE drop
   OBID-WBUF OBID-SHA OBLID-CK-EQ? if 0 else 1 then ;

: OBID-ID-NEG ( -- )   -1 RAW>OBLIGATION-ID ID-VALIDATE drop ;
: OBID-ID-BIG ( -- )   OBLID-N @ 100 + RAW>OBLIGATION-ID ID-VALIDATE drop ;

: DEP-CAP-FAIL ( -- )
   NEW
   0 begin dup DEP-CAP < while
      DEPX DEP+
      1+
   repeat
   drop
   DEPX DEP+ ;

\ ---- every payload variant constructs and dispatches through MATCH -------------------
\ The legs above reach the three result families only through DISCHARGE, DECODE and the
\ wire decoders. These construct the payload-carrying variant DIRECTLY through the private
\ production wrappers and match it straight back, which is what proves the named FIELDs
\ (`ev` on discharge-result's ok, `obl` on decode-result's ok, `id` on id-result's ok) bind
\ in declaration order. Each recovered payload is projected to its raw slot / registry raw
\ and compared with the value that was constructed, at TWO distinct values per family, so a
\ payload the constructor dropped, zeroed or replaced with a constant fails instead of
\ passing.
\
\ Construction is factored into one typed word per variant because the checker requires
\ MATCH's scrutinee to be a concretely instantiated family value: a single word that both
\ constructs and matches is refused, and the diagnostic names the family token as an
\ undefined word. That refusal predates this migration (it reproduces identically on the
\ legacy declaration) and is reported separately.
: TT-MK-DR-OK ( evidence -- discharge-result )   DR-OK ;
: TT-MK-DR-WS ( -- discharge-result )            DR-WSUBJECT ;
: TT-MK-DC-OK ( n -- decode-result )             DC-OK ;
: TT-MK-DC-MAL ( -- decode-result )              DC-MALFORMED ;
: TT-MK-ID-OK ( CAD-KIND:obligation-id -- id-result<CAD-KIND:obligation-id> )   IDR-OK ;
: TT-MK-ID-WW ( -- id-result<CAD-KIND:obligation-id> )   IDR-WRONG-WIDTH ;

: TT-DR-EV ( discharge-result -- n )             \ the ok evidence's slot, else -1
   MATCH discharge-result
      ok                   OF {: got:evidence :} got EV> ENDOF
      wrong-subject        OF -1 ENDOF
      wrong-domain         OF -1 ENDOF
      wrong-relation       OF -1 ENDOF
      wrong-environment    OF -1 ENDOF
      wrong-verifier-class OF -1 ENDOF
      not-independent      OF -1 ENDOF
   ;MATCH ;

: TT-DC-OBL ( decode-result -- n )               \ the ok obligation's slot, else -1
   MATCH decode-result
      ok               OF {: got:obligation :} got OBL> ENDOF
      malformed        OF -1 ENDOF
      noncanonical     OF -1 ENDOF
      bounds           OF -1 ENDOF
      duplicate        OF -1 ENDOF
      unknown-required OF -1 ENDOF
   ;MATCH ;

: TT-ID-RAW ( id-result<CAD-KIND:obligation-id> -- n )   \ the ok id's registry raw, else -1
   MATCH id-result
      ok          OF {: got:CAD-KIND:obligation-id :} got OBLIGATION-ID>RAW ENDOF
      wrong-width OF -1 ENDOF
      unknown     OF -1 ENDOF
   ;MATCH ;

: TT-DR-RT ( evidence -- n )                     \ 0 = this evidence came back unchanged
   dup EV> {: want:n :}
   TT-MK-DR-OK TT-DR-EV want = if 0 else 1 then ;
: TT-DC-RT ( n -- n )                            \ 0 = this obligation slot came back unchanged
   dup {: want:n :}
   TT-MK-DC-OK TT-DC-OBL want = if 0 else 1 then ;
: TT-ID-RT ( CAD-KIND:obligation-id -- n )       \ 0 = this id came back unchanged
   dup OBLIGATION-ID>RAW {: want:n :}
   TT-MK-ID-OK TT-ID-RAW want = if 0 else 1 then ;

: TT-DR-RT-A ( -- n )    EV-MATCH TT-DR-RT ;
: TT-DR-RT-B ( -- n )    EV-DEVICE TT-DR-RT ;
: TT-DR-AB-DIFF ( -- bool )   EV-MATCH EV> EV-DEVICE EV> <> ;   \ the two controls differ
: TT-DR-WS-EV ( -- n )   TT-MK-DR-WS TT-DR-EV ;                 \ a reject arm carries no evidence
: TT-DC-RT-A ( -- n )    OBL-CANON OBL> TT-DC-RT ;
: TT-DC-RT-B ( -- n )    OBL-OTHER OBL> TT-DC-RT ;
: TT-DC-AB-DIFF ( -- bool )   OBL-CANON OBL> OBL-OTHER OBL> <> ;
: TT-DC-MAL-OBL ( -- n ) TT-MK-DC-MAL TT-DC-OBL ;
: TT-ID-RT-A ( -- n )    OBL-CANON INTERN TT-ID-RT ;
: TT-ID-RT-B ( -- n )    OBL-OTHER INTERN TT-ID-RT ;
: TT-ID-AB-DIFF ( -- bool )
   OBL-CANON INTERN OBLIGATION-ID>RAW  OBL-OTHER INTERN OBLIGATION-ID>RAW  <> ;
: TT-ID-WW-RAW ( -- n )  TT-MK-ID-WW TT-ID-RAW ;

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
s" OB-MAKE-EFFECT ( n -- obligation ) OBLIG-OBLIGATION:MAKE" VCHECK -1 T=
s" OB-UNMAKE-EFFECT ( obligation -- n ) OBLIG-OBLIGATION:UNMAKE" VCHECK -1 T=
s" EV-MAKE-EFFECT ( n -- evidence ) OBLIG-EVIDENCE:MAKE" VCHECK -1 T=
s" EV-UNMAKE-EFFECT ( evidence -- n ) OBLIG-EVIDENCE:UNMAKE" VCHECK -1 T=
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

\ ---- obligation-id registry: content-addressed identity + wire codecs ----------
OBID-INTERN-EQ TTRUE                   \ equal obligations intern to ONE id
OBID-INTERN-NE TFALSE                  \ distinct obligations intern to distinct ids
OBID-WIRE-ALL 0 T=                     \ every interned id round-trips (process-local raw)
OBID-WIRE-WIDTH 2 T=                   \ a 4-byte buffer -> wrong-width
OBID-WIRE-UNKNOWN 3 T=                 \ an out-of-range raw -> unknown
OBID-CKEY-ALL 0 T=                     \ every interned id round-trips (cross-process content key)
OBID-CKEY-WIDTH 2 T=                   \ an 8-byte buffer -> wrong-width
OBID-CKEY-UNKNOWN 3 T=                 \ a non-registered 32-byte key -> unknown
OBID-CKEY-IS-SHA 0 T=                  \ KEY>WIRE == SHA-256(canonical encoding), not the raw index
' OBID-ID-NEG E-OBL-ID-RANGE TTHROWS
' OBID-ID-BIG E-OBL-ID-RANGE TTHROWS
\ static leg: obligation-id is a nominal the checker guards; the mint stays private.
s" OBID-VOK ( CAD-KIND:obligation-id -- CAD-KIND:obligation-id ) OBLIG:ID-VALIDATE" VCHECK -1 T=
s" OBID-VXA ( CAD-KIND:artifact-id -- CAD-KIND:obligation-id ) OBLIG:ID-VALIDATE" VCHECK 0 T=
s" OBID-VKW ( CAD-KIND:obligation-id ptr u8 n -- n ) OBLIG:KEY>WIRE" VCHECK -1 T=
s" OBID-VXKW ( CAD-KIND:evidence-id ptr u8 n -- n ) OBLIG:KEY>WIRE" VCHECK 0 T=
s" OBLIG:RAW>OBLIGATION-ID" 0 search-wl 0= TTRUE
s" OBLIG:OBLIGATION-ID>RAW" 0 search-wl 0= TTRUE
' DEP-CAP-FAIL E-OBL-CAP TTHROWS

\ ---- the three result families construct and dispatch through MATCH ------------------
TT-DR-AB-DIFF TTRUE        \ the two evidence controls are distinct values
TT-DR-RT-A 0 T=            \ discharge ok carries its `ev` payload through unchanged ...
TT-DR-RT-B 0 T=            \ ... at a second distinct evidence, so it is not a constant
TT-DR-WS-EV -1 T=          \ a reject arm carries no evidence
TT-DC-AB-DIFF TTRUE
TT-DC-RT-A 0 T=            \ decode ok carries its `obl` payload through unchanged ...
TT-DC-RT-B 0 T=            \ ... at a second distinct obligation
TT-DC-MAL-OBL -1 T=        \ a reject arm carries no obligation
TT-ID-AB-DIFF TTRUE
TT-ID-RT-A 0 T=            \ id-result ok carries its `id` payload through unchanged ...
TT-ID-RT-B 0 T=            \ ... at a second distinct id
TT-ID-WW-RAW -1 T=         \ a reject arm carries no id

;package

\ ---- how the three result families are DECLARED (dot habu-migrate-obligation-...) -----
\ All three keep a payload, so all three use the FULL ENUM form and stay general sums in
\ the type registry (kind 2). Nothing about them may move: this file's own legs and three
\ OTHER packages MATCH these families - maki/db/evidence-applicability.f and
\ maki/db/promotion-authority.f directly, and maki/db/commit-store.f through
\ promotion-authority's AUTHORIZED-DISCHARGE - so a drifted constructor spelling would
\ break consumers this suite never loads. The pins below read the family LIVE out of the
\ registry through the read-only accessors the checker publishes for public-signature
\ tooling (src/core/checker.f), because the ordinal decoders above are keyed by case NAME
\ and are therefore blind to a case-order change (wave C7 finding).
\
\ Identifying a family by its bare tail is NOT enough here: `id-result` alone is declared
\ by numpolicy, journal, rev, producer, evidence, suiteid, target AND obligation, so a
\ tail-only lookup silently pins another package's family. Each pin below therefore matches
\ the tail AND the constructor package its variants carry.
package OBLIG-TEST

: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

\ the three (tail, constructor package) identities this file pins. REFLECT
\ (test/checker-assert.f) does the reading, and its FAMS answers how many registered
\ families match an identity, so the `1` pinned below is the uniqueness assertion this
\ file's shared-tail hazard needs.
: DR$ ( -- ptr u8 n ptr u8 n )   s" discharge-result" s" OBLIG-DISCHARGE--RESULT" ;
: DC$ ( -- ptr u8 n ptr u8 n )   s" decode-result" s" OBLIG-DECODE--RESULT" ;
: IDR$ ( -- ptr u8 n ptr u8 n )  s" id-result" s" OBLIG-ID--RESULT" ;

public

\ Three shape twins, one per family: same arity, same cases in the same order, same named
\ payload field. They exist only so the negatives below can prove result identity is
\ NOMINAL - two identically shaped families never unify, in either direction. Public, so
\ each publishes constructors for its own positive control. The generated packages
\ (OBLIG--TEST-DR--TWIN, -DC--TWIN, -IDR--TWIN) are 20-21 bytes, inside the 32-byte
\ readable-spelling limit TF-CTOR-NAME-LIMIT (src/core/type-family.f).
ENUM dr-twin 0
   VARIANT ok FIELD ev OBLIG:evidence ;VARIANT
   VARIANT wrong-subject ;VARIANT
   VARIANT wrong-domain ;VARIANT
   VARIANT wrong-relation ;VARIANT
   VARIANT wrong-environment ;VARIANT
   VARIANT wrong-verifier-class ;VARIANT
   VARIANT not-independent ;VARIANT
;ENUM

ENUM dc-twin 0
   VARIANT ok FIELD obl OBLIG:obligation ;VARIANT
   VARIANT malformed ;VARIANT
   VARIANT noncanonical ;VARIANT
   VARIANT bounds ;VARIANT
   VARIANT duplicate ;VARIANT
   VARIANT unknown-required ;VARIANT
;ENUM

ENUM idr-twin 1
   VARIANT ok FIELD id a ;VARIANT
   VARIANT wrong-width ;VARIANT
   VARIANT unknown ;VARIANT
;ENUM

private

\ ---- live registry: discharge-result -------------------------------------------------
DR$ REFLECT:FAMS 1 T=
DR$ REFLECT:KIND TK-SUM T=         \ a payload family is a general sum ...
DR$ REFLECT:KIND TK-ENUM = 0 T=    \ ... never recorded as a payloadless enum
DR$ REFLECT:ARITY 0 T=
DR$ REFLECT:WIDTH 2 T=             \ tag + one payload cell
DR$ REFLECT:VIS 1 T=
DR$ REFLECT:VARS 7 T=
DR$ 0 REFLECT:ARM$ s" ok" T$=      \ case order fixes the tags
DR$ 1 REFLECT:ARM$ s" wrong-subject" T$=
DR$ 2 REFLECT:ARM$ s" wrong-domain" T$=
DR$ 3 REFLECT:ARM$ s" wrong-relation" T$=
DR$ 4 REFLECT:ARM$ s" wrong-environment" T$=
DR$ 5 REFLECT:ARM$ s" wrong-verifier-class" T$=
DR$ 6 REFLECT:ARM$ s" not-independent" T$=
DR$ 0 REFLECT:ARM-CTOR$ s" OBLIG-DISCHARGE--RESULT" T$=
DR$ 6 REFLECT:ARM-CTOR$ s" OBLIG-DISCHARGE--RESULT" T$=

\ ---- live registry: decode-result ----------------------------------------------------
DC$ REFLECT:FAMS 1 T=
DC$ REFLECT:KIND TK-SUM T=
DC$ REFLECT:ARITY 0 T=
DC$ REFLECT:WIDTH 2 T=
DC$ REFLECT:VIS 1 T=
DC$ REFLECT:VARS 6 T=
DC$ 0 REFLECT:ARM$ s" ok" T$=
DC$ 1 REFLECT:ARM$ s" malformed" T$=
DC$ 2 REFLECT:ARM$ s" noncanonical" T$=
DC$ 3 REFLECT:ARM$ s" bounds" T$=
DC$ 4 REFLECT:ARM$ s" duplicate" T$=
DC$ 5 REFLECT:ARM$ s" unknown-required" T$=
DC$ 0 REFLECT:ARM-CTOR$ s" OBLIG-DECODE--RESULT" T$=
DC$ 5 REFLECT:ARM-CTOR$ s" OBLIG-DECODE--RESULT" T$=

\ ---- live registry: id-result (the tail eight packages share) ------------------------
IDR$ REFLECT:FAMS 1 T=
IDR$ REFLECT:KIND TK-SUM T=
IDR$ REFLECT:ARITY 1 T=            \ the one type parameter the id rides in
IDR$ REFLECT:WIDTH 2 T=
IDR$ REFLECT:VIS 1 T=
IDR$ REFLECT:VARS 3 T=
IDR$ 0 REFLECT:ARM$ s" ok" T$=
IDR$ 1 REFLECT:ARM$ s" wrong-width" T$=
IDR$ 2 REFLECT:ARM$ s" unknown" T$=
IDR$ 0 REFLECT:ARM-CTOR$ s" OBLIG-ID--RESULT" T$=
IDR$ 2 REFLECT:ARM-CTOR$ s" OBLIG-ID--RESULT" T$=

\ ---- generated constructors: exact spelling + exact effect ---------------------------
\ The SPELLING is load-bearing: the checker answers 1 (uncheckable) for a name it cannot
\ resolve, and YES demands -1, so a -1 means the checker resolved EXACTLY this constructor
\ name; NO demands 0, which it can only reach after resolving the name and refusing the
\ types.
s" DR-C-OK ( OBLIG:evidence -- OBLIG:discharge-result ) OBLIG-DISCHARGE--RESULT:OK" YES
s" DR-C-WS ( -- OBLIG:discharge-result ) OBLIG-DISCHARGE--RESULT:WRONG-SUBJECT" YES
s" DR-C-WD ( -- OBLIG:discharge-result ) OBLIG-DISCHARGE--RESULT:WRONG-DOMAIN" YES
s" DR-C-WR ( -- OBLIG:discharge-result ) OBLIG-DISCHARGE--RESULT:WRONG-RELATION" YES
s" DR-C-WE ( -- OBLIG:discharge-result ) OBLIG-DISCHARGE--RESULT:WRONG-ENVIRONMENT" YES
s" DR-C-WV ( -- OBLIG:discharge-result ) OBLIG-DISCHARGE--RESULT:WRONG-VERIFIER-CLASS" YES
s" DR-C-NI ( -- OBLIG:discharge-result ) OBLIG-DISCHARGE--RESULT:NOT-INDEPENDENT" YES
s" DC-C-OK ( OBLIG:obligation -- OBLIG:decode-result ) OBLIG-DECODE--RESULT:OK" YES
s" DC-C-MAL ( -- OBLIG:decode-result ) OBLIG-DECODE--RESULT:MALFORMED" YES
s" DC-C-NC ( -- OBLIG:decode-result ) OBLIG-DECODE--RESULT:NONCANONICAL" YES
s" DC-C-BND ( -- OBLIG:decode-result ) OBLIG-DECODE--RESULT:BOUNDS" YES
s" DC-C-DUP ( -- OBLIG:decode-result ) OBLIG-DECODE--RESULT:DUPLICATE" YES
s" DC-C-UNK ( -- OBLIG:decode-result ) OBLIG-DECODE--RESULT:UNKNOWN-REQUIRED" YES
s" ID-C-OK ( CAD-KIND:obligation-id -- OBLIG:id-result<CAD-KIND:obligation-id> ) OBLIG-ID--RESULT:OK" YES
s" ID-C-WW ( -- OBLIG:id-result<CAD-KIND:obligation-id> ) OBLIG-ID--RESULT:WRONG-WIDTH" YES
s" ID-C-UNK ( -- OBLIG:id-result<CAD-KIND:obligation-id> ) OBLIG-ID--RESULT:UNKNOWN" YES
\ Forge negatives. The payload is mandatory and is not a bare scalar, a reject arm takes no
\ payload, and a raw cell cannot fill a payload slot. DR-F-SIB and DC-F-SIB are the sharp
\ ones: obligation and evidence are same-width sibling structures in this very package, so
\ each ok arm must refuse the other's role.
s" DR-F-NONE ( -- OBLIG:discharge-result ) OBLIG-DISCHARGE--RESULT:OK" NO
s" DR-F-BARE ( OBLIG:evidence -- n ) OBLIG-DISCHARGE--RESULT:OK" NO
s" DR-F-RAW ( n -- OBLIG:discharge-result ) OBLIG-DISCHARGE--RESULT:OK" NO
s" DR-F-SIB ( OBLIG:obligation -- OBLIG:discharge-result ) OBLIG-DISCHARGE--RESULT:OK" NO
s" DR-F-PAY ( OBLIG:evidence -- OBLIG:discharge-result ) OBLIG-DISCHARGE--RESULT:WRONG-SUBJECT" NO
s" DC-F-NONE ( -- OBLIG:decode-result ) OBLIG-DECODE--RESULT:OK" NO
s" DC-F-RAW ( n -- OBLIG:decode-result ) OBLIG-DECODE--RESULT:OK" NO
s" DC-F-SIB ( OBLIG:evidence -- OBLIG:decode-result ) OBLIG-DECODE--RESULT:OK" NO
s" ID-F-RAW ( n -- OBLIG:id-result<CAD-KIND:obligation-id> ) OBLIG-ID--RESULT:OK" NO
s" ID-F-BARE ( CAD-KIND:obligation-id -- n ) OBLIG-ID--RESULT:OK" NO
s" ID-F-NONE ( -- OBLIG:id-result<CAD-KIND:obligation-id> ) OBLIG-ID--RESULT:OK" NO
s" ID-F-FGN ( CAD-KIND:artifact-id -- OBLIG:id-result<CAD-KIND:obligation-id> ) OBLIG-ID--RESULT:OK" NO
\ Cross-family negatives between the two production families that share this package.
s" DC-F-XFAM ( OBLIG:evidence -- OBLIG:decode-result ) OBLIG-DISCHARGE--RESULT:OK" NO
s" DR-F-XFAM ( OBLIG:obligation -- OBLIG:discharge-result ) OBLIG-DECODE--RESULT:OK" NO
\ The three shape twins: same shape, different name, no unification in either direction.
s" TW-DR ( OBLIG:evidence -- dr-twin ) OBLIG--TEST-DR--TWIN:OK" YES
s" TW-DR-X1 ( OBLIG:evidence -- dr-twin ) OBLIG-DISCHARGE--RESULT:OK" NO
s" TW-DR-X2 ( OBLIG:evidence -- OBLIG:discharge-result ) OBLIG--TEST-DR--TWIN:OK" NO
s" TW-DC ( OBLIG:obligation -- dc-twin ) OBLIG--TEST-DC--TWIN:OK" YES
s" TW-DC-X1 ( OBLIG:obligation -- dc-twin ) OBLIG-DECODE--RESULT:OK" NO
s" TW-DC-X2 ( OBLIG:obligation -- OBLIG:decode-result ) OBLIG--TEST-DC--TWIN:OK" NO
s" TW-ID ( CAD-KIND:obligation-id -- idr-twin<CAD-KIND:obligation-id> ) OBLIG--TEST-IDR--TWIN:OK" YES
s" TW-ID-X1 ( CAD-KIND:obligation-id -- idr-twin<CAD-KIND:obligation-id> ) OBLIG-ID--RESULT:OK" NO
s" TW-ID-X2 ( CAD-KIND:obligation-id -- OBLIG:id-result<CAD-KIND:obligation-id> ) OBLIG--TEST-IDR--TWIN:OK" NO

;package

T-REPORT
