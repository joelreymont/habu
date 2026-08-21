\ maki/db/diff-suite-test.f - acceptance for the DifferentialSuite artifact schema
\ (maki/db/diff-suite.f; dot habu-v2-differential-suite-2d896ced).
\
\ Proves the plan:3782-3796 acceptance, each item by named test words (all sum / structure
\ / enum values are produced and consumed INSIDE colon words, never on the interpret-mode
\ stack):
\   F-* / SAME-* / GEN-ORDER-* : ACCEPTANCE (a) - the suite digest changes for EVERY
\          semantic field (a per-field flip matrix over seed, subject, policy, comparison
\          domain, tolerance, normalization, minimizer, target, budget, generators,
\          references, properties), while two identical suites (any insertion order) hash
\          equally - the maki/db/artifact.f envelope flip-matrix precedent.
\   TOL-*  : ACCEPTANCE (b) - incompatible (comparison domain, tolerance) pairs REJECT
\          typed (tolerance-mismatch); compatible pairs SEAL ok. Composes with NPOL:dom.
\   IND-*  : ACCEPTANCE (c) - a reference cannot alias the subject under an INDEPENDENT
\          policy (reference-not-independent); the SELF-VERIFY positive control permits it,
\          and an independent-but-distinct reference is the second positive control.
\   REPLAY-* : ACCEPTANCE (d) - suite replay derives identical case ids (same fields, any
\          insertion order -> identical case-id sequence; a different seed -> different).
\   ENC-*  : the canonical envelope round-trips byte-identically (order-independent) and
\          carries the DIGEST-INTO content digest in its stored-digest tail.
\   DSVC-* : the static leg - the nominal id families are checker-guarded, so passing a
\          producer-id where a numeric-policy-id is required (and vice versa) is REJECTED,
\          each paired with a certifying control (the cad-kinds verdict-fixture pattern).
\
\ The test reopens package DIFFSUITE (a friend) to reach private widths / helpers; the
\ happy path uses the public API. Identity fixtures mint real ids through their owner
\ constructors (PRODUCER/NPOL/TARGET) - never a raw cast; names carry the DS-test prefix
\ (docs/forth.md § "fixtures use unique test-owned names").

require lib/test.f
require lib/string.f
require test/checker-assert.f      \ CHECK-QUIET-CANDIDATE! plus REFLECT, which reads the registry
require maki/db/diff-suite.f
\ The OTHER production families on these two tails. DIAG declares its own build-result and
\ decode-result, OBLIG its own decode-result (already required below), so loading DIAG here
\ puts all of them in ONE process - the only condition under which the pair-keyed identity
\ pins and the cross-package non-unification negatives below prove anything.
require maki/db/diagnostic.f
require maki/numpolicy.f
require maki/producer.f
require maki/target/target.f
require maki/db/obligation.f
require maki/db/budget-dim.f

using TFAM

package DIFFSUITE

: DST-SUITE-ROUNDTRIP ( n -- n )
   >SUITE SUITE> ;

4 constant CASE-N                 \ case-ids derived per replay sequence

create DA CKW allot               \ digest buffer A
create DB CKW allot               \ digest buffer B
create SEQ-A CASE-N CKW * allot   \ case-id sequence A
create SEQ-B CASE-N CKW * allot   \ case-id sequence B
create EA 4096 allot              \ encode buffer A
create EB 4096 allot              \ encode buffer B
create VDIAG 4096 allot           \ checker candidate diagnostic sink

\ ---- shared identities (registered once; REGISTER/REGISTER intern by content) -----
: SUBJ-A ( -- CAD-KIND:producer-id )  s" diffsuite-test/subject-habu" PRODUCER:REGISTER ;
: SUBJ-B ( -- CAD-KIND:producer-id )  s" diffsuite-test/subject-habu-alt" PRODUCER:REGISTER ;
: REF-1 ( -- CAD-KIND:producer-id )   s" diffsuite-test/ref-torch" PRODUCER:REGISTER ;
: REF-2 ( -- CAD-KIND:producer-id )   s" diffsuite-test/ref-onnx" PRODUCER:REGISTER ;

: CMP-EXACT ( -- CAD-KIND:numeric-policy-id )  NPOL-DOM:EXACT NPOL:REGISTER ;
: CMP-ULP ( -- CAD-KIND:numeric-policy-id )    NPOL-DOM:ULP NPOL:REGISTER ;
: CMP-REL ( -- CAD-KIND:numeric-policy-id )    NPOL-DOM:RELATIVE NPOL:REGISTER ;

: T1 ( -- CAD-KIND:target-id )   TARGET:SM87 ;
\ A second target NOMINAL distinct from SM87, robust in isolation AND the integrated
\ suite: target-id lives in a small SHARED 16-slot registry that maki/target/target-test.f
\ fills earlier, so a FRESH descriptor cannot be minted when integrated. arch=100 matches
\ target-test's first cap-fill descriptor, so TARGET:REGISTER RESOLVES the existing id (no
\ new slot) in the full suite and mints one only when absent (isolation). The
\ maki/db/artifact-test.f ADT-TARGET2 precedent.
: T2 ( -- CAD-KIND:target-id )
   s" diffsuite-test/target-cap100"
   TARGET:ISA-PTX 100 32 1024 49152 TARGET:CAP-PTX TARGET:DESCRIPTOR
   TARGET:REGISTER ;

\ descriptor byte strings (hashed to content keys by the suite)
: GEN-1 ( -- ptr u8 n )   s" diffsuite-test/gen-uniform" ;
: GEN-2 ( -- ptr u8 n )   s" diffsuite-test/gen-adversarial" ;
: GEN-3 ( -- ptr u8 n )   s" diffsuite-test/gen-boundary" ;
: PROP-1 ( -- ptr u8 n )  s" diffsuite-test/prop-additivity" ;
: PROP-2 ( -- ptr u8 n )  s" diffsuite-test/prop-scale-invariance" ;
: NORM-CANON ( -- ptr u8 n )  s" diffsuite-test/norm-canonical" ;
: NORM-ALT ( -- ptr u8 n )    s" diffsuite-test/norm-alt" ;
: MIN-LIN ( -- ptr u8 n )     s" diffsuite-test/min-linear" ;
: MIN-BISECT ( -- ptr u8 n )  s" diffsuite-test/min-bisect" ;

\ ---- spec: canonical field values a flip test tweaks one at a time ----------------
TYPED-VARIABLE SP-SUBJECT CAD-KIND:producer-id
TYPED-VARIABLE SP-POLICY  OBLIG:independence
TYPED-VARIABLE SP-COMPARE CAD-KIND:numeric-policy-id
TYPED-VARIABLE SP-TARGET  CAD-KIND:target-id
variable SP-SEED
variable SP-TOL
variable SP-BUDGET
variable SP-NORM-ALT              \ bool: use NORM-ALT instead of NORM-CANON
variable SP-MIN-ALT               \ bool: use MIN-BISECT instead of MIN-LIN
variable SP-GEN-EXTRA             \ bool: add GEN-3
variable SP-REF-EXTRA             \ bool: add REF-2
variable SP-PROP-EXTRA            \ bool: add PROP-2

: SPEC-RESET ( -- )
   42 SP-SEED !
   SUBJ-A SP-SUBJECT !
   OBLIG-INDEPENDENCE:INDEPENDENT SP-POLICY !
   CMP-REL SP-COMPARE !  5 SP-TOL !
   T1 SP-TARGET !
   100 SP-BUDGET !
   false SP-NORM-ALT !  false SP-MIN-ALT !
   false SP-GEN-EXTRA !  false SP-REF-EXTRA !  false SP-PROP-EXTRA ! ;

: SPEC-NORM ( -- )   SP-NORM-ALT @ if NORM-ALT else NORM-CANON then NORMALIZATION ;
: SPEC-MIN ( -- )    SP-MIN-ALT @ if MIN-BISECT else MIN-LIN then MINIMIZER ;

\ populate the current suite from the spec (no SEAL).
: POPULATE ( -- )
   NEW
   SP-SEED @ SEED!
   SP-SUBJECT @ SUBJECT
   SP-POLICY @ POLICY
   SP-COMPARE @ SP-TOL @ COMPARISON
   SPEC-NORM  SPEC-MIN
   SP-TARGET @ TARGET-NEED
   BUDGET-DIM:COMPUTE-TIME SP-BUDGET @ BUDGET!
   GEN-1 GENERATOR+  GEN-2 GENERATOR+
   SP-GEN-EXTRA @ if GEN-3 GENERATOR+ then
   REF-1 REFERENCE+
   SP-REF-EXTRA @ if REF-2 REFERENCE+ then
   PROP-1 PROPERTY+
   SP-PROP-EXTRA @ if PROP-2 PROPERTY+ then ;

\ SEAL, requiring ok; the reject arms diverge (a build bug in a happy-path fixture).
: BUILD-OK ( -- suite )
   SEAL MATCH build-result
      ok OF ENDOF
      incomplete OF -777 throw ENDOF
      tolerance-mismatch OF -777 throw ENDOF
      reference-not-independent OF -777 throw ENDOF
   ;MATCH ;

\ SEAL as a code: 0 ok, 1 incomplete, 2 tolerance-mismatch, 3 reference-not-independent.
: SEAL-CODE ( -- n )
   SEAL MATCH build-result
      ok OF SUITE> drop 0 ENDOF
      incomplete OF 1 ENDOF
      tolerance-mismatch OF 2 ENDOF
      reference-not-independent OF 3 ENDOF
   ;MATCH ;

: BUILD-SPEC ( -- suite )   POPULATE BUILD-OK ;

\ ---- byte helpers -------------------------------------------------------------
: DIGEST! ( suite ptr u8 -- )   CKW DIGEST-INTO drop ;
: MEM= ( ptr u8 ptr u8 n -- bool ) {: pa:ptr pb:ptr n:n :}
   0 begin dup n < while
      dup {: k:n :}
      pa k + c@  pb k + c@  <> if drop false exit then
      1+
   repeat drop true ;
: KEY= ( ptr u8 ptr u8 -- bool )   CKW MEM= ;

\ ---- ACCEPTANCE (a): per-field digest flip matrix -----------------------------
: A-DIG ( -- )   SPEC-RESET BUILD-SPEC DA DIGEST! ;   \ canonical digest into DA
: B-DIG ( -- )   BUILD-SPEC DB DIGEST! ;              \ spec already tweaked, into DB
: FLIPS? ( -- bool )   DA DB KEY= 0= ;

: SAME-HASH ( -- bool )   A-DIG  SPEC-RESET B-DIG  DA DB KEY= ;
: F-SEED ( -- bool )      A-DIG  SPEC-RESET 99 SP-SEED ! B-DIG  FLIPS? ;
: F-SUBJECT ( -- bool )   A-DIG  SPEC-RESET SUBJ-B SP-SUBJECT ! B-DIG  FLIPS? ;
: F-POLICY ( -- bool )    A-DIG  SPEC-RESET OBLIG-INDEPENDENCE:SELF-VERIFY SP-POLICY ! B-DIG  FLIPS? ;
: F-COMPARE ( -- bool )   A-DIG  SPEC-RESET CMP-ULP SP-COMPARE ! B-DIG  FLIPS? ;
: F-TOL ( -- bool )       A-DIG  SPEC-RESET 6 SP-TOL ! B-DIG  FLIPS? ;
: F-NORM ( -- bool )      A-DIG  SPEC-RESET true SP-NORM-ALT ! B-DIG  FLIPS? ;
: F-MIN ( -- bool )       A-DIG  SPEC-RESET true SP-MIN-ALT ! B-DIG  FLIPS? ;
: F-TARGET ( -- bool )    A-DIG  SPEC-RESET T2 SP-TARGET ! B-DIG  FLIPS? ;
: F-BUDGET ( -- bool )    A-DIG  SPEC-RESET 200 SP-BUDGET ! B-DIG  FLIPS? ;
: F-GEN ( -- bool )       A-DIG  SPEC-RESET true SP-GEN-EXTRA ! B-DIG  FLIPS? ;
: F-REF ( -- bool )       A-DIG  SPEC-RESET true SP-REF-EXTRA ! B-DIG  FLIPS? ;
: F-PROP ( -- bool )      A-DIG  SPEC-RESET true SP-PROP-EXTRA ! B-DIG  FLIPS? ;

\ generator insertion order must not change the digest (canonical set)
: BUILD-GEN-ORDER ( bool -- suite ) {: rev:bool :}
   SPEC-RESET
   NEW  SP-SEED @ SEED!  SP-SUBJECT @ SUBJECT  SP-POLICY @ POLICY
   SP-COMPARE @ SP-TOL @ COMPARISON  SPEC-NORM SPEC-MIN  SP-TARGET @ TARGET-NEED
   BUDGET-DIM:COMPUTE-TIME SP-BUDGET @ BUDGET!
   rev if GEN-2 GENERATOR+ GEN-1 GENERATOR+ else GEN-1 GENERATOR+ GEN-2 GENERATOR+ then
   REF-1 REFERENCE+  PROP-1 PROPERTY+
   BUILD-OK ;
: GEN-ORDER-SAME ( -- bool )
   false BUILD-GEN-ORDER DA DIGEST!
   true  BUILD-GEN-ORDER DB DIGEST!
   DA DB KEY= ;

\ ---- ACCEPTANCE (b): incompatible tolerance/domain reject typed ---------------
: TOL-EXACT-OK ( -- n )   SPEC-RESET CMP-EXACT SP-COMPARE ! 0 SP-TOL ! POPULATE SEAL-CODE ;
: TOL-EXACT-BAD ( -- n )  SPEC-RESET CMP-EXACT SP-COMPARE ! 3 SP-TOL ! POPULATE SEAL-CODE ;
: TOL-ULP-BAD ( -- n )    SPEC-RESET CMP-ULP   SP-COMPARE ! 0 SP-TOL ! POPULATE SEAL-CODE ;
: TOL-ULP-OK ( -- n )     SPEC-RESET CMP-ULP   SP-COMPARE ! 1 SP-TOL ! POPULATE SEAL-CODE ;
: TOL-REL-BAD ( -- n )    SPEC-RESET CMP-REL   SP-COMPARE ! 0 SP-TOL ! POPULATE SEAL-CODE ;
: TOL-REL-OK ( -- n )     SPEC-RESET CMP-REL   SP-COMPARE ! 5 SP-TOL ! POPULATE SEAL-CODE ;

\ ---- ACCEPTANCE (c): reference cannot alias subject under independence ---------
\ subject and reference are BOTH SUBJ-A; the policy is the parameter.
: POP-ALIAS ( OBLIG:independence -- ) {: pol:OBLIG:independence :}
   NEW 42 SEED! SUBJ-A SUBJECT pol POLICY
   CMP-REL 5 COMPARISON  NORM-CANON NORMALIZATION  MIN-LIN MINIMIZER  T1 TARGET-NEED
   BUDGET-DIM:COMPUTE-TIME 100 BUDGET!
   GEN-1 GENERATOR+ GEN-2 GENERATOR+  SUBJ-A REFERENCE+  PROP-1 PROPERTY+ ;
: IND-INDEP-ALIAS ( -- n )    OBLIG-INDEPENDENCE:INDEPENDENT POP-ALIAS SEAL-CODE ;
: IND-SELF-ALIAS ( -- n )     OBLIG-INDEPENDENCE:SELF-VERIFY POP-ALIAS SEAL-CODE ;
: IND-INDEP-DISTINCT ( -- n ) SPEC-RESET POPULATE SEAL-CODE ;   \ subject SUBJ-A, ref REF-1

\ ---- ACCEPTANCE (d): deterministic case-id replay ------------------------------
: CASE-SEQ ( suite ptr u8 -- ) {: h:suite out:ptr :}
   0 begin dup CASE-N < while
      dup {: k:n :}
      h k  out k CKW * +  CASE-ID
      1+
   repeat drop ;
: SEQ= ( ptr u8 ptr u8 -- bool )   CASE-N CKW * MEM= ;

: REPLAY-SAME ( -- bool )
   SPEC-RESET BUILD-SPEC SEQ-A CASE-SEQ
   SPEC-RESET BUILD-SPEC SEQ-B CASE-SEQ
   SEQ-A SEQ-B SEQ= ;
: REPLAY-SEED-DIFF ( -- bool )
   SPEC-RESET BUILD-SPEC SEQ-A CASE-SEQ
   SPEC-RESET 7 SP-SEED ! BUILD-SPEC SEQ-B CASE-SEQ
   SEQ-A SEQ-B SEQ= 0= ;
: REPLAY-ORDER ( -- bool )        \ generator insertion order does not change the sequence
   false BUILD-GEN-ORDER SEQ-A CASE-SEQ
   true  BUILD-GEN-ORDER SEQ-B CASE-SEQ
   SEQ-A SEQ-B SEQ= ;

\ ---- canonical envelope --------------------------------------------------------
: ENC-EQ-BYTES ( -- bool )        \ two identical suites encode byte-identically
   SPEC-RESET BUILD-SPEC EA 4096 ENCODE {: la:n :}
   SPEC-RESET BUILD-SPEC EB 4096 ENCODE {: lb:n :}
   la lb <> if false exit then
   EA EB la MEM= ;
: ENC-ORDER ( -- bool )           \ generator order does not change the canonical bytes
   false BUILD-GEN-ORDER EA 4096 ENCODE {: la:n :}
   true  BUILD-GEN-ORDER EB 4096 ENCODE {: lb:n :}
   la lb <> if false exit then
   EA EB la MEM= ;
: ENC-DIGEST-TAIL ( -- bool )     \ the encoded stored-digest tail equals DIGEST-INTO
   SPEC-RESET BUILD-SPEC {: h:suite :}
   h EA 4096 ENCODE {: la:n :}
   h DB DIGEST!
   EA la CKW - +  DA  CKW BYTE-COPY    \ copy the stored-digest tail into DA
   DA DB KEY= ;

\ ---- structured DECODE (folded inverse of ENCODE) ------------------------------
: DECODE-CODE ( ptr u8 n -- n )   \ 0 ok, 1 malformed, 2 noncanonical, 3 bounds, 4 unknown
   DECODE MATCH decode-result
      ok OF SUITE> drop 0 ENDOF
      malformed OF 1 ENDOF
      noncanonical OF 2 ENDOF
      bounds OF 3 ENDOF
      unknown OF 4 ENDOF
   ;MATCH ;
\ decode, write the decoded suite's digest into out, and report whether decode was ok.
: DEC-DIGEST-OK? ( ptr u8 n ptr u8 -- bool ) {: a:ptr u:n out:ptr :}
   a u DECODE MATCH decode-result
      ok OF out DIGEST! true ENDOF
      malformed OF false ENDOF
      noncanonical OF false ENDOF
      bounds OF false ENDOF
      unknown OF false ENDOF
   ;MATCH ;
: DEC-RT-DIGEST? ( -- bool )      \ ENCODE|>DECODE round-trips to a suite of identical digest
   SPEC-RESET BUILD-SPEC {: h:suite :}
   h DA DIGEST!
   h EA 4096 ENCODE {: la:n :}
   EA la DB DEC-DIGEST-OK? 0= if false exit then
   DA DB KEY= ;
: DEC-RT-OK ( -- n )      SPEC-RESET BUILD-SPEC EA 4096 ENCODE {: la:n :} EA la DECODE-CODE ;
: DEC-TRUNC ( -- n )      \ a truncated envelope -> malformed
   SPEC-RESET BUILD-SPEC EA 4096 ENCODE {: la:n :}  EA la 3 - DECODE-CODE ;
: FLIP-BYTE ( ptr u8 n -- ) {: a:ptr off:n :}   a off + dup c@ 1+ $FF and swap c! ;
: DEC-TAMPER-DIGEST ( -- n )   \ flip a byte in the stored tag-13 digest tail -> noncanonical
   SPEC-RESET BUILD-SPEC EA 4096 ENCODE {: la:n :}
   EA la 1- FLIP-BYTE  EA la DECODE-CODE ;
: DEC-TAMPER-SUBJECT ( -- n )  \ corrupt the subject content key (offset 20) -> unknown
   SPEC-RESET BUILD-SPEC EA 4096 ENCODE {: la:n :}
   EA 20 FLIP-BYTE  EA la DECODE-CODE ;   \ [seed 14B][subj hdr 6B] -> subject key at 20
: DEC-ORDER-INDEP ( -- bool )  \ generator insertion order -> identical decoded digest
   false BUILD-GEN-ORDER EA 4096 ENCODE {: la:n :}
   true  BUILD-GEN-ORDER EB 4096 ENCODE {: lb:n :}
   EA la DA DEC-DIGEST-OK? 0= if false exit then
   EB lb DB DEC-DIGEST-OK? 0= if false exit then
   DA DB KEY= ;

\ ---- field read-back -----------------------------------------------------------
: RB-SEED ( -- n )    SPEC-RESET BUILD-SPEC SEED@ ;
: RB-TOL ( -- n )     SPEC-RESET BUILD-SPEC TOLERANCE@ ;
: RB-DOM ( -- bool )  SPEC-RESET BUILD-SPEC COMPARE-DOM@ NPOL-DOM:RELATIVE NPOL-DOM:EQ ;
: RB-POL ( -- bool )  SPEC-RESET BUILD-SPEC POLICY@ OBLIG-INDEPENDENCE:INDEPENDENT OBLIG-INDEPENDENCE:EQ ;
: RB-BUDGET ( -- n )  SPEC-RESET BUILD-SPEC BUDGET-DIM:COMPUTE-TIME BUDGET@ ;
: RB-GENN ( -- n )    SPEC-RESET BUILD-SPEC GEN-COUNT ;
: RB-REFN ( -- n )    SPEC-RESET BUILD-SPEC REF-COUNT ;

\ ---- incomplete gate -----------------------------------------------------------
: SEAL-EMPTY ( -- n )   NEW SEAL-CODE ;         \ nothing set -> incomplete

\ ---- static leg (the nominal id families are checker-guarded) ------------------
: VCHECK ( ptr u8 n -- n )
   VDIAG 4096 DIAG-BUFFER!
   CHECK-CANDIDATE!
   DIAG-BUFFER-OFF ;

\ ---- both outcome families: construct each arm directly and dispatch it back -----
\ SEAL and DECODE reach these arms only through a real build or a real byte decode. These
\ construct each arm DIRECTLY through the generated constructors the BR-* / DR-* wrappers
\ compile against, and match it straight back, so each named `suite` FIELD is proven to
\ bind in declaration order. Construction and matching are separate words because the
\ checker requires MATCH's scrutinee to be a concretely instantiated family value.
: MK-BR-OK ( suite -- build-result )   DIFFSUITE-BUILD--RESULT:OK ;
: MK-BR-INC ( -- build-result )        DIFFSUITE-BUILD--RESULT:INCOMPLETE ;
: MK-BR-TOL ( -- build-result )        DIFFSUITE-BUILD--RESULT:TOLERANCE-MISMATCH ;
: MK-BR-NI ( -- build-result )         DIFFSUITE-BUILD--RESULT:REFERENCE-NOT-INDEPENDENT ;
: MK-DR-OK ( suite -- decode-result )  DIFFSUITE-DECODE--RESULT:OK ;
: MK-DR-MF ( -- decode-result )        DIFFSUITE-DECODE--RESULT:MALFORMED ;
: MK-DR-NC ( -- decode-result )        DIFFSUITE-DECODE--RESULT:NONCANONICAL ;
: MK-DR-BD ( -- decode-result )        DIFFSUITE-DECODE--RESULT:BOUNDS ;
: MK-DR-UNK ( -- decode-result )       DIFFSUITE-DECODE--RESULT:UNKNOWN ;

: BR-ARM ( build-result -- n )     \ 1 ok / 2 incomplete / 3 tolerance / 4 not-independent
   MATCH build-result
      ok OF drop 1 ENDOF
      incomplete OF 2 ENDOF
      tolerance-mismatch OF 3 ENDOF
      reference-not-independent OF 4 ENDOF
   ;MATCH ;
: DR-ARM ( decode-result -- n )    \ 1 ok / 2 malformed / 3 noncanonical / 4 bounds / 5 unknown
   MATCH decode-result
      ok OF drop 1 ENDOF
      malformed OF 2 ENDOF
      noncanonical OF 3 ENDOF
      bounds OF 4 ENDOF
      unknown OF 5 ENDOF
   ;MATCH ;

\ The carried suite is compared by DIGEST, not by its pool slot: the digest is SHA-256 over
\ the whole canonical semantic prefix, so a payload the constructor dropped, zeroed or
\ exchanged cannot reproduce it. The slot is ALSO pinned non-zero, because the pool is a
\ ring whose cursor starts at 0 - a zeroed payload reads back as slot 0, and a comparison
\ riding slot 0 would pass on exactly the bug these pins exist to catch.
: BR-OK-SLOT ( build-result -- n )        \ the carried suite's pool slot, else -1
   MATCH build-result
      ok OF SUITE> ENDOF
      incomplete OF -1 ENDOF
      tolerance-mismatch OF -1 ENDOF
      reference-not-independent OF -1 ENDOF
   ;MATCH ;
: DR-OK-SLOT ( decode-result -- n )
   MATCH decode-result
      ok OF SUITE> ENDOF
      malformed OF -1 ENDOF
      noncanonical OF -1 ENDOF
      bounds OF -1 ENDOF
      unknown OF -1 ENDOF
   ;MATCH ;
: BR-OK-DIG? ( build-result ptr u8 -- bool )   {: out:ptr :}   \ digest of the carried suite
   MATCH build-result
      ok OF out DIGEST! true ENDOF
      incomplete OF false ENDOF
      tolerance-mismatch OF false ENDOF
      reference-not-independent OF false ENDOF
   ;MATCH ;
: DR-OK-DIG? ( decode-result ptr u8 -- bool )  {: out:ptr :}
   MATCH decode-result
      ok OF out DIGEST! true ENDOF
      malformed OF false ENDOF
      noncanonical OF false ENDOF
      bounds OF false ENDOF
      unknown OF false ENDOF
   ;MATCH ;

: RT-BR-ARM ( -- n )   SPEC-RESET BUILD-SPEC MK-BR-OK BR-ARM ;
: RT-BR-INC ( -- n )   MK-BR-INC BR-ARM ;
: RT-BR-TOL ( -- n )   MK-BR-TOL BR-ARM ;
: RT-BR-NI ( -- n )    MK-BR-NI BR-ARM ;
: RT-DR-ARM ( -- n )   SPEC-RESET BUILD-SPEC MK-DR-OK DR-ARM ;
: RT-DR-MF ( -- n )    MK-DR-MF DR-ARM ;
: RT-DR-NC ( -- n )    MK-DR-NC DR-ARM ;
: RT-DR-BD ( -- n )    MK-DR-BD DR-ARM ;
: RT-DR-UNK ( -- n )   MK-DR-UNK DR-ARM ;
: RT-BR-INC-SLOT ( -- n )   MK-BR-INC BR-OK-SLOT ;   \ the payloadless arms stay live
: RT-DR-MF-SLOT ( -- n )    MK-DR-MF DR-OK-SLOT ;

\ A result value is constructed twice rather than held in a typed local: typed locals do
\ not accept a sum family type. That is not this migration's doing - it rejects identically
\ for DIAG:build-result, which moved to the same full ENUM form in an earlier wave - and it
\ is fail-closed, so it cannot hide anything. The payload INSIDE a MATCH arm is what binds
\ to a typed local, which is what these projections do.
: RT-BR-CARRIES? ( -- bool )      \ ok hands back a suite of the SAME digest, on a live slot
   SPEC-RESET BUILD-SPEC {: h:suite :}
   h DA DIGEST!
   h MK-BR-OK BR-OK-SLOT 0 <>
   h MK-BR-OK DB BR-OK-DIG? and
   DA DB KEY= and ;
: RT-DR-CARRIES? ( -- bool )
   SPEC-RESET BUILD-SPEC {: h:suite :}
   h DA DIGEST!
   h MK-DR-OK DR-OK-SLOT 0 <>
   h MK-DR-OK DB DR-OK-DIG? and
   DA DB KEY= and ;
: RT-BR-DISTINCT? ( -- bool )     \ a DIFFERENT suite comes back different, so the pin is live
   SPEC-RESET BUILD-SPEC DA DIGEST!
   SPEC-RESET CMP-ULP SP-COMPARE ! 1 SP-TOL ! BUILD-SPEC {: h2:suite :}
   h2 MK-BR-OK DB BR-OK-DIG? if DA DB KEY= 0= else false then ;

\ Every row below runs AFTER T-RESET: that call zeroes the FAILURE counter as well as the
\ case counter, so an assertion placed above it is reported by T-REPORT as if it had never
\ run - a red row there would leave the suite green.
: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;
: UNRESOLVED ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! 1 T= ;

\ The families answering to these two tails. Identity is the tail PLUS the constructor
\ package its variants carry; the bare tails name none of them in particular.
: BR$ ( -- ptr u8 n ptr u8 n )        s" build-result" s" DIFFSUITE-BUILD--RESULT" ;
: DR$ ( -- ptr u8 n ptr u8 n )        s" decode-result" s" DIFFSUITE-DECODE--RESULT" ;
: DIAG-BR$ ( -- ptr u8 n ptr u8 n )   s" build-result" s" DIAG-BUILD--RESULT" ;
: DIAG-DR$ ( -- ptr u8 n ptr u8 n )   s" decode-result" s" DIAG-DECODE--RESULT" ;
: OBL-DR$ ( -- ptr u8 n ptr u8 n )    s" decode-result" s" OBLIG-DECODE--RESULT" ;

T-RESET

\ ---- generated handle contract --------------------------------------------------
37 DST-SUITE-ROUNDTRIP 37 T=
s" DST-MAKE ( n -- suite ) DIFFSUITE-SUITE:MAKE" VCHECK -1 T=
s" DST-UNMAKE ( suite -- n ) DIFFSUITE-SUITE:UNMAKE" VCHECK -1 T=
s" DST-FORGE ( bool -- suite ) DIFFSUITE-SUITE:MAKE" VCHECK 0 T=

\ ---- ACCEPTANCE (a): every semantic field flips the digest ---------------------
SAME-HASH TTRUE          \ two identical suites hash equally (baseline)
F-SEED TTRUE
F-SUBJECT TTRUE
F-POLICY TTRUE
F-COMPARE TTRUE
F-TOL TTRUE
F-NORM TTRUE
F-MIN TTRUE
F-TARGET TTRUE
F-BUDGET TTRUE
F-GEN TTRUE
F-REF TTRUE
F-PROP TTRUE
GEN-ORDER-SAME TTRUE     \ insertion order of the generator set does not change the digest

\ ---- ACCEPTANCE (b): incompatible tolerance/domain reject typed ----------------
TOL-EXACT-OK 0 T=        \ exact + zero tolerance -> ok
TOL-EXACT-BAD 2 T=       \ exact + nonzero tolerance -> tolerance-mismatch
TOL-ULP-BAD 2 T=         \ ulp + zero tolerance -> tolerance-mismatch
TOL-ULP-OK 0 T=          \ ulp + positive tolerance -> ok
TOL-REL-BAD 2 T=         \ relative + zero tolerance -> tolerance-mismatch
TOL-REL-OK 0 T=          \ relative + positive tolerance -> ok

\ ---- ACCEPTANCE (c): reference cannot alias subject under independence ----------
IND-INDEP-ALIAS 3 T=     \ independent + reference==subject -> reference-not-independent
IND-SELF-ALIAS 0 T=      \ self-verify policy permits reference==subject (positive control)
IND-INDEP-DISTINCT 0 T=  \ independent + distinct reference -> ok (positive control)

\ ---- ACCEPTANCE (d): deterministic case-id replay ------------------------------
REPLAY-SAME TTRUE        \ same suite -> identical case-id sequence
REPLAY-SEED-DIFF TTRUE   \ different seed -> different case-id sequence
REPLAY-ORDER TTRUE       \ generator insertion order -> identical case-id sequence

\ ---- canonical envelope --------------------------------------------------------
ENC-EQ-BYTES TTRUE
ENC-ORDER TTRUE
ENC-DIGEST-TAIL TTRUE

\ ---- structured DECODE ---------------------------------------------------------
DEC-RT-DIGEST? TTRUE      \ ENCODE|>DECODE reconstructs a suite of identical digest
DEC-RT-OK 0 T=           \ a well-formed envelope decodes ok
DEC-ORDER-INDEP TTRUE    \ generator insertion order does not change the decoded digest
DEC-TRUNC 1 T=           \ a truncated envelope -> malformed
DEC-TAMPER-DIGEST 2 T=   \ a tampered stored-digest tail -> noncanonical
DEC-TAMPER-SUBJECT 4 T=  \ an unresolvable subject content key -> unknown

\ ---- field read-back + incomplete ----------------------------------------------
RB-SEED 42 T=
RB-TOL 5 T=
RB-DOM TTRUE
RB-POL TTRUE
RB-BUDGET 100 T=
RB-GENN 2 T=
RB-REFN 1 T=
SEAL-EMPTY 1 T=

\ ---- static leg: nominal id families are checker-guarded -----------------------
\ COMPARISON requires a numeric-policy-id; a producer-id is rejected, control certifies.
s" DSVC1 ( CAD-KIND:numeric-policy-id n -- ) DIFFSUITE:COMPARISON" VCHECK -1 T=
s" DSVC2 ( CAD-KIND:producer-id n -- ) DIFFSUITE:COMPARISON" VCHECK 0 T=
\ SUBJECT requires a producer-id; a numeric-policy-id is rejected, control certifies.
s" DSVC3 ( CAD-KIND:producer-id -- ) DIFFSUITE:SUBJECT" VCHECK -1 T=
s" DSVC4 ( CAD-KIND:numeric-policy-id -- ) DIFFSUITE:SUBJECT" VCHECK 0 T=
\ TARGET-NEED requires a target-id; a producer-id is rejected, control certifies.
s" DSVC5 ( CAD-KIND:target-id -- ) DIFFSUITE:TARGET-NEED" VCHECK -1 T=
s" DSVC6 ( CAD-KIND:producer-id -- ) DIFFSUITE:TARGET-NEED" VCHECK 0 T=

\ ---- both outcome families construct and dispatch through MATCH -----------------
RT-BR-ARM 1 T=              \ a constructed ok reaches the ok arm
RT-BR-INC 2 T=
RT-BR-TOL 3 T=
RT-BR-NI 4 T=
RT-DR-ARM 1 T=
RT-DR-MF 2 T=
RT-DR-NC 3 T=
RT-DR-BD 4 T=
RT-DR-UNK 5 T=
RT-BR-INC-SLOT -1 T=        \ the payloadless arms of the slot projections are live
RT-DR-MF-SLOT -1 T=
RT-BR-CARRIES? TTRUE        \ ok carries the suite through: same digest, non-zero slot
RT-DR-CARRIES? TTRUE
RT-BR-DISTINCT? TTRUE       \ and a DIFFERENT suite reads back different

\ ---- the generated constructors: exact spelling + exact effect -------------------
\ The SPELLING is load-bearing: the checker answers 1 (uncheckable) for a name it cannot
\ resolve and -1 only for one it resolved and accepted, so a -1 means EXACTLY this
\ constructor name typechecked; a 0 means the name resolved and the TYPES were refused.
\ The 1-verdict rows below are the calibration that proves that three-way split.
s" VB-OK ( DIFFSUITE:suite -- DIFFSUITE:build-result ) DIFFSUITE-BUILD--RESULT:OK" YES
s" VB-INC ( -- DIFFSUITE:build-result ) DIFFSUITE-BUILD--RESULT:INCOMPLETE" YES
s" VB-TOL ( -- DIFFSUITE:build-result ) DIFFSUITE-BUILD--RESULT:TOLERANCE-MISMATCH" YES
s" VB-NI ( -- DIFFSUITE:build-result ) DIFFSUITE-BUILD--RESULT:REFERENCE-NOT-INDEPENDENT" YES
s" VD-OK ( DIFFSUITE:suite -- DIFFSUITE:decode-result ) DIFFSUITE-DECODE--RESULT:OK" YES
s" VD-MF ( -- DIFFSUITE:decode-result ) DIFFSUITE-DECODE--RESULT:MALFORMED" YES
s" VD-NC ( -- DIFFSUITE:decode-result ) DIFFSUITE-DECODE--RESULT:NONCANONICAL" YES
s" VD-BD ( -- DIFFSUITE:decode-result ) DIFFSUITE-DECODE--RESULT:BOUNDS" YES
s" VD-UNK ( -- DIFFSUITE:decode-result ) DIFFSUITE-DECODE--RESULT:UNKNOWN" YES
\ Forge negatives on each ok payload slot: a raw cell cannot fill it, the result is not a
\ bare scalar, and the payload is mandatory.
s" VB-RAW ( n -- DIFFSUITE:build-result ) DIFFSUITE-BUILD--RESULT:OK" NO
s" VB-BARE ( DIFFSUITE:suite -- n ) DIFFSUITE-BUILD--RESULT:OK" NO
s" VB-NONE ( -- DIFFSUITE:build-result ) DIFFSUITE-BUILD--RESULT:OK" NO
s" VD-RAW ( n -- DIFFSUITE:decode-result ) DIFFSUITE-DECODE--RESULT:OK" NO
s" VD-BARE ( DIFFSUITE:suite -- n ) DIFFSUITE-DECODE--RESULT:OK" NO
s" VD-NONE ( -- DIFFSUITE:decode-result ) DIFFSUITE-DECODE--RESULT:OK" NO
\ The two families in THIS package do not cross either, despite carrying the same suite.
s" VX-BD ( DIFFSUITE:suite -- DIFFSUITE:decode-result ) DIFFSUITE-BUILD--RESULT:OK" NO
s" VX-DB ( DIFFSUITE:suite -- DIFFSUITE:build-result ) DIFFSUITE-DECODE--RESULT:OK" NO
\ calibration: unknown case name, unknown constructor package, and a case that exists in a
\ SIBLING family on the same tail but not in this one.
s" VK-1 ( -- DIFFSUITE:build-result ) DIFFSUITE-BUILD--RESULT:NOPE" UNRESOLVED
s" VK-2 ( -- DIFFSUITE:decode-result ) DIFFSUITE-DECODE--RESULTX:MALFORMED" UNRESOLVED
s" VK-3 ( -- DIFFSUITE:decode-result ) DIFFSUITE-DECODE--RESULT:DUPLICATE" UNRESOLVED
s" VK-4 ( -- DIAG:build-result ) DIAG-BUILD--RESULT:TOLERANCE-MISMATCH" UNRESOLVED

\ ---- the shared tails: three production families, none unifying with another -----
\ This is the cross-PACKAGE negative and it needs no synthetic twin: DIAG declares real
\ build-result and decode-result families and OBLIG a real decode-result, so every row
\ below asks one production family's constructor to build another's result. Both
\ directions, on the payload arms and the payloadless arms. The YES rows above are the
\ positive controls that keep these from passing by being unresolvable rather than
\ ill-typed.
s" VX-1 ( DIAG:diagnostic -- DIFFSUITE:build-result ) DIAG-BUILD--RESULT:OK" NO
s" VX-2 ( DIFFSUITE:suite -- DIAG:build-result ) DIFFSUITE-BUILD--RESULT:OK" NO
s" VX-3 ( DIAG:diagnostic -- DIFFSUITE:decode-result ) DIAG-DECODE--RESULT:OK" NO
s" VX-4 ( DIFFSUITE:suite -- DIAG:decode-result ) DIFFSUITE-DECODE--RESULT:OK" NO
s" VX-5 ( OBLIG:obligation -- DIFFSUITE:decode-result ) OBLIG-DECODE--RESULT:OK" NO
s" VX-6 ( DIFFSUITE:suite -- OBLIG:decode-result ) DIFFSUITE-DECODE--RESULT:OK" NO
s" VX-7 ( -- DIFFSUITE:decode-result ) DIAG-DECODE--RESULT:MALFORMED" NO
s" VX-8 ( -- OBLIG:decode-result ) DIFFSUITE-DECODE--RESULT:MALFORMED" NO

\ ---- the recorded declaration shape, pair-keyed ---------------------------------
\ The MATCH projections dispatch on case NAME, so they are blind to a case-order change,
\ so these pins are the case-order and payload-slot detector either family has. All five
\ families on these two tails are
\ registered in this process, so FAMS = 1 per identity is a real uniqueness assertion.
BR$ REFLECT:FAMS 1 T=
BR$ REFLECT:KIND TK-SUM T=              \ a payload family stays a general sum ...
BR$ REFLECT:KIND TK-ENUM = 0 T=         \ ... and is NOT recorded as a compact enum
BR$ REFLECT:ARITY 0 T=
BR$ REFLECT:WIDTH 2 T=                  \ one payload cell plus one tag cell
BR$ REFLECT:VIS 1 T=
BR$ REFLECT:VARS 4 T=
BR$ 0 REFLECT:ARM$ s" ok" T$=           \ case order fixes the tags
BR$ 1 REFLECT:ARM$ s" incomplete" T$=
BR$ 2 REFLECT:ARM$ s" tolerance-mismatch" T$=
BR$ 3 REFLECT:ARM$ s" reference-not-independent" T$=
BR$ 0 REFLECT:ARM-CTOR$ s" DIFFSUITE-BUILD--RESULT" T$=
BR$ 3 REFLECT:ARM-CTOR$ s" DIFFSUITE-BUILD--RESULT" T$=
BR$ 0 REFLECT:ARM-FLDS 1 T=             \ exactly one named cell on ok, none elsewhere
BR$ 1 REFLECT:ARM-FLDS 0 T=
BR$ 2 REFLECT:ARM-FLDS 0 T=
BR$ 3 REFLECT:ARM-FLDS 0 T=
BR$ 0 s" suite" REFLECT:ARM-SLOT 0 T=   \ the payload is named `suite` at payload slot 0
BR$ 0 s" suite" REFLECT:ARM-CELLS 1 T=
BR$ 1 s" suite" REFLECT:ARM-SLOT -1 T=  \ the name is per-arm
BR$ 0 s" slot" REFLECT:ARM-SLOT -1 T=   \ `slot` is the carried STRUCTURE's field, not this one

DR$ REFLECT:FAMS 1 T=
DR$ REFLECT:KIND TK-SUM T=
DR$ REFLECT:ARITY 0 T=
DR$ REFLECT:WIDTH 2 T=
DR$ REFLECT:VIS 1 T=
DR$ REFLECT:VARS 5 T=
DR$ 0 REFLECT:ARM$ s" ok" T$=
DR$ 1 REFLECT:ARM$ s" malformed" T$=
DR$ 2 REFLECT:ARM$ s" noncanonical" T$=
DR$ 3 REFLECT:ARM$ s" bounds" T$=
DR$ 4 REFLECT:ARM$ s" unknown" T$=
DR$ 0 REFLECT:ARM-CTOR$ s" DIFFSUITE-DECODE--RESULT" T$=
DR$ 4 REFLECT:ARM-CTOR$ s" DIFFSUITE-DECODE--RESULT" T$=
DR$ 0 REFLECT:ARM-FLDS 1 T=
DR$ 1 REFLECT:ARM-FLDS 0 T=
DR$ 0 s" suite" REFLECT:ARM-SLOT 0 T=
DR$ 0 s" suite" REFLECT:ARM-CELLS 1 T=

\ Same tails, other constructor packages, other families. On `build-result` the case count
\ separates DIFFSUITE from DIAG; on `decode-result` DIAG and OBLIG BOTH have six cases, so
\ the count does not separate those two at all - the payload FIELD NAME does, and each
\ family owns exactly one of `suite`, `diag`, `obl`. None of these could hold if the
\ pair-keying collapsed to the bare tail.
DIAG-BR$ REFLECT:FAMS 1 T=
DIAG-BR$ REFLECT:VARS 3 T=              \ against DIFFSUITE's four
DIAG-DR$ REFLECT:FAMS 1 T=
DIAG-DR$ REFLECT:VARS 6 T=              \ against DIFFSUITE's five
OBL-DR$ REFLECT:FAMS 1 T=
OBL-DR$ REFLECT:VARS 6 T=               \ the same six as DIAG's: the count cannot separate them
DIAG-DR$ 0 s" diag" REFLECT:ARM-SLOT 0 T=      \ so the field name does
OBL-DR$ 0 s" obl" REFLECT:ARM-SLOT 0 T=
DIAG-DR$ 0 s" obl" REFLECT:ARM-SLOT -1 T=
OBL-DR$ 0 s" diag" REFLECT:ARM-SLOT -1 T=
DIAG-DR$ 0 s" suite" REFLECT:ARM-SLOT -1 T=    \ and neither sibling carries a `suite`
OBL-DR$ 0 s" suite" REFLECT:ARM-SLOT -1 T=
DR$ 0 s" diag" REFLECT:ARM-SLOT -1 T=          \ nor does DIFFSUITE carry theirs
DR$ 0 s" obl" REFLECT:ARM-SLOT -1 T=
\ a tail with a constructor package no family carries resolves nothing at all.
s" decode-result" s" NOPE-DECODE--RESULT" REFLECT:FAMS 0 T=

T-REPORT

;package

;using
