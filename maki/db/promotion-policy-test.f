\ maki/db/promotion-policy-test.f - acceptance for the promotion POLICY value + digest
\ (maki/db/promotion-policy.f; dot habu-v2-evidence-promotion-f8312ebe).
\
\ Proves: equal-field policies share a content digest; a policy differing in ANY bound
\ field digests differently (the digest-binding the "policy change invalidates" rule rests
\ on); BIND yields the model / expiry / digest-words SATISFY consumes; and the generated
\ PPOLICY-SPEC:MAKE / PPOLICY-SPEC:UNMAKE pair round-trips every bound field with the
\ checked effect its declaration order implies. All structure values are produced and
\ consumed INSIDE colon words (never on the interpret-mode stack). Fixtures mint real ids
\ through their owner constructors, never a raw cast.

require lib/test.f
require lib/string.f
require maki/db/promotion-policy.f
require maki/artifact.f
require maki/target/target.f
require maki/numpolicy.f
require maki/config.f
require maki/producer.f

package PROMO-POLICY-TEST

\ ---- shared identities (test-owned names; REGISTER interns by content) ----------
: M1 ( -- CAD-KIND:artifact-id )   s" promo-pol-test/model-1" ARTIFACT:REGISTER ;
: W1 ( -- CAD-KIND:artifact-id )   s" promo-pol-test/weights-1" ARTIFACT:REGISTER ;
: RB ( -- CAD-KIND:artifact-id )   s" promo-pol-test/rollback-1" ARTIFACT:REGISTER ;
: TGT ( -- CAD-KIND:target-id )    TARGET:SM87 ;
: NUM ( -- CAD-KIND:numeric-policy-id )   NPOL-DOM:EXACT NPOL:REGISTER ;
: POP ( -- CAD-KIND:config-id )    s" promo-pol-test/pop-1" CONFIG:REGISTER ;
: VERIF ( -- CAD-KIND:producer-id ) s" promo-pol-test/verifier-diff" PRODUCER:REGISTER ;

\ A second, distinct identity for each remaining bound field, so every one of the ten
\ fields gets its own one-field-different policy below.
: M2 ( -- CAD-KIND:artifact-id )    s" promo-pol-test/model-2" ARTIFACT:REGISTER ;
: W2 ( -- CAD-KIND:artifact-id )    s" promo-pol-test/weights-2" ARTIFACT:REGISTER ;
: RB2 ( -- CAD-KIND:artifact-id )   s" promo-pol-test/rollback-2" ARTIFACT:REGISTER ;
: TGT2 ( -- CAD-KIND:target-id )    TARGET:SM121A ;
: NUM2 ( -- CAD-KIND:numeric-policy-id )  NPOL-DOM:RELATIVE NPOL:REGISTER ;
: POP2 ( -- CAD-KIND:config-id )    s" promo-pol-test/pop-2" CONFIG:REGISTER ;
: VERIF2 ( -- CAD-KIND:producer-id ) s" promo-pol-test/verifier-2" PRODUCER:REGISTER ;

\ SPEC-A / SPEC-A2 are field-identical; every SPEC-<field> below differs from SPEC-A in
\ exactly one bound field, so the digest binding is pinned field by field.
: SPEC-A ( -- PPOLICY:spec )     M1 W1 TGT NUM POP VERIF  7 100 5000 RB PPOLICY:MK ;
: SPEC-A2 ( -- PPOLICY:spec )    M1 W1 TGT NUM POP VERIF  7 100 5000 RB PPOLICY:MK ;
: SPEC-MODEL ( -- PPOLICY:spec )  M2 W1 TGT NUM POP VERIF  7 100 5000 RB PPOLICY:MK ;
: SPEC-WEIGHTS ( -- PPOLICY:spec ) M1 W2 TGT NUM POP VERIF  7 100 5000 RB PPOLICY:MK ;
: SPEC-TARGET ( -- PPOLICY:spec ) M1 W1 TGT2 NUM POP VERIF  7 100 5000 RB PPOLICY:MK ;
: SPEC-NUM ( -- PPOLICY:spec )   M1 W1 TGT NUM2 POP VERIF  7 100 5000 RB PPOLICY:MK ;
: SPEC-POP ( -- PPOLICY:spec )   M1 W1 TGT NUM POP2 VERIF  7 100 5000 RB PPOLICY:MK ;
: SPEC-VERIF ( -- PPOLICY:spec ) M1 W1 TGT NUM POP VERIF2  7 100 5000 RB PPOLICY:MK ;
: SPEC-VVER ( -- PPOLICY:spec )  M1 W1 TGT NUM POP VERIF  9 100 5000 RB PPOLICY:MK ;
: SPEC-THRESH ( -- PPOLICY:spec ) M1 W1 TGT NUM POP VERIF  7 200 5000 RB PPOLICY:MK ;
: SPEC-EXP ( -- PPOLICY:spec )   M1 W1 TGT NUM POP VERIF  7 100 6000 RB PPOLICY:MK ;
: SPEC-RB ( -- PPOLICY:spec )    M1 W1 TGT NUM POP VERIF  7 100 5000 RB2 PPOLICY:MK ;

\ ---- digest equality: equal fields == one digest; any change != ----------------
: PP-EQ-SAME ( -- bool )     SPEC-A SPEC-A2 PPOLICY:DIGEST-EQ? ;
: PP-EQ-MODEL ( -- bool )    SPEC-A SPEC-MODEL PPOLICY:DIGEST-EQ? ;
: PP-EQ-WEIGHTS ( -- bool )  SPEC-A SPEC-WEIGHTS PPOLICY:DIGEST-EQ? ;
: PP-EQ-TARGET ( -- bool )   SPEC-A SPEC-TARGET PPOLICY:DIGEST-EQ? ;
: PP-EQ-NUM ( -- bool )      SPEC-A SPEC-NUM PPOLICY:DIGEST-EQ? ;
: PP-EQ-POP ( -- bool )      SPEC-A SPEC-POP PPOLICY:DIGEST-EQ? ;
: PP-EQ-VERIF ( -- bool )    SPEC-A SPEC-VERIF PPOLICY:DIGEST-EQ? ;
: PP-EQ-VVER ( -- bool )     SPEC-A SPEC-VVER PPOLICY:DIGEST-EQ? ;
: PP-EQ-THRESH ( -- bool )   SPEC-A SPEC-THRESH PPOLICY:DIGEST-EQ? ;
: PP-EQ-EXP ( -- bool )      SPEC-A SPEC-EXP PPOLICY:DIGEST-EQ? ;
: PP-EQ-RB ( -- bool )       SPEC-A SPEC-RB PPOLICY:DIGEST-EQ? ;

\ ---- BIND: the single-cell model / expiry / digest-words SATISFY consumes -------
: PP-BIND-MODEL ( -- bool )
   SPEC-A PPOLICY:BIND {: m:CAD-KIND:artifact-id e:n d0:n d1:n d2:n d3:n :}
   M1 m ARTIFACT:EQUAL? ;
: PP-BIND-EXPIRY ( -- n )
   SPEC-A PPOLICY:BIND {: m:CAD-KIND:artifact-id e:n d0:n d1:n d2:n d3:n :}
   e ;
: PP-BIND-DIGEST ( -- bool )     \ BIND's four words == DIGEST-WORDS
   SPEC-A PPOLICY:BIND {: m:CAD-KIND:artifact-id e:n a0:n a1:n a2:n a3:n :}
   SPEC-A PPOLICY:DIGEST-WORDS {: b0:n b1:n b2:n b3:n :}
   a0 b0 = a1 b1 = and a2 b2 = and a3 b3 = and ;

\ ---- MAKE/UNMAKE round-trip: every bound field comes back, in declaration order --
\ PPOLICY:MK is the MAKE side; PPOLICY-SPEC:UNMAKE is the generated read side. Each
\ half is asserted through its owner's equality, never a raw cell comparison.
: PP-RT-IDS ( -- bool )          \ the seven identity fields survive the round-trip
   SPEC-A PPOLICY-SPEC:UNMAKE
   {: model:CAD-KIND:artifact-id weights:CAD-KIND:artifact-id target:CAD-KIND:target-id
      numeric:CAD-KIND:numeric-policy-id population:CAD-KIND:config-id
      verifier:CAD-KIND:producer-id vversion:n threshold:n expiry:n
      rollback:CAD-KIND:artifact-id :}
   M1 model ARTIFACT:EQUAL?
   W1 weights ARTIFACT:EQUAL? and
   TGT target TARGET:EQUAL? and
   numeric NPOL:POLICY-DOM NPOL-DOM:EXACT NPOL-DOM:EQ and
   POP population CONFIG:EQUAL? and
   VERIF verifier PRODUCER:EQUAL? and
   RB rollback ARTIFACT:EQUAL? and ;

: PP-RT-SCALARS ( -- bool )      \ the three scalar fields survive the round-trip
   SPEC-A PPOLICY-SPEC:UNMAKE
   {: model:CAD-KIND:artifact-id weights:CAD-KIND:artifact-id target:CAD-KIND:target-id
      numeric:CAD-KIND:numeric-policy-id population:CAD-KIND:config-id
      verifier:CAD-KIND:producer-id vversion:n threshold:n expiry:n
      rollback:CAD-KIND:artifact-id :}
   vversion 7 =  threshold 100 = and  expiry 5000 = and ;

\ ---- static leg: the generated constructor pair's checked effects ---------------
\ Drives the checker over a candidate "NAME ( effect ) body" string; the verdict is
\ -1 certified / 0 rejected (CHECK-CANDIDATE!). A negative fixture's reject diagnostic
\ is captured into VDIAG so it does not print during a passing suite.
create VDIAG 4096 allot
: VCHECK ( ptr u8 n -- n )
   VDIAG 4096 DIAG-BUFFER!
   CHECK-CANDIDATE!
   DIAG-BUFFER-OFF ;

T-RESET

PP-EQ-SAME TTRUE                       \ field-identical policies share a digest
PP-EQ-MODEL TFALSE                     \ a changed model digests differently
PP-EQ-WEIGHTS TFALSE                   \ a changed weights identity digests differently
PP-EQ-TARGET TFALSE                    \ a changed deployment target digests differently
PP-EQ-NUM TFALSE                       \ a changed numeric proof domain digests differently
PP-EQ-POP TFALSE                       \ a changed evaluation population digests differently
PP-EQ-VERIF TFALSE                     \ a changed verifier identity digests differently
PP-EQ-VVER TFALSE                      \ a changed verifier version digests differently
PP-EQ-THRESH TFALSE                    \ a changed threshold digests differently
PP-EQ-EXP TFALSE                       \ a changed expiry digests differently
PP-EQ-RB TFALSE                        \ a changed rollback artifact digests differently

PP-BIND-MODEL TTRUE                    \ BIND yields the policy's model
PP-BIND-EXPIRY 5000 T=                 \ BIND yields the policy's expiry
PP-BIND-DIGEST TTRUE                   \ BIND's digest words match DIGEST-WORDS

\ ---- the generated constructor pair --------------------------------------------
PP-RT-IDS TTRUE                        \ MAKE/UNMAKE preserves every identity field
PP-RT-SCALARS TTRUE                    \ MAKE/UNMAKE preserves every scalar field
s" PP-MAKE-EFFECT ( CAD-KIND:artifact-id CAD-KIND:artifact-id CAD-KIND:target-id CAD-KIND:numeric-policy-id CAD-KIND:config-id CAD-KIND:producer-id n n n CAD-KIND:artifact-id -- PPOLICY:spec ) PPOLICY-SPEC:MAKE" VCHECK -1 T=
s" PP-UNMAKE-EFFECT ( PPOLICY:spec -- CAD-KIND:artifact-id CAD-KIND:artifact-id CAD-KIND:target-id CAD-KIND:numeric-policy-id CAD-KIND:config-id CAD-KIND:producer-id n n n CAD-KIND:artifact-id ) PPOLICY-SPEC:UNMAKE" VCHECK -1 T=
\ A raw cell cannot stand in for an identity field, adjacent identity roles cannot
\ trade places even though both are one cell wide, and the spec never unmakes into
\ a bare scalar.
s" PP-FORGE-MODEL ( n CAD-KIND:artifact-id CAD-KIND:target-id CAD-KIND:numeric-policy-id CAD-KIND:config-id CAD-KIND:producer-id n n n CAD-KIND:artifact-id -- PPOLICY:spec ) PPOLICY-SPEC:MAKE" VCHECK 0 T=
s" PP-FORGE-SWAP ( CAD-KIND:artifact-id CAD-KIND:artifact-id CAD-KIND:numeric-policy-id CAD-KIND:target-id CAD-KIND:config-id CAD-KIND:producer-id n n n CAD-KIND:artifact-id -- PPOLICY:spec ) PPOLICY-SPEC:MAKE" VCHECK 0 T=
s" PP-FORGE-UNMAKE ( PPOLICY:spec -- n ) PPOLICY-SPEC:UNMAKE" VCHECK 0 T=
s" PP-FORGE-SPEC ( n -- PPOLICY:spec ) PPOLICY-SPEC:MAKE" VCHECK 0 T=

T-REPORT

;package
