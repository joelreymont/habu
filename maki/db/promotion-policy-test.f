\ maki/db/promotion-policy-test.f - acceptance for the promotion POLICY value + digest
\ (maki/db/promotion-policy.f; dot habu-v2-evidence-promotion-f8312ebe).
\
\ Proves: equal-field policies share a content digest; a policy differing in ANY bound
\ field digests differently (the digest-binding the "policy change invalidates" rule rests
\ on); BIND yields the model / expiry / digest-words SATISFY consumes. All product values
\ are produced and consumed INSIDE colon words (never on the interpret-mode stack). Fixtures
\ mint real ids through their owner constructors, never a raw cast.

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

\ SPEC-A / SPEC-A2 are field-identical; SPEC-THRESH / SPEC-EXP / SPEC-VVER differ in
\ exactly one bound field (threshold / expiry / verifier version).
: SPEC-A ( -- PPOLICY:spec )     M1 W1 TGT NUM POP VERIF  7 100 5000 RB PPOLICY:MK ;
: SPEC-A2 ( -- PPOLICY:spec )    M1 W1 TGT NUM POP VERIF  7 100 5000 RB PPOLICY:MK ;
: SPEC-THRESH ( -- PPOLICY:spec ) M1 W1 TGT NUM POP VERIF  7 200 5000 RB PPOLICY:MK ;
: SPEC-EXP ( -- PPOLICY:spec )   M1 W1 TGT NUM POP VERIF  7 100 6000 RB PPOLICY:MK ;
: SPEC-VVER ( -- PPOLICY:spec )  M1 W1 TGT NUM POP VERIF  9 100 5000 RB PPOLICY:MK ;

\ ---- digest equality: equal fields == one digest; any change != ----------------
: PP-EQ-SAME ( -- bool )     SPEC-A SPEC-A2 PPOLICY:DIGEST-EQ? ;
: PP-EQ-THRESH ( -- bool )   SPEC-A SPEC-THRESH PPOLICY:DIGEST-EQ? ;
: PP-EQ-EXP ( -- bool )      SPEC-A SPEC-EXP PPOLICY:DIGEST-EQ? ;
: PP-EQ-VVER ( -- bool )     SPEC-A SPEC-VVER PPOLICY:DIGEST-EQ? ;

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

T-RESET

PP-EQ-SAME TTRUE                       \ field-identical policies share a digest
PP-EQ-THRESH TFALSE                    \ a changed threshold digests differently
PP-EQ-EXP TFALSE                       \ a changed expiry digests differently
PP-EQ-VVER TFALSE                      \ a changed verifier version digests differently

PP-BIND-MODEL TTRUE                    \ BIND yields the policy's model
PP-BIND-EXPIRY 5000 T=                 \ BIND yields the policy's expiry
PP-BIND-DIGEST TTRUE                   \ BIND's digest words match DIGEST-WORDS

T-REPORT

;package
