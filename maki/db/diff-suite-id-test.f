\ maki/db/diff-suite-id-test.f - acceptance for the durable DifferentialSuite identity
\ registry (maki/db/diff-suite-id.f; dot habu-v2-differential-runner-13359019).
\
\ Proves the § 23.9 content-addressed identity contract for CAD-KIND:suite-id:
\   SID-SAME    : two equal suites (rebuilt independently) intern to ONE id.
\   SID-ORDER   : generator INSERTION ORDER does not change the id (canonical digest).
\   SID-FLIP    : any digest-covered field flip (seed here) mints a DISTINCT id.
\   SID-WIRE    : KEY>WIRE / WIRE>KEY round-trips an id to its 32-byte suite digest and
\                 back BY CONTENT; the resolved id EQUAL?s the original.
\   SID-UNKNOWN : an unregistered content key resolves `unknown`, never a forged id.
\   SID-WIDTH   : a wrong-width wire buffer resolves `wrong-width`.
\
\ Fixtures reuse the diff-suite-test build discipline (real ids through their owner
\ constructors, never a raw cast); names carry the SID-test prefix.

require lib/test.f
require lib/string.f
require maki/db/diff-suite.f
require maki/db/diff-suite-id.f
require maki/numpolicy.f
require maki/producer.f
require maki/target/target.f
require maki/db/obligation.f
require maki/db/budget-dim.f

package SUITEID-TEST

32 constant CKW
create WBUF CKW allot             \ suite-digest wire buffer
create WBAD 16 allot              \ wrong-width wire buffer

: SUBJ-A ( -- CAD-KIND:producer-id )  s" suiteid-test/subject-habu" PRODUCER:REGISTER ;
: REF-1 ( -- CAD-KIND:producer-id )   s" suiteid-test/ref-torch" PRODUCER:REGISTER ;
: CMP-REL ( -- CAD-KIND:numeric-policy-id )  NPOL-DOM:RELATIVE NPOL:REGISTER ;
: T1 ( -- CAD-KIND:target-id )  TARGET:SM87 ;
: GEN-1 ( -- ptr u8 n )   s" suiteid-test/gen-uniform" ;
: GEN-2 ( -- ptr u8 n )   s" suiteid-test/gen-adversarial" ;

\ BUILD-SUITE ( seed genrev -- DIFFSUITE:suite ): a sealed suite parameterised by seed
\ (a digest-covered field) and generator insertion order (must NOT change the digest).
: SEAL-OK ( -- DIFFSUITE:suite )
   DIFFSUITE:SEAL MATCH DIFFSUITE:build-result
      ok OF ENDOF
      incomplete OF -777 throw ENDOF
      tolerance-mismatch OF -777 throw ENDOF
      reference-not-independent OF -777 throw ENDOF
   ;MATCH ;
: BUILD-SUITE ( n bool -- DIFFSUITE:suite ) {: seed:n genrev:bool :}
   DIFFSUITE:NEW
   seed DIFFSUITE:SEED!
   SUBJ-A DIFFSUITE:SUBJECT
   OBLIG-INDEPENDENCE:INDEPENDENT DIFFSUITE:POLICY
   CMP-REL 5 DIFFSUITE:COMPARISON
   s" suiteid-test/norm" DIFFSUITE:NORMALIZATION
   s" suiteid-test/min" DIFFSUITE:MINIMIZER
   T1 DIFFSUITE:TARGET-NEED
   BUDGET-DIM:COMPUTE-TIME 100 DIFFSUITE:BUDGET!
   genrev if GEN-2 DIFFSUITE:GENERATOR+ GEN-1 DIFFSUITE:GENERATOR+
          else GEN-1 DIFFSUITE:GENERATOR+ GEN-2 DIFFSUITE:GENERATOR+ then
   REF-1 DIFFSUITE:REFERENCE+
   s" suiteid-test/prop" DIFFSUITE:PROPERTY+
   SEAL-OK ;

: ID-OF ( n bool -- CAD-KIND:suite-id )   BUILD-SUITE SUITEID:REGISTER ;

\ ---- identity legs -------------------------------------------------------------------
: SID-SAME ( -- bool )
   42 false ID-OF  42 false ID-OF  SUITEID:EQUAL? ;
: SID-ORDER ( -- bool )
   42 false ID-OF  42 true ID-OF  SUITEID:EQUAL? ;
: SID-FLIP ( -- bool )
   42 false ID-OF  99 false ID-OF  SUITEID:EQUAL? 0= ;

\ ---- wire round-trip -----------------------------------------------------------------
: SID-WIRE ( -- bool )
   42 false ID-OF {: id:CAD-KIND:suite-id :}
   id WBUF CKW SUITEID:KEY>WIRE {: w:n :}
   w CKW <> if false exit then
   WBUF CKW SUITEID:WIRE>KEY MATCH SUITEID:id-result
      ok OF {: got:CAD-KIND:suite-id :} got id SUITEID:EQUAL? ENDOF
      wrong-width OF false ENDOF
      unknown OF false ENDOF
   ;MATCH ;

: SID-UNKNOWN ( -- bool )   \ a valid-width content key with one byte flipped is unregistered
   42 false ID-OF WBUF CKW SUITEID:KEY>WIRE drop
   WBUF c@ 1+ $FF and  WBUF c!            \ flip byte 0: no longer a registered suite digest
   WBUF CKW SUITEID:WIRE>KEY MATCH SUITEID:id-result
      ok OF drop false ENDOF
      wrong-width OF false ENDOF
      unknown OF true ENDOF
   ;MATCH ;

: SID-WIDTH ( -- bool )     \ a 16-byte buffer is not a 32-byte content key
   WBAD 16 SUITEID:WIRE>KEY MATCH SUITEID:id-result
      ok OF drop false ENDOF
      wrong-width OF true ENDOF
      unknown OF false ENDOF
   ;MATCH ;

T-RESET

SID-SAME TTRUE             \ equal suites intern to one id
SID-ORDER TTRUE           \ generator insertion order does not change the id
SID-FLIP TTRUE            \ a flipped semantic field mints a distinct id
SID-WIRE TTRUE            \ KEY>WIRE / WIRE>KEY round-trips by content
SID-UNKNOWN TTRUE         \ an unregistered content key resolves unknown
SID-WIDTH TTRUE           \ a wrong-width buffer resolves wrong-width

T-REPORT

;package
