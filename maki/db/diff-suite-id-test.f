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
require test/checker-assert.f
require maki/db/diff-suite.f
require maki/db/diff-suite-id.f
require maki/numpolicy.f
require maki/producer.f
require maki/target/target.f
require maki/db/obligation.f
require maki/db/budget-dim.f

package SUITEID-TEST

: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

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

\ ---- the generated id-result constructors: exact spelling + exact effect -------
\ id-result is declared through the unified ENUM front end in full mode, so these
\ pins are the migration's identity proof and must keep holding for every later
\ declaration change. The SPELLING is load-bearing here: the checker answers 1
\ (uncheckable) for a name it cannot resolve, and YES demands -1, so a -1 means the
\ checker resolved EXACTLY this constructor name; NO demands 0, which it can only
\ reach after resolving the name and refusing the types.
s" SID-C-OK ( CAD-KIND:suite-id -- SUITEID:id-result<CAD-KIND:suite-id> ) SUITEID-ID--RESULT:OK" YES
s" SID-C-WW ( -- SUITEID:id-result<CAD-KIND:suite-id> ) SUITEID-ID--RESULT:WRONG-WIDTH" YES
s" SID-C-UNK ( -- SUITEID:id-result<CAD-KIND:suite-id> ) SUITEID-ID--RESULT:UNKNOWN" YES
\ Forge negatives on the ok payload slot: a raw cell cannot fill it, the result is
\ not a bare scalar, the payload is mandatory (a payloadless ok is not constructible),
\ and a same-width FOREIGN identity role cannot stand in for the suite id.
s" SID-C-RAW ( n -- SUITEID:id-result<CAD-KIND:suite-id> ) SUITEID-ID--RESULT:OK" NO
s" SID-C-BARE ( CAD-KIND:suite-id -- n ) SUITEID-ID--RESULT:OK" NO
s" SID-C-NONE ( -- SUITEID:id-result<CAD-KIND:suite-id> ) SUITEID-ID--RESULT:OK" NO
s" SID-C-FGN ( CAD-KIND:producer-id -- SUITEID:id-result<CAD-KIND:suite-id> ) SUITEID-ID--RESULT:OK" NO

public

\ idr-twin is SUITEID:id-result's SHAPE under a different name: same arity, same
\ three variants in the same order, same named payload field. It exists only so the
\ negatives below can prove decode-result identity is NOMINAL - two identically
\ shaped ENUM families never unify, in either direction. It has to be public: a
\ private family publishes no constructors at all, and the positive control below
\ builds through the twin's own ok, so neither negative can pass by being
\ unresolvable rather than ill-typed.
ENUM idr-twin 1
   VARIANT ok FIELD id a ;VARIANT
   VARIANT wrong-width ;VARIANT
   VARIANT unknown ;VARIANT
;ENUM

private

s" SID-C-TWIN ( CAD-KIND:suite-id -- idr-twin<CAD-KIND:suite-id> ) SUITEID--TEST-IDR--TWIN:OK" YES
s" SID-C-TWIN-X1 ( CAD-KIND:suite-id -- idr-twin<CAD-KIND:suite-id> ) SUITEID-ID--RESULT:OK" NO
s" SID-C-TWIN-X2 ( CAD-KIND:suite-id -- SUITEID:id-result<CAD-KIND:suite-id> ) SUITEID--TEST-IDR--TWIN:OK" NO

;package

\ The public legs above reach the id-result arms only through a WIRE>KEY decode. The
\ variant producers R-OK / R-WRONG-WIDTH / R-UNKNOWN are owner-private, so reopen the
\ owning package to construct each variant DIRECTLY and match it straight back; that is
\ what proves the named payload FIELD binds in declaration order. The ok arm binds its
\ payload to a TYPED local and reports the recovered registry raw, which is exactly what
\ EQUAL? compares (content-addressed identity IS raw equality), so a payload the
\ constructor dropped or zeroed would come back as a different raw instead of passing.
\ The id under test is the LAST interned suite (raw >= 1 after the legs above), so a
\ zeroed payload is distinguishable from a live one.
\
\ Construction is factored into one typed word per variant because the checker requires
\ MATCH's scrutinee to be a concretely instantiated family value: a single word that
\ both constructs and matches is refused, and the diagnostic names the family token as
\ an undefined word. That refusal predates this migration (it reproduces identically on
\ the legacy declaration) and is reported separately.
package SUITEID

: TT-ID ( -- CAD-KIND:suite-id )   SID-N @ 1- RAW>SUITE-ID ;   \ last interned suite id

: TT-MK-OK ( CAD-KIND:suite-id -- id-result<CAD-KIND:suite-id> ) R-OK ;
: TT-MK-WW ( -- id-result<CAD-KIND:suite-id> )   R-WRONG-WIDTH ;
: TT-MK-UNK ( -- id-result<CAD-KIND:suite-id> )  R-UNKNOWN ;

: TT-ARM ( id-result<CAD-KIND:suite-id> -- n )   \ 1 ok, 2 wrong-width, 3 unknown
   MATCH id-result
      ok          OF drop 1 ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-OK-RAW ( id-result<CAD-KIND:suite-id> -- n )   \ ok payload's registry raw, else -1
   MATCH id-result
      ok          OF {: got:CAD-KIND:suite-id :} got SUITE-ID>RAW ENDOF
      wrong-width OF -1 ENDOF
      unknown     OF -1 ENDOF
   ;MATCH ;

: TT-RT-OK-ARM ( -- n )                         \ a constructed ok reaches the ok arm
   TT-ID TT-MK-OK TT-ARM ;
: TT-RT-OK-RAW ( -- n )                         \ 0 = the interned id came back unchanged
   TT-ID dup SUITE-ID>RAW {: want:n :}
   TT-MK-OK TT-OK-RAW want = if 0 else 1 then ;
: TT-RT-WW ( -- n )   TT-MK-WW TT-ARM ;
: TT-RT-UNK ( -- n )  TT-MK-UNK TT-ARM ;
: TT-WW-RAW ( -- n )  TT-MK-WW TT-OK-RAW ;      \ a payloadless arm carries no raw
: TT-ID-RAW ( -- n )  TT-ID SUITE-ID>RAW ;      \ the payload raw under test
: TT-COUNT ( -- n )   SID-N @ ;                 \ interned suites after the legs above

TT-COUNT 2 T=                                   \ the legs above interned exactly two distinct suites
TT-ID-RAW 1 T=                                  \ so the round-trip payload is raw 1, not the zero raw
TT-RT-OK-ARM 1 T=                               \ ok dispatches to its own arm
TT-RT-OK-RAW 0 T=                               \ and carries its payload through unchanged
TT-RT-WW 2 T=                                   \ wrong-width dispatches to its own arm
TT-RT-UNK 3 T=                                  \ unknown dispatches to its own arm
TT-WW-RAW -1 T=                                 \ the no-payload arms of TT-OK-RAW are live

;package

T-REPORT
