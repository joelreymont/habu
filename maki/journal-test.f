\ maki/journal-test.f - checked acceptance for the audit-event-id identity family
\ (maki/journal.f, dot habu-v2-txn-journal-d0bc644f). Covers the § 23.9 per-family
\ contract for the APPEND-ONLY origin class: OCCURRENCE identity (an identical
\ descriptor appended twice yields two DISTINCT monotonic ids - the key departure
\ from the content-addressed families), the ID>WIRE / WIRE>ID round-trip, fail-closed
\ decode (wrong width + unresolved sequence), cross-role rejection, and privacy (no
\ public raw cast). Every refusal is paired with a resolving positive control so no
\ TTHROWS is vacuous.

require lib/test.f
require test/checker-assert.f
require maki/journal.f
require maki/rev.f

package JOURNAL-TEST

variable BASE-N

: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

: APP ( ptr u8 n -- CAD-KIND:audit-event-id )  JOURNAL:APPEND ;

: BAD-EMPTY ( -- )   s" " JOURNAL:APPEND drop ;

T-RESET

\ ---- occurrence identity: each append mints a fresh monotonic id --------------
JOURNAL:COUNT BASE-N !
s" evt: encode weight-artifact" APP
s" evt: encode weight-artifact" APP JOURNAL:EQUAL? TFALSE        \ identical descriptor, DISTINCT event
JOURNAL:COUNT BASE-N @ 2 + T=                                     \ both appends took a slot
s" evt: encode weight-artifact" APP JOURNAL:SEQ  BASE-N @ 2 + T= \ next append gets the next sequence
JOURNAL:COUNT BASE-N @ 3 + T=

\ SEQ is the monotonic sequence; DESC$ projects the recorded descriptor back. The
\ minted id is reused across assertions via `dup` (top-level scope has no locals).
s" evt: commit revision r1" APP                                  \ ( id )
dup JOURNAL:SEQ  BASE-N @ 3 + T=                                  \ next sequence
dup JOURNAL:DESC$ s" evt: commit revision r1" T$=                 \ descriptor projects back
dup dup JOURNAL:EQUAL? TTRUE                                      \ an event equals itself
dup JOURNAL:VALIDATE JOURNAL:DESC$ s" evt: commit revision r1" T$=  \ validate is identity on a live id
drop

\ ---- fail-closed empty descriptor --------------------------------------------
' BAD-EMPTY E-JOURNAL-KEY TTHROWS

\ ---- checker: cross-role rejection + privacy (no public raw cast) -------------
s" JV-OK ( CAD-KIND:audit-event-id -- CAD-KIND:audit-event-id ) JOURNAL:VALIDATE" YES
s" JV-SEQ ( CAD-KIND:audit-event-id -- n ) JOURNAL:SEQ" YES
s" JV-XS ( CAD-KIND:schema-id -- CAD-KIND:audit-event-id ) JOURNAL:VALIDATE" NO   \ schema-id is not an audit-event-id
s" JV-XR ( CAD-KIND:rev-id -- ptr u8 n ) JOURNAL:DESC$" NO                        \ rev-id is not an audit-event-id
s" JOURNAL:RAW>AUDIT-EVENT-ID" 0 search-wl 0= TTRUE                               \ mint is private
s" JOURNAL:AUDIT-EVENT-ID>RAW" 0 search-wl 0= TTRUE                               \ projection is private

\ ---- the generated id-result constructors: exact spelling + exact effect -------
\ id-result is declared through the unified ENUM front end in full mode, so these
\ pins are the migration's identity proof and must keep holding for every later
\ declaration change. The SPELLING is load-bearing here: the checker answers 1
\ (uncheckable) for a name it cannot resolve, and YES demands -1, so a -1 means the
\ checker resolved EXACTLY this constructor name; NO demands 0, which it can only
\ reach after resolving the name and refusing the types.
s" JC-OK ( CAD-KIND:audit-event-id -- JOURNAL:id-result<CAD-KIND:audit-event-id> ) JOURNAL-ID--RESULT:OK" YES
s" JC-WW ( -- JOURNAL:id-result<CAD-KIND:audit-event-id> ) JOURNAL-ID--RESULT:WRONG-WIDTH" YES
s" JC-UNK ( -- JOURNAL:id-result<CAD-KIND:audit-event-id> ) JOURNAL-ID--RESULT:UNKNOWN" YES
\ Forge negatives on the ok payload slot: a raw cell cannot fill it, the result is
\ not a bare scalar, the payload is mandatory (a payloadless ok is not constructible),
\ and a same-width FOREIGN identity role cannot stand in for the audit-event id.
s" JC-RAW ( n -- JOURNAL:id-result<CAD-KIND:audit-event-id> ) JOURNAL-ID--RESULT:OK" NO
s" JC-BARE ( CAD-KIND:audit-event-id -- n ) JOURNAL-ID--RESULT:OK" NO
s" JC-NONE ( -- JOURNAL:id-result<CAD-KIND:audit-event-id> ) JOURNAL-ID--RESULT:OK" NO
s" JC-FGN ( CAD-KIND:rev-id -- JOURNAL:id-result<CAD-KIND:audit-event-id> ) JOURNAL-ID--RESULT:OK" NO

public

\ idr-twin is JOURNAL:id-result's SHAPE under a different name: same arity, same
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

s" JC-TWIN ( CAD-KIND:audit-event-id -- idr-twin<CAD-KIND:audit-event-id> ) JOURNAL--TEST-IDR--TWIN:OK" YES
s" JC-TWIN-X1 ( CAD-KIND:audit-event-id -- idr-twin<CAD-KIND:audit-event-id> ) JOURNAL-ID--RESULT:OK" NO
s" JC-TWIN-X2 ( CAD-KIND:audit-event-id -- JOURNAL:id-result<CAD-KIND:audit-event-id> ) JOURNAL--TEST-IDR--TWIN:OK" NO

;package

\ Nominal-id corruption seam: a bad CAD-KIND:audit-event-id is only mintable via the
\ private refinement, so reopen the owning package for the E-JOURNAL-ID negatives and
\ the wire round-trip over the journal internals (an out-of-range wire sequence is
\ only forgeable inside the owning package).
package JOURNAL

1024 constant TT-WCAP
create TT-WBUF TT-WCAP allot

: TT-ID-NEG ( -- )  -1 RAW>AUDIT-EVENT-ID VALIDATE drop ;
: TT-ID-BIG ( -- )  JRN-N @ 100 + RAW>AUDIT-EVENT-ID DESC$ 2drop ;

: TT-WIRE-RT ( CAD-KIND:audit-event-id -- n )   \ 0 = round-trips to an EQUAL? id
   dup {: orig:CAD-KIND:audit-event-id :}
   TT-WBUF TT-WCAP ID>WIRE {: len:n :}
   TT-WBUF len WIRE>ID
   MATCH id-result
      ok          OF orig EQUAL? if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-WIRE-ALL ( -- n )                          \ 0 iff EVERY appended event round-trips
   JRN-N @ 0 ?do
      i RAW>AUDIT-EVENT-ID TT-WIRE-RT 0<> if 1 unloop exit then
   loop 0 ;

: TT-WIRE-WIDTH ( -- n )                        \ a 4-byte buffer decodes as wrong-width
   TT-WBUF 4 WIRE>ID
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-WIRE-UNKNOWN ( -- n )                      \ an out-of-range sequence decodes as unknown
   JRN-N @ 100 +  TT-WBUF WIRE-BYTES LE-PUT
   TT-WBUF WIRE-BYTES WIRE>ID
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

\ ---- every variant constructs and dispatches through MATCH ---------------------
\ The wire words above reach the arms only through a decode. These construct each
\ variant DIRECTLY through the production producers and match it straight back, so
\ the named payload FIELD is proven to bind in declaration order. The ok arm binds
\ its payload to a TYPED local and reports the recovered sequence, which is exactly
\ what EQUAL? compares (same-occurrence identity IS raw sequence equality), so a
\ payload the constructor dropped or zeroed would come back as a different sequence
\ instead of passing.
\
\ Construction is factored into one typed word per variant because the checker
\ requires MATCH's scrutinee to be a concretely instantiated family value: a single
\ word that both constructs and matches is refused, and the diagnostic names the
\ family token as an undefined word. That refusal predates this migration (it
\ reproduces identically on the legacy declaration) and is reported separately.
: TT-MK-OK ( CAD-KIND:audit-event-id -- id-result<CAD-KIND:audit-event-id> ) R-OK ;
: TT-MK-WW ( -- id-result<CAD-KIND:audit-event-id> )   R-WRONG-WIDTH ;
: TT-MK-UNK ( -- id-result<CAD-KIND:audit-event-id> )  R-UNKNOWN ;

: TT-ARM ( id-result<CAD-KIND:audit-event-id> -- n )   \ 1 ok, 2 wrong-width, 3 unknown
   MATCH id-result
      ok          OF drop 1 ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-OK-SEQ ( id-result<CAD-KIND:audit-event-id> -- n )   \ ok payload's sequence, else -1
   MATCH id-result
      ok          OF {: got:CAD-KIND:audit-event-id :} got SEQ ENDOF
      wrong-width OF -1 ENDOF
      unknown     OF -1 ENDOF
   ;MATCH ;

: TT-RT-OK-ARM ( -- n )                         \ a constructed ok reaches the ok arm
   s" evt: match arm" APPEND TT-MK-OK TT-ARM ;
: TT-RT-OK-SEQ ( -- n )                         \ 0 = the appended id came back unchanged
   s" evt: match payload" APPEND dup SEQ {: want:n :}
   TT-MK-OK TT-OK-SEQ want = if 0 else 1 then ;
: TT-RT-WW ( -- n )   TT-MK-WW TT-ARM ;
: TT-RT-UNK ( -- n )  TT-MK-UNK TT-ARM ;
: TT-WW-SEQ ( -- n )  TT-MK-WW TT-OK-SEQ ;      \ a payloadless arm carries no sequence

' TT-ID-NEG E-JOURNAL-ID TTHROWS
' TT-ID-BIG E-JOURNAL-ID TTHROWS
TT-WIRE-ALL 0 T=
TT-WIRE-WIDTH 2 T=
TT-WIRE-UNKNOWN 3 T=
TT-RT-OK-ARM 1 T=                               \ ok dispatches to its own arm
TT-RT-OK-SEQ 0 T=                               \ and carries its payload through unchanged
TT-RT-WW 2 T=                                   \ wrong-width dispatches to its own arm
TT-RT-UNK 3 T=                                  \ unknown dispatches to its own arm
TT-WW-SEQ -1 T=                                 \ the no-payload arms of TT-OK-SEQ are live

;package

T-REPORT
