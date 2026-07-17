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

' TT-ID-NEG E-JOURNAL-ID TTHROWS
' TT-ID-BIG E-JOURNAL-ID TTHROWS
TT-WIRE-ALL 0 T=
TT-WIRE-WIDTH 2 T=
TT-WIRE-UNKNOWN 3 T=

;package

T-REPORT
