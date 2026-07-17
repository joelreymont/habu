\ maki/producer-test.f - checked acceptance for the producer-id identity family
\ (maki/producer.f, dot habu-producer-producer-id-5e016e1f). Covers the § 23.9
\ per-family contract: content-addressed interning (equal names share one id,
\ version-independent so a namespaced class rides the name), the ID>WIRE / WIRE>ID
\ round-trip, fail-closed decode (wrong width + unresolved raw), cross-role
\ rejection, and privacy (no public raw cast). Every refusal is paired with a
\ resolving positive control so no TTHROWS is vacuous.

require lib/test.f
require test/checker-assert.f
require maki/producer.f

package PRODUCER-TEST

variable BASE-N

: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

: REG ( ptr u8 n -- CAD-KIND:producer-id )  PRODUCER:REGISTER ;

: BAD-EMPTY ( -- )   s" " PRODUCER:REGISTER drop ;

T-RESET

\ ---- content-addressed interning: equal names one id; a namespaced class rides it ---
PRODUCER:COUNT BASE-N !
s" pass/lowering" REG
s" pass/lowering" REG PRODUCER:EQUAL? TTRUE            \ equal name interns to one id
PRODUCER:COUNT BASE-N @ 1+ T=                           \ only one new slot
s" pass/lowering" REG PRODUCER:NAME$ s" pass/lowering" T$=
s" agent/search" REG
s" pass/lowering" REG PRODUCER:EQUAL? TFALSE           \ distinct component, distinct id
PRODUCER:COUNT BASE-N @ 2 + T=

\ ---- validate + fail-closed empty name ------------------------------------------
s" pass/lowering" REG PRODUCER:VALIDATE PRODUCER:NAME$ s" pass/lowering" T$=
' BAD-EMPTY E-PRODUCER-KEY TTHROWS

\ ---- checker: cross-role rejection + privacy (no public raw cast) ---------------
s" PR-OK ( CAD-KIND:producer-id -- CAD-KIND:producer-id ) PRODUCER:VALIDATE" YES
s" PR-NAME ( CAD-KIND:producer-id -- ptr u8 n ) PRODUCER:NAME$" YES
s" PR-XS ( CAD-KIND:schema-id -- CAD-KIND:producer-id ) PRODUCER:VALIDATE" NO   \ schema-id is not a producer-id
s" PR-XC ( CAD-KIND:config-id -- ptr u8 n ) PRODUCER:NAME$" NO
s" PRODUCER:RAW>PRODUCER-ID" 0 search-wl 0= TTRUE                                \ mint is private
s" PRODUCER:PRODUCER-ID>RAW" 0 search-wl 0= TTRUE                                \ projection is private

;package

\ Nominal-id corruption seam: a bad CAD-KIND:producer-id is only mintable via the
\ private refinement, so reopen the owning package for the E-PRODUCER-ID negatives
\ and the wire round-trip over registry internals (an out-of-range wire raw is only
\ forgeable inside the owning package).
package PRODUCER

1024 constant TT-WCAP
create TT-WBUF TT-WCAP allot

: TT-ID-NEG ( -- )  -1 RAW>PRODUCER-ID VALIDATE drop ;
: TT-ID-BIG ( -- )  PRD-N @ 100 + RAW>PRODUCER-ID NAME$ 2drop ;

: TT-WIRE-RT ( CAD-KIND:producer-id -- n )      \ 0 = round-trips to an EQUAL? id
   dup {: orig:CAD-KIND:producer-id :}
   TT-WBUF TT-WCAP ID>WIRE {: len:n :}
   TT-WBUF len WIRE>ID
   MATCH id-result
      ok          OF orig EQUAL? if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-WIRE-ALL ( -- n )                          \ 0 iff EVERY registered producer round-trips
   PRD-N @ 0 ?do
      i RAW>PRODUCER-ID TT-WIRE-RT 0<> if 1 unloop exit then
   loop 0 ;

: TT-WIRE-WIDTH ( -- n )                        \ a 4-byte buffer decodes as wrong-width
   TT-WBUF 4 WIRE>ID
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-WIRE-UNKNOWN ( -- n )                      \ an out-of-range raw decodes as unknown
   PRD-N @ 100 +  TT-WBUF WIRE-BYTES LE-PUT
   TT-WBUF WIRE-BYTES WIRE>ID
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

' TT-ID-NEG E-PRODUCER-ID TTHROWS
' TT-ID-BIG E-PRODUCER-ID TTHROWS
TT-WIRE-ALL 0 T=
TT-WIRE-WIDTH 2 T=
TT-WIRE-UNKNOWN 3 T=

;package

T-REPORT
