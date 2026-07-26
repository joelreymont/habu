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
\ NOWORD is the verdict for a candidate naming a word that does not exist: the
\ checker reports 1 (uncheckable) rather than a type refusal. It is what makes the
\ constructor-spelling pins below bite instead of passing vacuously.
: NOWORD ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  1 T= ;

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
s" PR-KW ( CAD-KIND:producer-id ptr u8 n -- n ) PRODUCER:KEY>WIRE" YES            \ content-key encode
s" PR-XS ( CAD-KIND:schema-id -- CAD-KIND:producer-id ) PRODUCER:VALIDATE" NO   \ schema-id is not a producer-id
s" PR-XC ( CAD-KIND:config-id -- ptr u8 n ) PRODUCER:NAME$" NO
s" PR-XKW ( CAD-KIND:schema-id ptr u8 n -- n ) PRODUCER:KEY>WIRE" NO             \ a foreign id cannot encode
s" PRODUCER:RAW>PRODUCER-ID" 0 search-wl 0= TTRUE                                \ mint is private
s" PRODUCER:PRODUCER-ID>RAW" 0 search-wl 0= TTRUE                                \ projection is private

\ ---- the generated id-result constructors: exact spelling + exact effect -------
\ id-result is declared through the unified ENUM front end in full mode, so these
\ pins are the migration's identity proof and must keep holding for every later
\ declaration change. The SPELLING is load-bearing: the checker answers 1
\ (uncheckable) for a name it cannot resolve, and YES demands -1, so a -1 means the
\ checker resolved EXACTLY this constructor name; NO demands 0, which it can only
\ reach after resolving the name and refusing the types. The NOWORD control below
\ proves that split rather than assuming it.
s" PRC-OK ( CAD-KIND:producer-id -- PRODUCER:id-result<CAD-KIND:producer-id> ) PRODUCER-ID--RESULT:OK" YES
s" PRC-WW ( -- PRODUCER:id-result<CAD-KIND:producer-id> ) PRODUCER-ID--RESULT:WRONG-WIDTH" YES
s" PRC-UNK ( -- PRODUCER:id-result<CAD-KIND:producer-id> ) PRODUCER-ID--RESULT:UNKNOWN" YES
\ One character off the real spelling, everything else identical to PRC-OK: the
\ verdict is uncheckable, never a -1 and never a type refusal.
s" PRC-SPELL ( CAD-KIND:producer-id -- PRODUCER:id-result<CAD-KIND:producer-id> ) PRODUCER-ID--RESULTX:OK" NOWORD
\ Forge negatives on the ok payload slot: a raw cell cannot fill it, the result is
\ not a bare scalar, the payload is mandatory (a payloadless ok is not constructible),
\ and a same-width FOREIGN identity role cannot stand in for the producer id.
s" PRC-RAW ( n -- PRODUCER:id-result<CAD-KIND:producer-id> ) PRODUCER-ID--RESULT:OK" NO
s" PRC-BARE ( CAD-KIND:producer-id -- n ) PRODUCER-ID--RESULT:OK" NO
s" PRC-NONE ( -- PRODUCER:id-result<CAD-KIND:producer-id> ) PRODUCER-ID--RESULT:OK" NO
s" PRC-FGN ( CAD-KIND:schema-id -- PRODUCER:id-result<CAD-KIND:producer-id> ) PRODUCER-ID--RESULT:OK" NO

public

\ idr-twin is PRODUCER:id-result's SHAPE under a different name: same arity, same
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

s" PRC-TWIN ( CAD-KIND:producer-id -- idr-twin<CAD-KIND:producer-id> ) PRODUCER--TEST-IDR--TWIN:OK" YES
s" PRC-TWIN-X1 ( CAD-KIND:producer-id -- idr-twin<CAD-KIND:producer-id> ) PRODUCER-ID--RESULT:OK" NO
s" PRC-TWIN-X2 ( CAD-KIND:producer-id -- PRODUCER:id-result<CAD-KIND:producer-id> ) PRODUCER--TEST-IDR--TWIN:OK" NO

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

\ ---- cross-process content-key codec (KEY>WIRE / WIRE>KEY) ---------------------
create TT-SHA CK-BYTES allot

: TT-CKEY-RT ( CAD-KIND:producer-id -- n )     \ 0 = content key round-trips to an EQUAL? id
   dup {: orig:CAD-KIND:producer-id :}
   TT-WBUF TT-WCAP KEY>WIRE {: len:n :}
   TT-WBUF len WIRE>KEY
   MATCH id-result
      ok          OF orig EQUAL? if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-CKEY-ALL ( -- n )                         \ 0 iff EVERY registered producer key round-trips
   PRD-N @ 0 ?do
      i RAW>PRODUCER-ID TT-CKEY-RT 0<> if 1 unloop exit then
   loop 0 ;

: TT-CKEY-WIDTH ( -- n )                       \ an 8-byte buffer decodes as wrong-width
   TT-WBUF 8 WIRE>KEY
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-FILL-FF ( -- )                            \ 32 bytes no registered name can hash to
   0 begin dup CK-BYTES < while
      dup {: k:n :}
      $FF  TT-WBUF k +  c!
      1+
   repeat drop ;

: TT-CKEY-UNKNOWN ( -- n )                     \ a 32-byte non-registered key decodes as unknown
   TT-FILL-FF
   TT-WBUF CK-BYTES WIRE>KEY
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-CKEY-IS-SHA ( -- n )                      \ 0 iff KEY>WIRE == SHA-256(name), NOT the raw index
   0 RAW>PRODUCER-ID {: id:CAD-KIND:producer-id :}
   id NAME$ TT-SHA SHA256
   id TT-WBUF TT-WCAP KEY>WIRE drop
   TT-WBUF TT-SHA CK-EQ? if 0 else 1 then ;

\ ---- every variant constructs and dispatches through MATCH ---------------------
\ The wire words above reach the arms only through a decode. These construct each
\ variant DIRECTLY through the production producers and match it straight back, so
\ the named payload FIELD is proven to bind in declaration order. The ok arm binds
\ its payload to a TYPED local and reports the recovered registry index, which is
\ exactly what EQUAL? compares (interning makes name identity raw index equality),
\ so a payload the constructor dropped or zeroed would come back as a different
\ index instead of passing.
\
\ Construction is factored into one typed word per variant because the checker
\ requires MATCH's scrutinee to be a concretely instantiated family value: a single
\ word that both constructs and matches is refused, and the diagnostic names the
\ family token as an undefined word. That refusal predates this migration (it
\ reproduces identically on the legacy declaration) and is tracked separately by dot
\ habu-checker-ground-match-c0cb9d44.
: TT-MK-OK ( CAD-KIND:producer-id -- id-result<CAD-KIND:producer-id> ) R-OK ;
: TT-MK-WW ( -- id-result<CAD-KIND:producer-id> )   R-WRONG-WIDTH ;
: TT-MK-UNK ( -- id-result<CAD-KIND:producer-id> )  R-UNKNOWN ;

: TT-ARM ( id-result<CAD-KIND:producer-id> -- n )   \ 1 ok, 2 wrong-width, 3 unknown
   MATCH id-result
      ok          OF drop 1 ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-OK-RAW ( id-result<CAD-KIND:producer-id> -- n )   \ ok payload's registry index, else -1
   MATCH id-result
      ok          OF {: got:CAD-KIND:producer-id :} got PRODUCER-ID>RAW ENDOF
      wrong-width OF -1 ENDOF
      unknown     OF -1 ENDOF
   ;MATCH ;

: TT-RT-OK-ARM ( -- n )                         \ a constructed ok reaches the ok arm
   s" pass/lowering" REGISTER TT-MK-OK TT-ARM ;
\ The payload check deliberately rides the SECOND name registered above, whose
\ registry index is therefore at least one. The first name can legitimately land at
\ index 0, and a payload the constructor zeroed also reads back as 0, so comparing
\ against index 0 would pass on a zeroed payload. TT-RT-OK-NZ pins the index it
\ compares against as non-zero, so that escape stays closed.
: TT-RT-OK-RAW ( -- n )                         \ 0 = the registered id came back unchanged
   s" agent/search" REGISTER dup PRODUCER-ID>RAW {: want:n :}
   TT-MK-OK TT-OK-RAW want = if 0 else 1 then ;
: TT-RT-OK-NZ ( -- bool )                       \ that index is never the 0 a zeroed payload reads as
   s" agent/search" REGISTER PRODUCER-ID>RAW 0 > ;
: TT-RT-WW ( -- n )   TT-MK-WW TT-ARM ;
: TT-RT-UNK ( -- n )  TT-MK-UNK TT-ARM ;
: TT-WW-RAW ( -- n )  TT-MK-WW TT-OK-RAW ;      \ a payloadless arm carries no index

' TT-ID-NEG E-PRODUCER-ID TTHROWS
' TT-ID-BIG E-PRODUCER-ID TTHROWS
TT-WIRE-ALL 0 T=
TT-WIRE-WIDTH 2 T=
TT-WIRE-UNKNOWN 3 T=
TT-CKEY-ALL 0 T=
TT-CKEY-WIDTH 2 T=
TT-CKEY-UNKNOWN 3 T=
TT-CKEY-IS-SHA 0 T=
TT-RT-OK-ARM 1 T=                               \ ok dispatches to its own arm
TT-RT-OK-RAW 0 T=                               \ and carries its payload through unchanged
TT-RT-OK-NZ TTRUE                               \ against a non-zero index, so a zeroed payload fails
TT-RT-WW 2 T=                                   \ wrong-width dispatches to its own arm
TT-RT-UNK 3 T=                                  \ unknown dispatches to its own arm
TT-WW-RAW -1 T=                                 \ the no-payload arms of TT-OK-RAW are live

;package

T-REPORT
