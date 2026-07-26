\ maki/config-test.f - checked acceptance for the config-id identity family
\ (maki/config.f, dot habu-config-config-id-06aa21bd). Covers the § 23.9 per-family
\ contract: content-addressed interning (equal fact sets share one id), the ID>WIRE
\ / WIRE>ID round-trip, fail-closed decode (wrong width + unresolved raw), cross-role
\ rejection, and privacy (no public raw cast). Every refusal is paired with a
\ resolving positive control so no TTHROWS is vacuous.

require lib/test.f
require test/checker-assert.f
require maki/config.f

package CONFIG-TEST

variable BASE-N

: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;
\ NOWORD is the verdict for a candidate naming a word that does not exist: the
\ checker reports 1 (uncheckable) rather than a type refusal. It is what makes the
\ constructor-spelling pins below bite instead of passing vacuously.
: NOWORD ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  1 T= ;

: REG ( ptr u8 n -- CAD-KIND:config-id )  CONFIG:REGISTER ;

: BAD-EMPTY ( -- )   s" " CONFIG:REGISTER drop ;

T-RESET

\ ---- content-addressed interning: equal fact sets one id; distinct facts don't ----
CONFIG:COUNT BASE-N !
s" tc=ptxas-12.4|opt=O3|det=1" REG
s" tc=ptxas-12.4|opt=O3|det=1" REG CONFIG:EQUAL? TTRUE        \ equal fact set interns to one id
CONFIG:COUNT BASE-N @ 1+ T=                                    \ only one new slot
s" tc=ptxas-12.4|opt=O3|det=1" REG CONFIG:FACTS$ s" tc=ptxas-12.4|opt=O3|det=1" T$=
s" tc=ptxas-12.4|opt=O2|det=1" REG
s" tc=ptxas-12.4|opt=O3|det=1" REG CONFIG:EQUAL? TFALSE       \ distinct opt flag, distinct id
CONFIG:COUNT BASE-N @ 2 + T=

\ ---- validate + fail-closed empty fact string ------------------------------------
s" tc=ptxas-12.4|opt=O3|det=1" REG CONFIG:VALIDATE CONFIG:FACTS$ s" tc=ptxas-12.4|opt=O3|det=1" T$=
' BAD-EMPTY E-CONFIG-KEY TTHROWS

\ ---- checker: cross-role rejection + privacy (no public raw cast) ---------------
s" CF-OK ( CAD-KIND:config-id -- CAD-KIND:config-id ) CONFIG:VALIDATE" YES
s" CF-FACTS ( CAD-KIND:config-id -- ptr u8 n ) CONFIG:FACTS$" YES
s" CF-KW ( CAD-KIND:config-id ptr u8 n -- n ) CONFIG:KEY>WIRE" YES             \ content-key encode
s" CF-XT ( CAD-KIND:target-id -- CAD-KIND:config-id ) CONFIG:VALIDATE" NO      \ target-id is not a config-id
s" CF-XN ( CAD-KIND:numeric-policy-id -- ptr u8 n ) CONFIG:FACTS$" NO          \ numeric-policy-id is not a config-id
s" CF-XKW ( CAD-KIND:target-id ptr u8 n -- n ) CONFIG:KEY>WIRE" NO             \ a foreign id cannot encode
s" CONFIG:RAW>CONFIG-ID" 0 search-wl 0= TTRUE                                   \ mint is private
s" CONFIG:CONFIG-ID>RAW" 0 search-wl 0= TTRUE                                   \ projection is private

\ ---- the generated id-result constructors: exact spelling + exact effect -------
\ id-result is declared through the unified ENUM front end in full mode, so these
\ pins are the migration's identity proof and must keep holding for every later
\ declaration change. The SPELLING is load-bearing: the checker answers 1
\ (uncheckable) for a name it cannot resolve, and YES demands -1, so a -1 means the
\ checker resolved EXACTLY this constructor name; NO demands 0, which it can only
\ reach after resolving the name and refusing the types. The NOWORD control below
\ proves that split rather than assuming it.
s" CFC-OK ( CAD-KIND:config-id -- CONFIG:id-result<CAD-KIND:config-id> ) CONFIG-ID--RESULT:OK" YES
s" CFC-WW ( -- CONFIG:id-result<CAD-KIND:config-id> ) CONFIG-ID--RESULT:WRONG-WIDTH" YES
s" CFC-UNK ( -- CONFIG:id-result<CAD-KIND:config-id> ) CONFIG-ID--RESULT:UNKNOWN" YES
\ One character off the real spelling, everything else identical to CFC-OK: the
\ verdict is uncheckable, never a -1 and never a type refusal.
s" CFC-SPELL ( CAD-KIND:config-id -- CONFIG:id-result<CAD-KIND:config-id> ) CONFIG-ID--RESULTX:OK" NOWORD
\ Forge negatives on the ok payload slot: a raw cell cannot fill it, the result is
\ not a bare scalar, the payload is mandatory (a payloadless ok is not constructible),
\ and a same-width FOREIGN identity role cannot stand in for the config id.
s" CFC-RAW ( n -- CONFIG:id-result<CAD-KIND:config-id> ) CONFIG-ID--RESULT:OK" NO
s" CFC-BARE ( CAD-KIND:config-id -- n ) CONFIG-ID--RESULT:OK" NO
s" CFC-NONE ( -- CONFIG:id-result<CAD-KIND:config-id> ) CONFIG-ID--RESULT:OK" NO
s" CFC-FGN ( CAD-KIND:target-id -- CONFIG:id-result<CAD-KIND:config-id> ) CONFIG-ID--RESULT:OK" NO

public

\ idr-twin is CONFIG:id-result's SHAPE under a different name: same arity, same three
\ variants in the same order, same named payload field. It exists only so the
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

s" CFC-TWIN ( CAD-KIND:config-id -- idr-twin<CAD-KIND:config-id> ) CONFIG--TEST-IDR--TWIN:OK" YES
s" CFC-TWIN-X1 ( CAD-KIND:config-id -- idr-twin<CAD-KIND:config-id> ) CONFIG-ID--RESULT:OK" NO
s" CFC-TWIN-X2 ( CAD-KIND:config-id -- CONFIG:id-result<CAD-KIND:config-id> ) CONFIG--TEST-IDR--TWIN:OK" NO

;package

\ Nominal-id corruption seam: a bad CAD-KIND:config-id is only mintable via the
\ private refinement, so reopen the owning package for the E-CONFIG-ID negatives
\ and the wire round-trip over registry internals (an out-of-range wire raw is only
\ forgeable inside the owning package).
package CONFIG

1024 constant TT-WCAP
create TT-WBUF TT-WCAP allot

: TT-ID-NEG ( -- )  -1 RAW>CONFIG-ID VALIDATE drop ;
: TT-ID-BIG ( -- )  CFG-N @ 100 + RAW>CONFIG-ID FACTS$ 2drop ;

: TT-WIRE-RT ( CAD-KIND:config-id -- n )        \ 0 = round-trips to an EQUAL? id
   dup {: orig:CAD-KIND:config-id :}
   TT-WBUF TT-WCAP ID>WIRE {: len:n :}
   TT-WBUF len WIRE>ID
   MATCH id-result
      ok          OF orig EQUAL? if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-WIRE-ALL ( -- n )                          \ 0 iff EVERY registered config round-trips
   CFG-N @ 0 ?do
      i RAW>CONFIG-ID TT-WIRE-RT 0<> if 1 unloop exit then
   loop 0 ;

: TT-WIRE-WIDTH ( -- n )                        \ a 4-byte buffer decodes as wrong-width
   TT-WBUF 4 WIRE>ID
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-WIRE-UNKNOWN ( -- n )                      \ an out-of-range raw decodes as unknown
   CFG-N @ 100 +  TT-WBUF WIRE-BYTES LE-PUT
   TT-WBUF WIRE-BYTES WIRE>ID
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

\ ---- cross-process content-key codec (KEY>WIRE / WIRE>KEY) ---------------------
create TT-SHA CK-BYTES allot

: TT-CKEY-RT ( CAD-KIND:config-id -- n )       \ 0 = content key round-trips to an EQUAL? id
   dup {: orig:CAD-KIND:config-id :}
   TT-WBUF TT-WCAP KEY>WIRE {: len:n :}
   TT-WBUF len WIRE>KEY
   MATCH id-result
      ok          OF orig EQUAL? if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-CKEY-ALL ( -- n )                         \ 0 iff EVERY registered config key round-trips
   CFG-N @ 0 ?do
      i RAW>CONFIG-ID TT-CKEY-RT 0<> if 1 unloop exit then
   loop 0 ;

: TT-CKEY-WIDTH ( -- n )                       \ an 8-byte buffer decodes as wrong-width
   TT-WBUF 8 WIRE>KEY
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-FILL-FF ( -- )                            \ 32 bytes no registered fact set can hash to
   0 begin dup CK-BYTES < while
      dup {: k:n :}
      $FF  TT-WBUF k +  c!
      1+
   repeat drop ;

: TT-CKEY-UNKNOWN ( -- n )                     \ a 32-byte non-registered key decodes as unknown
   TT-FILL-FF
   TT-WBUF CK-BYTES WIRE>KEY
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-CKEY-IS-SHA ( -- n )                      \ 0 iff KEY>WIRE == SHA-256(facts), NOT the raw index
   0 RAW>CONFIG-ID {: id:CAD-KIND:config-id :}
   id FACTS$ TT-SHA SHA256
   id TT-WBUF TT-WCAP KEY>WIRE drop
   TT-WBUF TT-SHA CK-EQ? if 0 else 1 then ;

\ ---- every variant constructs and dispatches through MATCH ---------------------
\ The wire words above reach the arms only through a decode. These construct each
\ variant DIRECTLY through the production producers and match it straight back, so
\ the named payload FIELD is proven to bind in declaration order. The ok arm binds
\ its payload to a TYPED local and reports the recovered registry index, which is
\ exactly what EQUAL? compares (interning makes fact-set identity raw index
\ equality), so a payload the constructor dropped or zeroed would come back as a
\ different index instead of passing.
\
\ Construction is factored into one typed word per variant because the checker
\ requires MATCH's scrutinee to be a concretely instantiated family value: a single
\ word that both constructs and matches is refused, and the diagnostic names the
\ family token as an undefined word. That refusal predates this migration (it
\ reproduces identically on the legacy declaration) and is tracked separately by dot
\ habu-checker-ground-match-c0cb9d44.
: TT-MK-OK ( CAD-KIND:config-id -- id-result<CAD-KIND:config-id> ) R-OK ;
: TT-MK-WW ( -- id-result<CAD-KIND:config-id> )   R-WRONG-WIDTH ;
: TT-MK-UNK ( -- id-result<CAD-KIND:config-id> )  R-UNKNOWN ;

: TT-ARM ( id-result<CAD-KIND:config-id> -- n )   \ 1 ok, 2 wrong-width, 3 unknown
   MATCH id-result
      ok          OF drop 1 ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-OK-RAW ( id-result<CAD-KIND:config-id> -- n )   \ ok payload's registry index, else -1
   MATCH id-result
      ok          OF {: got:CAD-KIND:config-id :} got CONFIG-ID>RAW ENDOF
      wrong-width OF -1 ENDOF
      unknown     OF -1 ENDOF
   ;MATCH ;

: TT-RT-OK-ARM ( -- n )                         \ a constructed ok reaches the ok arm
   s" tc=ptxas-12.4|opt=O3|det=1" REGISTER TT-MK-OK TT-ARM ;
\ The payload check deliberately rides the SECOND fact set registered above, whose
\ registry index is therefore at least one. The first fact set can legitimately land
\ at index 0, and a payload the constructor zeroed also reads back as 0, so comparing
\ against index 0 would pass on a zeroed payload. TT-RT-OK-NZ pins the index it
\ compares against as non-zero, so that escape stays closed.
: TT-RT-OK-RAW ( -- n )                         \ 0 = the registered id came back unchanged
   s" tc=ptxas-12.4|opt=O2|det=1" REGISTER dup CONFIG-ID>RAW {: want:n :}
   TT-MK-OK TT-OK-RAW want = if 0 else 1 then ;
: TT-RT-OK-NZ ( -- bool )                       \ that index is never the 0 a zeroed payload reads as
   s" tc=ptxas-12.4|opt=O2|det=1" REGISTER CONFIG-ID>RAW 0 > ;
: TT-RT-WW ( -- n )   TT-MK-WW TT-ARM ;
: TT-RT-UNK ( -- n )  TT-MK-UNK TT-ARM ;
: TT-WW-RAW ( -- n )  TT-MK-WW TT-OK-RAW ;      \ a payloadless arm carries no index

' TT-ID-NEG E-CONFIG-ID TTHROWS
' TT-ID-BIG E-CONFIG-ID TTHROWS
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
