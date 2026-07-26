\ maki/db/evidence-test.f - checked acceptance for the evidence-id identity family
\ (maki/db/evidence.f, dot habu-v2-evidence-applicability-73ac58b9). Covers the § 23.9
\ per-family contract: content-addressed interning (equal descriptors share one id), the
\ ID>WIRE / WIRE>ID and cross-process KEY>WIRE / WIRE>KEY round-trips, fail-closed decode
\ (wrong width + unresolved raw), cross-role rejection, and privacy (no public raw cast).
\ Every refusal is paired with a resolving positive control so no TTHROWS is vacuous.

require lib/test.f
require test/checker-assert.f
require maki/db/evidence.f

package EVIDENCE-TEST

variable BASE-N

: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

: REG ( ptr u8 n -- CAD-KIND:evidence-id )  EVIDENCE:REGISTER ;

: BAD-EMPTY ( -- )   s" " EVIDENCE:REGISTER drop ;

T-RESET

\ ---- content-addressed interning: equal descriptors one id; distinct don't ----------
EVIDENCE:COUNT BASE-N !
s" subj=art1|dom=exact|rel=semantic-equiv|env=sm87|ver=diff|vc=differential-exec" REG
s" subj=art1|dom=exact|rel=semantic-equiv|env=sm87|ver=diff|vc=differential-exec" REG EVIDENCE:EQUAL? TTRUE
EVIDENCE:COUNT BASE-N @ 1+ T=                                   \ only one new slot
s" subj=art1|dom=exact|rel=semantic-equiv|env=sm87|ver=diff|vc=differential-exec" REG
   EVIDENCE:DESCRIPTOR$
   s" subj=art1|dom=exact|rel=semantic-equiv|env=sm87|ver=diff|vc=differential-exec" T$=
s" subj=art1|dom=device|rel=semantic-equiv|env=sm87|ver=diff|vc=device-measure" REG
s" subj=art1|dom=exact|rel=semantic-equiv|env=sm87|ver=diff|vc=differential-exec" REG EVIDENCE:EQUAL? TFALSE
EVIDENCE:COUNT BASE-N @ 2 + T=

\ ---- validate + fail-closed empty descriptor -------------------------------------
s" subj=art1|dom=exact|rel=semantic-equiv|env=sm87|ver=diff|vc=differential-exec" REG
   EVIDENCE:VALIDATE EVIDENCE:DESCRIPTOR$
   s" subj=art1|dom=exact|rel=semantic-equiv|env=sm87|ver=diff|vc=differential-exec" T$=
' BAD-EMPTY E-EVIDENCE-KEY TTHROWS

\ ---- checker: cross-role rejection + privacy (no public raw cast) ---------------
s" EV-OK ( CAD-KIND:evidence-id -- CAD-KIND:evidence-id ) EVIDENCE:VALIDATE" YES
s" EV-DESC ( CAD-KIND:evidence-id -- ptr u8 n ) EVIDENCE:DESCRIPTOR$" YES
s" EV-KW ( CAD-KIND:evidence-id ptr u8 n -- n ) EVIDENCE:KEY>WIRE" YES            \ content-key encode
s" EV-XO ( CAD-KIND:obligation-id -- CAD-KIND:evidence-id ) EVIDENCE:VALIDATE" NO \ obligation-id is not an evidence-id
s" EV-XA ( CAD-KIND:artifact-id -- ptr u8 n ) EVIDENCE:DESCRIPTOR$" NO            \ artifact-id is not an evidence-id
s" EV-XKW ( CAD-KIND:obligation-id ptr u8 n -- n ) EVIDENCE:KEY>WIRE" NO          \ a foreign id cannot encode
s" EVIDENCE:RAW>EVIDENCE-ID" 0 search-wl 0= TTRUE                                  \ mint is private
s" EVIDENCE:EVIDENCE-ID>RAW" 0 search-wl 0= TTRUE                                  \ projection is private

\ ---- the generated id-result constructors: exact spelling + exact effect -------
\ id-result is declared through the unified ENUM front end in full mode, so these
\ pins are the migration's identity proof and must keep holding for every later
\ declaration change. The SPELLING is load-bearing here: the checker answers 1
\ (uncheckable) for a name it cannot resolve, and YES demands -1, so a -1 means the
\ checker resolved EXACTLY this constructor name; NO demands 0, which it can only
\ reach after resolving the name and refusing the types.
s" EV-C-OK ( CAD-KIND:evidence-id -- EVIDENCE:id-result<CAD-KIND:evidence-id> ) EVIDENCE-ID--RESULT:OK" YES
s" EV-C-WW ( -- EVIDENCE:id-result<CAD-KIND:evidence-id> ) EVIDENCE-ID--RESULT:WRONG-WIDTH" YES
s" EV-C-UNK ( -- EVIDENCE:id-result<CAD-KIND:evidence-id> ) EVIDENCE-ID--RESULT:UNKNOWN" YES
\ Forge negatives on the ok payload slot: a raw cell cannot fill it, the result is
\ not a bare scalar, the payload is mandatory (a payloadless ok is not constructible),
\ and a same-width FOREIGN identity role cannot stand in for the evidence id.
s" EV-C-RAW ( n -- EVIDENCE:id-result<CAD-KIND:evidence-id> ) EVIDENCE-ID--RESULT:OK" NO
s" EV-C-BARE ( CAD-KIND:evidence-id -- n ) EVIDENCE-ID--RESULT:OK" NO
s" EV-C-NONE ( -- EVIDENCE:id-result<CAD-KIND:evidence-id> ) EVIDENCE-ID--RESULT:OK" NO
s" EV-C-FGN ( CAD-KIND:obligation-id -- EVIDENCE:id-result<CAD-KIND:evidence-id> ) EVIDENCE-ID--RESULT:OK" NO

public

\ idr-twin is EVIDENCE:id-result's SHAPE under a different name: same arity, same
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

s" EV-C-TWIN ( CAD-KIND:evidence-id -- idr-twin<CAD-KIND:evidence-id> ) EVIDENCE--TEST-IDR--TWIN:OK" YES
s" EV-C-TWIN-X1 ( CAD-KIND:evidence-id -- idr-twin<CAD-KIND:evidence-id> ) EVIDENCE-ID--RESULT:OK" NO
s" EV-C-TWIN-X2 ( CAD-KIND:evidence-id -- EVIDENCE:id-result<CAD-KIND:evidence-id> ) EVIDENCE--TEST-IDR--TWIN:OK" NO

;package

\ Nominal-id corruption seam: a bad CAD-KIND:evidence-id is only mintable via the
\ private refinement, so reopen the owning package for the E-EVIDENCE-ID negatives
\ and the wire round-trip over registry internals (an out-of-range wire raw is only
\ forgeable inside the owning package).
package EVIDENCE

1024 constant TT-WCAP
create TT-WBUF TT-WCAP allot

: TT-ID-NEG ( -- )  -1 RAW>EVIDENCE-ID VALIDATE drop ;
: TT-ID-BIG ( -- )  EVR-N @ 100 + RAW>EVIDENCE-ID DESCRIPTOR$ 2drop ;

: TT-WIRE-RT ( CAD-KIND:evidence-id -- n )      \ 0 = round-trips to an EQUAL? id
   dup {: orig:CAD-KIND:evidence-id :}
   TT-WBUF TT-WCAP ID>WIRE {: len:n :}
   TT-WBUF len WIRE>ID
   MATCH id-result
      ok          OF orig EQUAL? if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-WIRE-ALL ( -- n )                          \ 0 iff EVERY registered evidence round-trips
   EVR-N @ 0 ?do
      i RAW>EVIDENCE-ID TT-WIRE-RT 0<> if 1 unloop exit then
   loop 0 ;

: TT-WIRE-WIDTH ( -- n )                        \ a 4-byte buffer decodes as wrong-width
   TT-WBUF 4 WIRE>ID
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-WIRE-UNKNOWN ( -- n )                      \ an out-of-range raw decodes as unknown
   EVR-N @ 100 +  TT-WBUF WIRE-BYTES LE-PUT
   TT-WBUF WIRE-BYTES WIRE>ID
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

\ ---- cross-process content-key codec (KEY>WIRE / WIRE>KEY) ---------------------
create TT-SHA CK-BYTES allot

: TT-CKEY-RT ( CAD-KIND:evidence-id -- n )      \ 0 = content key round-trips to an EQUAL? id
   dup {: orig:CAD-KIND:evidence-id :}
   TT-WBUF TT-WCAP KEY>WIRE {: len:n :}
   TT-WBUF len WIRE>KEY
   MATCH id-result
      ok          OF orig EQUAL? if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-CKEY-ALL ( -- n )                          \ 0 iff EVERY registered evidence key round-trips
   EVR-N @ 0 ?do
      i RAW>EVIDENCE-ID TT-CKEY-RT 0<> if 1 unloop exit then
   loop 0 ;

: TT-CKEY-WIDTH ( -- n )                        \ an 8-byte buffer decodes as wrong-width
   TT-WBUF 8 WIRE>KEY
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-FILL-FF ( -- )                             \ 32 bytes no registered descriptor can hash to
   0 begin dup CK-BYTES < while
      dup {: k:n :}
      $FF  TT-WBUF k +  c!
      1+
   repeat drop ;

: TT-CKEY-UNKNOWN ( -- n )                      \ a 32-byte non-registered key decodes as unknown
   TT-FILL-FF
   TT-WBUF CK-BYTES WIRE>KEY
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-CKEY-IS-SHA ( -- n )                       \ 0 iff KEY>WIRE == SHA-256(descriptor), NOT the raw index
   0 RAW>EVIDENCE-ID {: id:CAD-KIND:evidence-id :}
   id DESCRIPTOR$ TT-SHA SHA256
   id TT-WBUF TT-WCAP KEY>WIRE drop
   TT-WBUF TT-SHA CK-EQ? if 0 else 1 then ;

\ ---- every variant constructs and dispatches through MATCH ---------------------
\ The wire words above reach the arms only through a decode. These construct each
\ variant DIRECTLY through the production producers and match it straight back, so
\ the named payload FIELD is proven to bind in declaration order. The ok arm binds
\ its payload to a TYPED local and reports the recovered registry raw, which is
\ exactly what EQUAL? compares (content-addressed identity IS raw equality), so a
\ payload the constructor dropped or zeroed would come back as a different raw
\ instead of passing.
\
\ Construction is factored into one typed word per variant because the checker
\ requires MATCH's scrutinee to be a concretely instantiated family value: a single
\ word that both constructs and matches is refused, and the diagnostic names the
\ family token as an undefined word. That refusal predates this migration (it
\ reproduces identically on the legacy declaration) and is reported separately.
: TT-MK-OK ( CAD-KIND:evidence-id -- id-result<CAD-KIND:evidence-id> ) R-OK ;
: TT-MK-WW ( -- id-result<CAD-KIND:evidence-id> )   R-WRONG-WIDTH ;
: TT-MK-UNK ( -- id-result<CAD-KIND:evidence-id> )  R-UNKNOWN ;

: TT-ARM ( id-result<CAD-KIND:evidence-id> -- n )   \ 1 ok, 2 wrong-width, 3 unknown
   MATCH id-result
      ok          OF drop 1 ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-OK-RAW ( id-result<CAD-KIND:evidence-id> -- n )   \ ok payload's registry raw, else -1
   MATCH id-result
      ok          OF {: got:CAD-KIND:evidence-id :} got EVIDENCE-ID>RAW ENDOF
      wrong-width OF -1 ENDOF
      unknown     OF -1 ENDOF
   ;MATCH ;

: TT-RT-OK-ARM ( -- n )                         \ a constructed ok reaches the ok arm
   s" subj=art9|dom=exact|rel=match-arm|env=sm87|ver=diff|vc=differential-exec"
   REGISTER TT-MK-OK TT-ARM ;
: TT-RT-OK-RAW ( -- n )                         \ 0 = the registered id came back unchanged
   s" subj=art9|dom=exact|rel=match-payload|env=sm87|ver=diff|vc=differential-exec"
   REGISTER dup EVIDENCE-ID>RAW {: want:n :}
   TT-MK-OK TT-OK-RAW want = if 0 else 1 then ;
: TT-RT-WW ( -- n )   TT-MK-WW TT-ARM ;
: TT-RT-UNK ( -- n )  TT-MK-UNK TT-ARM ;
: TT-WW-RAW ( -- n )  TT-MK-WW TT-OK-RAW ;      \ a payloadless arm carries no raw

' TT-ID-NEG E-EVIDENCE-ID TTHROWS
' TT-ID-BIG E-EVIDENCE-ID TTHROWS
TT-WIRE-ALL 0 T=
TT-WIRE-WIDTH 2 T=
TT-WIRE-UNKNOWN 3 T=
TT-CKEY-ALL 0 T=
TT-CKEY-WIDTH 2 T=
TT-CKEY-UNKNOWN 3 T=
TT-CKEY-IS-SHA 0 T=
TT-RT-OK-ARM 1 T=                               \ ok dispatches to its own arm
TT-RT-OK-RAW 0 T=                               \ and carries its payload through unchanged
TT-RT-WW 2 T=                                   \ wrong-width dispatches to its own arm
TT-RT-UNK 3 T=                                  \ unknown dispatches to its own arm
TT-WW-RAW -1 T=                                 \ the no-payload arms of TT-OK-RAW are live

;package

T-REPORT
