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

' TT-ID-NEG E-CONFIG-ID TTHROWS
' TT-ID-BIG E-CONFIG-ID TTHROWS
TT-WIRE-ALL 0 T=
TT-WIRE-WIDTH 2 T=
TT-WIRE-UNKNOWN 3 T=
TT-CKEY-ALL 0 T=
TT-CKEY-WIDTH 2 T=
TT-CKEY-UNKNOWN 3 T=
TT-CKEY-IS-SHA 0 T=

;package

T-REPORT
