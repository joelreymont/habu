\ maki/rev-test.f - checked acceptance for the rev-id identity family (maki/rev.f,
\ dot habu-v2-txn-journal-d0bc644f). Covers the § 23.9 per-family contract for the
\ content-addressed origin class: deterministic COMMIT (equal revision content shares
\ one id, so replaying the same transaction reproduces the rev-id), the ID>WIRE /
\ WIRE>ID round-trip, fail-closed decode (wrong width + unresolved raw), cross-role
\ rejection, and privacy (no public raw cast). Every refusal is paired with a
\ resolving positive control so no TTHROWS is vacuous.

require lib/test.f
require test/checker-assert.f
require maki/rev.f

package REV-TEST

variable BASE-N

: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

: RG ( ptr u8 n -- CAD-KIND:rev-id )  REV:COMMIT ;

: BAD-EMPTY ( -- )   s" " REV:COMMIT drop ;

T-RESET

\ ---- content-addressed COMMIT: equal content one id; distinct content don't ----
REV:COUNT BASE-N !
s" parent=- writes=obj-a,obj-b" RG
s" parent=- writes=obj-a,obj-b" RG REV:EQUAL? TTRUE               \ equal revision content interns to one id
REV:COUNT BASE-N @ 1+ T=                                          \ only one new slot
s" parent=- writes=obj-a,obj-b" RG REV:CONTENT$ s" parent=- writes=obj-a,obj-b" T$=
s" parent=r0 writes=obj-a,obj-b" RG
s" parent=- writes=obj-a,obj-b" RG REV:EQUAL? TFALSE             \ distinct parent, distinct id
REV:COUNT BASE-N @ 2 + T=

\ ---- validate + fail-closed empty content string -----------------------------
s" parent=- writes=obj-a,obj-b" RG REV:VALIDATE REV:CONTENT$ s" parent=- writes=obj-a,obj-b" T$=
' BAD-EMPTY E-REV-KEY TTHROWS

\ ---- checker: cross-role rejection + privacy (no public raw cast) -------------
s" RV-OK ( CAD-KIND:rev-id -- CAD-KIND:rev-id ) REV:VALIDATE" YES
s" RV-CONTENT ( CAD-KIND:rev-id -- ptr u8 n ) REV:CONTENT$" YES
s" RV-KW ( CAD-KIND:rev-id ptr u8 n -- n ) REV:KEY>WIRE" YES               \ content-key encode
s" RV-XA ( CAD-KIND:artifact-id -- CAD-KIND:rev-id ) REV:VALIDATE" NO      \ artifact-id is not a rev-id
s" RV-XT ( CAD-KIND:target-id -- ptr u8 n ) REV:CONTENT$" NO               \ target-id is not a rev-id
s" RV-XKW ( CAD-KIND:artifact-id ptr u8 n -- n ) REV:KEY>WIRE" NO          \ a foreign id cannot encode
s" REV:RAW>REV-ID" 0 search-wl 0= TTRUE                                     \ mint is private
s" REV:REV-ID>RAW" 0 search-wl 0= TTRUE                                     \ projection is private

;package

\ Nominal-id corruption seam: a bad CAD-KIND:rev-id is only mintable via the private
\ refinement, so reopen the owning package for the E-REV-ID negatives and the wire
\ round-trip over registry internals (an out-of-range wire raw is only forgeable
\ inside the owning package).
package REV

1024 constant TT-WCAP
create TT-WBUF TT-WCAP allot

: TT-ID-NEG ( -- )  -1 RAW>REV-ID VALIDATE drop ;
: TT-ID-BIG ( -- )  REV-N @ 100 + RAW>REV-ID CONTENT$ 2drop ;

: TT-WIRE-RT ( CAD-KIND:rev-id -- n )           \ 0 = round-trips to an EQUAL? id
   dup {: orig:CAD-KIND:rev-id :}
   TT-WBUF TT-WCAP ID>WIRE {: len:n :}
   TT-WBUF len WIRE>ID
   MATCH id-result
      ok          OF orig EQUAL? if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-WIRE-ALL ( -- n )                          \ 0 iff EVERY committed revision round-trips
   REV-N @ 0 ?do
      i RAW>REV-ID TT-WIRE-RT 0<> if 1 unloop exit then
   loop 0 ;

: TT-WIRE-WIDTH ( -- n )                        \ a 4-byte buffer decodes as wrong-width
   TT-WBUF 4 WIRE>ID
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-WIRE-UNKNOWN ( -- n )                      \ an out-of-range raw decodes as unknown
   REV-N @ 100 +  TT-WBUF WIRE-BYTES LE-PUT
   TT-WBUF WIRE-BYTES WIRE>ID
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

\ ---- cross-process content-key codec (KEY>WIRE / WIRE>KEY) ---------------------
create TT-SHA CK-BYTES allot

: TT-CKEY-RT ( CAD-KIND:rev-id -- n )          \ 0 = content key round-trips to an EQUAL? id
   dup {: orig:CAD-KIND:rev-id :}
   TT-WBUF TT-WCAP KEY>WIRE {: len:n :}
   TT-WBUF len WIRE>KEY
   MATCH id-result
      ok          OF orig EQUAL? if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-CKEY-ALL ( -- n )                         \ 0 iff EVERY committed revision key round-trips
   REV-N @ 0 ?do
      i RAW>REV-ID TT-CKEY-RT 0<> if 1 unloop exit then
   loop 0 ;

: TT-CKEY-WIDTH ( -- n )                       \ an 8-byte buffer decodes as wrong-width
   TT-WBUF 8 WIRE>KEY
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-FILL-FF ( -- )                            \ 32 bytes no committed revision can hash to
   0 begin dup CK-BYTES < while
      dup {: k:n :}
      $FF  TT-WBUF k +  c!
      1+
   repeat drop ;

: TT-CKEY-UNKNOWN ( -- n )                     \ a 32-byte non-registered key decodes as unknown
   TT-FILL-FF
   TT-WBUF CK-BYTES WIRE>KEY
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-CKEY-IS-SHA ( -- n )                      \ 0 iff KEY>WIRE == SHA-256(content), NOT the raw index
   0 RAW>REV-ID {: id:CAD-KIND:rev-id :}
   id CONTENT$ TT-SHA SHA256
   id TT-WBUF TT-WCAP KEY>WIRE drop
   TT-WBUF TT-SHA CK-EQ? if 0 else 1 then ;

' TT-ID-NEG E-REV-ID TTHROWS
' TT-ID-BIG E-REV-ID TTHROWS
TT-WIRE-ALL 0 T=
TT-WIRE-WIDTH 2 T=
TT-WIRE-UNKNOWN 3 T=
TT-CKEY-ALL 0 T=
TT-CKEY-WIDTH 2 T=
TT-CKEY-UNKNOWN 3 T=
TT-CKEY-IS-SHA 0 T=

;package

T-REPORT
