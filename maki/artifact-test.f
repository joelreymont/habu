\ artifact-test.f - built-artifact identity registry acceptance (dot
\ habu-public-producers-for-7084d81c).
\
\ Pins maki/artifact.f: REGISTER interns by store key (equal keys -> one id), KEY$
\ round-trips, EQUAL? is same-artifact identity, COUNT counts distinct artifacts, and
\ the id family / private mint are unforgeable. CHECK-QUIET-CANDIDATE! returns -1
\ certified / 0 type-reject / 1 unresolvable (test/checker-assert.f); every negative is
\ paired with a resolving positive control (per LESSONS).

require lib/test.f
require test/checker-assert.f
require maki/artifact.f

\ ---- executed registry behaviour ---------------------------------------------
\ interning: registering the same key twice yields ONE id (EQUAL? true); distinct
\ keys yield distinct ids (EQUAL? false).
: AT-SAME? ( -- bool )  s" at-k1" ARTIFACT:REGISTER s" at-k1" ARTIFACT:REGISTER ARTIFACT:EQUAL? ;
: AT-DIFF? ( -- bool )  s" at-k1" ARTIFACT:REGISTER s" at-k2" ARTIFACT:REGISTER ARTIFACT:EQUAL? ;

\ KEY$ projects an id back to the exact registered key.
: AT-KEY-ROUND ( -- )  s" at-round-key" ARTIFACT:REGISTER ARTIFACT:KEY$ s" at-round-key" T$= ;

\ COUNT rises by one for a fresh key, and not at all for a duplicate.
: AT-COUNT? ( -- bool )
   ARTIFACT:COUNT {: c0:n :}
   s" at-count-uniq" ARTIFACT:REGISTER drop
   ARTIFACT:COUNT {: c1:n :}
   s" at-count-uniq" ARTIFACT:REGISTER drop
   ARTIFACT:COUNT {: c2:n :}
   c1 c0 1+ =  c2 c1 =  and ;

\ ---- fail-closed boundaries (reopen ARTIFACT to reach the private mint) -------
create AT-BUF 8 allot
: AT-EMPTY ( -- )  AT-BUF 0 ARTIFACT:REGISTER drop ;   \ empty key rejects

package ARTIFACT
public
: AT-ID-NEG ( -- )  -1 RAW>ARTIFACT-ID VALIDATE-ID drop ;   \ negative id rejects
: AT-ID-BIG ( -- )  9999 RAW>ARTIFACT-ID KEY$ 2drop ;      \ out-of-range id rejects

1024 constant AT-WCAP
create AT-WBUF AT-WCAP allot
create AT-SHA CK-BYTES allot

: AT-CKEY-RT ( CAD-KIND:artifact-id -- n )     \ 0 = content key round-trips to an EQUAL? id
   dup {: orig:CAD-KIND:artifact-id :}
   AT-WBUF AT-WCAP KEY>WIRE {: len:n :}
   AT-WBUF len WIRE>KEY
   MATCH id-result
      ok          OF orig EQUAL? if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: AT-CKEY-ALL ( -- n )                         \ 0 iff EVERY registered artifact key round-trips
   COUNT 0 ?do
      i RAW>ARTIFACT-ID AT-CKEY-RT 0<> if 1 unloop exit then
   loop 0 ;

: AT-CKEY-WIDTH ( -- n )                       \ an 8-byte buffer decodes as wrong-width
   AT-WBUF 8 WIRE>KEY
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: AT-FILL-FF ( -- )                            \ 32 bytes no registered store key can hash to
   0 begin dup CK-BYTES < while
      dup {: k:n :}
      $FF  AT-WBUF k +  c!
      1+
   repeat drop ;

: AT-CKEY-UNKNOWN ( -- n )                     \ a 32-byte non-registered key decodes as unknown
   AT-FILL-FF
   AT-WBUF CK-BYTES WIRE>KEY
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: AT-CKEY-IS-SHA ( -- n )                      \ 0 iff KEY>WIRE == SHA-256(store key), NOT the raw index
   0 RAW>ARTIFACT-ID {: id:CAD-KIND:artifact-id :}
   id KEY$ AT-SHA SHA256
   id AT-WBUF AT-WCAP KEY>WIRE drop
   AT-WBUF AT-SHA CK-EQ? if 0 else 1 then ;
;package

T-RESET

\ ---- type candidates: the public producer surface certifies -------------------
s" AR-OK-REGISTER ( ptr u8 n -- CAD-KIND:artifact-id ) ARTIFACT:REGISTER"
   CHECK-QUIET-CANDIDATE! -1 T=
s" AR-OK-EQUAL ( CAD-KIND:artifact-id CAD-KIND:artifact-id -- bool ) ARTIFACT:EQUAL?"
   CHECK-QUIET-CANDIDATE! -1 T=
s" AR-OK-KEY ( CAD-KIND:artifact-id -- ptr u8 n ) ARTIFACT:KEY$"
   CHECK-QUIET-CANDIDATE! -1 T=
s" AR-OK-VALIDATE-ID ( CAD-KIND:artifact-id -- CAD-KIND:artifact-id ) ARTIFACT:VALIDATE-ID"
   CHECK-QUIET-CANDIDATE! -1 T=
s" AR-OK-KEYWIRE ( CAD-KIND:artifact-id ptr u8 n -- n ) ARTIFACT:KEY>WIRE"
   CHECK-QUIET-CANDIDATE! -1 T=

\ ---- forge / mis-bind negatives ----------------------------------------------
\ a foreign same-width id family cannot stand in for an artifact id -> reject.
s" AR-BAD-EQUAL ( CAD-KIND:schema-id CAD-KIND:artifact-id -- bool ) ARTIFACT:EQUAL?"
   CHECK-QUIET-CANDIDATE! 0 T=
\ a foreign id cannot content-key-encode as an artifact id -> reject.
s" AR-BAD-KEYWIRE ( CAD-KIND:schema-id ptr u8 n -- n ) ARTIFACT:KEY>WIRE"
   CHECK-QUIET-CANDIDATE! 0 T=
\ the raw artifact-id mint is PRIVATE to ARTIFACT: unresolvable outside its owner (1),
\ so no caller can forge an artifact id around REGISTER.
s" AR-BAD-MINT ( n -- CAD-KIND:artifact-id ) RAW>ARTIFACT-ID"
   CHECK-QUIET-CANDIDATE! 1 T=

\ ---- executed asserts --------------------------------------------------------
AT-SAME? T-ASSERT
AT-DIFF? 0= T-ASSERT
AT-KEY-ROUND
AT-COUNT? T-ASSERT
' AT-EMPTY E-ARTIFACT-KEY TTHROWS
' ARTIFACT:AT-ID-NEG E-ARTIFACT-ID TTHROWS
' ARTIFACT:AT-ID-BIG E-ARTIFACT-ID TTHROWS

\ ---- cross-process content-key codec (KEY>WIRE / WIRE>KEY) --------------------
ARTIFACT:AT-CKEY-ALL 0 T=
ARTIFACT:AT-CKEY-WIDTH 2 T=
ARTIFACT:AT-CKEY-UNKNOWN 3 T=
ARTIFACT:AT-CKEY-IS-SHA 0 T=

T-REPORT
