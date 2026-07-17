\ maki/schema-test.f - checked acceptance for the schema-id identity family
\ (maki/schema.f, dot habu-schema-schema-id-3a6827e9). Covers the § 23.9 per-family
\ contract: content-addressed interning (equal names share one id), the ID>WIRE /
\ WIRE>ID round-trip, fail-closed decode (wrong width + unresolved raw), cross-role
\ rejection (a schema-id API cannot take a foreign nominal), and privacy (no public
\ raw cast). Every refusal is paired with a resolving positive control so no
\ TTHROWS is vacuous.

require lib/test.f
require test/checker-assert.f
require maki/schema.f

package SCHEMA-TEST

variable BASE-N

: YES ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE! -1 T= ;
: NO  ( ptr u8 n -- )   CHECK-QUIET-CANDIDATE!  0 T= ;

: REG ( ptr u8 n -- CAD-KIND:schema-id )  SCHEMA:REGISTER ;

: BAD-EMPTY ( -- )   s" " SCHEMA:REGISTER drop ;

T-RESET

\ ---- content-addressed interning: equal names share one id, distinct names don't ----
SCHEMA:COUNT BASE-N !
s" promotion-policy" REG
s" promotion-policy" REG SCHEMA:EQUAL? TTRUE           \ equal name interns to one id
SCHEMA:COUNT BASE-N @ 1+ T=                             \ only one new slot
s" promotion-policy" REG SCHEMA:NAME$ s" promotion-policy" T$=
s" bench/v2" REG
s" promotion-policy" REG SCHEMA:EQUAL? TFALSE           \ distinct name, distinct id
SCHEMA:COUNT BASE-N @ 2 + T=

\ ---- validate + fail-closed empty name ------------------------------------------
s" promotion-policy" REG SCHEMA:VALIDATE SCHEMA:NAME$ s" promotion-policy" T$=
' BAD-EMPTY E-SCHEMA-KEY TTHROWS

\ ---- checker: cross-role rejection + privacy (no public raw cast) ---------------
s" SC-OK ( CAD-KIND:schema-id -- CAD-KIND:schema-id ) SCHEMA:VALIDATE" YES
s" SC-NAME ( CAD-KIND:schema-id -- ptr u8 n ) SCHEMA:NAME$" YES
s" SC-XA ( CAD-KIND:artifact-id -- ptr u8 n ) SCHEMA:NAME$" NO       \ artifact-id is not a schema-id
s" SC-XP ( CAD-KIND:producer-id -- CAD-KIND:schema-id ) SCHEMA:VALIDATE" NO
s" SCHEMA:RAW>SCHEMA-ID" 0 search-wl 0= TTRUE                         \ mint is private
s" SCHEMA:SCHEMA-ID>RAW" 0 search-wl 0= TTRUE                         \ projection is private

;package

\ Nominal-id corruption seam: a bad CAD-KIND:schema-id is only mintable via the
\ private refinement, so reopen the owning package for the E-SCHEMA-ID negatives
\ and the wire round-trip over registry internals (an out-of-range wire raw is only
\ forgeable inside the owning package).
package SCHEMA

1024 constant TT-WCAP
create TT-WBUF TT-WCAP allot

: TT-ID-NEG ( -- )  -1 RAW>SCHEMA-ID VALIDATE drop ;
: TT-ID-BIG ( -- )  SCH-N @ 100 + RAW>SCHEMA-ID NAME$ 2drop ;

: TT-WIRE-RT ( CAD-KIND:schema-id -- n )        \ 0 = round-trips to an EQUAL? id
   dup {: orig:CAD-KIND:schema-id :}
   TT-WBUF TT-WCAP ID>WIRE {: len:n :}
   TT-WBUF len WIRE>ID
   MATCH id-result
      ok          OF orig EQUAL? if 0 else 1 then ENDOF
      wrong-width OF 2 ENDOF
      unknown     OF 3 ENDOF
   ;MATCH ;

: TT-WIRE-ALL ( -- n )                          \ 0 iff EVERY registered schema round-trips
   SCH-N @ 0 ?do
      i RAW>SCHEMA-ID TT-WIRE-RT 0<> if 1 unloop exit then
   loop 0 ;

: TT-WIRE-WIDTH ( -- n )                        \ a 4-byte buffer decodes as wrong-width
   TT-WBUF 4 WIRE>ID
   MATCH id-result  ok OF drop 8 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

: TT-WIRE-UNKNOWN ( -- n )                      \ an out-of-range raw decodes as unknown
   SCH-N @ 100 +  TT-WBUF WIRE-BYTES LE-PUT
   TT-WBUF WIRE-BYTES WIRE>ID
   MATCH id-result  ok OF drop 9 ENDOF  wrong-width OF 2 ENDOF  unknown OF 3 ENDOF  ;MATCH ;

' TT-ID-NEG E-SCHEMA-ID TTHROWS
' TT-ID-BIG E-SCHEMA-ID TTHROWS
TT-WIRE-ALL 0 T=
TT-WIRE-WIDTH 2 T=
TT-WIRE-UNKNOWN 3 T=

;package

T-REPORT
