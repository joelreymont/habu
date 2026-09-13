\ Published named rows stay reusable after quotation inference.
require lib/errors.f
require lib/string.f
require lib/test.f
require lib/test/outcome.f
require lib/test/subject.f

package PROVIDER-ROW-TEST

$1000 constant CAP
20000 constant TIMEOUT-MS
create OUT CAP allot
create ERR CAP allot
create SRC-BUF CAP allot
variable SRC-U
variable OUT-U
variable ERR-U
variable RC
variable EXITED

: OUT$ ( -- ptr u8 n ) OUT OUT-U @ ;
: ERR$ ( -- ptr u8 n ) ERR ERR-U @ ;

: APPEND ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < u CAP SRC-U @ - > or if E-STR-CAPACITY throw then
   a SRC-BUF SRC-U @ + u >LEN BYTE-COPY-LEN
   u SRC-U +! ;

: STORE ( len len outcome -- )
   MATCH outcome
      exited OF RC ! true EXITED ! ENDOF
      signaled OF RC ! false EXITED ! ENDOF
      timeout OF 0 RC ! false EXITED ! ENDOF
   ;MATCH
   LEN>N ERR-U ! LEN>N OUT-U ! ;

\ Each child publishes the same providers once, then uses them at a separately
\ selected caller tier. Both empty and nonempty instantiations use those records.
: PROVIDERS$ ( -- ptr u8 n )
   S\" package PR\nvariable CLEANUPS\npublic\n: DIRECT ( R [ R -- S ] -- S ) execute ;\n: QUOTED ( R [ R -- S ] -- S ) [: DIRECT ;] execute ;\n: CLEAN ( -- ) 1 CLEANUPS +! ;\n: FINISHED ( R [ R -- S ] -- S ) [: CLEAN ;] finally ;\n: CLEAN-COUNT ( -- n ) CLEANUPS @ ;\n: DOUBLE ( R n [ R n n -- S ] -- S ) {: body :} dup body execute ;\n: WINDOW ( R n [ R n n -- S ] -- S ) [: DOUBLE ;] execute ;\n: HIGHER ( R [ R -- S ] [ S -- T ] -- T ) {: after :} QUOTED after QUOTED ;\n: RB1 ( R -- R ) dup drop ;\n: RB3 ( R -- R ) 1 + ;\nTRUSTED: DECLARED ( n -- n ) 1 + ;\n: ANON ( R [ n -- n ] -- R [ n -- n ] ) [: ;] execute ;\n;package\n" ;

\ RB1/RB3 retain the fixed input that the existing checker infers for their
\ declared row: restoring the whole pre-body scheme would incorrectly erase it.
: CALLERS$ ( -- ptr u8 n )
   S\" package PC\npublic\n: D-EMPTY ( [ -- ] -- ) PR:DIRECT ;\n: D-PREFIX ( x [ x -- x ] -- x ) PR:DIRECT ;\n: Q-EMPTY ( [ -- ] -- ) PR:QUOTED ;\n: Q-PREFIX ( x [ x -- x ] -- x ) PR:QUOTED ;\n: F-EMPTY ( [ -- ] -- ) PR:FINISHED ;\n: F-PREFIX ( x [ x -- x ] -- x ) PR:FINISHED ;\n: W-EMPTY ( n [ n n -- ] -- ) PR:WINDOW ;\n: W-PREFIX ( x n [ x n n -- x ] -- x ) PR:WINDOW ;\n: H-PREFIX ( x [ x -- y ] [ y -- z ] -- z ) PR:HIGHER ;\n: ANON-GOOD ( x [ n -- n ] -- x [ n -- n ] ) PR:ANON ;\n: RUN ( -- )\n   [: ;] D-EMPTY 17 [: ;] D-PREFIX .\n   [: ;] Q-EMPTY 19 [: ;] Q-PREFIX .\n   [: ;] F-EMPTY 23 [: ;] F-PREFIX .\n   5 [: 2drop ;] W-EMPTY 29 5 [: 2drop ;] W-PREFIX .\n   31 [: 1+ ;] [: 2 * ;] H-PREFIX .\n   37 PR:RB1 . 41 PR:RB3 . 46 PR:DECLARED .\n   43 [: 1+ ;] ANON-GOOD execute .\n   PR:CLEAN-COUNT . ;\n;package\nPC:RUN\n" ;

: TIER ( n -- )
   0= if s" 0 set-tier " else s" 1 set-tier " then APPEND ;

: RUN-SOURCE ( ptr u8 n n n -- ) {: src:ptr u:n provider:n caller:n :}
   0 SRC-U !
   provider TIER PROVIDERS$ APPEND
   caller TIER src u APPEND
   SRC-BUF SRC-U @ OUT CAP >LEN ERR CAP >LEN TIMEOUT-MS >MS SUBJECT:RUN STORE ;

: EXPECT-RC ( n -- ) {: want:n :}
   EXITED @ 0= RC @ want <> or if ERR$ type OUT$ type then
   EXITED @ TTRUE RC @ want T= ;

: POSITIVE ( n n -- )
   s" direct, quotation, finally, fixed windows and higher-order named rows" T-LABEL
   CALLERS$ 2swap RUN-SOURCE
   0 EXPECT-RC ERR-U @ 0 T=
   OUT$ S\" 17\n19\n23\n29\n64\n37\n42\n47\n44\n2\n" STR= TTRUE ;

: REJECT ( ptr u8 n n n -- )
   RUN-SOURCE
   70 EXPECT-RC OUT-U @ 0 T=
   ERR$ s" non-certified definition: bad" CONTAINS? TTRUE ;

\ Read both the stored effect and the dictionary's interpret guard without
\ making an underdepth call. The query boundary alone needs trusted access.
: MINIMUM ( n -- )
   s" verified input cells reach both the effect and the interpret guard" T-LABEL
   S\" TRUSTED: ROW-MINIMUM ( ptr u8 n -- )\n   2dup XREF-FIND dup XREF-FOUND? if XREF-FLAGS 52 rshift $FF and . else drop -1 . then\n   2dup SIG-MIN-IN .\n   EFFECT-QUERY if EFFECT-DIN-CELLS . else -1 . then ;\ns\" PR:RB1\" ROW-MINIMUM s\" PR:RB3\" ROW-MINIMUM\n" rot 0 RUN-SOURCE
   0 EXPECT-RC ERR-U @ 0 T=
   OUT$ S\" 1\n1\n1\n1\n1\n1\n" STR= TTRUE ;

: NEGATIVE ( n n -- ) {: provider:n caller:n :}
   s" typed callback mismatches still reject through every provider form" T-LABEL
   s" : BAD ( ptr u8 -- n ) [: 1+ ;] PR:DIRECT ;" provider caller REJECT
   s" : BAD ( ptr u8 -- n ) [: 1+ ;] PR:QUOTED ;" provider caller REJECT
   s" : BAD ( ptr u8 -- n ) [: 1+ ;] PR:FINISHED ;" provider caller REJECT
   s" : BAD ( ptr u8 -- n ) 5 [: + + ;] PR:WINDOW ;" provider caller REJECT
   s" independent anonymous callback windows retain their fixed kind" T-LABEL
   s" : BAD ( x [ x n -- x n ] -- x [ x n -- x n ] ) PR:ANON ;" provider caller REJECT
   s" cleanup borrow restrictions remain enforced" T-LABEL
   s" : BAD ( -- ) [: ;] [: 1+ ;] finally ;" provider caller REJECT
   s" verified fixed input types survive provider publication in both tiers" T-LABEL
   s" : BAD ( ptr u8 -- ptr u8 ) PR:RB3 ;" provider caller REJECT
   ERR$ s" expected: n actual: ptr u8" CONTAINS? TTRUE
   s" explicit trusted signatures still reach both tiers" T-LABEL
   s" : BAD ( ptr u8 -- ptr u8 ) PR:DECLARED ;" provider caller REJECT ;

: JIT-DEFINER ( -- )
   s" created-body validation preserves the JIT definer's verified effect" T-LABEL
   S\" : ROW-MAKE ( R -- R ) 1 + create does> ( -- ptr a ) ;\nTRUSTED: DEF-MINIMUM ( -- )\n   s\" ROW-MAKE\" SIG-MIN-IN .\n   s\" ROW-MAKE\" XREF-FIND dup XREF-FOUND? if XREF-FLAGS 52 rshift $FF and . else drop -1 . then ;\nDEF-MINIMUM\n41 ROW-MAKE MADE .\nMADE drop\n: BAD ( ptr u8 -- ptr u8 ) ROW-MAKE ;\n" 0 0 RUN-SOURCE
   70 EXPECT-RC
   OUT$ S\" 1\n1\n42\n" STR= TTRUE
   ERR$ s" expected: n actual: ptr u8" CONTAINS? TTRUE
   ERR$ s" non-certified definition: bad" CONTAINS? TTRUE ;

public

: RUN ( -- )
   T-RESET
   2 0 do
      2 0 do j i POSITIVE j i NEGATIVE loop
      i MINIMUM
   loop
   JIT-DEFINER
   T-REPORT ;

;package

PROVIDER-ROW-TEST:RUN
