\ source-compose-test.f - exact modular-source composition fixtures.

require lib/test.f
require lib/fs.f
require lib/fs-mutate.f
require tools/source-compose.f

package SOURCE-COMPOSE-TEST

private

FS-PATH-CAP constant PATH-CAP
10 constant TEST-LF
$20 constant DIGEST-U

create ROOT PATH-CAP allot
create ENTRY PATH-CAP allot
create DEP-A PATH-CAP allot
create DEP-B PATH-CAP allot
create SPECIAL PATH-CAP allot
create MISSING PATH-CAP allot
variable ROOT-U
variable ENTRY-U
variable DEP-A-U
variable DEP-B-U
variable SPECIAL-U
variable MISSING-U
variable SUB-N
variable SUB-I
PTR-VARIABLE BIG-A
create MAP-DG-A DIGEST-U allot
create MAP-DG-B DIGEST-U allot
create PLAN-DG-A DIGEST-U allot
create PLAN-DG-B DIGEST-U allot
create SOURCE-DG-A DIGEST-U allot
create SOURCE-DG-B DIGEST-U allot

: ROOT$ ( -- ptr u8 n )
   ROOT ROOT-U @ ;

: ENTRY$ ( -- ptr u8 n )
   ENTRY ENTRY-U @ ;

: DEP-A$ ( -- ptr u8 n )
   DEP-A DEP-A-U @ ;

: DEP-B$ ( -- ptr u8 n )
   DEP-B DEP-B-U @ ;

: SPECIAL$ ( -- ptr u8 n )
   SPECIAL SPECIAL-U @ ;

: MISSING$ ( -- ptr u8 n )
   MISSING MISSING-U @ ;

: COPY$ ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u:n dst:ptr up:ptr :}
   a dst u BYTE-COPY
   u up ! ;

: PREP ( -- )
   CLEANUP-RESET
   s" habu-source-compose-test" TMPDIR-MKDIR ROOT ROOT-U COPY$
   ROOT$ CLEANUP-TREE+
   ROOT$ s" entry.f" ENTRY JOIN-PATH ENTRY-U !
   ROOT$ s" dep-a.f" DEP-A JOIN-PATH DEP-A-U !
   ROOT$ s" dep-b.f" DEP-B JOIN-PATH DEP-B-U !
   ROOT$ S\" dep space\qquote.f" SPECIAL JOIN-PATH SPECIAL-U !
   ROOT$ s" absent.f" MISSING JOIN-PATH MISSING-U ! ;

: WRITE-ENTRY ( ptr u8 n -- )
   ENTRY$ 2swap WRITE-ALL ;

: WRITE-A ( ptr u8 n -- )
   DEP-A$ 2swap WRITE-ALL ;

: WRITE-B ( ptr u8 n -- )
   DEP-B$ 2swap WRITE-ALL ;

: REQUIRE-SOURCE$ ( ptr u8 n -- ptr u8 n )
   SB-RESET
   s" require " SB-APPEND
   SB-APPEND
   TEST-LF SB-APPEND-C
   SB$ ;

: INCLUDE-SOURCE$ ( ptr u8 n -- ptr u8 n )
   SB-RESET
   s" include " SB-APPEND
   SB-APPEND
   TEST-LF SB-APPEND-C
   SB$ ;

: ESC-PATH+ ( ptr u8 n -- ) {: a:ptr u:n :}
   0 begin dup u < while
      a over + c@ {: c:n :}
      c $22 = if $5C SB-APPEND-C $71 SB-APPEND-C else
         c $5C = if $5C SB-APPEND-C $5C SB-APPEND-C else c SB-APPEND-C then
      then
      1+
   repeat drop ;

: ESC-REQUIRE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   $53 SB-APPEND-C $5C SB-APPEND-C $22 SB-APPEND-C $20 SB-APPEND-C
   a u ESC-PATH+
   $22 SB-APPEND-C s"  required" SB-APPEND TEST-LF SB-APPEND-C ;

: ESC-REQUIRE$ ( ptr u8 n -- ptr u8 n )
   SB-RESET
   ESC-REQUIRE+
   SB$ ;

: HEX-PATH$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   SB-RESET
   0 begin dup u < while
      a over + c@ {: c:n :}
      c 4 rshift $F and NIB>HEX SB-APPEND-C
      c $F and NIB>HEX SB-APPEND-C
      1+
   repeat drop
   SB$ ;

: BUILD-ENTRY ( -- )
   ENTRY$ SOURCE-COMPOSE:BUILD ;

: SUB-COUNT ( ptr u8 n ptr u8 n -- n ) {: a:ptr u:n b:ptr v:n :}
   v 0= if 0 exit then
   0 SUB-N !
   0 SUB-I !
   begin SUB-I @ u v - <= while
      a SUB-I @ + v b v STR= if
         SUB-N @ 1+ SUB-N !
         SUB-I @ v + SUB-I !
      else
         SUB-I @ 1+ SUB-I !
      then
   repeat
   SUB-N @ ;

: TEST-TOP ( -- )
   S\" : DEP-A-WORD ( -- n ) 17 ;\n" WRITE-A
   DEP-A$ REQUIRE-SOURCE$ WRITE-ENTRY
   BUILD-ENTRY
   SOURCE-COMPOSE:SOURCE$ s" DEP-A-WORD" CONTAINS? TTRUE
   SOURCE-COMPOSE:SOURCE$ s" require " CONTAINS? TFALSE
   SOURCE-COMPOSE:MAP$ s" HABUMAP1" STARTS-WITH? TTRUE ;

: TEST-TRANSITIVE ( -- )
   S\" : DEP-B-WORD ( -- n ) 23 ;\n" WRITE-B
   DEP-B$ REQUIRE-SOURCE$ WRITE-A
   DEP-A$ REQUIRE-SOURCE$ WRITE-ENTRY
   BUILD-ENTRY
   SOURCE-COMPOSE:SOURCE$ s" DEP-B-WORD" CONTAINS? TTRUE ;

: TEST-DUP-REQUIRE ( -- )
   S\" : DEP-A-ONCE ( -- n ) 29 ;\n" WRITE-A
   SB-RESET
   s" require " SB-APPEND DEP-A$ SB-APPEND TEST-LF SB-APPEND-C
   s" require " SB-APPEND DEP-A$ SB-APPEND TEST-LF SB-APPEND-C
   SB$ WRITE-ENTRY
   BUILD-ENTRY
   SOURCE-COMPOSE:SOURCE$ s" DEP-A-ONCE" SUB-COUNT 1 T= ;

: TEST-REPEAT-INCLUDE ( -- )
   S\" : DEP-A-TWICE ( -- n ) 31 ;\n" WRITE-A
   SB-RESET
   s" include " SB-APPEND DEP-A$ SB-APPEND TEST-LF SB-APPEND-C
   s" include " SB-APPEND DEP-A$ SB-APPEND TEST-LF SB-APPEND-C
   SB$ WRITE-ENTRY
   BUILD-ENTRY
   SOURCE-COMPOSE:SOURCE$ s" DEP-A-TWICE" SUB-COUNT 2 T= ;

: TEST-PROVIDED ( -- )
   S\" : MUST-NOT-LOAD ( -- n ) 37 ;\n" WRITE-A
   SB-RESET
   S\" s\" " SB-APPEND DEP-A$ SB-APPEND S\" \" provided\n" SB-APPEND
   s" require " SB-APPEND DEP-A$ SB-APPEND TEST-LF SB-APPEND-C
   SB$ WRITE-ENTRY
   BUILD-ENTRY
   SOURCE-COMPOSE:SOURCE$ s" MUST-NOT-LOAD" CONTAINS? TFALSE ;

: TEST-RUNTIME-SOURCE$ ( -- ptr u8 n )
   SB-RESET
   S\" : BAD ( -- ) s\" " SB-APPEND DEP-A$ SB-APPEND S\" \" included ;\n" SB-APPEND
   SB$ ;

: TEST-RUNTIME-REJECT ( -- )
   TEST-RUNTIME-SOURCE$ WRITE-ENTRY
   [: BUILD-ENTRY ;] E-DISC-DYNAMIC TTHROWSQ
   SB-RESET
   s" : BAD ( -- ) include " SB-APPEND DEP-A$ SB-APPEND S\"  ;\n" SB-APPEND
   SB$ WRITE-ENTRY
   [: BUILD-ENTRY ;] E-DISC-DYNAMIC TTHROWSQ ;

: TEST-SHADOW-REJECT ( -- )
   S\" : require ( -- ) ;\n" WRITE-ENTRY
   [: BUILD-ENTRY ;] E-DISC-SHADOW TTHROWSQ
   S\" 1 constant included\n" WRITE-ENTRY
   [: BUILD-ENTRY ;] E-DISC-SHADOW TTHROWSQ ;

: TEST-CYCLE ( -- )
   DEP-B$ INCLUDE-SOURCE$ WRITE-A
   DEP-A$ INCLUDE-SOURCE$ WRITE-B
   DEP-A$ INCLUDE-SOURCE$ WRITE-ENTRY
   [: BUILD-ENTRY ;] E-DISC-CYCLE TTHROWSQ
   SOURCE-COMPOSE:FAILURE {: code:n file:n line:n col:n byte:n :}
   code E-DISC-CYCLE T= file 2 T= line 1 T= col 1 T= byte 0 T=
   SOURCE-COMPOSE:CHAIN-N 4 T=
   3 SOURCE-COMPOSE:CHAIN-FILE$ DEP-A$ T$= ;

: TEST-NUL-PATH ( -- )
   S\" S\\\" bad\\zpath\" required\n" WRITE-ENTRY
   [: BUILD-ENTRY ;] E-DISC-NUL-PATH TTHROWSQ ;

: TEST-MALFORMED-ESCAPE ( -- )
   S\" S\\\" bad\\xG0\" required\n" WRITE-ENTRY
   [: BUILD-ENTRY ;] E-DISC-MALFORMED TTHROWSQ ;

: TEST-OPENER ( -- )
   S\" C\\\" bad.f\" required\n" WRITE-ENTRY
   [: BUILD-ENTRY ;] E-DISC-OPENER TTHROWSQ ;

: TEST-MISSING ( -- )
   MISSING$ REQUIRE-SOURCE$ WRITE-A
   DEP-A$ REQUIRE-SOURCE$ WRITE-ENTRY
   [: BUILD-ENTRY ;] E-DISC-MISSING TTHROWSQ
   SOURCE-COMPOSE:FAILURE {: code:n file:n line:n col:n byte:n :}
   code E-DISC-MISSING T= file 1 T= line 1 T= col 1 T= byte 0 T=
   SOURCE-COMPOSE:FAILURE-FILE$ DEP-A$ T$=
   SOURCE-COMPOSE:CHAIN-N 3 T=
   0 SOURCE-COMPOSE:CHAIN-FILE$ ENTRY$ T$=
   1 SOURCE-COMPOSE:CHAIN-FILE$ DEP-A$ T$=
   2 SOURCE-COMPOSE:CHAIN-FILE$ MISSING$ T$=
   SOURCE-COMPOSE:TEXT$ s" E-DISC-MISSING" CONTAINS? TTRUE
   SOURCE-COMPOSE:TEXT$ MISSING$ CONTAINS? TTRUE
   SOURCE-COMPOSE:JSON$ S\" \qinclude_chain\q" CONTAINS? TTRUE
   SOURCE-COMPOSE:JSON$ MISSING$ CONTAINS? TTRUE ;

: TEST-ESCAPED-PATH ( -- )
   S\" : SPECIAL-WORD ( -- n ) 47 ;\n" SPECIAL$ 2swap WRITE-ALL
   SPECIAL$ ESC-REQUIRE$ WRITE-ENTRY
   BUILD-ENTRY
   SOURCE-COMPOSE:SOURCE$ s" SPECIAL-WORD" CONTAINS? TTRUE
   SOURCE-COMPOSE:MAP$ SPECIAL$ HEX-PATH$ CONTAINS? TTRUE ;

: TEST-LINE-ENDINGS ( -- )
   S\" : CRLF-WORD ( -- n ) 53 ;\r\n" WRITE-A
   s" : FINAL-WORD ( -- n ) 59 ;" WRITE-B
   SPECIAL$ s" " WRITE-ALL
   SB-RESET
   s" require " SB-APPEND DEP-A$ SB-APPEND TEST-LF SB-APPEND-C
   s" require " SB-APPEND DEP-B$ SB-APPEND TEST-LF SB-APPEND-C
   SPECIAL$ ESC-REQUIRE+
   SB$ WRITE-ENTRY
   BUILD-ENTRY
   SOURCE-COMPOSE:SOURCE$ s" CRLF-WORD" CONTAINS? TTRUE
   SOURCE-COMPOSE:SOURCE$ s" FINAL-WORD" CONTAINS? TTRUE ;

: TEST-MAP-DIGEST ( -- )
   S\" : MAP-V1 ( -- n ) 61 ;\n" WRITE-A
   DEP-A$ REQUIRE-SOURCE$ WRITE-ENTRY
   BUILD-ENTRY
   SOURCE-COMPOSE:MAP-DIGEST$ drop MAP-DG-A DIGEST-U BYTE-COPY
   S\" \n: MAP-V2 ( -- n ) 67 ;\n" DEP-A$ 2swap APPEND-FILE
   BUILD-ENTRY
   SOURCE-COMPOSE:MAP-DIGEST$ drop MAP-DG-B DIGEST-U BYTE-COPY
   MAP-DG-A DIGEST-U MAP-DG-B DIGEST-U STR= TFALSE ;

: TEST-EVENT-DIGEST ( -- )
   s" " WRITE-A
   DEP-A$ REQUIRE-SOURCE$ WRITE-ENTRY
   BUILD-ENTRY
   SOURCE-COMPOSE:SOURCE$ SOURCE-DG-A SHA256
   SOURCE-COMPOSE:DIGEST$ drop PLAN-DG-A DIGEST-U BYTE-COPY
   DEP-A$ INCLUDE-SOURCE$ WRITE-ENTRY
   BUILD-ENTRY
   SOURCE-COMPOSE:SOURCE$ SOURCE-DG-B SHA256
   SOURCE-COMPOSE:DIGEST$ drop PLAN-DG-B DIGEST-U BYTE-COPY
   SOURCE-DG-A DIGEST-U SOURCE-DG-B DIGEST-U STR= TTRUE
   PLAN-DG-A DIGEST-U PLAN-DG-B DIGEST-U STR= TFALSE ;

: BIG-BUF ( -- ptr u8 )
   BIG-A @ 0= if SOURCE-ARENA-CAP MEM:BYTES-ALLOC-LEN MEM:ALLOC-BYTES drop BIG-A ! then
   BIG-A @ ;

: BIG-FILL ( -- )
   0 begin dup SOURCE-ARENA-CAP < while
      dup BIG-BUF + $20 swap c!
      1+
   repeat drop ;

: TEST-CAPACITY ( -- )
   BIG-FILL
   ENTRY$ BIG-BUF SOURCE-ARENA-CAP WRITE-ALL
   [: BUILD-ENTRY ;] E-DISC-CAPACITY TTHROWSQ ;

: TEST-CAPACITY-NO-LF ( -- )
   BIG-FILL
   $58 BIG-BUF SOURCE-ARENA-CAP 1- + c!
   ENTRY$ BIG-BUF SOURCE-ARENA-CAP WRITE-ALL
   [: BUILD-ENTRY ;] E-DISC-CAPACITY TTHROWSQ ;

: TEST-HOSTILE-LEXING ( -- )
   S\" ( require ignored.f )\nS\" require ignored.f\" 2drop\n(CMP) constant CMP-TAG" WRITE-ENTRY
   BUILD-ENTRY
   SOURCE-COMPOSE:SOURCE$ s" (CMP) constant CMP-TAG" CONTAINS? TTRUE ;

: TEST-FROZEN ( -- )
   S\" : FROZEN-OLD ( -- n ) 41 ;\n" WRITE-A
   DEP-A$ REQUIRE-SOURCE$ WRITE-ENTRY
   BUILD-ENTRY
   S\" : FROZEN-NEW ( -- n ) 43 ;\n" WRITE-A
   SOURCE-COMPOSE:SOURCE$ s" FROZEN-OLD" CONTAINS? TTRUE
   SOURCE-COMPOSE:SOURCE$ s" FROZEN-NEW" CONTAINS? TFALSE ;

public

: RUN ( -- )
   T-RESET
   PREP
   TEST-TOP
   TEST-TRANSITIVE
   TEST-DUP-REQUIRE
   TEST-REPEAT-INCLUDE
   TEST-PROVIDED
   TEST-RUNTIME-REJECT
   TEST-SHADOW-REJECT
   TEST-CYCLE
   TEST-NUL-PATH
   TEST-MALFORMED-ESCAPE
   TEST-OPENER
   TEST-MISSING
   TEST-ESCAPED-PATH
   TEST-LINE-ENDINGS
   TEST-MAP-DIGEST
   TEST-EVENT-DIGEST
   TEST-CAPACITY
   TEST-CAPACITY-NO-LF
   TEST-HOSTILE-LEXING
   TEST-FROZEN
   CLEANUP-RUN
   T-REPORT
   s" source-compose-test: ok" type cr ;

;package

SOURCE-COMPOSE-TEST:RUN
