\ report.f - Habu-native LLM benchmark report reducer.
\ Load after lib/errors.f, lib/string.f, lib/fs.f, tools/json.f, tools/argv.f.

\ Tool boundary: streaming JSONL, large replay strings, and CLI/file IO live here.
0 set-check

1024 constant RR-ROW-MAX
128 constant RR-TASK-MAX
128 constant RR-MODEL-MAX
64 constant RR-CAT-MAX
16 constant RR-ARM-MAX
$80000 constant RR-LINE-CAP
$4000 constant RR-READ-CAP
$40000 constant RR-PERF-CAP
$10000 constant RR-STR-CAP
512 constant RR-KEY-CAP
1024 constant RR-VALUE-CAP
32 constant RR-NUM-CAP
128 constant RR-STAT-MAX

-9223372036854775808 constant RR-NULL
76 constant RR-E-INTERNAL

8 constant RR-ARM#
0 constant RR-HABU-A
1 constant RR-HABU-LIB
2 constant RR-HABU-STDLIB
3 constant RR-HABU-SKELETON
4 constant RR-JS
5 constant RR-PYTHON
6 constant RR-TS
7 constant RR-RUST

9 constant RR-TAB
10 constant RR-LF
13 constant RR-CR
32 constant RR-SP
34 constant RR-DQ
44 constant RR-COMMA
45 constant RR-MINUS
46 constant RR-DOT
48 constant RR-ZERO
58 constant RR-COLON
91 constant RR-LBRACK
92 constant RR-BACKSLASH
93 constant RR-RBRACK
123 constant RR-LBRACE
125 constant RR-RBRACE

create RR-LINE RR-LINE-CAP allot
create RR-READ RR-READ-CAP allot
create RR-PERF RR-PERF-CAP allot
create RR-STR RR-STR-CAP allot
create RR-KEY RR-KEY-CAP allot
create RR-VALUE RR-VALUE-CAP allot
create RR-NUM RR-NUM-CAP allot
create RR-VALS RR-ROW-MAX cells allot

create R-TASK-ID RR-ROW-MAX cells allot
create R-NAME-O RR-ROW-MAX cells allot
create R-NAME-U RR-ROW-MAX cells allot
create R-MODEL-ID-O RR-ROW-MAX cells allot
create R-MODEL-ID-U RR-ROW-MAX cells allot
create R-MODEL-O RR-ROW-MAX cells allot
create R-MODEL-U RR-ROW-MAX cells allot
create R-MODEL-KEY-O RR-ROW-MAX cells allot
create R-MODEL-KEY-U RR-ROW-MAX cells allot
create R-ARM RR-ROW-MAX cells allot
create R-CAT-O RR-ROW-MAX cells allot
create R-CAT-U RR-ROW-MAX cells allot
create R-OUT-O RR-ROW-MAX cells allot
create R-OUT-U RR-ROW-MAX cells allot
create R-ROUNDS RR-ROW-MAX cells allot
create R-FIRST RR-ROW-MAX cells allot
create R-TOKENS RR-ROW-MAX cells allot
create R-RUNTIME RR-ROW-MAX cells allot
create R-RUNTIME-KNOWN RR-ROW-MAX cells allot
create R-WALL RR-ROW-MAX cells allot
create R-DIAGOK RR-ROW-MAX cells allot

create T-NAME-O RR-TASK-MAX cells allot
create T-NAME-U RR-TASK-MAX cells allot

create M-KEY-O RR-MODEL-MAX cells allot
create M-KEY-U RR-MODEL-MAX cells allot
create M-LABEL-O RR-MODEL-MAX cells allot
create M-LABEL-U RR-MODEL-MAX cells allot

create C-NAME-O RR-CAT-MAX cells allot
create C-NAME-U RR-CAT-MAX cells allot

create U-TASK-ID RR-ROW-MAX cells allot
create U-NAME-O RR-ROW-MAX cells allot
create U-NAME-U RR-ROW-MAX cells allot
create U-MODEL-O RR-ROW-MAX cells allot
create U-MODEL-U RR-ROW-MAX cells allot
create U-PASS RR-ROW-MAX cells allot

variable RR-ROWS
variable RR-STR-U
variable RR-LINE-U
variable RR-LINE#
variable RR-RFD
variable RR-I
variable RR-J
variable RR-K
variable RR-MET-A
variable RR-MET-B
variable RR-MET-C
variable RR-BEST-TOK
variable RR-DEPTH
variable RR-KEY-U
variable RR-VALUE-U
variable RR-NUM-I
variable RR-SAVED-O
variable RR-SAVED-U
variable RR-TASK-N
variable RR-MODEL-N
variable RR-CAT-N

variable CUR-TASK-ID
variable CUR-NAME-O
variable CUR-NAME-U
variable CUR-MODEL-ID-O
variable CUR-MODEL-ID-U
variable CUR-MODEL-O
variable CUR-MODEL-U
variable CUR-MODEL-KEY-O
variable CUR-MODEL-KEY-U
variable CUR-ARM
variable CUR-CAT-O
variable CUR-CAT-U
variable CUR-OUT-O
variable CUR-OUT-U
variable CUR-ROUNDS
variable CUR-FIRST
variable CUR-TOKENS
variable CUR-RUNTIME
variable CUR-RUNTIME-KNOWN
variable CUR-WALL
variable CUR-DTOK
variable CUR-DSPAN
variable CUR-DEXPECT
variable CUR-DACTUAL
variable CUR-DCODE
variable CUR-DCLASS
variable CUR-AESTABLE

variable S-TRIALS
variable S-PASSED
variable S-FIRST
variable S-NONPASS
variable S-TASKS
variable S-PASSK
variable S-MISSING-TOKENS
variable S-MISSING-RUNTIME
variable S-ROUND-SUM
variable S-DIAGOK
variable S-TOK#
variable S-TOK-SUM
variable S-TOK-MAX
variable S-TOK-MED
variable S-TOK-MEAN
variable S-RUN#
variable S-RUN-SUM
variable S-RUN-MAX
variable S-RUN-MED
variable S-WALL#
variable S-WALL-SUM
variable S-WALL-MAX
variable S-WALL-MED
variable S-MEAN-ROUNDS

variable U-COUNT
variable RR-PERF-ROOT
variable RR-PERF-ARR

: RR-CHECK-HOOK ( -- )
   CHECK! ;
' RR-CHECK-HOOK set-check

: RR-FAIL ( a u -- )
   type cr RR-E-INTERNAL die ;

: RR-CELL ( ptr n n -- ptr n ) cells swap + ;
: RR-A@ ( ptr n n -- n ) RR-CELL @ ;
: RR-A! ( n ptr n n -- ) RR-CELL ! ;
: RR-AT ( n ptr n -- n ) swap RR-A@ ;

: RR-OUT ( a u -- ) type ;
: RR-NL ( -- ) RR-LF emit ;
: RR-SPC ( -- ) RR-SP emit ;
: RR-C ( c -- ) emit ;
: RR-DASH$ ( -- a u ) s" —" ;
: RR-UNKNOWN$ ( -- a u ) s" unknown" ;

: RR-CHECK-ROW ( -- )
   RR-ROWS @ RR-ROW-MAX >= if s" report: too many rows" RR-FAIL then ;

: RR-CHECK-STR ( n -- )
   RR-STR-U @ + RR-STR-CAP > if s" report: string pool full" RR-FAIL then ;

: RR-SAVE$ ( ptr u8 n -- n n )
   {: a:ptr u :}
   u RR-CHECK-STR
   a RR-STR RR-STR-U @ + u BYTE-COPY
   RR-STR-U @ u
   RR-STR-U @ u + RR-STR-U ! ;

: RR-$ ( n n -- ptr u8 n )
   {: off u :} off RR-STR + u ;

: RR-STORE$ ( ptr u8 n ptr n ptr n n -- )
   {: a:ptr u oa:ptr ua:ptr idx :}
   a u RR-SAVE$ RR-SAVED-U ! RR-SAVED-O !
   RR-SAVED-U @ ua idx RR-A!
   RR-SAVED-O @ oa idx RR-A! ;

: RR-ROW-NAME$ ( n -- ptr u8 n ) dup R-NAME-O swap RR-A@ swap R-NAME-U swap RR-A@ RR-$ ;
: RR-ROW-MODEL$ ( n -- ptr u8 n ) dup R-MODEL-O swap RR-A@ swap R-MODEL-U swap RR-A@ RR-$ ;
: RR-ROW-MODEL-KEY$ ( n -- ptr u8 n ) dup R-MODEL-KEY-O swap RR-A@ swap R-MODEL-KEY-U swap RR-A@ RR-$ ;
: RR-ROW-CAT$ ( n -- ptr u8 n ) dup R-CAT-O swap RR-A@ swap R-CAT-U swap RR-A@ RR-$ ;
: RR-ROW-OUT$ ( n -- ptr u8 n ) dup R-OUT-O swap RR-A@ swap R-OUT-U swap RR-A@ RR-$ ;
: RR-MODEL-KEY$ ( n -- ptr u8 n ) dup M-KEY-O swap RR-A@ swap M-KEY-U swap RR-A@ RR-$ ;
: RR-MODEL-LABEL$ ( n -- ptr u8 n ) dup M-LABEL-O swap RR-A@ swap M-LABEL-U swap RR-A@ RR-$ ;
: RR-TASK$ ( n -- ptr u8 n ) dup T-NAME-O swap RR-A@ swap T-NAME-U swap RR-A@ RR-$ ;
: RR-CAT$ ( n -- ptr u8 n ) dup C-NAME-O swap RR-A@ swap C-NAME-U swap RR-A@ RR-$ ;

: RR-BOOL ( f -- bool ) 0= 0= ;

: RR-U$ ( n -- ptr u8 n )
   {: u:n :}
   RR-NUM-CAP RR-NUM-I !
   u 0= if
      RR-NUM-I @ 1- RR-NUM-I !
      RR-ZERO RR-NUM RR-NUM-I @ + c!
      RR-NUM RR-NUM-I @ + 1
      exit
   then
   u begin dup 0 > while
      dup 10 mod RR-ZERO +
      RR-NUM-I @ 1- RR-NUM-I !
      RR-NUM RR-NUM-I @ + c!
      10 /
   repeat drop
   RR-NUM RR-NUM-I @ + RR-NUM-CAP RR-NUM-I @ - ;

: RR-N$ ( n -- ptr u8 n )
   dup 0 < if
      negate RR-U$ >r >r
      RR-NUM-I @ 1- RR-NUM-I !
      RR-MINUS RR-NUM RR-NUM-I @ + c!
      RR-NUM RR-NUM-I @ + RR-NUM-CAP RR-NUM-I @ -
      r> r> 2drop
      exit
   then
   RR-U$ ;

: RR-U. ( n -- ) RR-U$ RR-OUT ;
: RR-N. ( n -- ) RR-N$ RR-OUT ;

: RR-ABS ( n -- u ) dup 0 < if negate then ;

: RR-SCALED. ( n -- )
   {: x:n :}
   x 0 < if RR-MINUS emit x negate else x then
   dup 100 / RR-U$ RR-OUT
   dup 100 mod dup 0= if 2drop exit then
   RR-DOT emit
   dup 10 < if RR-ZERO emit then
   swap drop
   dup 10 mod 0= if 10 / RR-U$ RR-OUT else RR-U$ RR-OUT then ;

: RR-FMT-SCALED ( n -- )
   {: val:n :}
   val RR-NULL = if RR-DASH$ RR-OUT exit then
   val RR-SCALED. ;

: RR-FMT-INT ( n -- )
   {: val:n :}
   val RR-NULL = if RR-DASH$ RR-OUT exit then
   val RR-N. ;

: RR-PCT. ( n n -- )
   {: num:n den:n :}
   den 0= if RR-DASH$ RR-OUT exit then
   num 100 * den 2 / + den / RR-U. 37 emit ;

: RR-ROUND-DIV ( n d -- n )
   {: num:n den:n :}
   den 0= if 0 exit then
   num 0 < if num negate den 2 / + den / negate else num den 2 / + den / then ;

: RR-RATIO. ( n d -- )
   {: num:n den:n :}
   num RR-NULL = if RR-DASH$ RR-OUT exit then
   den RR-NULL = if RR-DASH$ RR-OUT exit then
   den 0= if RR-DASH$ RR-OUT exit then
   num 10 * den RR-ROUND-DIV dup 100 < if
      dup 10 / RR-U$ RR-OUT
      dup 10 mod dup 0= if 2drop else RR-DOT emit RR-U$ RR-OUT then
   else
      drop num den RR-ROUND-DIV RR-U.
   then
   120 emit ;

: RR-Q. ( ptr u8 n -- )
   {: a:ptr u :}
   0 begin dup u < while
      dup a + c@ dup 124 = if drop RR-BACKSLASH emit 124 emit else emit then
      1+
   repeat drop ;

: RR-LABEL$ ( n -- ptr u8 n )
   dup RR-HABU-A = if drop s" Habu raw" exit then
   dup RR-HABU-LIB = if drop s" Habu + array helpers" exit then
   dup RR-HABU-STDLIB = if drop s" Habu + stdlib" exit then
   dup RR-HABU-SKELETON = if drop s" Habu + skeleton" exit then
   dup RR-JS = if drop s" JavaScript" exit then
   dup RR-PYTHON = if drop s" Python" exit then
   dup RR-TS = if drop s" TypeScript" exit then
   dup RR-RUST = if drop s" Rust" exit then
   drop s" unknown" ;

: RR-ARM-ID ( ptr u8 n -- n )
   2dup s" habu-a" STR= if 2drop RR-HABU-A exit then
   2dup s" habu-lib" STR= if 2drop RR-HABU-LIB exit then
   2dup s" habu-stdlib" STR= if 2drop RR-HABU-STDLIB exit then
   2dup s" habu-skeleton" STR= if 2drop RR-HABU-SKELETON exit then
   2dup s" js" STR= if 2drop RR-JS exit then
   2dup s" python" STR= if 2drop RR-PYTHON exit then
   2dup s" ts" STR= if 2drop RR-TS exit then
   2dup s" rust" STR= if 2drop RR-RUST exit then
   2drop -1 ;

: RR-END? ( -- f ) RR-I @ RR-LINE-U @ >= ;
: RR-CH@ ( -- c )
   RR-END? if s" report: malformed JSON row" RR-FAIL then
   RR-LINE RR-I @ + c@ ;
: RR-ADV ( -- ) RR-I @ 1+ RR-I ! ;

: RR-WS? ( c -- f )
   dup RR-SP = over RR-TAB = or over RR-LF = or swap RR-CR = or ;

: RR-SKIP-WS ( -- )
   begin RR-END? 0= while
      RR-CH@ RR-WS? if RR-ADV else exit then
   repeat ;

: RR-EXPECT ( c -- )
   {: c:n :} RR-SKIP-WS RR-CH@ c <> if s" report: unexpected JSON character" RR-FAIL then RR-ADV ;

: RR-BUF+ ( u8 ptr u8 n ptr n -- )
   {: c:n dst:ptr cap:n lenp:ptr :}
   lenp @ 1+ cap > if s" report: JSON string field too long" RR-FAIL then
   c lenp @ dst + c!
   lenp @ 1+ lenp ! ;

: RR-HEX? ( c -- n )
   dup 48 >= over 57 <= and if 48 - exit then
   dup 65 >= over 70 <= and if 55 - exit then
   dup 97 >= over 102 <= and if 87 - exit then
   drop -1 ;

: RR-SKIP-HEX4 ( -- )
   0 begin dup 4 < while
      RR-CH@ RR-HEX? 0 < if s" report: bad JSON unicode escape" RR-FAIL then
      RR-ADV 1+
   repeat drop ;

: RR-ESCAPE-COPY ( ptr u8 n ptr n -- )
   {: dst:ptr cap:n lenp:ptr :}
   RR-CH@ dup 117 = if
      drop RR-ADV RR-SKIP-HEX4 63 dst cap lenp RR-BUF+ exit
   then
   dup 110 = if drop RR-ADV RR-LF dst cap lenp RR-BUF+ exit then
   dup 114 = if drop RR-ADV RR-CR dst cap lenp RR-BUF+ exit then
   dup 116 = if drop RR-ADV RR-TAB dst cap lenp RR-BUF+ exit then
   dup 98 = if drop RR-ADV 8 dst cap lenp RR-BUF+ exit then
   dup 102 = if drop RR-ADV 12 dst cap lenp RR-BUF+ exit then
   RR-ADV dst cap lenp RR-BUF+ ;

: RR-STRING-INTO ( ptr u8 n ptr n -- )
   {: dst:ptr cap:n lenp:ptr :}
   0 lenp !
   RR-SKIP-WS RR-DQ RR-EXPECT
   begin
      RR-CH@ dup RR-DQ = if drop RR-ADV exit then
      dup RR-BACKSLASH = if drop RR-ADV dst cap lenp RR-ESCAPE-COPY
      else RR-ADV dst cap lenp RR-BUF+ then
   again ;

: RR-SKIP-STRING ( -- )
   RR-DQ RR-EXPECT
   begin
      RR-CH@ dup RR-DQ = if drop RR-ADV exit then
      RR-BACKSLASH = if RR-ADV RR-END? if s" report: bad JSON escape" RR-FAIL then then
      RR-ADV
   again ;

: RR-SKIP-LITERAL ( -- )
   begin RR-END? 0= while
      RR-CH@ dup RR-COMMA = swap RR-RBRACE = or if exit then
      RR-ADV
   repeat ;

: RR-SKIP-COMPOUND ( n n -- )
   {: open:n close:n :}
   0 RR-DEPTH !
   begin
      RR-CH@ dup RR-DQ = if drop RR-SKIP-STRING
      else
         dup open = if RR-DEPTH @ 1+ RR-DEPTH ! then
         dup close = if RR-DEPTH @ 1- RR-DEPTH ! then
         drop RR-ADV
         RR-DEPTH @ 0= if exit then
      then
   again ;

: RR-SKIP-VALUE ( -- )
   RR-SKIP-WS
   RR-CH@ dup RR-DQ = if drop RR-SKIP-STRING exit then
   dup RR-LBRACE = if drop RR-LBRACE RR-RBRACE RR-SKIP-COMPOUND exit then
   dup RR-LBRACK = if drop RR-LBRACK RR-RBRACK RR-SKIP-COMPOUND exit then
   drop RR-SKIP-LITERAL ;

: RR-VALUE-TOKEN$ ( -- ptr u8 n )
   RR-SKIP-WS
   RR-I @ RR-J !
   begin RR-END? 0= while
      RR-CH@ dup RR-COMMA = swap RR-RBRACE = or if
         RR-J @ RR-LINE + RR-I @ RR-J @ - TRIM exit
      then
      RR-ADV
   repeat
   RR-J @ RR-LINE + RR-I @ RR-J @ - TRIM ;

: RR-TOKEN-NUM ( -- n )
   RR-VALUE-TOKEN$ STR>NUMBER? 0= if drop 0 then ;

: RR-TOKEN-BOOL ( -- f )
   RR-VALUE-TOKEN$ s" true" STR= ;

: RR-TOKEN-NULL? ( -- f )
   RR-VALUE-TOKEN$ s" null" STR= ;

: RR-CUR-RESET ( -- )
   -1 CUR-TASK-ID !
   0 CUR-NAME-O ! 0 CUR-NAME-U !
   0 CUR-MODEL-ID-O ! 0 CUR-MODEL-ID-U !
   0 CUR-MODEL-O ! 0 CUR-MODEL-U !
   0 CUR-MODEL-KEY-O ! 0 CUR-MODEL-KEY-U !
   -1 CUR-ARM !
   RR-UNKNOWN$ RR-SAVE$ CUR-CAT-U ! CUR-CAT-O !
   0 CUR-OUT-O ! 0 CUR-OUT-U !
   0 CUR-ROUNDS ! 0 CUR-FIRST ! 0 CUR-TOKENS !
   0 CUR-RUNTIME ! 0 CUR-RUNTIME-KNOWN !
   0 CUR-WALL !
   0 CUR-DTOK ! 0 CUR-DSPAN ! 0 CUR-DEXPECT ! 0 CUR-DACTUAL !
   0 CUR-DCODE ! 0 CUR-DCLASS ! 0 CUR-AESTABLE ! ;

: RR-CUR-STRING! ( ptr n ptr n -- )
   {: offp:ptr lenp:ptr :}
   RR-VALUE RR-VALUE-U @ RR-SAVE$ lenp ! offp ! ;

: RR-PARSE-STRING-VALUE ( -- )
   RR-VALUE RR-VALUE-CAP RR-VALUE-U RR-STRING-INTO ;

: RR-KEY= ( ptr u8 n -- bool )
   {: a:ptr u :} RR-KEY RR-KEY-U @ a u STR= ;

: RR-FIELD-STRING ( ptr n ptr n -- )
   RR-PARSE-STRING-VALUE RR-CUR-STRING! ;

: RR-DISPATCH-FIELD ( -- )
   s" task_id" RR-KEY= if RR-TOKEN-NUM CUR-TASK-ID ! exit then
   s" name" RR-KEY= if CUR-NAME-O CUR-NAME-U RR-FIELD-STRING exit then
   s" model_id" RR-KEY= if CUR-MODEL-ID-O CUR-MODEL-ID-U RR-FIELD-STRING exit then
   s" model" RR-KEY= if CUR-MODEL-O CUR-MODEL-U RR-FIELD-STRING exit then
   s" arm" RR-KEY= if
      RR-PARSE-STRING-VALUE RR-VALUE RR-VALUE-U @ RR-ARM-ID CUR-ARM !
      exit
   then
   s" task_family" RR-KEY= if CUR-CAT-O CUR-CAT-U RR-FIELD-STRING exit then
   s" category" RR-KEY= if
      CUR-CAT-U @ 7 = if CUR-CAT-O CUR-CAT-U RR-FIELD-STRING else RR-SKIP-VALUE then
      exit
   then
   s" outcome" RR-KEY= if CUR-OUT-O CUR-OUT-U RR-FIELD-STRING exit then
   s" rounds" RR-KEY= if RR-TOKEN-NUM CUR-ROUNDS ! exit then
   s" first_pass" RR-KEY= if RR-TOKEN-BOOL CUR-FIRST ! exit then
	   s" tokens" RR-KEY= if RR-TOKEN-NUM CUR-TOKENS ! exit then
	   s" runtime_ms" RR-KEY= if
	      RR-I @ RR-J !
	      RR-TOKEN-NULL? if 0 CUR-RUNTIME-KNOWN ! else RR-J @ RR-I ! RR-TOKEN-NUM CUR-RUNTIME ! -1 CUR-RUNTIME-KNOWN ! then
	      exit
	   then
   s" wall_ms" RR-KEY= if RR-TOKEN-NUM CUR-WALL ! exit then
   s" diagnostic_token" RR-KEY= if RR-TOKEN-BOOL CUR-DTOK ! exit then
   s" diagnostic_span" RR-KEY= if RR-TOKEN-BOOL CUR-DSPAN ! exit then
   s" diagnostic_expected" RR-KEY= if RR-TOKEN-BOOL CUR-DEXPECT ! exit then
   s" diagnostic_actual" RR-KEY= if RR-TOKEN-BOOL CUR-DACTUAL ! exit then
   s" diagnostic_code" RR-KEY= if RR-TOKEN-BOOL CUR-DCODE ! exit then
   s" diagnostic_repair_class" RR-KEY= if RR-TOKEN-BOOL CUR-DCLASS ! exit then
   s" all_errors_stable" RR-KEY= if RR-TOKEN-BOOL CUR-AESTABLE ! exit then
   RR-SKIP-VALUE ;

: RR-ROW-DIAGOK ( -- f )
   CUR-DTOK @ CUR-DSPAN @ and CUR-DEXPECT @ and CUR-DACTUAL @ and
   CUR-DCODE @ and CUR-DCLASS @ and CUR-AESTABLE @ and ;

: RR-CUR-MODEL-KEY ( -- )
   CUR-MODEL-ID-U @ 0 > if
      CUR-MODEL-ID-O @ CUR-MODEL-KEY-O !
      CUR-MODEL-ID-U @ CUR-MODEL-KEY-U !
      exit
   then
   CUR-MODEL-U @ 0 > if
      CUR-MODEL-O @ CUR-MODEL-KEY-O !
      CUR-MODEL-U @ CUR-MODEL-KEY-U !
      exit
   then
   RR-UNKNOWN$ RR-SAVE$ CUR-MODEL-KEY-U ! CUR-MODEL-KEY-O ! ;

: RR-SET-DEFAULTS ( -- )
   CUR-NAME-U @ 0= if s" unknown" RR-SAVE$ CUR-NAME-U ! CUR-NAME-O ! then
   CUR-MODEL-U @ 0= if CUR-MODEL-KEY-U @ 0 > if
      CUR-MODEL-KEY-O @ CUR-MODEL-O ! CUR-MODEL-KEY-U @ CUR-MODEL-U !
   then then
   CUR-OUT-U @ 0= if s" unknown" RR-SAVE$ CUR-OUT-U ! CUR-OUT-O ! then ;

: RR-STORE-ROW ( -- )
   RR-CHECK-ROW
   RR-CUR-MODEL-KEY
   RR-SET-DEFAULTS
   CUR-TASK-ID @ R-TASK-ID RR-ROWS @ RR-A!
   CUR-NAME-O @ R-NAME-O RR-ROWS @ RR-A!
   CUR-NAME-U @ R-NAME-U RR-ROWS @ RR-A!
   CUR-MODEL-ID-O @ R-MODEL-ID-O RR-ROWS @ RR-A!
   CUR-MODEL-ID-U @ R-MODEL-ID-U RR-ROWS @ RR-A!
   CUR-MODEL-O @ R-MODEL-O RR-ROWS @ RR-A!
   CUR-MODEL-U @ R-MODEL-U RR-ROWS @ RR-A!
   CUR-MODEL-KEY-O @ R-MODEL-KEY-O RR-ROWS @ RR-A!
   CUR-MODEL-KEY-U @ R-MODEL-KEY-U RR-ROWS @ RR-A!
   CUR-ARM @ R-ARM RR-ROWS @ RR-A!
   CUR-CAT-O @ R-CAT-O RR-ROWS @ RR-A!
   CUR-CAT-U @ R-CAT-U RR-ROWS @ RR-A!
   CUR-OUT-O @ R-OUT-O RR-ROWS @ RR-A!
   CUR-OUT-U @ R-OUT-U RR-ROWS @ RR-A!
   CUR-ROUNDS @ R-ROUNDS RR-ROWS @ RR-A!
   CUR-FIRST @ R-FIRST RR-ROWS @ RR-A!
   CUR-TOKENS @ R-TOKENS RR-ROWS @ RR-A!
   CUR-RUNTIME @ R-RUNTIME RR-ROWS @ RR-A!
   CUR-RUNTIME-KNOWN @ R-RUNTIME-KNOWN RR-ROWS @ RR-A!
   CUR-WALL @ R-WALL RR-ROWS @ RR-A!
   RR-ROW-DIAGOK R-DIAGOK RR-ROWS @ RR-A!
   RR-ROWS @ 1+ RR-ROWS ! ;

: RR-PARSE-FIELD ( -- )
   RR-KEY RR-KEY-CAP RR-KEY-U RR-STRING-INTO
   RR-COLON RR-EXPECT
   RR-DISPATCH-FIELD ;

: RR-PARSE-ROW ( -- )
   RR-CUR-RESET
   0 RR-I !
   RR-LBRACE RR-EXPECT
   RR-SKIP-WS
   RR-CH@ RR-RBRACE = if RR-ADV exit then
   begin
      RR-PARSE-FIELD
      RR-SKIP-WS
      RR-CH@ RR-RBRACE = if RR-ADV RR-STORE-ROW exit then
      RR-COMMA RR-EXPECT
   again ;

: RR-FINISH-LINE ( -- )
   RR-LINE-U @ 0= if exit then
   RR-PARSE-ROW
   0 RR-LINE-U !
   RR-LINE# @ 1+ RR-LINE# ! ;

: RR-LINE-BYTE ( c -- )
   dup RR-LF = if drop RR-FINISH-LINE exit then
   RR-LINE-U @ RR-LINE-CAP >= if s" report: JSONL line too long" RR-FAIL then
   RR-LINE RR-LINE-U @ + c!
   RR-LINE-U @ 1+ RR-LINE-U ! ;

: RR-SCAN-BUF ( n -- )
   {: n :}
   0 begin dup n < while
      RR-READ over + c@ RR-LINE-BYTE
      1+
   repeat drop ;

: RR-SCAN-RUN ( ptr u8 n -- )
   FS-PATHZ open-rd RR-RFD !
   RR-RFD @ 0 < if s" report: cannot open run JSONL" RR-FAIL then
   begin
      RR-RFD @ RR-READ RR-READ-CAP read dup 0 < if RR-RFD @ close s" report: read failed" RR-FAIL then
      dup 0 > while
      RR-SCAN-BUF
   repeat drop
   RR-RFD @ close
   RR-FINISH-LINE ;

: RR-STR-EQ-OFF ( n n ptr u8 n -- bool )
   {: off:n u:n a:ptr v:n :}
   off RR-STR + u a v STR= ;

: RR-ROW-PASS? ( n -- bool )
   RR-ROW-OUT$ s" pass" STR= ;

: RR-TOKEN-KNOWN? ( i -- f )
   R-TOKENS RR-AT 0 > ;

: RR-RUNTIME-KNOWN? ( i -- f )
   R-RUNTIME-KNOWN RR-AT 0= 0= ;

: RR-ROW-MODEL-MATCH? ( n n -- bool )
   {: idx:n model:n :}
   model 0 < if -1 exit then
   idx RR-ROW-MODEL-KEY$ model RR-MODEL-KEY$ STR= ;

: RR-ROW-CAT-MATCH? ( n n -- bool )
   {: idx:n cat:n :}
   cat 0 < if -1 exit then
   idx RR-ROW-CAT$ cat RR-CAT$ STR= ;

: RR-ROW-SELECT? ( n n n n -- bool )
   {: idx:n arm:n model:n cat:n :}
   idx R-ARM RR-AT arm <> if 0 exit then
   idx model RR-ROW-MODEL-MATCH? 0= if 0 exit then
   idx cat RR-ROW-CAT-MATCH? 0= if 0 exit then
   -1 ;

: RR-TASK-INDEX ( ptr u8 n -- n )
   {: a:ptr u :}
   0 begin dup RR-TASK-N @ < while
      dup RR-TASK$ a u STR= if exit then
      1+
   repeat drop -1 ;

: RR-ADD-TASK ( ptr u8 n -- )
   RR-TASK-N @ RR-TASK-MAX >= if s" report: too many tasks" RR-FAIL then
   T-NAME-O T-NAME-U RR-TASK-N @ RR-STORE$
   RR-TASK-N @ 1+ RR-TASK-N ! ;

: RR-MODEL-INDEX ( ptr u8 n -- n )
   {: a:ptr u :}
   0 begin dup RR-MODEL-N @ < while
      dup RR-MODEL-KEY$ a u STR= if exit then
      1+
   repeat drop -1 ;

: RR-ADD-MODEL ( ptr u8 n ptr u8 n -- )
   {: ka:ptr ku la:ptr lu :}
   RR-MODEL-N @ RR-MODEL-MAX >= if s" report: too many models" RR-FAIL then
   ka ku M-KEY-O M-KEY-U RR-MODEL-N @ RR-STORE$
   lu 0 > if la lu else ka ku then M-LABEL-O M-LABEL-U RR-MODEL-N @ RR-STORE$
   RR-MODEL-N @ 1+ RR-MODEL-N ! ;

: RR-CAT-INDEX ( ptr u8 n -- n )
   {: a:ptr u :}
   0 begin dup RR-CAT-N @ < while
      dup RR-CAT$ a u STR= if exit then
      1+
   repeat drop -1 ;

: RR-ADD-CAT ( ptr u8 n -- )
   RR-CAT-N @ RR-CAT-MAX >= if s" report: too many categories" RR-FAIL then
   C-NAME-O C-NAME-U RR-CAT-N @ RR-STORE$
   RR-CAT-N @ 1+ RR-CAT-N ! ;

: RR-INDEX-ROW ( n -- )
   {: row:n :}
   row RR-ROW-NAME$ 2dup RR-TASK-INDEX 0 < if RR-ADD-TASK else 2drop then
   row RR-ROW-MODEL-KEY$ 2dup RR-MODEL-INDEX 0 < if
      row RR-ROW-MODEL$ RR-ADD-MODEL
   else
      2drop
   then
   row RR-ROW-CAT$ 2dup RR-CAT-INDEX 0 < if RR-ADD-CAT else 2drop then ;

: RR-INDEX-DIMENSIONS ( -- )
   0 RR-TASK-N ! 0 RR-MODEL-N ! 0 RR-CAT-N !
   0 begin dup RR-ROWS @ < while
      dup RR-INDEX-ROW
      1+
   repeat drop ;

: RR-VAL+ ( n -- )
   S-TOK# @ RR-ROW-MAX >= if s" report: stat value overflow" RR-FAIL then
   RR-VALS S-TOK# @ RR-A!
   S-TOK# @ 1+ S-TOK# ! ;

: RR-VAL@ ( n -- n ) RR-VALS swap RR-A@ ;
: RR-VAL! ( n n -- ) RR-VALS swap RR-A! ;

: RR-SORT-VALS ( n -- )
   {: count:n :}
   0 RR-I !
   begin RR-I @ count < while
      RR-I @ 1+ RR-J !
      begin RR-J @ count < while
         RR-I @ RR-VAL@ RR-J @ RR-VAL@ > if
            RR-I @ RR-VAL@ RR-K !
            RR-J @ RR-VAL@ RR-I @ RR-VAL!
            RR-K @ RR-J @ RR-VAL!
         then
         RR-J @ 1+ RR-J !
      repeat
      RR-I @ 1+ RR-I !
   repeat ;

: RR-MEDIAN ( n -- n )
   {: count:n :}
   count 0= if RR-NULL exit then
   count RR-SORT-VALS
   count 2 mod 1 = if count 2 / RR-VAL@ 100 * exit then
   count 2 / RR-I !
   RR-I @ 1- RR-VAL@ RR-J !
   RR-I @ RR-VAL@ RR-J @ + 50 * ;

: RR-MEAN ( n n -- n )
   {: sum:n count:n :}
   count 0= if RR-NULL exit then
   sum 100 * count RR-ROUND-DIV ;

: RR-STATS-RESET ( -- )
   0 S-TRIALS ! 0 S-PASSED ! 0 S-FIRST ! 0 S-NONPASS !
   0 S-TASKS ! 0 S-PASSK ! 0 S-MISSING-TOKENS ! 0 S-MISSING-RUNTIME !
   0 S-ROUND-SUM ! 0 S-DIAGOK !
   0 S-TOK# ! 0 S-TOK-SUM ! RR-NULL S-TOK-MAX ! RR-NULL S-TOK-MED ! RR-NULL S-TOK-MEAN !
   0 S-RUN# ! 0 S-RUN-SUM ! RR-NULL S-RUN-MAX ! RR-NULL S-RUN-MED !
   0 S-WALL# ! 0 S-WALL-SUM ! RR-NULL S-WALL-MAX ! RR-NULL S-WALL-MED !
   RR-NULL S-MEAN-ROUNDS !
   0 U-COUNT ! ;

: RR-UNIT-MATCH? ( n n -- bool )
   {: row:n unit:n :}
   row RR-ROW-MODEL-KEY$ unit U-MODEL-O RR-AT unit U-MODEL-U RR-AT RR-$ STR= 0= if 0 exit then
   row R-TASK-ID RR-AT unit U-TASK-ID RR-AT <> if 0 exit then
   row R-TASK-ID RR-AT 0 < if
      row RR-ROW-NAME$ unit U-NAME-O RR-AT unit U-NAME-U RR-AT RR-$ STR=
   else -1 then ;

: RR-UNIT-INDEX ( n -- n )
   {: row:n :}
   0 begin dup U-COUNT @ < while
      row over RR-UNIT-MATCH? if exit then
      1+
   repeat drop -1 ;

: RR-ADD-UNIT ( n -- n )
   {: row:n :}
   U-COUNT @ RR-ROW-MAX >= if s" report: too many task units" RR-FAIL then
   row R-TASK-ID RR-AT U-TASK-ID U-COUNT @ RR-A!
   row R-NAME-O RR-AT U-NAME-O U-COUNT @ RR-A!
   row R-NAME-U RR-AT U-NAME-U U-COUNT @ RR-A!
   row R-MODEL-KEY-O RR-AT U-MODEL-O U-COUNT @ RR-A!
   row R-MODEL-KEY-U RR-AT U-MODEL-U U-COUNT @ RR-A!
   0 U-PASS U-COUNT @ RR-A!
   U-COUNT @ dup 1+ U-COUNT ! ;

: RR-ACC-UNIT ( n -- )
   {: row:n :}
   row RR-UNIT-INDEX dup 0 < if drop row RR-ADD-UNIT then RR-K !
   row RR-ROW-PASS? if -1 U-PASS RR-K @ RR-A! then ;

: RR-MAX! ( n ptr n -- )
   {: val:n addr:ptr :}
   addr @ RR-NULL = if val addr ! exit then
   val addr @ > if val addr ! then ;

: RR-COLLECT-STATS ( n n n -- )
   {: arm:n model:n cat:n :}
   RR-STATS-RESET
   0 begin dup RR-ROWS @ < while
      dup arm model cat RR-ROW-SELECT? if
         S-TRIALS @ 1+ S-TRIALS !
         dup RR-ACC-UNIT
         dup R-DIAGOK RR-AT if S-DIAGOK @ 1+ S-DIAGOK ! then
         dup RR-ROW-PASS? if
            S-PASSED @ 1+ S-PASSED !
            dup R-FIRST RR-AT if S-FIRST @ 1+ S-FIRST ! then
            dup R-ROUNDS RR-AT S-ROUND-SUM @ + S-ROUND-SUM !
            dup RR-TOKEN-KNOWN? if
               dup R-TOKENS RR-AT dup S-TOK-SUM @ + S-TOK-SUM !
               dup S-TOK-MAX RR-MAX!
               RR-VAL+
            else
               S-MISSING-TOKENS @ 1+ S-MISSING-TOKENS !
            then
	            dup RR-RUNTIME-KNOWN? if
	               dup R-RUNTIME RR-AT dup S-RUN-SUM @ + S-RUN-SUM !
	               dup S-RUN-MAX RR-MAX!
	               drop
	            else
	               S-MISSING-RUNTIME @ 1+ S-MISSING-RUNTIME !
	            then
	            dup R-WALL RR-AT dup S-WALL-SUM @ + S-WALL-SUM !
	            dup S-WALL-MAX RR-MAX!
	            drop
         else
            S-NONPASS @ 1+ S-NONPASS !
         then
      then
      1+
   repeat drop
   U-COUNT @ S-TASKS !
   0 begin dup U-COUNT @ < while
      dup U-PASS RR-AT if S-PASSK @ 1+ S-PASSK ! then
      1+
   repeat drop
   S-ROUND-SUM @ S-PASSED @ RR-MEAN S-MEAN-ROUNDS !
   S-TOK# @ RR-MEDIAN S-TOK-MED !
   S-TOK-SUM @ S-TOK# @ RR-MEAN S-TOK-MEAN !
   \ Runtime median uses the first S-RUN# values; recollect them because token median sorted RR-VALS.
   0 S-RUN# ! 0 begin dup RR-ROWS @ < while
      dup arm model cat RR-ROW-SELECT? over RR-ROW-PASS? and over RR-RUNTIME-KNOWN? and if
         dup R-RUNTIME RR-AT RR-VALS S-RUN# @ RR-A!
         S-RUN# @ 1+ S-RUN# !
      then
      1+
   repeat drop
   S-RUN# @ RR-MEDIAN S-RUN-MED !
   0 S-WALL# ! 0 begin dup RR-ROWS @ < while
      dup arm model cat RR-ROW-SELECT? over RR-ROW-PASS? and if
         dup R-WALL RR-AT RR-VALS S-WALL# @ RR-A!
         S-WALL# @ 1+ S-WALL# !
      then
      1+
   repeat drop
   S-WALL# @ RR-MEDIAN S-WALL-MED ! ;

: RR-HAS-LIB? ( -- bool )
   RR-HABU-LIB -1 -1 RR-COLLECT-STATS
   S-TRIALS @ 0 > ;

: RR-SEC-SCALED ( n -- n )
   {: ms:n :}
   ms RR-NULL = if RR-NULL exit then
   ms 500 + 1000 / ;

: RR-PASS-RATE ( -- n )
   S-TASKS @ 0= if RR-NULL exit then
   S-PASSK @ 10000 * S-TASKS @ RR-ROUND-DIV ;

: RR-PASS-DELTA. ( n n n -- )
   {: a:n b:n cat:n :}
   a -1 cat RR-COLLECT-STATS RR-PASS-RATE RR-MET-A !
   b -1 cat RR-COLLECT-STATS RR-PASS-RATE RR-MET-B !
   RR-MET-A @ RR-NULL = if RR-DASH$ RR-OUT exit then
   RR-MET-B @ RR-NULL = if RR-DASH$ RR-OUT exit then
   RR-MET-A @ RR-MET-B @ - dup 0 > if 43 emit then
   100 RR-ROUND-DIV RR-N. s" pp" RR-OUT ;

: RR-TASK-TOKEN-MAX ( n n -- n )
   {: task:n arm:n :}
   RR-NULL RR-MET-C !
   0 begin dup RR-ROWS @ < while
      dup R-ARM RR-AT arm = if
         dup RR-ROW-NAME$ task RR-TASK$ STR= if
            dup RR-ROW-PASS? over RR-TOKEN-KNOWN? and if
               dup R-TOKENS RR-AT
               RR-MET-C @ RR-NULL = if
                  RR-MET-C !
               else
                  dup RR-MET-C @ > if RR-MET-C ! else drop then
               then
            then
         then
      then
      1+
   repeat drop RR-MET-C @ ;

: RR-BEST-KEEP ( n -- )
   {: val:n :}
   val RR-NULL = if exit then
   RR-BEST-TOK @ RR-NULL = if val RR-BEST-TOK ! exit then
   val RR-BEST-TOK @ < if val RR-BEST-TOK ! then ;

: RR-BEST-BASELINE-MAX ( n -- n )
   {: task:n :}
   RR-NULL RR-BEST-TOK !
   task RR-JS RR-TASK-TOKEN-MAX RR-BEST-KEEP
   task RR-PYTHON RR-TASK-TOKEN-MAX RR-BEST-KEEP
   task RR-TS RR-TASK-TOKEN-MAX RR-BEST-KEEP
   task RR-RUST RR-TASK-TOKEN-MAX RR-BEST-KEEP
   RR-BEST-TOK @ ;

: RR-TASK-MAX. ( n n -- )
   RR-TASK-TOKEN-MAX RR-FMT-INT ;

: RR-TASK-RATIO. ( n n n -- )
   {: task:n arm:n best:n :}
   task arm RR-TASK-TOKEN-MAX best RR-RATIO. ;

: RR-ARM-OUTCOMES. ( n n -- )
   {: task:n arm:n :}
   0 RR-K !
   0 begin dup RR-ROWS @ < while
      dup R-ARM RR-AT arm = if
         dup RR-ROW-NAME$ task RR-TASK$ STR= if
            RR-K @ 0 > if RR-COMMA emit then
            dup RR-ROW-OUT$ RR-OUT
            47 emit
            dup R-ROUNDS RR-AT RR-N.
            RR-K @ 1+ RR-K !
         then
      then
      1+
   repeat drop
   RR-K @ 0= if RR-DASH$ RR-OUT then ;

: RR-MODELS. ( -- )
   0 begin dup RR-MODEL-N @ < while
      dup 0 > if s" , " RR-OUT then
      96 emit dup RR-MODEL-LABEL$ RR-Q. 96 emit
      1+
   repeat drop ;

: RR-RELIABILITY-ROW. ( n -- )
   {: arm:n :}
   arm -1 -1 RR-COLLECT-STATS
   s" | " RR-OUT arm RR-LABEL$ RR-OUT s"  | " RR-OUT
   S-TRIALS @ RR-U. s"  | " RR-OUT
   S-PASSED @ RR-U. s"  | " RR-OUT
   S-PASSED @ S-TRIALS @ RR-PCT. s"  | " RR-OUT
   S-FIRST @ S-TRIALS @ RR-PCT. s"  | " RR-OUT
   S-PASSK @ S-TASKS @ RR-PCT. s"  | " RR-OUT
   S-NONPASS @ RR-U. s"  |" RR-OUT RR-NL ;

: RR-PER-MODEL-ROW. ( n n -- )
   {: model:n arm:n :}
   arm model -1 RR-COLLECT-STATS
   S-TRIALS @ 0= if exit then
   s" | " RR-OUT model RR-MODEL-LABEL$ RR-Q. s"  | " RR-OUT
   arm RR-LABEL$ RR-OUT s"  | " RR-OUT
   S-TRIALS @ RR-U. s"  | " RR-OUT
   S-PASSED @ RR-U. s"  | " RR-OUT
   S-PASSED @ S-TRIALS @ RR-PCT. s"  | " RR-OUT
   S-FIRST @ S-TRIALS @ RR-PCT. s"  | " RR-OUT
   S-PASSK @ S-TASKS @ RR-PCT. s"  | " RR-OUT
   S-NONPASS @ RR-U. s"  |" RR-OUT RR-NL ;

: RR-EFFORT-ROW. ( n -- )
   {: arm:n :}
   arm -1 -1 RR-COLLECT-STATS
   s" | " RR-OUT arm RR-LABEL$ RR-OUT s"  | " RR-OUT
   S-MEAN-ROUNDS @ RR-FMT-SCALED s"  | " RR-OUT
   S-TOK-MED @ RR-FMT-SCALED s"  | **" RR-OUT
   S-TOK-MEAN @ RR-FMT-SCALED s" ** | " RR-OUT
   S-TOK-MAX @ RR-FMT-INT s"  | " RR-OUT
   S-RUN-MED @ RR-FMT-SCALED s"  | " RR-OUT
   S-RUN-MAX @ RR-FMT-INT s"  | " RR-OUT
   S-WALL-MED @ RR-SEC-SCALED RR-FMT-SCALED s"  | " RR-OUT
   S-WALL-MAX @ 100 * RR-SEC-SCALED RR-FMT-SCALED s"  |" RR-OUT RR-NL ;

: RR-CATEGORY-ROW. ( n n -- )
   {: cat:n arm:n :}
   arm -1 cat RR-COLLECT-STATS
   S-TRIALS @ 0= if exit then
   s" | " RR-OUT cat RR-CAT$ RR-Q. s"  | " RR-OUT
   arm RR-LABEL$ RR-OUT s"  | " RR-OUT
   S-TRIALS @ RR-U. s"  | " RR-OUT
   S-PASSED @ RR-U. s"  | " RR-OUT
   S-PASSED @ S-TRIALS @ RR-PCT. s"  | " RR-OUT
   S-PASSK @ S-TASKS @ RR-PCT. s"  | " RR-OUT
   S-MEAN-ROUNDS @ RR-FMT-SCALED s"  | " RR-OUT
   S-TOK-MEAN @ RR-FMT-SCALED s"  | " RR-OUT
   S-RUN-MED @ RR-FMT-SCALED s"  | " RR-OUT
   S-DIAGOK @ S-TRIALS @ RR-PCT. s"  |" RR-OUT RR-NL ;

: RR-CATEGORY-DELTA-ROW. ( n -- )
   {: cat:n :}
   s" | " RR-OUT cat RR-CAT$ RR-Q. s"  | " RR-OUT
   RR-HABU-A -1 cat RR-COLLECT-STATS S-PASSK @ S-TASKS @ RR-PCT. s"  | " RR-OUT
   RR-HABU-STDLIB -1 cat RR-COLLECT-STATS S-PASSK @ S-TASKS @ RR-PCT. s"  | " RR-OUT
   RR-HABU-SKELETON -1 cat RR-COLLECT-STATS S-PASSK @ S-TASKS @ RR-PCT. s"  | " RR-OUT
   RR-HABU-STDLIB RR-HABU-A cat RR-PASS-DELTA. s"  | " RR-OUT
   RR-HABU-SKELETON RR-HABU-STDLIB cat RR-PASS-DELTA. s"  | " RR-OUT
   RR-HABU-A -1 cat RR-COLLECT-STATS S-TOK-MEAN @ RR-MET-A !
   RR-HABU-STDLIB -1 cat RR-COLLECT-STATS S-TOK-MEAN @ RR-MET-B !
   RR-MET-B @ RR-MET-A @ RR-RATIO. s"  | " RR-OUT
   RR-HABU-SKELETON -1 cat RR-COLLECT-STATS S-TOK-MEAN @ RR-MET-C !
   RR-MET-C @ RR-MET-B @ RR-RATIO. s"  | " RR-OUT
   RR-HABU-A -1 cat RR-COLLECT-STATS S-RUN-MED @ RR-MET-A !
   RR-HABU-STDLIB -1 cat RR-COLLECT-STATS S-RUN-MED @ RR-MET-B !
   RR-MET-B @ RR-MET-A @ RR-RATIO. s"  | " RR-OUT
   RR-HABU-SKELETON -1 cat RR-COLLECT-STATS S-RUN-MED @ RR-MET-C !
   RR-MET-C @ RR-MET-B @ RR-RATIO. s"  |" RR-OUT RR-NL ;

: RR-MISSING-TOKENS-NOTE. ( -- )
   0 RR-MET-A !
   0 begin dup RR-ARM# < while
      dup -1 -1 RR-COLLECT-STATS
      RR-MET-A @ S-MISSING-TOKENS @ + RR-MET-A !
      1+
   repeat drop
   RR-MET-A @ 0= if exit then
   s" Output-token metrics exclude " RR-OUT RR-MET-A @ RR-U.
   s"  passing row(s) with missing/zero token counts (" RR-OUT
   0 RR-MET-B !
   0 begin dup RR-ARM# < while
      dup -1 -1 RR-COLLECT-STATS
      S-MISSING-TOKENS @ 0 > if
         RR-MET-B @ 0 > if s" , " RR-OUT then
         dup RR-LABEL$ RR-OUT RR-SPC S-MISSING-TOKENS @ RR-U.
         RR-MET-B @ 1+ RR-MET-B !
      then
      1+
   repeat drop
   s" ). Reliability, repair-round, and wall-time metrics still include those rows." RR-OUT RR-NL RR-NL ;

: RR-MISSING-RUNTIME-NOTE. ( -- )
   0 RR-MET-A !
   0 begin dup RR-ARM# < while
      dup -1 -1 RR-COLLECT-STATS
      RR-MET-A @ S-MISSING-RUNTIME @ + RR-MET-A !
      1+
   repeat drop
   RR-MET-A @ 0= if exit then
   s" Runtime metrics exclude " RR-OUT RR-MET-A @ RR-U.
   s"  passing row(s) without measured runtime (" RR-OUT
   0 RR-MET-B !
   0 begin dup RR-ARM# < while
      dup -1 -1 RR-COLLECT-STATS
      S-MISSING-RUNTIME @ 0 > if
         RR-MET-B @ 0 > if s" , " RR-OUT then
         dup RR-LABEL$ RR-OUT RR-SPC S-MISSING-RUNTIME @ RR-U.
         RR-MET-B @ 1+ RR-MET-B !
      then
      1+
   repeat drop
   s" ). Reliability, repair-round, token, and wall-time metrics still include those rows." RR-OUT RR-NL RR-NL ;

: RR-PERF-GET ( n ptr u8 n -- n )
   {: node:n a:ptr u:n :}
   node 0 < if -1 exit then
   node JSON-KIND J-OBJ <> if -1 exit then
   node a u JSON-GET ;

: RR-PERF-NUM. ( n -- )
   dup 0 < if drop RR-DASH$ RR-OUT exit then
   dup JSON-KIND J-NUM <> if drop RR-DASH$ RR-OUT exit then
   JSON-NUMBER$ STR>NUMBER? if RR-N. else drop RR-DASH$ RR-OUT then ;

: RR-PERF-MS-SCALED ( n -- n )
   dup 0 < if drop RR-NULL exit then
   dup JSON-KIND J-NUM <> if drop RR-NULL exit then
   JSON-NUMBER$ STR>NUMBER? if 100 * else drop RR-NULL then ;

: RR-PERF. ( -- )
   ARGV-POS# 1 <= if s" No perf JSON artifact was supplied with this report run." RR-OUT RR-NL RR-NL exit then
   1 ARGV-POS$ RR-PERF RR-PERF-CAP READ-ALL
   RR-PERF swap JSON-PARSE RR-PERF-ROOT !
   RR-PERF-ROOT @ s" results" RR-PERF-GET RR-PERF-ARR !
   RR-PERF-ARR @ 0 < if s" No perf JSON artifact was supplied with this report run." RR-OUT RR-NL RR-NL exit then
   s" | check | wall ms | wall s |" RR-OUT RR-NL
   s" |---|---:|---:|" RR-OUT RR-NL
   0 begin dup RR-PERF-ARR @ JSON-COUNT < while
      RR-PERF-ARR @ over JSON-ARR@ RR-K !
      s" | " RR-OUT RR-K @ s" name" RR-PERF-GET JSON-STRING$ RR-Q. s"  | " RR-OUT
      RR-K @ s" wall_ms" RR-PERF-GET dup RR-PERF-NUM.
      s"  | " RR-OUT
      RR-PERF-MS-SCALED RR-SEC-SCALED RR-FMT-SCALED
      s"  |" RR-OUT RR-NL
      1+
   repeat drop RR-NL ;

: RR-NONPASS. ( -- )
   0 RR-I !
   0 begin dup RR-ROWS @ < while
      dup RR-ROW-PASS? 0= if RR-I @ 1+ RR-I ! then
      1+
   repeat drop
   RR-I @ 0= if exit then
   s" ## Non-Pass Rows" RR-OUT RR-NL RR-NL
   s" | task | language | outcome | rounds | output tokens | wall s |" RR-OUT RR-NL
   s" |---|---|---|---:|---:|---:|" RR-OUT RR-NL
   0 begin dup RR-ROWS @ < while
      dup RR-ROW-PASS? 0= if
         s" | " RR-OUT dup RR-ROW-NAME$ RR-Q. s"  | " RR-OUT
         dup R-ARM RR-AT RR-LABEL$ RR-OUT s"  | " RR-OUT
         dup RR-ROW-OUT$ RR-Q. s"  | " RR-OUT
         dup R-ROUNDS RR-AT RR-N. s"  | " RR-OUT
         dup R-TOKENS RR-AT RR-N. s"  | " RR-OUT
         dup R-WALL RR-AT 100 * RR-SEC-SCALED RR-FMT-SCALED s"  |" RR-OUT RR-NL
      then
      1+
   repeat drop RR-NL ;

: RR-TASK-TABLE. ( -- )
   s" ## Per-Task Max Output Tokens" RR-OUT RR-NL RR-NL
   s" | task | Habu raw | Habu + helpers | Habu + stdlib | Habu + skeleton | JS | Python | TypeScript | Rust | raw/best | helpers/best | stdlib/best | skeleton/best | trial outcomes (raw/helpers/stdlib/skeleton/js/python/ts/rust) |" RR-OUT RR-NL
   s" |---|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---:|---|" RR-OUT RR-NL
	   0 begin dup RR-TASK-N @ < while
	      dup RR-BEST-BASELINE-MAX RR-J !
	      s" | " RR-OUT dup RR-TASK$ RR-Q. s"  | " RR-OUT
	      dup RR-HABU-A RR-TASK-MAX. s"  | " RR-OUT
	      dup RR-HABU-LIB RR-TASK-MAX. s"  | " RR-OUT
	      dup RR-HABU-STDLIB RR-TASK-MAX. s"  | " RR-OUT
	      dup RR-HABU-SKELETON RR-TASK-MAX. s"  | " RR-OUT
	      dup RR-JS RR-TASK-MAX. s"  | " RR-OUT
	      dup RR-PYTHON RR-TASK-MAX. s"  | " RR-OUT
	      dup RR-TS RR-TASK-MAX. s"  | " RR-OUT
	      dup RR-RUST RR-TASK-MAX. s"  | " RR-OUT
	      dup RR-HABU-A RR-J @ RR-TASK-RATIO. s"  | " RR-OUT
	      dup RR-HABU-LIB RR-J @ RR-TASK-RATIO. s"  | " RR-OUT
	      dup RR-HABU-STDLIB RR-J @ RR-TASK-RATIO. s"  | " RR-OUT
	      dup RR-HABU-SKELETON RR-J @ RR-TASK-RATIO. s"  | " RR-OUT
      s" raw " RR-OUT dup RR-HABU-A RR-ARM-OUTCOMES.
      s" ; helpers " RR-OUT dup RR-HABU-LIB RR-ARM-OUTCOMES.
      s" ; stdlib " RR-OUT dup RR-HABU-STDLIB RR-ARM-OUTCOMES.
      s" ; skeleton " RR-OUT dup RR-HABU-SKELETON RR-ARM-OUTCOMES.
      s" ; js " RR-OUT dup RR-JS RR-ARM-OUTCOMES.
      s" ; python " RR-OUT dup RR-PYTHON RR-ARM-OUTCOMES.
      s" ; ts " RR-OUT dup RR-TS RR-ARM-OUTCOMES.
      s" ; rust " RR-OUT dup RR-RUST RR-ARM-OUTCOMES.
      s"  |" RR-OUT RR-NL
      1+
   repeat drop RR-NL
   s" Cells are max output tokens among passing trials with positive output-token counts. Habu ratios compare each Habu arm with the cheaper mainstream arm; the jump from ~1x on elementwise tasks to the hard-task tail is the main raw-Habu signal." RR-OUT RR-NL ;

: RR-REPORT. ( -- )
   s" # RESULTS.md — Habu vs JavaScript, Python, TypeScript, and Rust: LLM codegen on array/memory algorithms" RR-OUT RR-NL RR-NL
   s" Generated from `results/run.jsonl` (" RR-OUT RR-ROWS @ RR-U. s"  trials). Models: " RR-OUT RR-MODELS.
   s" . Tasks: " RR-OUT RR-TASK-N @ RR-U. s"  algorithms over an integer array (sum/max/min/argmax/count, reverse/prefix-sum/square/negate/running-max)." RR-OUT RR-NL
   s" Raw Habu requires typed pointers, `i cells arr + @`/`!` indexing, in-place mutation, and concatenative" RR-OUT RR-NL
   s" loops — unfamiliar territory for an LLM. The Habu + array helpers arm exposes checked helpers for array access and" RR-OUT RR-NL
   s" common index patterns; JS, Python, TypeScript, and Rust use idiomatic array/list/slice APIs." RR-OUT RR-NL
   RR-HAS-LIB? 0= if
      RR-NL
      s" **Habu + array helpers data is missing from this committed run.** The harness now runs the `habu-lib` arm, but" RR-OUT RR-NL
      s" the checked-in `results/run.jsonl` predates that arm; re-run `sh bench/llm/run-bench.sh 2` to fill it." RR-OUT RR-NL
   then
   RR-NL
   s" _Each task: the model writes the function in the target language; we compile/check + run all io-vectors," RR-OUT RR-NL
   s" feeding the failure (checker/compiler diagnostic or failing case) back for up to 5 repair rounds. A trial" RR-OUT RR-NL
   s" is green only when every io-vector passes. Metric: output tokens to green (input excluded — Claude Code" RR-OUT RR-NL
   s" harness overhead + caching distort it). Output tokens are generated-token cost, not direct access to hidden" RR-OUT RR-NL
   s" reasoning. They are still a useful effort proxy: habu source is terser, yet output tokens run HIGHER on hard tasks" RR-OUT RR-NL
   s" — the reasoning cost of the unfamiliar memory model dominates the terseness saving._" RR-OUT RR-NL RR-NL
   s" ## Evidence Contract" RR-OUT RR-NL RR-NL
   s" V2 live rows are identified by `run_id`, `model_id`, `arm`, `task_id`, and `trial_id`; duplicate full keys are invalid while multiple trials for the same task are expected." RR-OUT RR-NL
   s" Rows also carry `task_family`, `model_version`, `model_date`, trial/order metadata, outcome and repair counters, diagnostic-quality booleans, `source_chars`, and warmed-runtime fields. Unknown model version/date are recorded as `unknown` rather than omitted." RR-OUT RR-NL
   s" Replayable rows retain `prompt`, `raw_response`, `extracted_candidate`, `checker_diagnostics`, `repair_packet`, `test_output`, and `final_bundle`, each with a `*_sha256` field so artifacts can be matched to archived files or inline payloads." RR-OUT RR-NL RR-NL
   s" ## Limitations" RR-OUT RR-NL RR-NL
   s" - **nondeterminism**: model sampling, provider scheduling, local load, and transient tool latency can change individual rows." RR-OUT RR-NL
   s" - **k/N confidence**: pass rates are point estimates for the recorded k trials over N selected tasks, not confidence intervals." RR-OUT RR-NL
   s" - **token proxy limits**: output tokens exclude input, hidden reasoning, prompt-cache effects, and harness overhead." RR-OUT RR-NL
   s" - **scaffold fairness**: each arm gets the same repair budget, but language prompts, compilers, and diagnostics differ." RR-OUT RR-NL
   s" - **library comparability**: `habu-lib` and `habu-stdlib` measure checked helper surfaces, `habu-skeleton` measures scaffold help, while JS, Python, TypeScript, and Rust use their familiar standard library idioms." RR-OUT RR-NL
   s" - **task selection**: the suite stresses integer array and memory algorithms; it does not represent every programming workload." RR-OUT RR-NL
   s" - **environment**: wall/runtime timings are tied to the local machine, OS, toolchain, and current `bin/hb` build." RR-OUT RR-NL
   s" - **deterministic-vs-live boundary**: shell fixtures verify the harness deterministically; benchmark claims require archived live V2 rows." RR-OUT RR-NL RR-NL
   s" ## Reliability" RR-OUT RR-NL RR-NL
   s" | language | trials | green trials | trial pass | first-try green | task pass@k | non-pass rows |" RR-OUT RR-NL
   s" |---|---:|---:|---:|---:|---:|---:|" RR-OUT RR-NL
   0 begin dup RR-ARM# < while dup RR-RELIABILITY-ROW. 1+ repeat drop RR-NL
   s" `trial pass` is passed trials over k. `task pass@k` is any green trial per task+arm+model; a task can have a failed trial and still pass at task level when another trial is green for the same model." RR-OUT RR-NL RR-NL
   s" ## Per-Model Reliability" RR-OUT RR-NL RR-NL
   s" | model | language | trials | green trials | trial pass | first-try green | task pass@k | non-pass rows |" RR-OUT RR-NL
   s" |---|---|---:|---:|---:|---:|---:|---:|" RR-OUT RR-NL
   0 begin dup RR-MODEL-N @ < while
      dup 0 begin dup RR-ARM# < while 2dup RR-PER-MODEL-ROW. 1+ repeat drop
      1+
   repeat drop RR-NL
   s" Aggregate language tables above pool rows only after this per-model breakdown makes each model family visible." RR-OUT RR-NL RR-NL
   s" ## Effort To Green" RR-OUT RR-NL RR-NL
   s" | language | mean rounds | median output tokens | **mean output tokens** | max output tokens | median runtime ms | max runtime ms | median wall s | max wall s |" RR-OUT RR-NL
   s" |---|---:|---:|---:|---:|---:|---:|---:|---:|" RR-OUT RR-NL
   0 begin dup RR-ARM# < while dup RR-EFFORT-ROW. 1+ repeat drop RR-NL
   s" Effort metrics use passing trials with a positive output-token count. Runtime metrics use `runtime_ms`, a warmed candidate execution over fixed vectors and repetitions; wall time remains model/checker/compiler/feedback latency. Mean/max matter more than the median: Habu's cost is skewed — cheap on simple tasks, spiking on hard ones." RR-OUT RR-NL RR-NL
   RR-MISSING-TOKENS-NOTE.
   RR-MISSING-RUNTIME-NOTE.
   s" ## Category Reliability And Effort" RR-OUT RR-NL RR-NL
   s" | category | language | trials | green trials | trial pass | task pass@k | mean rounds | mean output tokens | median runtime ms | diagnostic complete |" RR-OUT RR-NL
   s" |---|---|---:|---:|---:|---:|---:|---:|---:|---:|" RR-OUT RR-NL
   0 begin dup RR-CAT-N @ < while
      dup 0 begin dup RR-ARM# < while 2dup RR-CATEGORY-ROW. 1+ repeat drop
      1+
   repeat drop RR-NL
   s" Category rows keep the same trial pass, task pass@k, repair-round, token, runtime, and diagnostic-quality semantics as the aggregate tables, but make weak task families visible." RR-OUT RR-NL RR-NL
   s" ## Habu Arm Deltas By Category" RR-OUT RR-NL RR-NL
   s" | category | raw task pass@k | stdlib task pass@k | skeleton task pass@k | stdlib - raw pass | skeleton - stdlib pass | stdlib/raw tokens | skeleton/stdlib tokens | stdlib/raw runtime | skeleton/stdlib runtime |" RR-OUT RR-NL
   s" |---|---:|---:|---:|---:|---:|---:|---:|---:|---:|" RR-OUT RR-NL
   0 begin dup RR-CAT-N @ < while dup RR-CATEGORY-DELTA-ROW. 1+ repeat drop RR-NL
   s" Positive pass deltas mean the later Habu arm solved more tasks in that category. Token and runtime ratios below 1x mean the later arm was cheaper among passing trials with measured values." RR-OUT RR-NL RR-NL
   s" ## LLM Feedback Latency" RR-OUT RR-NL RR-NL
   s" Source: `bench/llm/perf.sh --json`; these timings measure local checker/test/report feedback latency, not model inference latency." RR-OUT RR-NL RR-NL
   RR-PERF.
   s" ## Verdict — how does Habu stack up?" RR-OUT RR-NL RR-NL
   s" Task pass@k is reported in the tables above. The raw-Habu cost split is bimodal:" RR-OUT RR-NL RR-NL
   s" - **Simple elementwise loops** (sum, square, negate, max) — raw Habu is **comparable or cheaper** than baseline languages (its source is terse and the pattern is regular)." RR-OUT RR-NL
   s" - **Anything needing index tracking, carried state, or in-place rearrangement** (argmax, reverse, prefix-sum, running-max) remains the hard tail." RR-OUT RR-NL RR-NL
   s" The raw-Habu gap is the corpus-familiarity tax: Habu's typed pointers (`arr:ptr`), `i cells arr + @`/`!` indexing, and in-place concatenative loops have much less model prior than JavaScript arrays, Python lists, TypeScript arrays, or Rust slices. That makes obvious stack shapes cheap and stateful/indexed loops expensive." RR-OUT RR-NL RR-NL
   RR-NONPASS.
   RR-TASK-TABLE. ;

: RR-USAGE ( -- )
   s" bench/llm/report.f RUN.jsonl [perf.json]" ARGV-USAGE! ;

: REPORT-MAIN ( -- )
   RR-USAGE
   ARGV-PARSE
   1 2 ARGV-EXPECT-POS
   0 RR-ROWS ! 0 RR-STR-U ! 0 RR-LINE-U ! 1 RR-LINE# !
   0 ARGV-POS$ RR-SCAN-RUN
   RR-INDEX-DIMENSIONS
   RR-REPORT. ;

REPORT-MAIN
