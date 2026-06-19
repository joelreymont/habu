\ validate-results.f - validate and summarize LLM benchmark metrics.
\ Load after tools/lint/lib.f, tools/json.f, and tools/argv.f.

0 set-check

$8000 constant LV-TASK-CAP
$20000 constant LV-RESULT-CAP
$4000 constant LV-READ-CAP
256 constant LV-MAX
2048 constant LV-ROW-MAX
32 constant LV-NUM-CAP
256 constant LV-PATH-CAP
256 constant LV-RUN-CAP
128 constant LV-MODEL-CAP
$1000 constant LV-RC-STR-CAP
$40000 constant LV-KEY-STR-CAP
64 constant LV-SHA-LEN

0 constant LV-MODE-REFERENCE
1 constant LV-MODE-SUMMARY
45 constant LV-MIN-TASKS

9 constant LV-TAB
10 constant LV-LF
13 constant LV-CR
48 constant LV-ZERO
57 constant LV-NINE
58 constant LV-COLON
65 constant LV-UPPER-A
70 constant LV-UPPER-F
97 constant LV-LOWER-A
102 constant LV-LOWER-F

create LV-TASK-BUF LV-TASK-CAP allot
create LV-RESULT-BUF LV-RESULT-CAP allot
create LV-READ-BUF LV-READ-CAP allot
create LV-NUM-BUF LV-NUM-CAP allot
create LV-RUN-BUF LV-RUN-CAP allot
create LV-MODEL-BUF LV-MODEL-CAP allot
create LV-RC-STR LV-RC-STR-CAP allot
create LV-KEY-STR LV-KEY-STR-CAP allot

create LV-TASK-ID LV-MAX cells allot
create LV-TASK-NAME-A LV-MAX cells allot
create LV-TASK-NAME-U LV-MAX cells allot
create LV-TASK-CAT-A LV-MAX cells allot
create LV-TASK-CAT-U LV-MAX cells allot
create LV-SEEN-ID LV-MAX cells allot

create LV-KEY-TASK-ID LV-ROW-MAX cells allot
create LV-KEY-RUN-A LV-ROW-MAX cells allot
create LV-KEY-RUN-U LV-ROW-MAX cells allot
create LV-KEY-MODEL-A LV-ROW-MAX cells allot
create LV-KEY-MODEL-U LV-ROW-MAX cells allot
create LV-KEY-ARM-A LV-ROW-MAX cells allot
create LV-KEY-ARM-U LV-ROW-MAX cells allot
create LV-KEY-TRIAL-A LV-ROW-MAX cells allot
create LV-KEY-TRIAL-U LV-ROW-MAX cells allot

create LV-CAT-A LV-MAX cells allot
create LV-CAT-U LV-MAX cells allot
create LV-CAT-ROWS LV-MAX cells allot
create LV-CAT-CERT LV-MAX cells allot
create LV-CAT-TESTS LV-MAX cells allot

create LV-RC-A LV-MAX cells allot
create LV-RC-U LV-MAX cells allot
create LV-RC-ROWS LV-MAX cells allot
create LV-RC-SUCCESS LV-MAX cells allot
create LV-RC-REPAIRS LV-MAX cells allot
create LV-RC-DIAGS LV-MAX cells allot
create LV-RC-TOKDELTA LV-MAX cells allot

variable LV-TASK#
variable LV-REF-TASK#
variable LV-SEEN#
variable LV-CAT#
variable LV-RC#
variable LV-KEY#
variable LV-RC-STR-U
variable LV-KEY-STR-U
variable LV-SCHEMA
variable LV-SCHEMA-SET
variable LV-NUM-I
variable LV-I
variable LV-J
variable LV-H
variable LV-C
variable LV-M
variable LV-N
variable LV-ID
variable LV-ROOT
variable LV-NODE
variable LV-LINE
variable LV-LA
variable LV-LU
variable LV-LX
variable LV-LS
variable LV-LE
variable LV-A
variable LV-U
variable LV-K
variable LV-TASK-K
variable LV-P
variable LV-MODE
variable LV-JSON
variable LV-RESULT-PATH-A
variable LV-RESULT-PATH-U
variable LV-RUN-U
variable LV-RUN-SET
variable LV-MODEL-U
variable LV-MODEL-SET
variable LV-CUR-RUN-A
variable LV-CUR-RUN-U
variable LV-CUR-MODEL-A
variable LV-CUR-MODEL-U
variable LV-CUR-ARM-A
variable LV-CUR-ARM-U
variable LV-CUR-TRIAL-A
variable LV-CUR-TRIAL-U
variable LV-ROWS
variable LV-CERT
variable LV-FIRST-TESTS
variable LV-TESTS
variable LV-REPAIRS
variable LV-CHECKERS
variable LV-DIAGS
variable LV-DTOK
variable LV-DSPAN
variable LV-DEXPECT
variable LV-DACTUAL
variable LV-DCODE
variable LV-DCLASS
variable LV-AESTABLE
variable LV-TOKENS
variable LV-WALL
variable LV-CHARS
variable LV-TRUST
variable LV-SIGWEAK
variable LV-BAD-CHECKER
variable LV-BAD-FIRST
variable LV-BAD-TESTS
variable LV-BAD-TRUST
variable LV-BAD-SIG
variable LV-BAD-DTOK
variable LV-BAD-DSPAN
variable LV-BAD-DEXP
variable LV-BAD-DACT
variable LV-BAD-DCODE
variable LV-BAD-DCLASS
variable LV-BAD-AE
variable LV-ROW-DIAGS
variable LV-ROW-TESTS
variable LV-RC-DIAG
variable LV-RC-SUCC
variable LV-RC-ROUND
variable LV-RC-TOK
variable LV-RC-SUM
variable LV-RFD
variable LV-RGOT
variable LV-BI
variable LV-LINE-U

: LV-CHECK-HOOK ( -- )
   CHECK! ;
' LV-CHECK-HOOK set-check

: LV-OUT ( ptr u8 n -- ) type ;
: LV-NL ( -- ) 10 emit ;
: LV-SPACE ( -- ) 32 emit ;
: LV-EQ ( -- ) 61 emit ;

: LV-U$ {: u :} ( n -- ptr u8 n )
   LV-NUM-CAP LV-NUM-I !
   u 0= IF
      LV-NUM-I @ 1- LV-NUM-I !
      LV-ZERO LV-NUM-BUF LV-NUM-I @ + c!
      LV-NUM-BUF LV-NUM-I @ + 1
      exit
   THEN
   u begin dup 0 > while
      dup 10 mod LV-ZERO +
      LV-NUM-I @ 1- LV-NUM-I !
      LV-NUM-BUF LV-NUM-I @ + c!
      10 /
   repeat drop
   LV-NUM-BUF LV-NUM-I @ + LV-NUM-CAP LV-NUM-I @ - ;

: LV-U. ( n -- )
   LV-U$ LV-OUT ;

: LV-CELL++ ( ptr n -- )
   dup @ 1+ swap ! ;

: LV-CELL+! {: n a :} ( n ptr n -- )
   a @ n + a ! ;

: LV-RESULT-PATH$ ( -- ptr u8 n )
   LV-RESULT-PATH-A @ LV-RESULT-PATH-U @ ;

: LV-RESULT-PATH! {: a:ptr u :} ( ptr u8 n -- )
   a LV-RESULT-PATH-A !
   u LV-RESULT-PATH-U ! ;

: LV-FAIL ( ptr u8 n -- )
   s" llm-results: " LV-OUT
   LV-OUT
   LV-NL
   1 throw ;

: LV-FAIL-AT ( ptr u8 n -- )
   s" llm-results: " LV-OUT
   LV-RESULT-PATH$ LV-OUT
   s" :" LV-OUT
   LV-LINE @ LV-U.
   s" : " LV-OUT
   LV-OUT
   LV-NL
   1 throw ;

: LV-FAIL-AT-ID {: a:ptr u id :} ( ptr u8 n n -- )
   s" llm-results: " LV-OUT
   LV-RESULT-PATH$ LV-OUT
   s" :" LV-OUT
   LV-LINE @ LV-U.
   s" : " LV-OUT
   a u LV-OUT
   id LV-U.
   LV-NL
   1 throw ;

: LV-LINE-LEN ( ptr u8 n -- ptr u8 n )
   dup 0 > IF
      2dup + 1- c@ LV-CR = IF 1- THEN
   THEN ;

: LV-TAB-AT ( ptr u8 n n -- n )
   LV-P ! LV-U ! LV-A !
   LV-P @ begin dup LV-U @ < while
      LV-A @ over + c@ LV-TAB = IF exit THEN
      1+
   repeat drop -1 ;

: LV-U? ( ptr u8 n -- n bool )
   LV-U ! LV-A !
   LV-U @ 0= IF 0 0 exit THEN
   0 LV-K !
   0
   begin LV-K @ LV-U @ < while
      LV-A @ LV-K @ + c@ dup LV-ZERO < over LV-NINE > or IF drop drop 0 0 exit THEN
      LV-ZERO - swap 10 * +
      LV-K @ 1+ LV-K !
   repeat -1 ;

: LV-TASK-ID@ ( n -- n )
   cells LV-TASK-ID + @ ;

: LV-TASK-ID! ( n n -- )
   cells LV-TASK-ID + ! ;

: LV-TASK-NAME! ( ptr u8 n n -- )
   LV-K ! LV-U ! LV-A !
   LV-A @ LV-TASK-NAME-A LV-K @ cells + !
   LV-U @ LV-TASK-NAME-U LV-K @ cells + ! ;

: LV-TASK-NAME$ {: k :} ( n -- ptr u8 n )
   LV-TASK-NAME-A k cells + @
   LV-TASK-NAME-U k cells + @ ;

: LV-TASK-CAT! ( ptr u8 n n -- )
   LV-K ! LV-U ! LV-A !
   LV-A @ LV-TASK-CAT-A LV-K @ cells + !
   LV-U @ LV-TASK-CAT-U LV-K @ cells + ! ;

: LV-TASK-CAT$ {: k :} ( n -- ptr u8 n )
   LV-TASK-CAT-A k cells + @
   LV-TASK-CAT-U k cells + @ ;

: LV-FIND-TASK-CAT {: a:ptr u :} ( ptr u8 n -- n )
   0 begin dup LV-TASK# @ < while
      dup LV-TASK-CAT$ a u STR= IF exit THEN
      1+
   repeat drop -1 ;

: LV-MISSING-CAT {: a:ptr u :} ( ptr u8 n -- )
   s" llm-results: missing required benchmark category " LV-OUT
   a u LV-OUT
   LV-NL
   1 throw ;

: LV-REQ-TASK-CAT {: a:ptr u :} ( ptr u8 n -- )
   a u LV-FIND-TASK-CAT 0< IF a u LV-MISSING-CAT THEN ;

: LV-CHECK-TASK-COVERAGE ( -- )
   LV-TASK# @ LV-MIN-TASKS < IF
      s" llm-results: need at least " LV-OUT
      LV-MIN-TASKS LV-U.
      s"  tasks, found " LV-OUT
      LV-TASK# @ LV-U.
      LV-NL
      1 throw
   THEN
   s" quotation" LV-REQ-TASK-CAT
   s" return-stack" LV-REQ-TASK-CAT
   s" strings" LV-REQ-TASK-CAT
   s" files" LV-REQ-TASK-CAT
   s" aot-safe" LV-REQ-TASK-CAT ;

: LV-SEEN@ ( n -- n )
   cells LV-SEEN-ID + @ ;

: LV-SEEN! ( n n -- )
   cells LV-SEEN-ID + ! ;

: LV-FIND-TASK {: id :} ( n -- n )
   0 begin dup LV-TASK# @ < while
      dup LV-TASK-ID@ id = IF exit THEN
      1+
   repeat drop -1 ;

: LV-FORTH-HARNESS? {: a:ptr :} ( ptr u8 -- bool )
   a LV-M @ 1+ + LV-N @ LV-M @ 1+ - s" forth" STR= ;

: LV-FIND-SEEN {: id :} ( n -- n )
   0 begin dup LV-SEEN# @ < while
      dup LV-SEEN@ id = IF exit THEN
      1+
   repeat drop -1 ;

: LV-SEEN+ {: id :} ( n -- )
   LV-SEEN# @ LV-MAX >= IF s" too many result rows" LV-FAIL THEN
   id LV-SEEN# @ LV-SEEN!
   LV-SEEN# @ 1+ LV-SEEN# ! ;

: LV-KEY-COPY$ {: a:ptr u :} ( ptr u8 n -- ptr u8 n )
   LV-KEY-STR-U @ u + LV-KEY-STR-CAP > IF s" result identity strings too long" LV-FAIL THEN
   LV-KEY-STR LV-KEY-STR-U @ + LV-A !
   u LV-U !
   a LV-A @ u BMOVE
   LV-KEY-STR-U @ u + LV-KEY-STR-U !
   LV-A @ LV-U @ ;

: LV-CUR-KEY-MATCH? {: k :} ( n -- bool )
   LV-KEY-TASK-ID k cells + @ LV-ID @ <> IF 0 exit THEN
   LV-KEY-RUN-A k cells + @ LV-KEY-RUN-U k cells + @ LV-CUR-RUN-A @ LV-CUR-RUN-U @ STR= 0= IF 0 exit THEN
   LV-KEY-MODEL-A k cells + @ LV-KEY-MODEL-U k cells + @ LV-CUR-MODEL-A @ LV-CUR-MODEL-U @ STR= 0= IF 0 exit THEN
   LV-KEY-ARM-A k cells + @ LV-KEY-ARM-U k cells + @ LV-CUR-ARM-A @ LV-CUR-ARM-U @ STR= 0= IF 0 exit THEN
   LV-KEY-TRIAL-A k cells + @ LV-KEY-TRIAL-U k cells + @ LV-CUR-TRIAL-A @ LV-CUR-TRIAL-U @ STR= ;

: LV-FIND-KEY ( -- n )
   0 begin dup LV-KEY# @ < while
      dup LV-CUR-KEY-MATCH? IF exit THEN
      1+
   repeat drop -1 ;

: LV-KEY+ ( -- )
   LV-KEY# @ LV-ROW-MAX >= IF s" too many result identities" LV-FAIL THEN
   LV-ID @ LV-KEY-TASK-ID LV-KEY# @ cells + !
   LV-CUR-RUN-A @ LV-CUR-RUN-U @ LV-KEY-COPY$ LV-KEY-RUN-U LV-KEY# @ cells + ! LV-KEY-RUN-A LV-KEY# @ cells + !
   LV-CUR-MODEL-A @ LV-CUR-MODEL-U @ LV-KEY-COPY$ LV-KEY-MODEL-U LV-KEY# @ cells + ! LV-KEY-MODEL-A LV-KEY# @ cells + !
   LV-CUR-ARM-A @ LV-CUR-ARM-U @ LV-KEY-COPY$ LV-KEY-ARM-U LV-KEY# @ cells + ! LV-KEY-ARM-A LV-KEY# @ cells + !
   LV-CUR-TRIAL-A @ LV-CUR-TRIAL-U @ LV-KEY-COPY$ LV-KEY-TRIAL-U LV-KEY# @ cells + ! LV-KEY-TRIAL-A LV-KEY# @ cells + !
   LV-KEY# @ 1+ LV-KEY# ! ;

: LV-CAT-ROWS@ ( n -- n )
   cells LV-CAT-ROWS + @ ;

: LV-CAT-CERT@ ( n -- n )
   cells LV-CAT-CERT + @ ;

: LV-CAT-TESTS@ ( n -- n )
   cells LV-CAT-TESTS + @ ;

: LV-CAT-ROWS++ ( n -- )
   cells LV-CAT-ROWS + LV-CELL++ ;

: LV-CAT-CERT++ ( n -- )
   cells LV-CAT-CERT + LV-CELL++ ;

: LV-CAT-TESTS++ ( n -- )
   cells LV-CAT-TESTS + LV-CELL++ ;

: LV-FIND-CAT {: a:ptr u :} ( ptr u8 n -- n )
   0 begin dup LV-CAT# @ < while
      dup cells LV-CAT-A + @
      over cells LV-CAT-U + @
      a u STR= IF exit THEN
      1+
   repeat drop -1 ;

: LV-CAT+ {: a:ptr u :} ( ptr u8 n -- n )
   LV-CAT# @ LV-MAX >= IF s" too many categories" LV-FAIL THEN
   LV-CAT# @ LV-P !
   a LV-CAT-A LV-P @ cells + !
   u LV-CAT-U LV-P @ cells + !
   0 LV-CAT-ROWS LV-P @ cells + !
   0 LV-CAT-CERT LV-P @ cells + !
   0 LV-CAT-TESTS LV-P @ cells + !
   LV-CAT# @ 1+ LV-CAT# !
   LV-P @ ;

: LV-CAT-ID {: a:ptr u :} ( ptr u8 n -- n )
   a u LV-FIND-CAT dup 0 >= IF exit THEN
   drop a u LV-CAT+ ;

: LV-RC$ {: k :} ( n -- ptr u8 n )
   LV-RC-A k cells + @
   LV-RC-U k cells + @ ;

: LV-RC-ROWS@ ( n -- n )
   cells LV-RC-ROWS + @ ;

: LV-RC-SUCCESS@ ( n -- n )
   cells LV-RC-SUCCESS + @ ;

: LV-RC-REPAIRS@ ( n -- n )
   cells LV-RC-REPAIRS + @ ;

: LV-RC-DIAGS@ ( n -- n )
   cells LV-RC-DIAGS + @ ;

: LV-RC-TOKDELTA@ ( n -- n )
   cells LV-RC-TOKDELTA + @ ;

: LV-RC-ROWS++ ( n -- )
   cells LV-RC-ROWS + LV-CELL++ ;

: LV-RC-SUCCESS++ ( n -- )
   cells LV-RC-SUCCESS + LV-CELL++ ;

: LV-RC-REPAIRS+ {: n k :} ( n n -- )
   n LV-RC-REPAIRS k cells + LV-CELL+! ;

: LV-RC-DIAGS+ {: n k :} ( n n -- )
   n LV-RC-DIAGS k cells + LV-CELL+! ;

: LV-RC-TOKDELTA+ {: n k :} ( n n -- )
   n LV-RC-TOKDELTA k cells + LV-CELL+! ;

: LV-FIND-RC {: a:ptr u :} ( ptr u8 n -- n )
   0 begin dup LV-RC# @ < while
      dup LV-RC$ a u STR= IF exit THEN
      1+
   repeat drop -1 ;

: LV-RC-COPY! {: a:ptr u k :} ( ptr u8 n n -- )
   LV-RC-STR-U @ u + LV-RC-STR-CAP > IF s" repair class names too long" LV-FAIL THEN
   a LV-RC-STR LV-RC-STR-U @ + u BMOVE
   LV-RC-STR LV-RC-STR-U @ + LV-RC-A k cells + !
   u LV-RC-U k cells + !
   LV-RC-STR-U @ u + LV-RC-STR-U ! ;

: LV-RC+ {: a:ptr u :} ( ptr u8 n -- n )
   LV-RC# @ LV-MAX >= IF s" too many repair classes" LV-FAIL THEN
   LV-RC# @ LV-P !
   a u LV-P @ LV-RC-COPY!
   0 LV-RC-ROWS LV-P @ cells + !
   0 LV-RC-SUCCESS LV-P @ cells + !
   0 LV-RC-REPAIRS LV-P @ cells + !
   0 LV-RC-DIAGS LV-P @ cells + !
   0 LV-RC-TOKDELTA LV-P @ cells + !
   LV-RC# @ 1+ LV-RC# !
   LV-P @ ;

: LV-RC-ID {: a:ptr u :} ( ptr u8 n -- n )
   a u LV-FIND-RC dup 0 >= IF exit THEN
   drop a u LV-RC+ ;

: LV-TASK-LINE {: a:ptr u :} ( ptr u8 n -- )
   LV-LINE @ 1 = IF exit THEN
   LV-TASK# @ LV-MAX >= IF s" too many tasks" LV-FAIL THEN
   a u 0 LV-TAB-AT dup 0 < IF drop exit THEN
   LV-I !
   a u LV-I @ 1+ LV-TAB-AT dup 0 < IF drop exit THEN
   LV-J !
   a u LV-J @ 1+ LV-TAB-AT dup 0 < IF drop exit THEN
   LV-H !
   a u LV-H @ 1+ LV-TAB-AT dup 0 < IF drop exit THEN
   LV-C !
   a u LV-C @ 1+ LV-TAB-AT dup 0 < IF drop exit THEN
   LV-M !
   a u LV-M @ 1+ LV-TAB-AT dup 0 < IF drop exit THEN
   LV-N !
   a LV-I @ LV-U? 0= IF drop s" invalid task id" LV-FAIL THEN
   LV-TASK# @ LV-TASK-ID!
   a LV-I @ 1+ + LV-J @ LV-I @ 1+ - LV-TASK# @ LV-TASK-NAME!
   a LV-H @ 1+ + LV-C @ LV-H @ 1+ - LV-TASK# @ LV-TASK-CAT!
   a LV-FORTH-HARNESS? IF LV-REF-TASK# LV-CELL++ THEN
   LV-TASK# @ 1+ LV-TASK# ! ;

: LV-DO-TASK-LINE ( n -- )
   LV-LE !
   LV-LINE @ 1+ LV-LINE !
   LV-LA @ LV-LS @ + LV-LE @ LV-LS @ - LV-LINE-LEN
   LV-TASK-LINE
   LV-LE @ 1+ LV-LS ! ;

: LV-FOR-TASK-LINES ( ptr u8 n -- )
   LV-LU ! LV-LA !
   0 LV-LINE ! 0 LV-LX ! 0 LV-LS !
   begin LV-LX @ LV-LU @ < while
      LV-LA @ LV-LX @ + c@ LV-LF = IF LV-LX @ LV-DO-TASK-LINE THEN
      LV-LX @ 1+ LV-LX !
   repeat
   LV-LS @ LV-LU @ < IF LV-LU @ LV-DO-TASK-LINE THEN ;

: LV-SCAN-TASKS ( -- )
   0 LV-TASK# !
   0 LV-REF-TASK# !
   s" bench/llm/tasks.tsv" LV-TASK-BUF LV-TASK-CAP READ-FILE LV-FOR-TASK-LINES
   LV-CHECK-TASK-COVERAGE ;

: LV-GET {: root a:ptr u :} ( n ptr u8 n -- n )
   root a u JSON-GET ;

: LV-HAS? ( n ptr u8 n -- bool )
   LV-GET -1 <> ;

: LV-MISSING {: a:ptr u :} ( ptr u8 n -- )
   s" llm-results: " LV-OUT
   LV-RESULT-PATH$ LV-OUT
   s" :" LV-OUT
   LV-LINE @ LV-U.
   s" : missing fields " LV-OUT
   a u LV-OUT
   LV-NL
   1 throw ;

: LV-REQ {: root a:ptr u :} ( n ptr u8 n -- )
   root a u LV-HAS? 0= IF a u LV-MISSING THEN ;

: LV-REQS {: root :} ( n -- )
   root s" schema_version" LV-REQ
   root s" run_id" LV-REQ
   root s" task_id" LV-REQ
   root s" name" LV-REQ
   root s" model" LV-REQ
   root s" attempt" LV-REQ
   root s" first_pass_checker" LV-REQ
   root s" first_pass_tests" LV-REQ
   root s" tests_passed" LV-REQ
   root s" repair_iterations" LV-REQ
   root s" checker_iterations" LV-REQ
   root s" diagnostic_count" LV-REQ
   root s" diagnostic_token" LV-REQ
   root s" diagnostic_span" LV-REQ
   root s" diagnostic_expected" LV-REQ
   root s" diagnostic_actual" LV-REQ
   root s" diagnostic_code" LV-REQ
   root s" diagnostic_repair_class" LV-REQ
   root s" all_errors_stable" LV-REQ
   root s" tokens_used" LV-REQ
   root s" wall_ms" LV-REQ
   root s" final_chars" LV-REQ
   root s" trust_uses" LV-REQ
   root s" signature_weakened" LV-REQ ;

: LV-INT-FIELD {: root a:ptr u :} ( n ptr u8 n -- n )
   root a u LV-GET dup JSON-KIND J-NUM <> IF drop s" invalid integer field" LV-FAIL-AT THEN
   JSON-NUMBER$ LV-U? 0= IF drop s" invalid integer field" LV-FAIL-AT THEN ;

: LV-STR-FIELD {: root a:ptr u :} ( n ptr u8 n -- ptr u8 n )
   root a u LV-GET dup JSON-KIND J-STR <> IF drop s" invalid string field" LV-FAIL-AT THEN
   JSON-STRING$ ;

: LV-BOOL-FIELD {: root a:ptr u :} ( n ptr u8 n -- bool )
   root a u LV-GET dup JSON-KIND J-BOOL <> IF drop s" invalid bool field" LV-FAIL-AT THEN
   JSON-BOOL@ ;

: LV-CHECK-SCHEMA {: n :} ( n -- )
   n 1 <> n 2 <> and IF s" unsupported schema_version" LV-FAIL-AT THEN
   LV-SCHEMA-SET @ 0= IF
      n LV-SCHEMA !
      -1 LV-SCHEMA-SET !
      exit
   THEN
   n LV-SCHEMA @ <> IF s" mixed schema_version values" LV-FAIL-AT THEN ;

: LV-HEX? {: c :} ( n -- bool )
   c LV-ZERO >= c LV-NINE <= and
   c LV-UPPER-A >= c LV-UPPER-F <= and or
   c LV-LOWER-A >= c LV-LOWER-F <= and or ;

: LV-SHA256? ( ptr u8 n -- bool )
   LV-U ! LV-A !
   LV-U @ LV-SHA-LEN <> IF 0 exit THEN
   0 LV-K !
   begin LV-K @ LV-U @ < while
      LV-A @ LV-K @ + c@ LV-HEX? 0= IF 0 exit THEN
      LV-K @ 1+ LV-K !
   repeat -1 ;

: LV-CHECK-ARTIFACT {: root a:ptr u ha:ptr hu must :} ( n ptr u8 n ptr u8 n bool -- )
   root a u LV-REQ
   root a u LV-STR-FIELD
   must IF dup 0= IF 2drop s" empty string field" LV-FAIL-AT THEN THEN
   2drop
   root ha hu LV-REQ
   root ha hu LV-STR-FIELD LV-SHA256? 0= IF s" invalid sha256 hash" LV-FAIL-AT THEN ;

: LV-CHECK-V2-ARTIFACTS {: root :} ( n -- )
   root s" prompt" s" prompt_sha256" -1 LV-CHECK-ARTIFACT
   root s" raw_response" s" raw_response_sha256" -1 LV-CHECK-ARTIFACT
   root s" extracted_candidate" s" extracted_candidate_sha256" -1 LV-CHECK-ARTIFACT
   root s" checker_diagnostics" s" checker_diagnostics_sha256" 0 LV-CHECK-ARTIFACT
   root s" repair_packet" s" repair_packet_sha256" 0 LV-CHECK-ARTIFACT
   root s" test_output" s" test_output_sha256" 0 LV-CHECK-ARTIFACT
   root s" final_bundle" s" final_bundle_sha256" -1 LV-CHECK-ARTIFACT ;

: LV-CHECK-V2-IDENTITY {: root :} ( n -- )
   root s" run_id" LV-REQ
   root s" run_id" LV-STR-FIELD dup 0= IF 2drop s" empty string field" LV-FAIL-AT THEN LV-CUR-RUN-U ! LV-CUR-RUN-A !
   root s" model_id" LV-REQ
   root s" model_id" LV-STR-FIELD dup 0= IF 2drop s" empty string field" LV-FAIL-AT THEN LV-CUR-MODEL-U ! LV-CUR-MODEL-A !
   root s" arm" LV-REQ
   root s" arm" LV-STR-FIELD dup 0= IF 2drop s" empty string field" LV-FAIL-AT THEN LV-CUR-ARM-U ! LV-CUR-ARM-A !
   root s" trial_id" LV-REQ
   root s" trial_id" LV-STR-FIELD dup 0= IF 2drop s" empty string field" LV-FAIL-AT THEN LV-CUR-TRIAL-U ! LV-CUR-TRIAL-A !
   LV-FIND-KEY 0 >= IF s" duplicate result identity" LV-FAIL-AT THEN
   LV-KEY+ ;

: LV-REQ-NONEMPTY-STR {: root a:ptr u :} ( n ptr u8 n -- )
   root a u LV-REQ
   root a u LV-STR-FIELD dup 0= IF 2drop s" empty string field" LV-FAIL-AT THEN
   2drop ;

: LV-REQ-NONNEG-INT {: root a:ptr u :} ( n ptr u8 n -- n )
   root a u LV-REQ
   root a u LV-INT-FIELD dup 0 < IF drop s" invalid integer field" LV-FAIL-AT THEN ;

: LV-CHECK-TASK-FAMILY {: root :} ( n -- )
   root s" task_family" LV-REQ
   root s" task_family" LV-STR-FIELD dup 0= IF 2drop s" empty string field" LV-FAIL-AT THEN
   LV-TASK-K @ LV-TASK-CAT$ STR= 0= IF s" task_family/category drift" LV-FAIL-AT THEN ;

: LV-CHECK-DATE-OR-UNKNOWN {: a:ptr u :} ( ptr u8 n -- )
   a u s" unknown" STR= IF exit THEN
   u DATE-LEN <> IF s" invalid model_date" LV-FAIL-AT THEN
   a u PARSE-YMD 0= IF drop s" invalid model_date" LV-FAIL-AT THEN
   drop ;

: LV-CHECK-MODEL-DATE {: root :} ( n -- )
   root s" model_date" LV-REQ
   root s" model_date" LV-STR-FIELD dup 0= IF 2drop s" empty string field" LV-FAIL-AT THEN
   LV-CHECK-DATE-OR-UNKNOWN ;

: LV-CHECK-RUNTIME-MS {: root :} ( n -- )
   root s" runtime_ms" LV-REQ
   root s" runtime_ms" LV-GET dup JSON-KIND J-NULL = IF drop exit THEN
   dup JSON-KIND J-NUM <> IF drop s" invalid runtime_ms" LV-FAIL-AT THEN
   JSON-NUMBER$ LV-U? 0= IF drop s" invalid runtime_ms" LV-FAIL-AT THEN
   drop ;

: LV-CHECK-V2-META {: root :} ( n -- )
   root LV-CHECK-TASK-FAMILY
   root s" model_version" LV-REQ-NONEMPTY-STR
   root LV-CHECK-MODEL-DATE
   root s" trial" LV-REQ-NONNEG-INT drop
   root s" task_order" LV-REQ-NONNEG-INT drop
   root s" k_trials" LV-REQ-NONNEG-INT drop
   root s" order_seed" LV-REQ-NONEMPTY-STR
   root s" outcome" LV-REQ-NONEMPTY-STR
   root s" rounds" LV-REQ-NONNEG-INT drop
   root s" first_pass" LV-REQ
   root s" first_pass" LV-BOOL-FIELD drop
   root s" tokens" LV-REQ-NONNEG-INT drop
   root s" source_chars" LV-REQ-NONNEG-INT dup 0 <= IF drop s" invalid source_chars" LV-FAIL-AT THEN drop
   root LV-CHECK-RUNTIME-MS
   root s" runtime_repetitions" LV-REQ-NONNEG-INT drop
   root s" runtime_warmups" LV-REQ-NONNEG-INT drop
   root s" runtime_status" LV-REQ-NONEMPTY-STR ;

: LV-CHECK-INT= {: root a:ptr u want msg:ptr mu :} ( n ptr u8 n n ptr u8 n -- )
   root a u LV-INT-FIELD want <> IF msg mu LV-FAIL-AT THEN ;

: LV-CHECK-STR= {: root a:ptr u want:ptr wu msg:ptr mu :} ( n ptr u8 n ptr u8 n ptr u8 n -- )
   root a u LV-STR-FIELD want wu STR= 0= IF msg mu LV-FAIL-AT THEN ;

: LV-CHECK-BOOL= {: root a:ptr u want msg:ptr mu :} ( n ptr u8 n bool ptr u8 n -- )
   root a u LV-BOOL-FIELD want <> IF msg mu LV-FAIL-AT THEN ;

: LV-CERTIFIED? {: root :} ( n -- bool )
   root s" first_pass_checker" LV-STR-FIELD s" certified" STR= ;

: LV-RUN-CHECK {: a:ptr u :} ( ptr u8 n -- )
   LV-RUN-SET @ 0= IF
      u LV-RUN-CAP > IF s" run_id too long" LV-FAIL-AT THEN
      a LV-RUN-BUF u BMOVE
      u LV-RUN-U !
      -1 LV-RUN-SET !
      exit
   THEN
   a u LV-RUN-BUF LV-RUN-U @ STR= 0= IF s" mixed run_id values" LV-FAIL-AT THEN ;

: LV-DATE-RUN? {: a:ptr u :} ( ptr u8 n -- bool )
   u DATE-LEN 1+ < IF 0 0= 0= exit THEN
   a u DATE-LEN - 1- + c@ DATE-DASH = ;

: LV-CHECK-RUN-DATE {: a:ptr u :} ( ptr u8 n -- )
   a u LV-DATE-RUN? IF
      a u DATE-LEN - + DATE-LEN PARSE-YMD 0= IF
         drop s" invalid run_id date" LV-FAIL-AT
      THEN
      drop
   THEN ;

: LV-MODEL-CHECK {: a:ptr u :} ( ptr u8 n -- )
   LV-MODEL-SET @ 0= IF
      u LV-MODEL-CAP > IF s" model too long" LV-FAIL-AT THEN
      a LV-MODEL-BUF u BMOVE
      u LV-MODEL-U !
      -1 LV-MODEL-SET !
      exit
   THEN
   a u LV-MODEL-BUF LV-MODEL-U @ STR= 0= IF s" mixed model values" LV-FAIL-AT THEN ;

: LV-RUN$ ( -- ptr u8 n )
   LV-RUN-BUF LV-RUN-U @ ;

: LV-MODEL$ ( -- ptr u8 n )
   LV-MODEL-BUF LV-MODEL-U @ ;

: LV-CHECK-STRING-META {: root :} ( n -- )
   root s" run_id" LV-STR-FIELD
   dup 0= IF 2drop s" empty run_id" LV-FAIL-AT THEN
   2dup LV-CHECK-RUN-DATE
   LV-MODE @ LV-MODE-SUMMARY = IF LV-RUN-CHECK ELSE 2drop THEN
   root s" model" LV-STR-FIELD
   dup 0= IF 2drop s" empty model" LV-FAIL-AT THEN
   LV-MODE @ LV-MODE-SUMMARY = IF LV-MODEL-CHECK ELSE 2drop THEN ;

: LV-CHECK-COMMON {: root :} ( n -- )
   root LV-REQS
   root s" schema_version" LV-INT-FIELD LV-CHECK-SCHEMA
   root s" task_id" LV-INT-FIELD LV-ID !
   LV-SCHEMA @ 2 = IF
      root LV-CHECK-V2-IDENTITY
      LV-ID @ LV-FIND-SEEN 0 < IF LV-ID @ LV-SEEN+ THEN
   ELSE
      LV-ID @ LV-FIND-SEEN 0 >= IF s" duplicate task_id " LV-ID @ LV-FAIL-AT-ID THEN
      LV-ID @ LV-SEEN+
   THEN
   LV-ID @ LV-FIND-TASK dup 0 < IF drop s" task/name drift for id " LV-ID @ LV-FAIL-AT-ID THEN
   LV-TASK-K !
   root s" name" LV-STR-FIELD LV-TASK-K @ LV-TASK-NAME$ STR= 0= IF s" task/name drift for id " LV-ID @ LV-FAIL-AT-ID THEN
   root LV-CHECK-STRING-META
   LV-SCHEMA @ 2 = IF root LV-CHECK-V2-META THEN
   root s" attempt" LV-INT-FIELD drop
   root s" first_pass_checker" LV-STR-FIELD 2drop
   root s" first_pass_tests" LV-BOOL-FIELD drop
   root s" tests_passed" LV-BOOL-FIELD drop
   root s" repair_iterations" LV-INT-FIELD drop
   root s" checker_iterations" LV-INT-FIELD drop
   root s" diagnostic_count" LV-INT-FIELD drop
   root s" diagnostic_token" LV-BOOL-FIELD drop
   root s" diagnostic_span" LV-BOOL-FIELD drop
   root s" diagnostic_expected" LV-BOOL-FIELD drop
   root s" diagnostic_actual" LV-BOOL-FIELD drop
   root s" diagnostic_code" LV-BOOL-FIELD drop
   root s" diagnostic_repair_class" LV-BOOL-FIELD drop
   root s" all_errors_stable" LV-BOOL-FIELD drop
   root s" tokens_used" LV-INT-FIELD drop
   root s" wall_ms" LV-INT-FIELD drop
   root s" final_chars" LV-INT-FIELD 0 <= IF s" invalid final_chars" LV-FAIL-AT THEN
   root s" trust_uses" LV-INT-FIELD drop
   root s" signature_weakened" LV-BOOL-FIELD drop
   LV-SCHEMA @ 2 = IF root LV-CHECK-V2-ARTIFACTS THEN ;

: LV-CHECK-REFERENCE {: root :} ( n -- )
   root s" schema_version" 1 s" reference schema_version must be 1" LV-CHECK-INT=
   root s" model" s" reference" s" reference file contains non-reference model" LV-CHECK-STR=
   root s" attempt" 1 s" reference should be attempt 1" LV-CHECK-INT=
   root s" first_pass_checker" s" certified" s" reference solution not certified" LV-CHECK-STR=
   root s" first_pass_tests" -1 s" reference tests not passing" LV-CHECK-BOOL=
   root s" tests_passed" -1 s" final tests not passing" LV-CHECK-BOOL=
   root s" repair_iterations" 0 s" reference should need zero repairs" LV-CHECK-INT=
   root s" checker_iterations" 1 s" reference should need one checker iteration" LV-CHECK-INT=
   root s" diagnostic_count" 0 s" reference should have zero diagnostics" LV-CHECK-INT=
   root s" diagnostic_token" -1 s" reference should have token diagnostics available" LV-CHECK-BOOL=
   root s" diagnostic_span" -1 s" reference should have span diagnostics available" LV-CHECK-BOOL=
   root s" diagnostic_expected" -1 s" reference should have expected diagnostics available" LV-CHECK-BOOL=
   root s" diagnostic_actual" -1 s" reference should have actual diagnostics available" LV-CHECK-BOOL=
   root s" diagnostic_code" -1 s" reference should have code diagnostics available" LV-CHECK-BOOL=
   root s" diagnostic_repair_class" -1 s" reference should have repair class diagnostics available" LV-CHECK-BOOL=
   root s" all_errors_stable" -1 s" reference should have stable all-errors diagnostics" LV-CHECK-BOOL=
   root s" trust_uses" 0 s" benchmark task used TRUST" LV-CHECK-INT=
   root s" signature_weakened" 0 s" reference weakened a signature" LV-CHECK-BOOL= ;

: LV-CAT-ACCUM {: root :} ( n -- )
   LV-TASK-K @ LV-TASK-CAT$ LV-CAT-ID LV-P !
   LV-P @ LV-CAT-ROWS++
   root LV-CERTIFIED? IF LV-P @ LV-CAT-CERT++ THEN
   root s" tests_passed" LV-BOOL-FIELD IF LV-P @ LV-CAT-TESTS++ THEN ;

: LV-ACC-BOOL {: root a:ptr u good:ptr bad:ptr :} ( n ptr u8 n ptr n ptr n -- )
   root a u LV-BOOL-FIELD IF good LV-CELL++ ELSE bad LV-CELL++ THEN ;

: LV-RC-OBJ {: item :} ( n -- )
   item JSON-KIND J-OBJ <> IF s" invalid repair_class_stats item" LV-FAIL-AT THEN ;

: LV-RC-REQS {: item :} ( n -- )
   item s" repair_class" LV-REQ
   item s" diagnostic_count" LV-REQ
   item s" repair_success" LV-REQ
   item s" repair_iterations" LV-REQ
   item s" token_delta" LV-REQ ;

: LV-ACCUM-RC-ITEM {: item :} ( n -- )
   item LV-RC-OBJ
   item LV-RC-REQS
   item s" repair_class" LV-STR-FIELD dup 0= IF 2drop s" empty repair_class" LV-FAIL-AT THEN
   LV-RC-ID LV-P !
   item s" diagnostic_count" LV-INT-FIELD
   dup 0 <= IF drop s" invalid repair class diagnostic_count" LV-FAIL-AT THEN
   dup LV-RC-DIAG ! LV-RC-SUM LV-CELL+!
   item s" repair_success" LV-BOOL-FIELD LV-RC-SUCC !
   LV-RC-SUCC @ LV-ROW-TESTS @ 0= and IF s" repair class success on failed row" LV-FAIL-AT THEN
   item s" repair_iterations" LV-INT-FIELD
   dup 0 < IF drop s" invalid repair class repair_iterations" LV-FAIL-AT THEN
   LV-RC-ROUND !
   item s" token_delta" LV-INT-FIELD
   dup 0 < IF drop s" invalid repair class token_delta" LV-FAIL-AT THEN
   LV-RC-TOK !
   LV-P @ LV-RC-ROWS++
   LV-RC-SUCC @ IF LV-P @ LV-RC-SUCCESS++ THEN
   LV-RC-ROUND @ LV-P @ LV-RC-REPAIRS+
   LV-RC-DIAG @ LV-P @ LV-RC-DIAGS+
   LV-RC-TOK @ LV-P @ LV-RC-TOKDELTA+ ;

: LV-ACCUM-RC-STATS {: root :} ( n -- )
   root s" repair_class_stats" LV-GET dup -1 = IF
      drop
      LV-ROW-DIAGS @ 0 > IF s" missing fields repair_class_stats" LV-FAIL-AT THEN
      exit
   THEN
   dup JSON-KIND J-ARR <> IF drop s" invalid repair_class_stats" LV-FAIL-AT THEN
   LV-NODE !
   0 LV-RC-SUM !
   0 begin dup LV-NODE @ JSON-COUNT < while
      LV-NODE @ over JSON-ARR@ LV-ACCUM-RC-ITEM
      1+
   repeat drop
   LV-RC-SUM @ LV-ROW-DIAGS @ <> IF s" repair class diagnostic_count mismatch" LV-FAIL-AT THEN ;

: LV-ACCUM-ROW {: root :} ( n -- )
   LV-ROWS LV-CELL++
   root LV-CERTIFIED? IF LV-CERT LV-CELL++ ELSE LV-BAD-CHECKER LV-CELL++ THEN
   root s" first_pass_tests" LV-BOOL-FIELD IF LV-FIRST-TESTS LV-CELL++ ELSE LV-BAD-FIRST LV-CELL++ THEN
   root s" tests_passed" LV-BOOL-FIELD dup LV-ROW-TESTS ! IF LV-TESTS LV-CELL++ ELSE LV-BAD-TESTS LV-CELL++ THEN
   root s" repair_iterations" LV-INT-FIELD LV-REPAIRS LV-CELL+!
   root s" checker_iterations" LV-INT-FIELD LV-CHECKERS LV-CELL+!
   root s" diagnostic_count" LV-INT-FIELD dup LV-ROW-DIAGS ! LV-DIAGS LV-CELL+!
   root s" diagnostic_token" LV-DTOK LV-BAD-DTOK LV-ACC-BOOL
   root s" diagnostic_span" LV-DSPAN LV-BAD-DSPAN LV-ACC-BOOL
   root s" diagnostic_expected" LV-DEXPECT LV-BAD-DEXP LV-ACC-BOOL
   root s" diagnostic_actual" LV-DACTUAL LV-BAD-DACT LV-ACC-BOOL
   root s" diagnostic_code" LV-DCODE LV-BAD-DCODE LV-ACC-BOOL
   root s" diagnostic_repair_class" LV-DCLASS LV-BAD-DCLASS LV-ACC-BOOL
   root s" all_errors_stable" LV-AESTABLE LV-BAD-AE LV-ACC-BOOL
   root s" tokens_used" LV-INT-FIELD LV-TOKENS LV-CELL+!
   root s" wall_ms" LV-INT-FIELD LV-WALL LV-CELL+!
   root s" final_chars" LV-INT-FIELD LV-CHARS LV-CELL+!
   root s" trust_uses" LV-INT-FIELD dup LV-TRUST LV-CELL+! 0 > IF LV-BAD-TRUST LV-CELL++ THEN
   root s" signature_weakened" LV-BOOL-FIELD dup IF LV-SIGWEAK LV-CELL++ LV-BAD-SIG LV-CELL++ ELSE drop THEN
   root LV-ACCUM-RC-STATS
   root LV-CAT-ACCUM ;

: LV-CHECK-ROW {: root :} ( n -- )
   root LV-CHECK-COMMON
   LV-MODE @ LV-MODE-REFERENCE = IF
      root LV-CHECK-REFERENCE
   ELSE
      root LV-ACCUM-ROW
   THEN ;

: LV-RESULT-LINE ( ptr u8 n -- )
   JSON-PARSE LV-ROOT !
   LV-ROOT @ LV-CHECK-ROW ;

: LV-FINISH-RESULT-LINE ( -- )
   LV-LINE @ 1+ LV-LINE !
   LV-RESULT-BUF LV-LINE-U @ LV-LINE-LEN LV-RESULT-LINE
   0 LV-LINE-U ! ;

: LV-RESULT-BYTE {: c :} ( n -- )
   c LV-LF = IF LV-FINISH-RESULT-LINE exit THEN
   LV-LINE-U @ LV-RESULT-CAP >= IF s" result line too long" LV-FAIL THEN
   c LV-RESULT-BUF LV-LINE-U @ + c!
   LV-LINE-U @ 1+ LV-LINE-U ! ;

: LV-SCAN-RESULT-BYTES {: n :} ( n -- )
   0 LV-BI !
   begin LV-BI @ n < while
      LV-READ-BUF LV-BI @ + c@ LV-RESULT-BYTE
      LV-BI @ 1+ LV-BI !
   repeat ;

: LV-OPEN-RESULT ( -- )
   LV-RESULT-PATH$ PATHZ
   PATHBUF 0 0 open LV-RFD !
   LV-RFD @ 0 < IF s" cannot open result file" LV-FAIL THEN ;

: LV-RESET-SUMMARY ( -- )
   0 LV-SEEN# !
   0 LV-CAT# !
   0 LV-RC# !
   0 LV-KEY# !
   0 LV-RC-STR-U !
   0 LV-KEY-STR-U !
   0 LV-SCHEMA !
   0 LV-SCHEMA-SET !
   0 LV-RUN-U !
   0 LV-RUN-SET !
   0 LV-MODEL-U !
   0 LV-MODEL-SET !
   0 LV-ROWS !
   0 LV-CERT !
   0 LV-FIRST-TESTS !
   0 LV-TESTS !
   0 LV-REPAIRS !
   0 LV-CHECKERS !
   0 LV-DIAGS !
   0 LV-DTOK !
   0 LV-DSPAN !
   0 LV-DEXPECT !
   0 LV-DACTUAL !
   0 LV-DCODE !
   0 LV-DCLASS !
   0 LV-AESTABLE !
   0 LV-TOKENS !
   0 LV-WALL !
   0 LV-CHARS !
   0 LV-TRUST !
   0 LV-SIGWEAK !
   0 LV-BAD-CHECKER !
   0 LV-BAD-FIRST !
   0 LV-BAD-TESTS !
   0 LV-BAD-TRUST !
   0 LV-BAD-SIG !
   0 LV-BAD-DTOK !
   0 LV-BAD-DSPAN !
   0 LV-BAD-DEXP !
   0 LV-BAD-DACT !
   0 LV-BAD-DCODE !
   0 LV-BAD-DCLASS !
   0 LV-BAD-AE !
   0 LV-ROW-DIAGS !
   0 LV-ROW-TESTS ! ;

: LV-SCAN-RESULTS ( -- )
   LV-RESET-SUMMARY
   0 LV-LINE !
   0 LV-LINE-U !
   LV-OPEN-RESULT
   begin
      LV-RFD @ LV-READ-BUF LV-READ-CAP read dup LV-RGOT ! 0 >
   while
      LV-RGOT @ LV-SCAN-RESULT-BYTES
   repeat
   LV-RFD @ close
   LV-RGOT @ 0 < IF s" result read failed" LV-FAIL THEN
   LV-LINE-U @ 0 > IF LV-FINISH-RESULT-LINE THEN ;

: LV-OUTPUT-REFERENCE ( -- )
   LV-SEEN# @ LV-REF-TASK# @ <> IF
      s" results/tasks mismatch: " LV-FAIL
   THEN
   s" llm-results: " LV-OUT
   LV-SEEN# @ LV-U.
   s"  reference metric row(s), 0 finding(s)" LV-OUT
   LV-NL ;

: LV-NAME=U ( ptr u8 n n -- )
   -rot LV-OUT LV-EQ LV-U. ;

: LV-TEXT-FIELD ( ptr u8 n n -- )
   LV-SPACE LV-NAME=U ;

: LV-OUTPUT-CATEGORIES ( -- )
   0 begin dup LV-CAT# @ < while
      dup LV-P !
      s" llm-results: category " LV-OUT
      LV-P @ cells LV-CAT-A + @
      LV-P @ cells LV-CAT-U + @
      LV-OUT
      s" rows" LV-P @ LV-CAT-ROWS@ LV-TEXT-FIELD
      s" certified" LV-P @ LV-CAT-CERT@ LV-TEXT-FIELD
      s" tests" LV-P @ LV-CAT-TESTS@ LV-TEXT-FIELD
      LV-NL
      1+
   repeat drop ;

: LV-OUTPUT-REPAIR-CLASSES ( -- )
   0 begin dup LV-RC# @ < while
      dup LV-P !
      s" llm-results: repair_class " LV-OUT
      LV-P @ LV-RC$ LV-OUT
      s" rows" LV-P @ LV-RC-ROWS@ LV-TEXT-FIELD
      s" repair_success" LV-P @ LV-RC-SUCCESS@ LV-TEXT-FIELD
      s" repair_iterations" LV-P @ LV-RC-REPAIRS@ LV-TEXT-FIELD
      s" diagnostics" LV-P @ LV-RC-DIAGS@ LV-TEXT-FIELD
      s" token_delta" LV-P @ LV-RC-TOKDELTA@ LV-TEXT-FIELD
      LV-NL
      1+
   repeat drop ;

: LV-OUTPUT-SUMMARY-TEXT ( -- )
   s" llm-results: run=" LV-OUT LV-RUN$ LV-OUT
   s"  model=" LV-OUT LV-MODEL$ LV-OUT
   s" rows" LV-ROWS @ LV-TEXT-FIELD
   s" certified" LV-CERT @ LV-TEXT-FIELD
   s" first_tests" LV-FIRST-TESTS @ LV-TEXT-FIELD
   s" tests" LV-TESTS @ LV-TEXT-FIELD
   s" repairs" LV-REPAIRS @ LV-TEXT-FIELD
   s" checker_iterations" LV-CHECKERS @ LV-TEXT-FIELD
   s" diagnostics" LV-DIAGS @ LV-TEXT-FIELD
   s" tokens" LV-TOKENS @ LV-TEXT-FIELD
   s" wall_ms" LV-WALL @ LV-TEXT-FIELD
   s" final_chars" LV-CHARS @ LV-TEXT-FIELD
   LV-NL
   s" llm-results: buckets" LV-OUT
   s" checker_rejected" LV-BAD-CHECKER @ LV-TEXT-FIELD
   s" first_tests_failed" LV-BAD-FIRST @ LV-TEXT-FIELD
   s" tests_failed" LV-BAD-TESTS @ LV-TEXT-FIELD
   s" trust_used" LV-BAD-TRUST @ LV-TEXT-FIELD
   s" signature_weakened" LV-BAD-SIG @ LV-TEXT-FIELD
   LV-NL
   s" llm-results: diagnostic_quality" LV-OUT
   s" token" LV-DTOK @ LV-TEXT-FIELD
   s" span" LV-DSPAN @ LV-TEXT-FIELD
   s" expected" LV-DEXPECT @ LV-TEXT-FIELD
   s" actual" LV-DACTUAL @ LV-TEXT-FIELD
   s" code" LV-DCODE @ LV-TEXT-FIELD
   s" repair_class" LV-DCLASS @ LV-TEXT-FIELD
   s" all_errors_stable" LV-AESTABLE @ LV-TEXT-FIELD
   LV-NL
   s" llm-results: diagnostic_gaps" LV-OUT
   s" token" LV-BAD-DTOK @ LV-TEXT-FIELD
   s" span" LV-BAD-DSPAN @ LV-TEXT-FIELD
   s" expected" LV-BAD-DEXP @ LV-TEXT-FIELD
   s" actual" LV-BAD-DACT @ LV-TEXT-FIELD
   s" code" LV-BAD-DCODE @ LV-TEXT-FIELD
   s" repair_class" LV-BAD-DCLASS @ LV-TEXT-FIELD
   s" all_errors_stable" LV-BAD-AE @ LV-TEXT-FIELD
   LV-NL
   LV-OUTPUT-REPAIR-CLASSES
   LV-OUTPUT-CATEGORIES ;

: LV-JSON-U ( n -- )
   LV-U$ JSONW-RAW ;

: LV-JSON-UF {: a:ptr u n :} ( ptr u8 n n -- )
   a u JSONW-KEY n LV-JSON-U ;

: LV-JSON-COMMA-UF ( ptr u8 n n -- )
   LV-JSON-UF JSONW-COMMA ;

: LV-OUTPUT-BUCKETS-JSON ( -- )
   JSONW-OBJECT-START
   s" checker_rejected" LV-BAD-CHECKER @ LV-JSON-COMMA-UF
   s" first_tests_failed" LV-BAD-FIRST @ LV-JSON-COMMA-UF
   s" tests_failed" LV-BAD-TESTS @ LV-JSON-COMMA-UF
   s" trust_used" LV-BAD-TRUST @ LV-JSON-COMMA-UF
   s" signature_weakened" LV-BAD-SIG @ LV-JSON-UF
   JSONW-OBJECT-END ;

: LV-DQ-JSON ( -- )
   JSONW-OBJECT-START
   s" token" LV-DTOK @ LV-JSON-COMMA-UF
   s" span" LV-DSPAN @ LV-JSON-COMMA-UF
   s" expected" LV-DEXPECT @ LV-JSON-COMMA-UF
   s" actual" LV-DACTUAL @ LV-JSON-COMMA-UF
   s" code" LV-DCODE @ LV-JSON-COMMA-UF
   s" repair_class" LV-DCLASS @ LV-JSON-COMMA-UF
   s" all_errors_stable" LV-AESTABLE @ LV-JSON-UF
   JSONW-OBJECT-END ;

: LV-DG-JSON ( -- )
   JSONW-OBJECT-START
   s" token" LV-BAD-DTOK @ LV-JSON-COMMA-UF
   s" span" LV-BAD-DSPAN @ LV-JSON-COMMA-UF
   s" expected" LV-BAD-DEXP @ LV-JSON-COMMA-UF
   s" actual" LV-BAD-DACT @ LV-JSON-COMMA-UF
   s" code" LV-BAD-DCODE @ LV-JSON-COMMA-UF
   s" repair_class" LV-BAD-DCLASS @ LV-JSON-COMMA-UF
   s" all_errors_stable" LV-BAD-AE @ LV-JSON-UF
   JSONW-OBJECT-END ;

: LV-OUTPUT-CATEGORY-JSON {: k :} ( n -- )
   JSONW-OBJECT-START
   s" category" JSONW-KEY
   LV-CAT-A k cells + @
   LV-CAT-U k cells + @
   JSONW-STRING JSONW-COMMA
   s" rows" k LV-CAT-ROWS@ LV-JSON-COMMA-UF
   s" certified" k LV-CAT-CERT@ LV-JSON-COMMA-UF
   s" tests_passed" k LV-CAT-TESTS@ LV-JSON-UF
   JSONW-OBJECT-END ;

: LV-OUTPUT-RC-JSON {: k :} ( n -- )
   JSONW-OBJECT-START
   s" repair_class" JSONW-KEY
   k LV-RC$ JSONW-STRING JSONW-COMMA
   s" rows" k LV-RC-ROWS@ LV-JSON-COMMA-UF
   s" repair_success" k LV-RC-SUCCESS@ LV-JSON-COMMA-UF
   s" repair_iterations" k LV-RC-REPAIRS@ LV-JSON-COMMA-UF
   s" diagnostic_count" k LV-RC-DIAGS@ LV-JSON-COMMA-UF
   s" token_delta" k LV-RC-TOKDELTA@ LV-JSON-UF
   JSONW-OBJECT-END ;

: LV-OUT-CATS-JSON ( -- )
   JSONW-ARRAY-START
   0 begin dup LV-CAT# @ < while
      dup 0 > IF JSONW-COMMA THEN
      dup LV-OUTPUT-CATEGORY-JSON
      1+
   repeat drop
   JSONW-ARRAY-END ;

: LV-OUT-RCS-JSON ( -- )
   JSONW-ARRAY-START
   0 begin dup LV-RC# @ < while
      dup 0 > IF JSONW-COMMA THEN
      dup LV-OUTPUT-RC-JSON
      1+
   repeat drop
   JSONW-ARRAY-END ;

: LV-OUTPUT-SUMMARY-JSON ( -- )
   JSONW-RESET
   JSONW-OBJECT-START
   s" schema_version" LV-SCHEMA @ LV-JSON-COMMA-UF
   s" run_id" JSONW-KEY LV-RUN$ JSONW-STRING JSONW-COMMA
   s" model" JSONW-KEY LV-MODEL$ JSONW-STRING JSONW-COMMA
   s" rows" LV-ROWS @ LV-JSON-COMMA-UF
   s" certified" LV-CERT @ LV-JSON-COMMA-UF
   s" first_tests_passed" LV-FIRST-TESTS @ LV-JSON-COMMA-UF
   s" tests_passed" LV-TESTS @ LV-JSON-COMMA-UF
   s" repair_iterations" LV-REPAIRS @ LV-JSON-COMMA-UF
   s" checker_iterations" LV-CHECKERS @ LV-JSON-COMMA-UF
   s" diagnostic_count" LV-DIAGS @ LV-JSON-COMMA-UF
   s" tokens_used" LV-TOKENS @ LV-JSON-COMMA-UF
   s" wall_ms" LV-WALL @ LV-JSON-COMMA-UF
   s" final_chars" LV-CHARS @ LV-JSON-COMMA-UF
   s" trust_uses" LV-TRUST @ LV-JSON-COMMA-UF
   s" signature_weakened" LV-SIGWEAK @ LV-JSON-COMMA-UF
   s" buckets" JSONW-KEY LV-OUTPUT-BUCKETS-JSON JSONW-COMMA
   s" diagnostic_quality" JSONW-KEY LV-DQ-JSON JSONW-COMMA
   s" diagnostic_gaps" JSONW-KEY LV-DG-JSON JSONW-COMMA
   s" repair_classes" JSONW-KEY LV-OUT-RCS-JSON JSONW-COMMA
   s" categories" JSONW-KEY LV-OUT-CATS-JSON
   JSONW-OBJECT-END
   JSON-OUT-BUF JSON-OUT-LEN @ LV-OUT LV-NL ;

: LV-OUTPUT-SUMMARY ( -- )
   LV-SEEN# @ 0= IF s" empty result file" LV-FAIL THEN
   LV-SCHEMA @ 1 = LV-SEEN# @ LV-REF-TASK# @ <> and IF
      s" results/tasks mismatch: " LV-FAIL
   THEN
   LV-JSON @ IF LV-OUTPUT-SUMMARY-JSON ELSE LV-OUTPUT-SUMMARY-TEXT THEN ;

: LV-CONFIG ( -- )
   s" bench/llm/validate-results.f [--json] [result.jsonl]" ARGV-USAGE!
   ARGV-PARSE
   ARGV-JSON? LV-JSON !
   ARGV-POS# 0= IF
      LV-MODE-REFERENCE LV-MODE !
      s" bench/llm/results/reference.jsonl" LV-RESULT-PATH!
      exit
   THEN
   ARGV-POS# 1 = IF
      LV-MODE-SUMMARY LV-MODE !
      0 ARGV-POS$ LV-RESULT-PATH!
      exit
   THEN
   s" wrong number of result files" ARGV-FAIL ;

: VALIDATE-RESULTS ( -- )
   LV-CONFIG
   LV-SCAN-TASKS
   LV-SCAN-RESULTS
   LV-MODE @ LV-MODE-REFERENCE = IF
      LV-OUTPUT-REFERENCE
   ELSE
      LV-OUTPUT-SUMMARY
   THEN ;

VALIDATE-RESULTS
