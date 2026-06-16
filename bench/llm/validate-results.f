\ validate-results.f - validate and summarize LLM benchmark metrics.
\ Load after tools/lint/lib.f, tools/json.f, and tools/argv.f.

0 set-check

$1000 constant LV-TASK-CAP
$20000 constant LV-RESULT-CAP
64 constant LV-MAX
32 constant LV-NUM-CAP
256 constant LV-PATH-CAP
256 constant LV-RUN-CAP
128 constant LV-MODEL-CAP

0 constant LV-MODE-REFERENCE
1 constant LV-MODE-SUMMARY
45 constant LV-MIN-TASKS

9 constant LV-TAB
10 constant LV-LF
13 constant LV-CR
48 constant LV-ZERO
58 constant LV-COLON

create LV-TASK-BUF LV-TASK-CAP allot
create LV-RESULT-BUF LV-RESULT-CAP allot
create LV-NUM-BUF LV-NUM-CAP allot
create LV-RUN-BUF LV-RUN-CAP allot
create LV-MODEL-BUF LV-MODEL-CAP allot

create LV-TASK-ID LV-MAX cells allot
create LV-TASK-NAME-A LV-MAX cells allot
create LV-TASK-NAME-U LV-MAX cells allot
create LV-TASK-CAT-A LV-MAX cells allot
create LV-TASK-CAT-U LV-MAX cells allot
create LV-SEEN-ID LV-MAX cells allot

create LV-CAT-A LV-MAX cells allot
create LV-CAT-U LV-MAX cells allot
create LV-CAT-ROWS LV-MAX cells allot
create LV-CAT-CERT LV-MAX cells allot
create LV-CAT-TESTS LV-MAX cells allot

variable LV-TASK#
variable LV-SEEN#
variable LV-CAT#
variable LV-NUM-I
variable LV-I
variable LV-J
variable LV-H
variable LV-C
variable LV-ID
variable LV-ROOT
variable LV-NODE
variable LV-LINE
variable LV-LA
variable LV-LU
variable LV-LX
variable LV-LS
variable LV-LXT
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

: LV-OUT ( a u -- ) type ;
: LV-NL ( -- ) 10 emit ;
: LV-SPACE ( -- ) 32 emit ;
: LV-EQ ( -- ) 61 emit ;

: LV-U$ {: u :} ( u -- a u )
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

: LV-U. ( u -- )
   LV-U$ LV-OUT ;

: LV-CELL++ ( a -- )
   dup @ 1+ swap ! ;

: LV-CELL+! {: n a :} ( n a -- )
   a @ n + a ! ;

: LV-RESULT-PATH$ ( -- a u )
   LV-RESULT-PATH-A @ LV-RESULT-PATH-U @ ;

: LV-RESULT-PATH! {: a u :} ( a u -- )
   a LV-RESULT-PATH-A !
   u LV-RESULT-PATH-U ! ;

: LV-FAIL ( a u -- )
   s" llm-results: " LV-OUT
   LV-OUT
   LV-NL
   1 throw ;

: LV-FAIL-AT ( a u -- )
   s" llm-results: " LV-OUT
   LV-RESULT-PATH$ LV-OUT
   s" :" LV-OUT
   LV-LINE @ LV-U.
   s" : " LV-OUT
   LV-OUT
   LV-NL
   1 throw ;

: LV-FAIL-AT-ID {: a u id :} ( a u id -- )
   s" llm-results: " LV-OUT
   LV-RESULT-PATH$ LV-OUT
   s" :" LV-OUT
   LV-LINE @ LV-U.
   s" : " LV-OUT
   a u LV-OUT
   id LV-U.
   LV-NL
   1 throw ;

: LV-LINE-LEN ( a u -- a u' )
   dup 0 > IF
      2dup + 1- c@ LV-CR = IF 1- THEN
   THEN ;

: LV-DO-LINE ( end -- )
   LV-LE !
   LV-LINE @ 1+ LV-LINE !
   LV-LA @ LV-LS @ + LV-LE @ LV-LS @ - LV-LINE-LEN
   LV-LXT @ execute
   LV-LE @ 1+ LV-LS ! ;

: LV-FOR-LINES ( a u xt -- )
   LV-LXT ! LV-LU ! LV-LA !
   0 LV-LINE ! 0 LV-LX ! 0 LV-LS !
   begin LV-LX @ LV-LU @ < while
      LV-LA @ LV-LX @ + c@ LV-LF = IF LV-LX @ LV-DO-LINE THEN
      LV-LX @ 1+ LV-LX !
   repeat
   LV-LS @ LV-LU @ < IF LV-LU @ LV-DO-LINE THEN ;

: LV-TAB-AT ( a u start -- idx|-1 )
   LV-P ! LV-U ! LV-A !
   LV-P @ begin dup LV-U @ < while
      LV-A @ over + c@ LV-TAB = IF exit THEN
      1+
   repeat drop -1 ;

: LV-U? ( a u -- n ok )
   LV-U ! LV-A !
   LV-U @ 0= IF 0 0 exit THEN
   0 LV-K !
   0
   begin LV-K @ LV-U @ < while
      LV-A @ LV-K @ + c@ dup LV-ZERO < over 57 > or IF drop drop 0 0 exit THEN
      LV-ZERO - swap 10 * +
      LV-K @ 1+ LV-K !
   repeat -1 ;

: LV-TASK-ID@ ( k -- n )
   cells LV-TASK-ID + @ ;

: LV-TASK-ID! ( n k -- )
   cells LV-TASK-ID + ! ;

: LV-TASK-NAME! ( a u k -- )
   LV-K ! LV-U ! LV-A !
   LV-A @ LV-TASK-NAME-A LV-K @ cells + !
   LV-U @ LV-TASK-NAME-U LV-K @ cells + ! ;

: LV-TASK-NAME$ {: k :} ( k -- a u )
   LV-TASK-NAME-A k cells + @
   LV-TASK-NAME-U k cells + @ ;

: LV-TASK-CAT! ( a u k -- )
   LV-K ! LV-U ! LV-A !
   LV-A @ LV-TASK-CAT-A LV-K @ cells + !
   LV-U @ LV-TASK-CAT-U LV-K @ cells + ! ;

: LV-TASK-CAT$ {: k :} ( k -- a u )
   LV-TASK-CAT-A k cells + @
   LV-TASK-CAT-U k cells + @ ;

: LV-FIND-TASK-CAT {: a u :} ( a u -- k|-1 )
   0 begin dup LV-TASK# @ < while
      dup LV-TASK-CAT$ a u STR= IF exit THEN
      1+
   repeat drop -1 ;

: LV-MISSING-CAT {: a u :} ( a u -- )
   s" llm-results: missing required benchmark category " LV-OUT
   a u LV-OUT
   LV-NL
   1 throw ;

: LV-REQ-TASK-CAT {: a u :} ( a u -- )
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

: LV-SEEN@ ( k -- n )
   cells LV-SEEN-ID + @ ;

: LV-SEEN! ( n k -- )
   cells LV-SEEN-ID + ! ;

: LV-FIND-TASK {: id :} ( id -- k|-1 )
   0 begin dup LV-TASK# @ < while
      dup LV-TASK-ID@ id = IF exit THEN
      1+
   repeat drop -1 ;

: LV-FIND-SEEN {: id :} ( id -- k|-1 )
   0 begin dup LV-SEEN# @ < while
      dup LV-SEEN@ id = IF exit THEN
      1+
   repeat drop -1 ;

: LV-SEEN+ {: id :} ( id -- )
   LV-SEEN# @ LV-MAX >= IF s" too many result rows" LV-FAIL THEN
   id LV-SEEN# @ LV-SEEN!
   LV-SEEN# @ 1+ LV-SEEN# ! ;

: LV-CAT-ROWS@ ( k -- n )
   cells LV-CAT-ROWS + @ ;

: LV-CAT-CERT@ ( k -- n )
   cells LV-CAT-CERT + @ ;

: LV-CAT-TESTS@ ( k -- n )
   cells LV-CAT-TESTS + @ ;

: LV-CAT-ROWS++ ( k -- )
   cells LV-CAT-ROWS + LV-CELL++ ;

: LV-CAT-CERT++ ( k -- )
   cells LV-CAT-CERT + LV-CELL++ ;

: LV-CAT-TESTS++ ( k -- )
   cells LV-CAT-TESTS + LV-CELL++ ;

: LV-FIND-CAT {: a u :} ( a u -- k|-1 )
   0 begin dup LV-CAT# @ < while
      dup cells LV-CAT-A + @
      over cells LV-CAT-U + @
      a u STR= IF exit THEN
      1+
   repeat drop -1 ;

: LV-CAT+ {: a u :} ( a u -- k )
   LV-CAT# @ LV-MAX >= IF s" too many categories" LV-FAIL THEN
   LV-CAT# @ LV-P !
   a LV-CAT-A LV-P @ cells + !
   u LV-CAT-U LV-P @ cells + !
   0 LV-CAT-ROWS LV-P @ cells + !
   0 LV-CAT-CERT LV-P @ cells + !
   0 LV-CAT-TESTS LV-P @ cells + !
   LV-CAT# @ 1+ LV-CAT# !
   LV-P @ ;

: LV-CAT-ID {: a u :} ( a u -- k )
   a u LV-FIND-CAT dup 0 >= IF exit THEN
   drop a u LV-CAT+ ;

: LV-TASK-LINE {: a u :} ( a u -- )
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
   a LV-I @ LV-U? 0= IF drop s" invalid task id" LV-FAIL THEN
   LV-TASK# @ LV-TASK-ID!
   a LV-I @ 1+ + LV-J @ LV-I @ 1+ - LV-TASK# @ LV-TASK-NAME!
   a LV-H @ 1+ + LV-C @ LV-H @ 1+ - LV-TASK# @ LV-TASK-CAT!
   LV-TASK# @ 1+ LV-TASK# ! ;

: LV-SCAN-TASKS ( -- )
   0 LV-TASK# !
   s" bench/llm/tasks.tsv" LV-TASK-BUF LV-TASK-CAP READ-FILE ['] LV-TASK-LINE LV-FOR-LINES
   LV-CHECK-TASK-COVERAGE ;

: LV-GET {: root a u :} ( root a u -- node )
   root a u JSON-GET ;

: LV-HAS? ( root a u -- f )
   LV-GET -1 <> ;

: LV-MISSING {: a u :} ( a u -- )
   s" missing fields " LV-FAIL-AT ;

: LV-REQ {: root a u :} ( root a u -- )
   root a u LV-HAS? 0= IF a u LV-MISSING THEN ;

: LV-REQS {: root :} ( root -- )
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

: LV-INT-FIELD {: root a u :} ( root a u -- n )
   root a u LV-GET dup JSON-KIND J-NUM <> IF drop s" invalid integer field" LV-FAIL-AT THEN
   JSON-NUMBER$ LV-U? 0= IF drop s" invalid integer field" LV-FAIL-AT THEN ;

: LV-STR-FIELD {: root a u :} ( root a u -- a u )
   root a u LV-GET dup JSON-KIND J-STR <> IF drop s" invalid string field" LV-FAIL-AT THEN
   JSON-STRING$ ;

: LV-BOOL-FIELD {: root a u :} ( root a u -- f )
   root a u LV-GET dup JSON-KIND J-BOOL <> IF drop s" invalid bool field" LV-FAIL-AT THEN
   JSON-BOOL@ ;

: LV-CHECK-INT= {: root a u want msg mu :} ( root a u want msg mu -- )
   root a u LV-INT-FIELD want <> IF msg mu LV-FAIL-AT THEN ;

: LV-CHECK-STR= {: root a u want wu msg mu :} ( root a u want wu msg mu -- )
   root a u LV-STR-FIELD want wu STR= 0= IF msg mu LV-FAIL-AT THEN ;

: LV-CHECK-BOOL= {: root a u want msg mu :} ( root a u want msg mu -- )
   root a u LV-BOOL-FIELD want <> IF msg mu LV-FAIL-AT THEN ;

: LV-CERTIFIED? {: root :} ( root -- f )
   root s" first_pass_checker" LV-STR-FIELD s" certified" STR= ;

: LV-RUN-CHECK {: a u :} ( a u -- )
   LV-RUN-SET @ 0= IF
      u LV-RUN-CAP > IF s" run_id too long" LV-FAIL-AT THEN
      a LV-RUN-BUF u BMOVE
      u LV-RUN-U !
      -1 LV-RUN-SET !
      exit
   THEN
   a u LV-RUN-BUF LV-RUN-U @ STR= 0= IF s" mixed run_id values" LV-FAIL-AT THEN ;

: LV-DATE-RUN? {: a u :} ( a u -- f )
   u DATE-LEN 1+ < IF 0 0= 0= exit THEN
   a u DATE-LEN - 1- + c@ DATE-DASH = ;

: LV-CHECK-RUN-DATE {: a u :} ( a u -- )
   a u LV-DATE-RUN? IF
      a u DATE-LEN - + DATE-LEN PARSE-YMD 0= IF
         drop s" invalid run_id date" LV-FAIL-AT
      THEN
      drop
   THEN ;

: LV-MODEL-CHECK {: a u :} ( a u -- )
   LV-MODEL-SET @ 0= IF
      u LV-MODEL-CAP > IF s" model too long" LV-FAIL-AT THEN
      a LV-MODEL-BUF u BMOVE
      u LV-MODEL-U !
      -1 LV-MODEL-SET !
      exit
   THEN
   a u LV-MODEL-BUF LV-MODEL-U @ STR= 0= IF s" mixed model values" LV-FAIL-AT THEN ;

: LV-RUN$ ( -- a u )
   LV-RUN-BUF LV-RUN-U @ ;

: LV-MODEL$ ( -- a u )
   LV-MODEL-BUF LV-MODEL-U @ ;

: LV-CHECK-STRING-META {: root :} ( root -- )
   root s" run_id" LV-STR-FIELD
   dup 0= IF 2drop s" empty run_id" LV-FAIL-AT THEN
   2dup LV-CHECK-RUN-DATE
   LV-MODE @ LV-MODE-SUMMARY = IF LV-RUN-CHECK ELSE 2drop THEN
   root s" model" LV-STR-FIELD
   dup 0= IF 2drop s" empty model" LV-FAIL-AT THEN
   LV-MODE @ LV-MODE-SUMMARY = IF LV-MODEL-CHECK ELSE 2drop THEN ;

: LV-CHECK-COMMON {: root :} ( root -- )
   root LV-REQS
   root s" task_id" LV-INT-FIELD LV-ID !
   LV-ID @ LV-FIND-SEEN 0 >= IF s" duplicate task_id " LV-ID @ LV-FAIL-AT-ID THEN
   LV-ID @ LV-SEEN+
   LV-ID @ LV-FIND-TASK dup 0 < IF drop s" task/name drift for id " LV-ID @ LV-FAIL-AT-ID THEN
   LV-TASK-K !
   root s" name" LV-STR-FIELD LV-TASK-K @ LV-TASK-NAME$ STR= 0= IF s" task/name drift for id " LV-ID @ LV-FAIL-AT-ID THEN
   root s" schema_version" 1 s" unsupported schema_version" LV-CHECK-INT=
   root LV-CHECK-STRING-META
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
   root s" signature_weakened" LV-BOOL-FIELD drop ;

: LV-CHECK-REFERENCE {: root :} ( root -- )
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

: LV-CAT-ACCUM {: root :} ( root -- )
   LV-TASK-K @ LV-TASK-CAT$ LV-CAT-ID LV-P !
   LV-P @ LV-CAT-ROWS++
   root LV-CERTIFIED? IF LV-P @ LV-CAT-CERT++ THEN
   root s" tests_passed" LV-BOOL-FIELD IF LV-P @ LV-CAT-TESTS++ THEN ;

: LV-ACC-BOOL {: root a u good bad :} ( root a u good bad -- )
   root a u LV-BOOL-FIELD IF good LV-CELL++ ELSE bad LV-CELL++ THEN ;

: LV-ACCUM-ROW {: root :} ( root -- )
   LV-ROWS LV-CELL++
   root LV-CERTIFIED? IF LV-CERT LV-CELL++ ELSE LV-BAD-CHECKER LV-CELL++ THEN
   root s" first_pass_tests" LV-BOOL-FIELD IF LV-FIRST-TESTS LV-CELL++ ELSE LV-BAD-FIRST LV-CELL++ THEN
   root s" tests_passed" LV-BOOL-FIELD IF LV-TESTS LV-CELL++ ELSE LV-BAD-TESTS LV-CELL++ THEN
   root s" repair_iterations" LV-INT-FIELD LV-REPAIRS LV-CELL+!
   root s" checker_iterations" LV-INT-FIELD LV-CHECKERS LV-CELL+!
   root s" diagnostic_count" LV-INT-FIELD LV-DIAGS LV-CELL+!
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
   root LV-CAT-ACCUM ;

: LV-CHECK-ROW {: root :} ( root -- )
   root LV-CHECK-COMMON
   LV-MODE @ LV-MODE-REFERENCE = IF
      root LV-CHECK-REFERENCE
   ELSE
      root LV-ACCUM-ROW
   THEN ;

: LV-RESULT-LINE ( a u -- )
   JSON-PARSE LV-ROOT !
   LV-ROOT @ LV-CHECK-ROW ;

: LV-RESET-SUMMARY ( -- )
   0 LV-SEEN# !
   0 LV-CAT# !
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
   0 LV-BAD-AE ! ;

: LV-SCAN-RESULTS ( -- )
   LV-RESET-SUMMARY
   LV-RESULT-PATH$ LV-RESULT-BUF LV-RESULT-CAP READ-FILE
   ['] LV-RESULT-LINE LV-FOR-LINES ;

: LV-OUTPUT-REFERENCE ( -- )
   LV-SEEN# @ LV-TASK# @ <> IF
      s" results/tasks mismatch: " LV-FAIL
   THEN
   s" llm-results: " LV-OUT
   LV-SEEN# @ LV-U.
   s"  reference metric row(s), 0 finding(s)" LV-OUT
   LV-NL ;

: LV-NAME=U ( a u n -- )
   -rot LV-OUT LV-EQ LV-U. ;

: LV-TEXT-FIELD ( a u n -- )
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
   LV-OUTPUT-CATEGORIES ;

: LV-JSON-U ( u -- )
   LV-U$ JSONW-RAW ;

: LV-JSON-UF {: a u n :} ( a u n -- )
   a u JSONW-KEY n LV-JSON-U ;

: LV-JSON-COMMA-UF ( a u n -- )
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

: LV-OUTPUT-CATEGORY-JSON {: k :} ( k -- )
   JSONW-OBJECT-START
   s" category" JSONW-KEY
   LV-CAT-A k cells + @
   LV-CAT-U k cells + @
   JSONW-STRING JSONW-COMMA
   s" rows" k LV-CAT-ROWS@ LV-JSON-COMMA-UF
   s" certified" k LV-CAT-CERT@ LV-JSON-COMMA-UF
   s" tests_passed" k LV-CAT-TESTS@ LV-JSON-UF
   JSONW-OBJECT-END ;

: LV-OUT-CATS-JSON ( -- )
   JSONW-ARRAY-START
   0 begin dup LV-CAT# @ < while
      dup 0 > IF JSONW-COMMA THEN
      dup LV-OUTPUT-CATEGORY-JSON
      1+
   repeat drop
   JSONW-ARRAY-END ;

: LV-OUTPUT-SUMMARY-JSON ( -- )
   JSONW-RESET
   JSONW-OBJECT-START
   s" schema_version" 1 LV-JSON-COMMA-UF
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
   s" categories" JSONW-KEY LV-OUT-CATS-JSON
   JSONW-OBJECT-END
   JSON-OUT-BUF JSON-OUT-LEN @ LV-OUT LV-NL ;

: LV-OUTPUT-SUMMARY ( -- )
   LV-SEEN# @ 0= IF s" empty result file" LV-FAIL THEN
   LV-SEEN# @ LV-TASK# @ <> IF
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
