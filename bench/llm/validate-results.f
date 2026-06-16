\ validate-results.f - validate reference LLM benchmark metrics.
\ Load after tools/lint/lib.f and tools/json.f.

0 set-check

$1000 constant LV-TASK-CAP
$3000 constant LV-RESULT-CAP
64 constant LV-MAX
32 constant LV-NUM-CAP

9 constant LV-TAB
10 constant LV-LF
13 constant LV-CR
48 constant LV-ZERO
58 constant LV-COLON

create LV-TASK-BUF LV-TASK-CAP allot
create LV-RESULT-BUF LV-RESULT-CAP allot
create LV-NUM-BUF LV-NUM-CAP allot

create LV-TASK-ID LV-MAX cells allot
create LV-TASK-NAME-A LV-MAX cells allot
create LV-TASK-NAME-U LV-MAX cells allot
create LV-SEEN-ID LV-MAX cells allot

variable LV-TASK#
variable LV-SEEN#
variable LV-NUM-I
variable LV-I
variable LV-J
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
variable LV-P

: LV-OUT ( a u -- ) type ;
: LV-NL ( -- ) 10 emit ;

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

: LV-FAIL ( a u -- )
   s" llm-results: " LV-OUT
   LV-OUT
   LV-NL
   1 throw ;

: LV-FAIL-AT ( a u -- )
   s" llm-results: bench/llm/results/reference.jsonl:" LV-OUT
   LV-LINE @ LV-U.
   s" : " LV-OUT
   LV-OUT
   LV-NL
   1 throw ;

: LV-FAIL-AT-ID {: a u id :} ( a u id -- )
   s" llm-results: bench/llm/results/reference.jsonl:" LV-OUT
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

: LV-TASK-LINE {: a u :} ( a u -- )
   LV-LINE @ 1 = IF exit THEN
   LV-TASK# @ LV-MAX >= IF s" too many tasks" LV-FAIL THEN
   a u 0 LV-TAB-AT dup 0 < IF drop exit THEN
   LV-I !
   a u LV-I @ 1+ LV-TAB-AT dup 0 < IF drop exit THEN
   LV-J !
   a LV-I @ LV-U? 0= IF drop s" invalid task id" LV-FAIL THEN
   LV-TASK# @ LV-TASK-ID!
   a LV-I @ 1+ + LV-J @ LV-I @ 1+ - LV-TASK# @ LV-TASK-NAME!
   LV-TASK# @ 1+ LV-TASK# ! ;

: LV-SCAN-TASKS ( -- )
   0 LV-TASK# !
   s" bench/llm/tasks.tsv" LV-TASK-BUF LV-TASK-CAP READ-FILE ['] LV-TASK-LINE LV-FOR-LINES
   LV-TASK# @ 30 < IF
      s" need at least 30 tasks, found " LV-OUT
      LV-TASK# @ LV-U.
      LV-NL
      1 throw
   THEN ;

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

: LV-CHECK-ROW {: root :} ( root -- )
   root LV-REQS
   root s" task_id" LV-INT-FIELD LV-ID !
   LV-ID @ LV-FIND-SEEN 0 >= IF s" duplicate task_id " LV-ID @ LV-FAIL-AT-ID THEN
   LV-ID @ LV-SEEN+
   LV-ID @ LV-FIND-TASK dup 0 < IF drop s" task/name drift for id " LV-ID @ LV-FAIL-AT-ID THEN
   root s" name" LV-STR-FIELD rot LV-TASK-NAME$ STR= 0= IF s" task/name drift for id " LV-ID @ LV-FAIL-AT-ID THEN
   root s" model" s" reference" s" reference file contains non-reference model" LV-CHECK-STR=
   root s" schema_version" 1 s" unsupported schema_version" LV-CHECK-INT=
   root s" run_id" LV-STR-FIELD nip 0= IF s" empty run_id" LV-FAIL-AT THEN
   root s" attempt" 1 s" reference should be attempt 1" LV-CHECK-INT=
   root s" first_pass_checker" s" certified" s" reference solution not certified" LV-CHECK-STR=
   root s" first_pass_tests" -1 s" reference tests not passing" LV-CHECK-BOOL=
   root s" tests_passed" -1 s" final tests not passing" LV-CHECK-BOOL=
   root s" repair_iterations" 0 s" reference should need zero repairs" LV-CHECK-INT=
   root s" checker_iterations" 1 s" reference should need one checker iteration" LV-CHECK-INT=
   root s" diagnostic_count" 0 s" reference should have zero diagnostics" LV-CHECK-INT=
   root s" trust_uses" 0 s" benchmark task used TRUST" LV-CHECK-INT=
   root s" signature_weakened" 0 s" reference weakened a signature" LV-CHECK-BOOL=
   root s" wall_ms" LV-INT-FIELD drop
   root s" final_chars" LV-INT-FIELD 0 <= IF s" invalid final_chars" LV-FAIL-AT THEN ;

: LV-RESULT-LINE ( a u -- )
   JSON-PARSE LV-ROOT !
   LV-ROOT @ LV-CHECK-ROW ;

: LV-SCAN-RESULTS ( -- )
   0 LV-SEEN# !
   s" bench/llm/results/reference.jsonl" LV-RESULT-BUF LV-RESULT-CAP READ-FILE
   ['] LV-RESULT-LINE LV-FOR-LINES ;

: VALIDATE-RESULTS ( -- )
   LV-SCAN-TASKS
   LV-SCAN-RESULTS
   LV-SEEN# @ LV-TASK# @ <> IF
      s" results/tasks mismatch: " LV-FAIL
   THEN
   s" llm-results: " LV-OUT
   LV-SEEN# @ LV-U.
   s"  reference metric row(s), 0 finding(s)" LV-OUT
   LV-NL ;

VALIDATE-RESULTS
