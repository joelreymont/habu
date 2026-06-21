\ validate-results-test.f - checked fixtures for validate-results.f.
\
\ Load after lib/errors.f, lib/string.f, lib/test.f, lib/fs.f,
\ lib/fs-mutate.f, lib/process.f, lib/process-argv.f, lib/process-env.f,
\ lib/json-write.f, bench/llm/fixture-text.f, and bench/llm/manifest.f.

120000 constant VRT-TIMEOUT-MS
$20000 constant VRT-CAP
$10000 constant VRT-TASK-CAP
10 constant VRT-LF
58 constant VRT-COLON
48 constant VRT-ZERO

0 constant VRT-MODE-COUNT
1 constant VRT-MODE-REFERENCE
2 constant VRT-MODE-LIVE

0 constant VRT-ROW-NORMAL
1 constant VRT-ROW-MISSING-META
2 constant VRT-ROW-MISSING-RAW-HASH
3 constant VRT-ROW-BAD-FINAL-HASH

create VRT-ROOT FS-PATH-CAP allot
create VRT-TASKS FS-PATH-CAP allot
create VRT-RESULT-DIR FS-PATH-CAP allot
create VRT-REF FS-PATH-CAP allot
create VRT-ATTEMPT FS-PATH-CAP allot
create VRT-LIVE FS-PATH-CAP allot
create VRT-OUT VRT-CAP allot
create VRT-ERR VRT-CAP allot
create VRT-TASK-BUF VRT-TASK-CAP allot
create VRT-TAB-BUF 1 allot
create VRT-LF-BUF 1 allot
create VRT-EMPTY 1 allot

variable VRT-ROOT-U
variable VRT-TASKS-U
variable VRT-RESULT-DIR-U
variable VRT-REF-U
variable VRT-ATTEMPT-U
variable VRT-LIVE-U
variable VRT-OUT-U
variable VRT-TASK-U
variable VRT-DEST-A
variable VRT-DEST-U
variable VRT-LINE-N
variable VRT-NEXT
variable VRT-ALL
variable VRT-FORTH
variable VRT-ARITH
variable VRT-ARRAYS
variable VRT-K
variable VRT-ROW-MODE

BM-TAB VRT-TAB-BUF c!
VRT-LF VRT-LF-BUF c!

: VRT-TRUE ( -- bool )
   STR-TRUE ;

: VRT-FALSE ( -- bool )
   STR-FALSE ;

: VRT-BOOL>U ( bool -- n )
   if 1 else 0 then ;

: VRT-CHECKER$ ( bool -- ptr u8 n )
   if s" certified" else s" rejected" then ;

: VRT-OUTCOME$ ( bool -- ptr u8 n )
   if s" pass" else s" fail" then ;

: VRT-ROW-NORMAL! ( -- )
   VRT-ROW-NORMAL VRT-ROW-MODE ! ;

: VRT-COPY! ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr up:ptr :}
   u FS-PATH-CAP > if E-FS-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: VRT-ROOT$ ( -- ptr u8 n )
   VRT-ROOT VRT-ROOT-U @ ;

: VRT-TASKS$ ( -- ptr u8 n )
   VRT-TASKS VRT-TASKS-U @ ;

: VRT-RESULT-DIR$ ( -- ptr u8 n )
   VRT-RESULT-DIR VRT-RESULT-DIR-U @ ;

: VRT-REF$ ( -- ptr u8 n )
   VRT-REF VRT-REF-U @ ;

: VRT-ATTEMPT$ ( -- ptr u8 n )
   VRT-ATTEMPT VRT-ATTEMPT-U @ ;

: VRT-LIVE$ ( -- ptr u8 n )
   VRT-LIVE VRT-LIVE-U @ ;

: VRT-EMPTY$ ( -- ptr u8 n )
   VRT-EMPTY 0 ;

: VRT-JOIN! ( ptr u8 n ptr u8 ptr n -- ) {: name:ptr nameu dst:ptr up:ptr :}
   VRT-ROOT$ name nameu dst JOIN-PATH up ! ;

: VRT-DEST! ( ptr u8 n -- ) {: a:ptr u :}
   a VRT-DEST-A !
   u VRT-DEST-U ! ;

: VRT-DEST$ ( -- ptr u8 n )
   VRT-DEST-A @ VRT-DEST-U @ ;

: VRT-CLEAR-DEST ( ptr u8 n -- )
   2dup VRT-DEST!
   s" " WRITE-ALL ;

: VRT-APPEND-ROW ( -- )
   VRT-DEST$ JW$ APPEND-FILE
   VRT-DEST$ VRT-LF-BUF 1 APPEND-FILE ;

: VRT-APPEND-RAW ( ptr u8 n -- )
   VRT-DEST$ 2swap APPEND-FILE ;

: VRT-APPEND-TAB ( -- )
   VRT-DEST$ VRT-TAB-BUF 1 APPEND-FILE ;

: VRT-APPEND-LF ( -- )
   VRT-DEST$ VRT-LF-BUF 1 APPEND-FILE ;

: VRT-FIELD-S ( ptr u8 n ptr u8 n -- )
   JW-COMMA
   JW-FIELD-S ;

: VRT-FIELD-U ( ptr u8 n n -- )
   JW-COMMA
   JW-FIELD-U ;

: VRT-FIELD-BOOL ( ptr u8 n bool -- )
   JW-COMMA
   JW-FIELD-BOOL ;

: VRT-FIELD-NULL ( ptr u8 n -- )
   JW-COMMA
   JW-FIELD-NULL ;

: VRT-TASK-ID ( ptr u8 n -- n )
   BM-T-ID BM-TASK-FIELD$ STR>NUMBER? 0= if E-BM-FIELD throw then ;

: VRT-TASK-NAME$ ( ptr u8 n -- ptr u8 n )
   BM-T-NAME BM-TASK-FIELD$ ;

: VRT-TASK-CAT$ ( ptr u8 n -- ptr u8 n )
   BM-T-CATEGORY BM-TASK-FIELD$ ;

: VRT-TASK-FORTH? ( ptr u8 n -- bool )
   BM-T-HARNESS BM-TASK-FIELD$ s" forth" STR= ;

: VRT-TASK-CAT= ( ptr u8 n ptr u8 n -- bool ) {: line:ptr lineu want:ptr wantu :}
   line lineu VRT-TASK-CAT$ want wantu STR= ;

: VRT-COUNT++ ( ptr a -- ) {: p:ptr :}
   p @ 1+ p ! ;

: VRT-COUNT-TASK ( ptr u8 n -- ) {: line:ptr lineu :}
   VRT-ALL VRT-COUNT++
   line lineu VRT-TASK-FORTH? if VRT-FORTH VRT-COUNT++ then
   line lineu s" arithmetic" VRT-TASK-CAT= if VRT-ARITH VRT-COUNT++ then
   line lineu s" arrays" VRT-TASK-CAT= if VRT-ARRAYS VRT-COUNT++ then ;

: VRT-U+ ( n -- ) {: n :}
   n 0 < if E-BM-FIELD throw then
   n 10 >= if n 10 / RECURSE then
   n 10 mod VRT-ZERO + SB-APPEND-C ;

: VRT-Q+ ( ptr u8 n -- )
   JW-DQ SB-APPEND-C
   SB-APPEND
   JW-DQ SB-APPEND-C ;

: VRT-QFIELD+ ( ptr u8 n -- )
   VRT-Q+
   VRT-COLON SB-APPEND-C ;

: VRT-HASH$ ( -- ptr u8 n )
   s" 0123456789abcdef0123456789abcdef0123456789abcdef0123456789abcdef" ;

: VRT-TRIAL-ID$ ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n n -- ptr u8 n )
   {: run:ptr runu model:ptr modelu arm:ptr armu line:ptr lineu trial :}
   SB-RESET
   run runu SB-APPEND
   VRT-COLON SB-APPEND-C
   model modelu SB-APPEND
   VRT-COLON SB-APPEND-C
   arm armu SB-APPEND
   VRT-COLON SB-APPEND-C
   line lineu VRT-TASK-ID VRT-U+
   VRT-COLON SB-APPEND-C
   trial VRT-U+
   SB$ ;

: VRT-DIAG-BOOLS ( bool -- ) {: ok :}
   s" diagnostic_token" ok VRT-FIELD-BOOL
   s" diagnostic_span" ok VRT-FIELD-BOOL
   s" diagnostic_expected" ok VRT-FIELD-BOOL
   s" diagnostic_actual" ok VRT-FIELD-BOOL
   s" diagnostic_code" ok VRT-FIELD-BOOL
   s" diagnostic_repair_class" ok VRT-FIELD-BOOL
   s" all_errors_stable" ok VRT-FIELD-BOOL ;

: VRT-V1-PASS-FIELDS ( -- )
   s" first_pass_checker" s" certified" VRT-FIELD-S
   s" first_pass_tests" VRT-TRUE VRT-FIELD-BOOL
   s" tests_passed" VRT-TRUE VRT-FIELD-BOOL
   s" repair_iterations" 0 VRT-FIELD-U
   s" checker_iterations" 1 VRT-FIELD-U
   s" diagnostic_count" 0 VRT-FIELD-U
   VRT-TRUE VRT-DIAG-BOOLS ;

: VRT-RC-STAT ( ptr u8 n n n n -- )
   {: class:ptr classu diags iters delta :}
   JW-OBJECT-START
   s" repair_class" class classu JW-FIELD-S
   s" diagnostic_count" diags VRT-FIELD-U
   s" repair_success" VRT-FALSE VRT-FIELD-BOOL
   s" repair_iterations" iters VRT-FIELD-U
   s" token_delta" delta VRT-FIELD-U
   JW-OBJECT-END ;

: VRT-RC-STATS ( -- )
   JW-ARRAY-START
   s" remove_producer" 2 1 30 VRT-RC-STAT
   JW-COMMA
   s" add_producer" 1 2 50 VRT-RC-STAT
   JW-COMMA
   s" fix_type" 1 1 20 VRT-RC-STAT
   JW-ARRAY-END ;

: VRT-V1-FAIL-FIELDS ( -- )
   s" first_pass_checker" s" rejected" VRT-FIELD-S
   s" first_pass_tests" VRT-FALSE VRT-FIELD-BOOL
   s" tests_passed" VRT-FALSE VRT-FIELD-BOOL
   s" repair_iterations" 2 VRT-FIELD-U
   s" checker_iterations" 3 VRT-FIELD-U
   s" diagnostic_count" 4 VRT-FIELD-U
   VRT-FALSE VRT-DIAG-BOOLS
   JW-COMMA s" repair_class_stats" JW-KEY VRT-RC-STATS ;

: VRT-V1-TRAILER ( n bool -- ) {: task tflag :}
   task 1 = if
      s" tokens_used" 100 VRT-FIELD-U
      s" wall_ms" 250 VRT-FIELD-U
   else
      s" tokens_used" 0 VRT-FIELD-U
      s" wall_ms" 0 VRT-FIELD-U
   then
   s" final_chars" 1 VRT-FIELD-U
   s" trust_uses" tflag VRT-BOOL>U VRT-FIELD-U
   s" signature_weakened" tflag VRT-FIELD-BOOL ;

: VRT-V1-HEAD ( ptr u8 n ptr u8 n ptr u8 n -- n )
   {: line:ptr lineu run:ptr runu model:ptr modelu :}
   JW-RESET
   JW-OBJECT-START
   s" schema_version" 1 JW-FIELD-U
   s" run_id" run runu VRT-FIELD-S
   line lineu VRT-TASK-ID {: task :}
   s" task_id" task VRT-FIELD-U
   s" name" line lineu VRT-TASK-NAME$ VRT-FIELD-S
   s" model" model modelu VRT-FIELD-S
   s" attempt" 1 VRT-FIELD-U
   task ;

: VRT-REFERENCE-RUN-ROW ( ptr u8 n ptr u8 n -- ) {: line:ptr lineu run:ptr runu :}
   line lineu run runu s" reference" VRT-V1-HEAD {: task :}
   VRT-V1-PASS-FIELDS
   task VRT-FALSE VRT-V1-TRAILER
   JW-OBJECT-END
   VRT-APPEND-ROW ;

: VRT-REFERENCE-ROW ( ptr u8 n -- ) {: line:ptr lineu :}
   line lineu s" reference-2026-06-18" VRT-REFERENCE-RUN-ROW ;

: VRT-REFERENCE-BAD-DATE-ROW ( ptr u8 n -- ) {: line:ptr lineu :}
   line lineu s" reference-2026-02-29" VRT-REFERENCE-RUN-ROW ;

: VRT-ATTEMPT-ROW ( ptr u8 n -- ) {: line:ptr lineu :}
   line lineu s" attempt-fixture" s" toy-model" VRT-V1-HEAD {: task :}
   task 1 = if VRT-V1-FAIL-FIELDS else VRT-V1-PASS-FIELDS then
   task task 2 = VRT-V1-TRAILER
   JW-OBJECT-END
   VRT-APPEND-ROW ;

: VRT-V2-ROW ( ptr u8 n n n ptr u8 n ptr u8 n ptr u8 n ptr u8 n ptr u8 n bool -- )
   {: line:ptr lineu trial k run:ptr runu modelid:ptr modelidu model:ptr modelu arm:ptr armu seed:ptr seedu pass :}
   line lineu VRT-TASK-ID {: task :}
   JW-RESET
   JW-OBJECT-START
   s" schema_version" 2 JW-FIELD-U
   s" run_id" run runu VRT-FIELD-S
   s" model_id" modelid modelidu VRT-FIELD-S
   s" arm" arm armu VRT-FIELD-S
   s" trial_id" run runu modelid modelidu arm armu line lineu trial VRT-TRIAL-ID$ VRT-FIELD-S
   VRT-ROW-MODE @ VRT-ROW-MISSING-META <> if
      s" task_family" line lineu VRT-TASK-CAT$ VRT-FIELD-S
      s" model_version" s" unknown" VRT-FIELD-S
      s" model_date" s" unknown" VRT-FIELD-S
   then
   s" trial" trial VRT-FIELD-U
   s" task_order" task VRT-FIELD-U
   s" k_trials" k VRT-FIELD-U
   s" order_seed" seed seedu VRT-FIELD-S
   s" task_id" task VRT-FIELD-U
   s" name" line lineu VRT-TASK-NAME$ VRT-FIELD-S
   s" model" model modelu VRT-FIELD-S
   s" attempt" trial VRT-FIELD-U
   s" first_pass_checker" pass VRT-CHECKER$ VRT-FIELD-S
   s" first_pass_tests" pass VRT-FIELD-BOOL
   s" tests_passed" pass VRT-FIELD-BOOL
   s" repair_iterations" 0 VRT-FIELD-U
   s" checker_iterations" 1 VRT-FIELD-U
   s" diagnostic_count" 0 VRT-FIELD-U
   VRT-TRUE VRT-DIAG-BOOLS
   s" tokens_used" 0 VRT-FIELD-U
   s" wall_ms" 0 VRT-FIELD-U
   s" final_chars" 1 VRT-FIELD-U
   s" trust_uses" 0 VRT-FIELD-U
   s" signature_weakened" VRT-FALSE VRT-FIELD-BOOL
   s" outcome" pass VRT-OUTCOME$ VRT-FIELD-S
   s" rounds" 1 VRT-FIELD-U
   s" first_pass" pass VRT-FIELD-BOOL
   s" tokens" 0 VRT-FIELD-U
   VRT-ROW-MODE @ VRT-ROW-MISSING-META <> if
      s" source_chars" 1 VRT-FIELD-U
   then
   s" runtime_ms" VRT-FIELD-NULL
   s" runtime_repetitions" 100 VRT-FIELD-U
   s" runtime_warmups" 10 VRT-FIELD-U
   s" runtime_status" s" not_run" VRT-FIELD-S
   s" prompt" s" prompt" VRT-FIELD-S
   s" prompt_sha256" VRT-HASH$ VRT-FIELD-S
   s" raw_response" s" raw" VRT-FIELD-S
   VRT-ROW-MODE @ VRT-ROW-MISSING-RAW-HASH <> if
      s" raw_response_sha256" VRT-HASH$ VRT-FIELD-S
   then
   s" extracted_candidate" s" candidate" VRT-FIELD-S
   s" extracted_candidate_sha256" VRT-HASH$ VRT-FIELD-S
   s" checker_diagnostics" s" " VRT-FIELD-S
   s" checker_diagnostics_sha256" VRT-HASH$ VRT-FIELD-S
   s" repair_packet" s" " VRT-FIELD-S
   s" repair_packet_sha256" VRT-HASH$ VRT-FIELD-S
   s" test_output" s" ok" VRT-FIELD-S
   s" test_output_sha256" VRT-HASH$ VRT-FIELD-S
   s" final_bundle" s" bundle" VRT-FIELD-S
   s" final_bundle_sha256"
   VRT-ROW-MODE @ VRT-ROW-BAD-FINAL-HASH = if s" not-a-sha" else VRT-HASH$ then
   VRT-FIELD-S
   JW-OBJECT-END
   VRT-APPEND-ROW ;

: VRT-HANDLE-LIVE-TASK ( ptr u8 n -- ) {: line:ptr lineu :}
   1 begin dup VRT-K @ <= while
      dup line lineu rot VRT-K @ s" live-fixture-2026-06-18" s" toy-model" s" toy-model" s" forth" s" live-fixture" VRT-TRUE VRT-V2-ROW
      1+
   repeat drop ;

: VRT-HANDLE-TASK ( ptr u8 n n -- ) {: line:ptr lineu mode :}
   VRT-LINE-N @ 0= if exit then
   line lineu BM-BLANK-OR-COMMENT? if exit then
   mode VRT-MODE-COUNT = if line lineu VRT-COUNT-TASK exit then
   mode VRT-MODE-REFERENCE = if
      line lineu VRT-TASK-FORTH? if line lineu VRT-REFERENCE-ROW then
      exit
   then
   mode VRT-MODE-LIVE = if line lineu VRT-HANDLE-LIVE-TASK exit then
   E-BM-FIELD throw ;

: VRT-EACH-TASK ( n -- ) {: mode :}
   VRT-TASKS$ VRT-TASK-BUF VRT-TASK-CAP READ-ALL VRT-TASK-U !
   0 VRT-NEXT !
   0 VRT-LINE-N !
   begin VRT-TASK-BUF VRT-TASK-U @ VRT-NEXT @ BM-LINE-NEXT while
      VRT-NEXT !
      mode VRT-HANDLE-TASK
      VRT-LINE-N @ 1+ VRT-LINE-N !
   repeat drop 2drop ;

: VRT-COUNT-TASKS ( -- )
   0 VRT-ALL !
   0 VRT-FORTH !
   0 VRT-ARITH !
   0 VRT-ARRAYS !
   VRT-MODE-COUNT VRT-EACH-TASK ;

: VRT-WRITE-REFERENCE ( -- )
   VRT-REF$ VRT-CLEAR-DEST
   VRT-MODE-REFERENCE VRT-EACH-TASK ;

: VRT-WRITE-REFERENCE-BAD-DATE ( -- )
   VRT-REF$ VRT-CLEAR-DEST
   VRT-TASKS$ VRT-TASK-BUF VRT-TASK-CAP READ-ALL VRT-TASK-U !
   0 VRT-NEXT !
   0 VRT-LINE-N !
   begin VRT-TASK-BUF VRT-TASK-U @ VRT-NEXT @ BM-LINE-NEXT while
      VRT-NEXT !
      VRT-LINE-N @ 0 > if
         2dup BM-BLANK-OR-COMMENT? 0= if
            2dup VRT-TASK-FORTH? if 2dup VRT-REFERENCE-BAD-DATE-ROW then
         then
      then
      2drop
      VRT-LINE-N @ 1+ VRT-LINE-N !
   repeat drop 2drop ;

: VRT-APPEND-FIRST-REFERENCE ( -- )
   VRT-TASKS$ VRT-TASK-BUF VRT-TASK-CAP READ-ALL VRT-TASK-U !
   0 VRT-NEXT !
   0 VRT-LINE-N !
   begin VRT-TASK-BUF VRT-TASK-U @ VRT-NEXT @ BM-LINE-NEXT while
      VRT-NEXT !
      VRT-LINE-N @ 0 > if
         2dup BM-BLANK-OR-COMMENT? 0= if
            2dup VRT-TASK-FORTH? if 2dup VRT-REFERENCE-ROW 2drop exit then
         then
      then
      2drop
      VRT-LINE-N @ 1+ VRT-LINE-N !
   repeat drop 2drop ;

: VRT-WRITE-ATTEMPT ( -- )
   VRT-ATTEMPT$ VRT-CLEAR-DEST
   VRT-TASKS$ VRT-TASK-BUF VRT-TASK-CAP READ-ALL VRT-TASK-U !
   0 VRT-NEXT !
   0 VRT-LINE-N !
   begin VRT-TASK-BUF VRT-TASK-U @ VRT-NEXT @ BM-LINE-NEXT while
      VRT-NEXT !
      VRT-LINE-N @ 0 > if
         2dup BM-BLANK-OR-COMMENT? 0= if
            2dup VRT-TASK-FORTH? if 2dup VRT-ATTEMPT-ROW then
         then
      then
      2drop
      VRT-LINE-N @ 1+ VRT-LINE-N !
   repeat drop 2drop ;

: VRT-WRITE-LIVE ( n -- ) {: k :}
   VRT-ROW-NORMAL!
   k VRT-K !
   VRT-LIVE$ VRT-CLEAR-DEST
   VRT-MODE-LIVE VRT-EACH-TASK ;

: VRT-WRITE-SINGLE-V2 ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n bool -- )
   {: run:ptr runu modelid:ptr modelidu model:ptr modelu arm:ptr armu pass :}
   VRT-ROW-NORMAL!
   VRT-TASKS$ VRT-TASK-BUF VRT-TASK-CAP READ-ALL VRT-TASK-U !
   0 VRT-NEXT !
   begin VRT-TASK-BUF VRT-TASK-U @ VRT-NEXT @ BM-LINE-NEXT while
      VRT-NEXT !
      2dup BM-TASK-HEADER? 0= if
         2dup BM-BLANK-OR-COMMENT? 0= if
            2dup VRT-TASK-ID 1 = if
               2dup 1 1 run runu modelid modelidu model modelu arm armu run runu pass VRT-V2-ROW
               2drop exit
            then
         then
      then
      2drop
   repeat drop 2drop ;

: VRT-APPEND-FIRST-V2 ( n n n -- ) {: trial k mode :}
   mode VRT-ROW-MODE !
   VRT-TASKS$ VRT-TASK-BUF VRT-TASK-CAP READ-ALL VRT-TASK-U !
   0 VRT-NEXT !
   begin VRT-TASK-BUF VRT-TASK-U @ VRT-NEXT @ BM-LINE-NEXT while
      VRT-NEXT !
      2dup BM-TASK-HEADER? 0= if
         2dup BM-BLANK-OR-COMMENT? 0= if
            2dup VRT-TASK-ID 1 = if
               2dup trial k s" live-fixture-2026-06-18" s" toy-model" s" toy-model" s" forth" s" live-fixture" VRT-TRUE VRT-V2-ROW
               VRT-ROW-NORMAL!
               2drop exit
            then
         then
      then
      2drop
   repeat drop 2drop
   VRT-ROW-NORMAL! ;

: VRT-CLEAR-LIVE ( -- )
   VRT-LIVE$ VRT-CLEAR-DEST ;

: VRT-WRITE-MISSING-K ( -- )
   VRT-CLEAR-LIVE
   2 2 VRT-ROW-NORMAL VRT-APPEND-FIRST-V2 ;

: VRT-WRITE-EXTRA-K ( -- )
   VRT-CLEAR-LIVE
   1 1 VRT-ROW-NORMAL VRT-APPEND-FIRST-V2
   2 1 VRT-ROW-NORMAL VRT-APPEND-FIRST-V2 ;

: VRT-WRITE-MISSING-META ( -- )
   VRT-CLEAR-LIVE
   1 1 VRT-ROW-MISSING-META VRT-APPEND-FIRST-V2 ;

: VRT-WRITE-DUP-V2 ( -- )
   VRT-CLEAR-LIVE
   1 1 VRT-ROW-NORMAL VRT-APPEND-FIRST-V2
   1 1 VRT-ROW-NORMAL VRT-APPEND-FIRST-V2 ;

: VRT-WRITE-MISSING-RAW-HASH ( -- )
   VRT-CLEAR-LIVE
   1 1 VRT-ROW-MISSING-RAW-HASH VRT-APPEND-FIRST-V2 ;

: VRT-WRITE-BAD-FINAL-HASH ( -- )
   VRT-CLEAR-LIVE
   1 1 VRT-ROW-BAD-FINAL-HASH VRT-APPEND-FIRST-V2 ;

: VRT-WRITE-ARMS ( -- )
   VRT-LIVE$ VRT-CLEAR-DEST
   s" arm-fixture-2026-06-18" s" toy-model" s" toy-model" s" habu-forth" VRT-TRUE VRT-WRITE-SINGLE-V2
   s" arm-fixture-2026-06-18" s" toy-model" s" toy-model" s" habu-forth-raw" VRT-TRUE VRT-WRITE-SINGLE-V2
   s" arm-fixture-2026-06-18" s" toy-model" s" toy-model" s" habu-forth-blind" VRT-TRUE VRT-WRITE-SINGLE-V2 ;

: VRT-WRITE-MODELS ( -- )
   VRT-LIVE$ VRT-CLEAR-DEST
   s" multi-model-fixture-2026-06-18" s" alpha" s" Alpha" s" forth" VRT-TRUE VRT-WRITE-SINGLE-V2
   s" multi-model-fixture-2026-06-18" s" beta" s" Beta" s" forth" VRT-TRUE VRT-WRITE-SINGLE-V2 ;

: VRT-WRITE-CONFIDENCE ( -- )
   VRT-ROW-NORMAL!
   VRT-LIVE$ VRT-CLEAR-DEST
   VRT-TASKS$ VRT-TASK-BUF VRT-TASK-CAP READ-ALL VRT-TASK-U !
   0 VRT-NEXT !
   begin VRT-TASK-BUF VRT-TASK-U @ VRT-NEXT @ BM-LINE-NEXT while
      VRT-NEXT !
      2dup BM-TASK-HEADER? 0= if
         2dup BM-BLANK-OR-COMMENT? 0= if
            2dup VRT-TASK-ID 1 = if
               2dup 1 2 s" confidence-fixture-2026-06-18" s" toy-model" s" toy-model" s" forth" s" confidence-fixture" VRT-TRUE VRT-V2-ROW
               2dup 2 2 s" confidence-fixture-2026-06-18" s" toy-model" s" toy-model" s" forth" s" confidence-fixture" VRT-FALSE VRT-V2-ROW
               2drop exit
            then
         then
      then
      2drop
   repeat drop 2drop ;

: VRT-TASK-FIELD+ ( ptr u8 n n -- ) {: line:ptr lineu idx :}
   line lineu idx BM-TASK-FIELD$ VRT-APPEND-RAW ;

: VRT-TASK-FIELD-TAB+ ( ptr u8 n n -- ) {: line:ptr lineu idx :}
   line lineu idx VRT-TASK-FIELD+
   VRT-APPEND-TAB ;

: VRT-TASK-BAD-CAT+ ( ptr u8 n -- ) {: line:ptr lineu :}
   line lineu VRT-TASK-CAT$ s" aot-safe" STR= if
      s" parsing" VRT-APPEND-RAW
   else
      line lineu VRT-TASK-CAT$ VRT-APPEND-RAW
   then ;

: VRT-BAD-CAT-TASK-ROW ( ptr u8 n -- ) {: line:ptr lineu :}
   line lineu BM-T-ID VRT-TASK-FIELD-TAB+
   line lineu BM-T-NAME VRT-TASK-FIELD-TAB+
   line lineu BM-T-SIGNATURE VRT-TASK-FIELD-TAB+
   line lineu VRT-TASK-BAD-CAT+ VRT-APPEND-TAB
   line lineu BM-T-TESTS VRT-TASK-FIELD-TAB+
   line lineu BM-T-HARNESS VRT-TASK-FIELD-TAB+
   line lineu BM-T-CONV VRT-TASK-FIELD-TAB+
   line lineu BM-T-SPEC VRT-TASK-FIELD-TAB+
   line lineu BM-T-VECTORS VRT-TASK-FIELD-TAB+
   line lineu BM-T-TAGS VRT-TASK-FIELD-TAB+
   line lineu BM-T-JS-SIGNATURE VRT-TASK-FIELD-TAB+
   line lineu BM-T-RUST-SIGNATURE VRT-TASK-FIELD+
   VRT-APPEND-LF ;

: VRT-WRITE-TASKS-MISSING-CAT ( -- )
   VRT-TASKS$ VRT-CLEAR-DEST
   BM-TASK-HEADER$ VRT-APPEND-RAW
   VRT-APPEND-LF
   s" bench/llm/tasks.tsv" VRT-TASK-BUF VRT-TASK-CAP READ-ALL VRT-TASK-U !
   0 VRT-NEXT !
   0 VRT-LINE-N !
   begin VRT-TASK-BUF VRT-TASK-U @ VRT-NEXT @ BM-LINE-NEXT while
      VRT-NEXT !
      VRT-LINE-N @ 0 > if
         2dup BM-BLANK-OR-COMMENT? 0= if 2dup VRT-BAD-CAT-TASK-ROW then
      then
      2drop
      VRT-LINE-N @ 1+ VRT-LINE-N !
   repeat drop 2drop ;

: VRT-COPY-TASKS ( -- )
   s" bench/llm/tasks.tsv" VRT-TASKS$ COPY-FILE-STREAM ;

: VRT-VALIDATOR-LOADS ( -- )
   s" --load" PROC-ARGV+
   s" tools/date.f" PROC-ARGV+
   s" tools/lint/lib.f" PROC-ARGV+
   s" tools/json.f" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" bench/llm/validate-results.f" PROC-ARGV+ ;

: VRT-CAPTURE-VALIDATE ( ptr u8 n bool -- n n n ) {: path:ptr pathu json :}
   PROC-ARGV-ENV-RESET
   s" BENCH_TASKS" VRT-TASKS$ PROC-ENV+
   s" BENCH_REFERENCE_RESULTS" VRT-REF$ PROC-ENV+
   PROC-ENV-INHERIT-MISSING
   VRT-VALIDATOR-LOADS
   s" --" PROC-ARGV+
   json if s" --json" PROC-ARGV+ then
   pathu 0 > if path pathu PROC-ARGV+ then
   s" bin/hb" VRT-OUT VRT-CAP VRT-ERR VRT-CAP VRT-TIMEOUT-MS RUN-ARGV-ENV-CAPTURE ;

: VRT-RUN-VALIDATE ( ptr u8 n bool -- ) {: path:ptr pathu json :}
   path pathu json VRT-CAPTURE-VALIDATE {: outu erru rc :}
   outu VRT-OUT-U !
   rc 0 T=
   erru 0 T= ;

: VRT-RUN-VALIDATE-FAIL ( ptr u8 n bool -- ) {: path:ptr pathu json :}
   path pathu json VRT-CAPTURE-VALIDATE {: outu erru rc :}
   outu VRT-OUT-U !
   rc 0 T<>
   erru 0 T= ;

: VRT-OUT-CONTAINS ( ptr u8 n -- )
   VRT-OUT VRT-OUT-U @ 2swap CONTAINS? TTRUE ;

: VRT-EXPECT-FAIL ( ptr u8 n ptr u8 n -- ) {: path:ptr pathu needle:ptr needleu :}
   path pathu VRT-FALSE VRT-RUN-VALIDATE-FAIL
   needle needleu VRT-OUT-CONTAINS ;

: VRT-REF-SUMMARY$ ( -- ptr u8 n )
   SB-RESET
   s" llm-results: " SB-APPEND
   VRT-FORTH @ VRT-U+
   s"  reference metric row(s), 0 finding(s)" SB-APPEND
   SB$ ;

: VRT-ATTEMPT-SUMMARY$ ( -- ptr u8 n )
   SB-RESET
   s" run=attempt-fixture model=toy-model rows=" SB-APPEND
   VRT-FORTH @ VRT-U+
   s"  certified=" SB-APPEND
   VRT-FORTH @ 1 - VRT-U+
   s"  first_tests=" SB-APPEND
   VRT-FORTH @ 1 - VRT-U+
   s"  tests=" SB-APPEND
   VRT-FORTH @ 1 - VRT-U+
   s"  repairs=2 checker_iterations=" SB-APPEND
   VRT-FORTH @ 2 + VRT-U+
   s"  diagnostics=4 tokens=100 wall_ms=250" SB-APPEND
   SB$ ;

: VRT-ATTEMPT-CATEGORY$ ( -- ptr u8 n )
   SB-RESET
   s" category arithmetic rows=" SB-APPEND
   VRT-ARITH @ VRT-U+
   s"  certified=" SB-APPEND
   VRT-ARITH @ 1 - VRT-U+
   s"  tests=" SB-APPEND
   VRT-ARITH @ 1 - VRT-U+
   SB$ ;

: VRT-JSON-ROWS$ ( n -- ptr u8 n ) {: rows :}
   SB-RESET
   s" rows" VRT-QFIELD+
   rows VRT-U+
   SB$ ;

: VRT-JSON-DIAG-QUALITY$ ( -- ptr u8 n )
   SB-RESET
   s" diagnostic_quality" VRT-QFIELD+
   JW-LBRACE SB-APPEND-C
   s" token" VRT-QFIELD+
   VRT-FORTH @ 1 - VRT-U+
   JW-COMMA-C SB-APPEND-C s" span" VRT-QFIELD+
   VRT-FORTH @ 1 - VRT-U+
   JW-COMMA-C SB-APPEND-C s" expected" VRT-QFIELD+
   VRT-FORTH @ 1 - VRT-U+
   JW-COMMA-C SB-APPEND-C s" actual" VRT-QFIELD+
   VRT-FORTH @ 1 - VRT-U+
   JW-COMMA-C SB-APPEND-C s" code" VRT-QFIELD+
   VRT-FORTH @ 1 - VRT-U+
   JW-COMMA-C SB-APPEND-C s" repair_class" VRT-QFIELD+
   VRT-FORTH @ 1 - VRT-U+
   JW-COMMA-C SB-APPEND-C s" all_errors_stable" VRT-QFIELD+
   VRT-FORTH @ 1 - VRT-U+
   JW-RBRACE SB-APPEND-C
   SB$ ;

: VRT-LIVE-SUMMARY$ ( -- ptr u8 n )
   SB-RESET
   s" run=live-fixture-2026-06-18 model=toy-model rows=" SB-APPEND
   VRT-ALL @ 2 * VRT-U+
   s"  certified=" SB-APPEND
   VRT-ALL @ 2 * VRT-U+
   s"  first_tests=" SB-APPEND
   VRT-ALL @ 2 * VRT-U+
   s"  tests=" SB-APPEND
   VRT-ALL @ 2 * VRT-U+
   s"  repairs=0 checker_iterations=" SB-APPEND
   VRT-ALL @ 2 * VRT-U+
   s"  diagnostics=0 tokens=0 wall_ms=0" SB-APPEND
   SB$ ;

: VRT-LIVE-CATEGORY$ ( ptr u8 n n -- ptr u8 n ) {: name:ptr nameu rows :}
   SB-RESET
   s" category " SB-APPEND
   name nameu SB-APPEND
   s"  rows=" SB-APPEND
   rows VRT-U+
   s"  certified=" SB-APPEND
   rows VRT-U+
   s"  tests=" SB-APPEND
   rows VRT-U+
   SB$ ;

: VRT-TEST-REFERENCE ( -- )
   VRT-WRITE-REFERENCE
   VRT-EMPTY$ VRT-FALSE VRT-RUN-VALIDATE
   VRT-REF-SUMMARY$ VRT-OUT-CONTAINS ;

: VRT-TEST-ATTEMPT-TEXT ( -- )
   VRT-WRITE-ATTEMPT
   VRT-ATTEMPT$ VRT-FALSE VRT-RUN-VALIDATE
   VRT-ATTEMPT-SUMMARY$ VRT-OUT-CONTAINS
   s" buckets checker_rejected=1 first_tests_failed=1 tests_failed=1 trust_used=1 signature_weakened=1" VRT-OUT-CONTAINS
   s" diagnostic_gaps token=1 span=1 expected=1 actual=1 code=1 repair_class=1 all_errors_stable=1" VRT-OUT-CONTAINS
   VRT-ATTEMPT-CATEGORY$ VRT-OUT-CONTAINS
   s" repair_class remove_producer rows=1 repair_success=0 repair_iterations=1 diagnostics=2 token_delta=30" VRT-OUT-CONTAINS
   s" repair_class add_producer rows=1 repair_success=0 repair_iterations=2 diagnostics=1 token_delta=50" VRT-OUT-CONTAINS
   s" repair_class fix_type rows=1 repair_success=0 repair_iterations=1 diagnostics=1 token_delta=20" VRT-OUT-CONTAINS ;

: VRT-TEST-ATTEMPT-JSON ( -- )
   VRT-ATTEMPT$ VRT-TRUE VRT-RUN-VALIDATE
   VRT-FORTH @ VRT-JSON-ROWS$ VRT-OUT-CONTAINS
   s" checker_rejected" VRT-OUT-CONTAINS
   VRT-JSON-DIAG-QUALITY$ VRT-OUT-CONTAINS
   s" diagnostic_gaps" VRT-OUT-CONTAINS
   s" repair_classes" VRT-OUT-CONTAINS
   s" remove_producer" VRT-OUT-CONTAINS ;

: VRT-TEST-LIVE ( -- )
   2 VRT-WRITE-LIVE
   VRT-LIVE$ VRT-FALSE VRT-RUN-VALIDATE
   VRT-LIVE-SUMMARY$ VRT-OUT-CONTAINS
   s" arithmetic" VRT-ARITH @ 2 * VRT-LIVE-CATEGORY$ VRT-OUT-CONTAINS
   s" arrays" VRT-ARRAYS @ 2 * VRT-LIVE-CATEGORY$ VRT-OUT-CONTAINS
   VRT-LIVE$ VRT-TRUE VRT-RUN-VALIDATE
   s" schema_version" VRT-OUT-CONTAINS
   VRT-ALL @ 2 * VRT-JSON-ROWS$ VRT-OUT-CONTAINS ;

: VRT-TEST-ARMS ( -- )
   VRT-WRITE-ARMS
   VRT-LIVE$ VRT-FALSE VRT-RUN-VALIDATE
   s" arm habu-forth rows=1 certified=1 first_tests=1 tests=1 repairs=0 checker_iterations=1 diagnostics=0 tokens=0 wall_ms=0 final_chars=1" VRT-OUT-CONTAINS
   s" arm habu-forth-raw rows=1 certified=1 first_tests=1 tests=1 repairs=0 checker_iterations=1 diagnostics=0 tokens=0 wall_ms=0 final_chars=1" VRT-OUT-CONTAINS
   s" arm habu-forth-blind rows=1 certified=1 first_tests=1 tests=1 repairs=0 checker_iterations=1 diagnostics=0 tokens=0 wall_ms=0 final_chars=1" VRT-OUT-CONTAINS
   VRT-LIVE$ VRT-TRUE VRT-RUN-VALIDATE
   s" task_groups" VRT-OUT-CONTAINS
   s" arms" VRT-OUT-CONTAINS
   s" habu-forth" VRT-OUT-CONTAINS
   s" habu-forth-raw" VRT-OUT-CONTAINS
   s" habu-forth-blind" VRT-OUT-CONTAINS ;

: VRT-TEST-MODELS ( -- )
   VRT-WRITE-MODELS
   VRT-LIVE$ VRT-FALSE VRT-RUN-VALIDATE
   s" run=multi-model-fixture-2026-06-18 model=multiple rows=2 certified=2 first_tests=2 tests=2 repairs=0 checker_iterations=2 diagnostics=0 tokens=0 wall_ms=0" VRT-OUT-CONTAINS
   VRT-LIVE$ VRT-TRUE VRT-RUN-VALIDATE
   s" multi-model-fixture-2026-06-18" VRT-OUT-CONTAINS
   s" multiple" VRT-OUT-CONTAINS
   2 VRT-JSON-ROWS$ VRT-OUT-CONTAINS ;

: VRT-TEST-CONFIDENCE ( -- )
   VRT-WRITE-CONFIDENCE
   VRT-LIVE$ VRT-FALSE VRT-RUN-VALIDATE
   s" pass_at_k task_groups=1 task_passed=1 trial_pass_bp=5000 trial_ci95_low_bp=0 trial_ci95_high_bp=10000 task_pass_bp=10000 task_ci95_low_bp=10000 task_ci95_high_bp=10000" VRT-OUT-CONTAINS
   VRT-LIVE$ VRT-TRUE VRT-RUN-VALIDATE
   2 VRT-JSON-ROWS$ VRT-OUT-CONTAINS
   s" task_groups" VRT-OUT-CONTAINS
   s" task_pass_at_k" VRT-OUT-CONTAINS
   s" trial_pass_bp" VRT-OUT-CONTAINS
   s" 5000" VRT-OUT-CONTAINS ;

: VRT-TEST-REJECTIONS ( -- )
   2 VRT-WRITE-LIVE
   VRT-WRITE-MISSING-K
   VRT-LIVE$ s" k_trials coverage mismatch task=1 model=toy-model arm=forth rows=1 k_trials=2" VRT-EXPECT-FAIL
   VRT-WRITE-EXTRA-K
   VRT-LIVE$ s" k_trials coverage mismatch task=1 model=toy-model arm=forth rows=2 k_trials=1" VRT-EXPECT-FAIL
   VRT-WRITE-MISSING-META
   VRT-LIVE$ s" missing fields task_family" VRT-EXPECT-FAIL
   VRT-WRITE-DUP-V2
   VRT-LIVE$ s" duplicate result identity" VRT-EXPECT-FAIL
   VRT-WRITE-MISSING-RAW-HASH
   VRT-LIVE$ s" missing fields raw_response_sha256" VRT-EXPECT-FAIL
   VRT-WRITE-BAD-FINAL-HASH
   VRT-LIVE$ s" invalid sha256 hash" VRT-EXPECT-FAIL
   VRT-WRITE-REFERENCE
   VRT-APPEND-FIRST-REFERENCE
   VRT-EMPTY$ s" duplicate task_id" VRT-EXPECT-FAIL
   VRT-WRITE-REFERENCE-BAD-DATE
   VRT-EMPTY$ s" invalid run_id date" VRT-EXPECT-FAIL
   VRT-WRITE-REFERENCE
   VRT-WRITE-TASKS-MISSING-CAT
   VRT-EMPTY$ s" missing required benchmark category aot-safe" VRT-EXPECT-FAIL ;

: VRT-PREPARE ( -- )
   CLEANUP-RESET
   s" habu-validate-results-test" TMPDIR-MKDIR VRT-ROOT VRT-ROOT-U VRT-COPY!
   VRT-ROOT$ CLEANUP-TREE+
   s" bench/llm/tasks.tsv" VRT-TASKS VRT-TASKS-U VRT-JOIN!
   s" bench/llm/results" VRT-RESULT-DIR VRT-RESULT-DIR-U VRT-JOIN!
   s" bench/llm/results/reference.jsonl" VRT-REF VRT-REF-U VRT-JOIN!
   s" bench/llm/results/attempt.jsonl" VRT-ATTEMPT VRT-ATTEMPT-U VRT-JOIN!
   s" bench/llm/results/live.jsonl" VRT-LIVE VRT-LIVE-U VRT-JOIN!
   VRT-RESULT-DIR$ MAKE-DIRS
   VRT-COPY-TASKS
   VRT-COUNT-TASKS ;

: VRT-MAIN ( -- )
   T-RESET
   VRT-PREPARE
   VRT-TEST-REFERENCE
   VRT-TEST-ATTEMPT-TEXT
   VRT-TEST-ATTEMPT-JSON
   VRT-TEST-LIVE
   VRT-TEST-ARMS
   VRT-TEST-MODELS
   VRT-TEST-CONFIDENCE
   VRT-TEST-REJECTIONS
   CLEANUP-RUN
   T-REPORT
   s" validate-results-test: ok" type cr ;

VRT-MAIN
