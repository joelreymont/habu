\ live-row.f - checked schema-v2 row emitter for live LLM benchmark arms.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/json-write.f, and
\ src/core/sha256.f.

64 constant LR-SHA-LEN
65536 constant LR-FILE-CAP
-3220 constant E-LR-CAPACITY
-3221 constant E-LR-HASH

create LR-FILE-BUF LR-FILE-CAP allot
create LR-SHA LR-SHA-LEN allot
create LR-TRIAL-ID-BUF 256 allot
create LR-PROMPT-PATH FS-PATH-CAP allot
create LR-RAW-PATH FS-PATH-CAP allot
create LR-CAND-PATH FS-PATH-CAP allot
create LR-DIAG-PATH FS-PATH-CAP allot
create LR-REPAIR-PATH FS-PATH-CAP allot
create LR-TEST-PATH FS-PATH-CAP allot
create LR-BUNDLE-PATH FS-PATH-CAP allot

variable LR-FILE-U
variable LR-TRIAL-ID-U
variable LR-PROMPT-U
variable LR-RAW-U
variable LR-CAND-U
variable LR-DIAG-U
variable LR-REPAIR-U
variable LR-TEST-U
variable LR-BUNDLE-U

variable LR-RUN-ID-A
variable LR-RUN-ID-U
variable LR-NAME-A
variable LR-NAME-U
variable LR-MODEL-ID-A
variable LR-MODEL-ID-U
variable LR-MODEL-A
variable LR-MODEL-U
variable LR-ARM-A
variable LR-ARM-U
variable LR-SEED-A
variable LR-SEED-U
variable LR-OUTCOME-A
variable LR-OUTCOME-U
variable LR-FAMILY-A
variable LR-FAMILY-U
variable LR-MODEL-VERSION-A
variable LR-MODEL-VERSION-U
variable LR-MODEL-DATE-A
variable LR-MODEL-DATE-U
variable LR-FIRST-CHECKER-A
variable LR-FIRST-CHECKER-U
variable LR-RUNTIME-STATUS-A
variable LR-RUNTIME-STATUS-U
variable LR-REPAIR-CLASS-A
variable LR-REPAIR-CLASS-U

variable LR-TASK-ID
variable LR-TRIAL
variable LR-TASK-ORDER
variable LR-K
variable LR-ROUNDS
variable LR-TOKENS
variable LR-WALL-MS
variable LR-SOURCE-CHARS
variable LR-FIRST-PASS
variable LR-FIRST-TESTS
variable LR-TESTS-PASSED
variable LR-REPAIR-ITERATIONS
variable LR-CHECKER-ITERATIONS
variable LR-DIAG-COUNT
variable LR-DIAG-TOKEN
variable LR-DIAG-SPAN
variable LR-DIAG-EXPECTED
variable LR-DIAG-ACTUAL
variable LR-DIAG-CODE
variable LR-DIAG-CLASS
variable LR-ALL-ERRORS-STABLE
variable LR-TRUST-USES
variable LR-SIGNATURE-WEAKENED
variable LR-RUNTIME-REPS
variable LR-RUNTIME-WARMUPS

TRUSTED: LR-SET$ ( ptr u8 n ptr n ptr n -- ) {: a:ptr u ap:ptr up:ptr :}
   a ap !
   u up ! ;

: LR-RUN-ID! ( ptr u8 n -- )
   LR-RUN-ID-A LR-RUN-ID-U LR-SET$ ;

: LR-NAME! ( ptr u8 n -- )
   LR-NAME-A LR-NAME-U LR-SET$ ;

: LR-MODEL-ID! ( ptr u8 n -- )
   LR-MODEL-ID-A LR-MODEL-ID-U LR-SET$ ;

: LR-MODEL! ( ptr u8 n -- )
   LR-MODEL-A LR-MODEL-U LR-SET$ ;

: LR-ARM! ( ptr u8 n -- )
   LR-ARM-A LR-ARM-U LR-SET$ ;

: LR-SEED! ( ptr u8 n -- )
   LR-SEED-A LR-SEED-U LR-SET$ ;

: LR-OUTCOME! ( ptr u8 n -- )
   LR-OUTCOME-A LR-OUTCOME-U LR-SET$ ;

: LR-FAMILY! ( ptr u8 n -- )
   LR-FAMILY-A LR-FAMILY-U LR-SET$ ;

: LR-MODEL-VERSION! ( ptr u8 n -- )
   LR-MODEL-VERSION-A LR-MODEL-VERSION-U LR-SET$ ;

: LR-MODEL-DATE! ( ptr u8 n -- )
   LR-MODEL-DATE-A LR-MODEL-DATE-U LR-SET$ ;

: LR-FIRST-CHECKER! ( ptr u8 n -- )
   LR-FIRST-CHECKER-A LR-FIRST-CHECKER-U LR-SET$ ;

: LR-RUNTIME-STATUS! ( ptr u8 n -- )
   LR-RUNTIME-STATUS-A LR-RUNTIME-STATUS-U LR-SET$ ;

: LR-REPAIR-CLASS! ( ptr u8 n -- )
   LR-REPAIR-CLASS-A LR-REPAIR-CLASS-U LR-SET$ ;

TRUSTED: LR-RUN-ID$ ( -- ptr u8 n )
   LR-RUN-ID-A @ LR-RUN-ID-U @ ;

TRUSTED: LR-NAME$ ( -- ptr u8 n )
   LR-NAME-A @ LR-NAME-U @ ;

TRUSTED: LR-MODEL-ID$ ( -- ptr u8 n )
   LR-MODEL-ID-A @ LR-MODEL-ID-U @ ;

TRUSTED: LR-MODEL$ ( -- ptr u8 n )
   LR-MODEL-A @ LR-MODEL-U @ ;

TRUSTED: LR-ARM$ ( -- ptr u8 n )
   LR-ARM-A @ LR-ARM-U @ ;

TRUSTED: LR-SEED$ ( -- ptr u8 n )
   LR-SEED-A @ LR-SEED-U @ ;

TRUSTED: LR-OUTCOME$ ( -- ptr u8 n )
   LR-OUTCOME-A @ LR-OUTCOME-U @ ;

TRUSTED: LR-FAMILY$ ( -- ptr u8 n )
   LR-FAMILY-A @ LR-FAMILY-U @ ;

TRUSTED: LR-MODEL-VERSION$ ( -- ptr u8 n )
   LR-MODEL-VERSION-A @ LR-MODEL-VERSION-U @ ;

TRUSTED: LR-MODEL-DATE$ ( -- ptr u8 n )
   LR-MODEL-DATE-A @ LR-MODEL-DATE-U @ ;

TRUSTED: LR-FIRST-CHECKER$ ( -- ptr u8 n )
   LR-FIRST-CHECKER-A @ LR-FIRST-CHECKER-U @ ;

TRUSTED: LR-RUNTIME-STATUS$ ( -- ptr u8 n )
   LR-RUNTIME-STATUS-A @ LR-RUNTIME-STATUS-U @ ;

TRUSTED: LR-REPAIR-CLASS$ ( -- ptr u8 n )
   LR-REPAIR-CLASS-A @ LR-REPAIR-CLASS-U @ ;

: LR-COPY-PATH ( ptr u8 n ptr u8 ptr n -- ) {: a:ptr u dst:ptr up:ptr :}
   u 0 <= if E-FS-PATH throw then
   u FS-PATH-CAP > if E-LR-CAPACITY throw then
   a dst u BYTE-COPY
   u up ! ;

: LR-PROMPT! ( ptr u8 n -- )
   LR-PROMPT-PATH LR-PROMPT-U LR-COPY-PATH ;

: LR-RAW! ( ptr u8 n -- )
   LR-RAW-PATH LR-RAW-U LR-COPY-PATH ;

: LR-CANDIDATE! ( ptr u8 n -- )
   LR-CAND-PATH LR-CAND-U LR-COPY-PATH ;

: LR-DIAGNOSTICS! ( ptr u8 n -- )
   LR-DIAG-PATH LR-DIAG-U LR-COPY-PATH ;

: LR-REPAIR! ( ptr u8 n -- )
   LR-REPAIR-PATH LR-REPAIR-U LR-COPY-PATH ;

: LR-TEST! ( ptr u8 n -- )
   LR-TEST-PATH LR-TEST-U LR-COPY-PATH ;

: LR-BUNDLE! ( ptr u8 n -- )
   LR-BUNDLE-PATH LR-BUNDLE-U LR-COPY-PATH ;

: LR-PROMPT$ ( -- ptr u8 n )
   LR-PROMPT-PATH LR-PROMPT-U @ ;

: LR-RAW$ ( -- ptr u8 n )
   LR-RAW-PATH LR-RAW-U @ ;

: LR-CANDIDATE$ ( -- ptr u8 n )
   LR-CAND-PATH LR-CAND-U @ ;

: LR-DIAGNOSTICS$ ( -- ptr u8 n )
   LR-DIAG-PATH LR-DIAG-U @ ;

: LR-REPAIR$ ( -- ptr u8 n )
   LR-REPAIR-PATH LR-REPAIR-U @ ;

: LR-TEST$ ( -- ptr u8 n )
   LR-TEST-PATH LR-TEST-U @ ;

: LR-BUNDLE$ ( -- ptr u8 n )
   LR-BUNDLE-PATH LR-BUNDLE-U @ ;

: LR-POSITIVE ( n -- n )
   dup 0 <= if drop 1 then ;

: LR-RESET ( -- )
   s" manifest" LR-RUN-ID!
   s" unknown" LR-NAME!
   s" unknown" LR-MODEL-ID!
   s" unknown" LR-MODEL!
   s" habu-stdlib" LR-ARM!
   s" manifest" LR-SEED!
   s" reject" LR-OUTCOME!
   s" unknown" LR-FAMILY!
   s" unknown" LR-MODEL-VERSION!
   s" unknown" LR-MODEL-DATE!
   s" rejected" LR-FIRST-CHECKER!
   s" not_run" LR-RUNTIME-STATUS!
   s" diagnostic" LR-REPAIR-CLASS!
   0 LR-TASK-ID !
   0 LR-TRIAL !
   0 LR-TASK-ORDER !
   1 LR-K !
   0 LR-ROUNDS !
   0 LR-TOKENS !
   0 LR-WALL-MS !
   1 LR-SOURCE-CHARS !
   0 LR-FIRST-PASS !
   0 LR-FIRST-TESTS !
   0 LR-TESTS-PASSED !
   0 LR-REPAIR-ITERATIONS !
   0 LR-CHECKER-ITERATIONS !
   0 LR-DIAG-COUNT !
   -1 LR-DIAG-TOKEN !
   -1 LR-DIAG-SPAN !
   -1 LR-DIAG-EXPECTED !
   -1 LR-DIAG-ACTUAL !
   -1 LR-DIAG-CODE !
   -1 LR-DIAG-CLASS !
   -1 LR-ALL-ERRORS-STABLE !
   0 LR-TRUST-USES !
   0 LR-SIGNATURE-WEAKENED !
   100 LR-RUNTIME-REPS !
   10 LR-RUNTIME-WARMUPS !
   0 LR-PROMPT-U !
   0 LR-RAW-U !
   0 LR-CAND-U !
   0 LR-DIAG-U !
   0 LR-REPAIR-U !
   0 LR-TEST-U !
   0 LR-BUNDLE-U ! ;

: LR-U+ ( n -- ) {: n :}
   n 0 < if E-LR-CAPACITY throw then
   n 10 >= if n 10 / recurse then
   n 10 mod 48 + SB-APPEND-C ;

: LR-TRIAL-ID$ ( -- ptr u8 n )
   SB-RESET
   LR-SEED$ SB-APPEND
   58 SB-APPEND-C
   LR-MODEL-ID$ SB-APPEND
   58 SB-APPEND-C
   LR-ARM$ SB-APPEND
   58 SB-APPEND-C
   LR-TASK-ID @ LR-U+
   58 SB-APPEND-C
   LR-TRIAL @ LR-U+
   SB$ {: a:ptr u :}
   u 256 > if E-LR-CAPACITY throw then
   a LR-TRIAL-ID-BUF u BYTE-COPY
   u LR-TRIAL-ID-U !
   LR-TRIAL-ID-BUF LR-TRIAL-ID-U @ ;

: LR-KEY-SUFFIX ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu suffix:ptr suffixu :}
   JW-DQ JW-C
   key keyu JW-RAW
   suffix suffixu JW-RAW
   JW-DQ JW-C
   JW-COLON-C JW-C ;

: LR-FILE-SHA ( ptr u8 n -- ptr u8 n ) {: path:ptr pathu :}
   path pathu LR-SHA SHA256-FILE-HEX dup 0 <> if E-LR-HASH throw then
   drop LR-SHA LR-SHA-LEN ;

: LR-FILE-CONTENT ( ptr u8 n -- ptr u8 n ) {: path:ptr pathu :}
   path pathu LR-FILE-BUF LR-FILE-CAP READ-ALL LR-FILE-U !
   LR-FILE-BUF LR-FILE-U @ ;

: LR-FILE-FIELD ( ptr u8 n ptr u8 n -- ) {: key:ptr keyu path:ptr pathu :}
   key keyu path pathu LR-FILE-CONTENT JW-FIELD-S
   JW-COMMA
   key keyu s" _sha256" LR-KEY-SUFFIX
   path pathu LR-FILE-SHA JW-STRING ;

: LR-COMMA-FIELD-S ( ptr u8 n ptr u8 n -- )
   JW-COMMA JW-FIELD-S ;

: LR-COMMA-FIELD-U ( ptr u8 n n -- )
   JW-COMMA JW-FIELD-U ;

: LR-COMMA-FIELD-BOOL ( ptr u8 n bool -- )
   JW-COMMA JW-FIELD-BOOL ;

: LR-COMMA-FIELD-RAW ( ptr u8 n ptr u8 n -- )
   JW-COMMA JW-FIELD-RAW ;

: LR-COMMA-FIELD-NULL ( ptr u8 n -- )
   JW-COMMA JW-FIELD-NULL ;

: LR-RAW-DQ ( -- )
   JW-DQ SB-APPEND-C ;

: LR-RAW-KEY ( ptr u8 n -- )
   LR-RAW-DQ
   SB-APPEND
   LR-RAW-DQ
   JW-COLON-C SB-APPEND-C ;

: LR-RAW-S ( ptr u8 n -- )
   LR-RAW-DQ
   SB-APPEND
   LR-RAW-DQ ;

: LR-RAW-BOOL ( bool -- )
   if s" true" else s" false" then SB-APPEND ;

: LR-REPAIR-STATS$ ( -- ptr u8 n )
   LR-DIAG-COUNT @ 0= if s" []" exit then
   SB-RESET
   JW-LBRACK SB-APPEND-C
   JW-LBRACE SB-APPEND-C
   s" repair_class" LR-RAW-KEY
   LR-REPAIR-CLASS$ LR-RAW-S
   JW-COMMA-C SB-APPEND-C
   s" diagnostic_count" LR-RAW-KEY
   LR-DIAG-COUNT @ LR-U+
   JW-COMMA-C SB-APPEND-C
   s" repair_success" LR-RAW-KEY
   LR-TESTS-PASSED @ LR-RAW-BOOL
   JW-COMMA-C SB-APPEND-C
   s" repair_iterations" LR-RAW-KEY
   LR-REPAIR-ITERATIONS @ LR-U+
   JW-COMMA-C SB-APPEND-C
   s" token_delta" LR-RAW-KEY
   0 LR-U+
   JW-RBRACE SB-APPEND-C
   JW-RBRACK SB-APPEND-C
   SB$ ;

: LR-BUILD-ROW ( -- )
   JW-RESET
   JW-OBJECT-START
   s" schema_version" 2 JW-FIELD-U
   s" run_id" LR-RUN-ID$ LR-COMMA-FIELD-S
   s" task_id" LR-TASK-ID @ LR-COMMA-FIELD-U
   s" name" LR-NAME$ LR-COMMA-FIELD-S
   s" model_id" LR-MODEL-ID$ LR-COMMA-FIELD-S
   s" model" LR-MODEL$ LR-COMMA-FIELD-S
   s" arm" LR-ARM$ LR-COMMA-FIELD-S
   s" trial_id" LR-TRIAL-ID$ LR-COMMA-FIELD-S
   s" trial" LR-TRIAL @ LR-COMMA-FIELD-U
   s" task_order" LR-TASK-ORDER @ LR-COMMA-FIELD-U
   s" k_trials" LR-K @ LR-COMMA-FIELD-U
   s" order_seed" LR-SEED$ LR-COMMA-FIELD-S
   s" outcome" LR-OUTCOME$ LR-COMMA-FIELD-S
   s" rounds" LR-ROUNDS @ LR-COMMA-FIELD-U
   s" first_pass" LR-FIRST-PASS @ LR-COMMA-FIELD-BOOL
   s" tokens" LR-TOKENS @ LR-COMMA-FIELD-U
   s" wall_ms" LR-WALL-MS @ LR-COMMA-FIELD-U
   s" source_chars" LR-SOURCE-CHARS @ LR-POSITIVE LR-COMMA-FIELD-U
   s" runtime_ms" LR-COMMA-FIELD-NULL
   s" runtime_repetitions" LR-RUNTIME-REPS @ LR-COMMA-FIELD-U
   s" runtime_warmups" LR-RUNTIME-WARMUPS @ LR-COMMA-FIELD-U
   s" runtime_status" LR-RUNTIME-STATUS$ LR-COMMA-FIELD-S
   s" task_family" LR-FAMILY$ LR-COMMA-FIELD-S
   s" model_version" LR-MODEL-VERSION$ LR-COMMA-FIELD-S
   s" model_date" LR-MODEL-DATE$ LR-COMMA-FIELD-S
   s" attempt" LR-ROUNDS @ LR-COMMA-FIELD-U
   s" first_pass_checker" LR-FIRST-CHECKER$ LR-COMMA-FIELD-S
   s" first_pass_tests" LR-FIRST-TESTS @ LR-COMMA-FIELD-BOOL
   s" tests_passed" LR-TESTS-PASSED @ LR-COMMA-FIELD-BOOL
   s" repair_iterations" LR-REPAIR-ITERATIONS @ LR-COMMA-FIELD-U
   s" checker_iterations" LR-CHECKER-ITERATIONS @ LR-COMMA-FIELD-U
   s" diagnostic_count" LR-DIAG-COUNT @ LR-COMMA-FIELD-U
   s" diagnostic_token" LR-DIAG-TOKEN @ LR-COMMA-FIELD-BOOL
   s" diagnostic_span" LR-DIAG-SPAN @ LR-COMMA-FIELD-BOOL
   s" diagnostic_expected" LR-DIAG-EXPECTED @ LR-COMMA-FIELD-BOOL
   s" diagnostic_actual" LR-DIAG-ACTUAL @ LR-COMMA-FIELD-BOOL
   s" diagnostic_code" LR-DIAG-CODE @ LR-COMMA-FIELD-BOOL
   s" diagnostic_repair_class" LR-DIAG-CLASS @ LR-COMMA-FIELD-BOOL
   s" all_errors_stable" LR-ALL-ERRORS-STABLE @ LR-COMMA-FIELD-BOOL
   s" tokens_used" LR-TOKENS @ LR-COMMA-FIELD-U
   s" final_chars" LR-SOURCE-CHARS @ LR-POSITIVE LR-COMMA-FIELD-U
   s" trust_uses" LR-TRUST-USES @ LR-COMMA-FIELD-U
   s" signature_weakened" LR-SIGNATURE-WEAKENED @ LR-COMMA-FIELD-BOOL
   s" repair_class_stats" LR-REPAIR-STATS$ LR-COMMA-FIELD-RAW
   JW-COMMA s" prompt" LR-PROMPT$ LR-FILE-FIELD
   JW-COMMA s" raw_response" LR-RAW$ LR-FILE-FIELD
   JW-COMMA s" extracted_candidate" LR-CANDIDATE$ LR-FILE-FIELD
   JW-COMMA s" checker_diagnostics" LR-DIAGNOSTICS$ LR-FILE-FIELD
   JW-COMMA s" repair_packet" LR-REPAIR$ LR-FILE-FIELD
   JW-COMMA s" test_output" LR-TEST$ LR-FILE-FIELD
   JW-COMMA s" final_bundle" LR-BUNDLE$ LR-FILE-FIELD
   JW-OBJECT-END ;

: LR-ROW$ ( -- ptr u8 n )
   LR-BUILD-ROW
   JW$ ;

: LR-EMIT ( -- )
   LR-ROW$ type cr ;
