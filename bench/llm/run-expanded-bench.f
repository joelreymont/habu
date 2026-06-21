\ run-expanded-bench.f - native expanded live benchmark runner.
\
\ Load after lib/errors.f, lib/string.f, lib/fs.f, lib/process.f,
\ lib/process-argv.f, lib/process-env.f, lib/argv.f, and
\ bench/llm/manifest.f.

64 constant RB-USAGE-RC
74 constant RB-RUN-RC
65536 constant RB-TASK-CAP
8192 constant RB-MODEL-CAP
524288 constant RB-OUT-CAP
32 constant RB-SPACE
44 constant RB-COMMA
48 constant RB-ZERO
10 constant RB-DEC
10 constant RB-LF

create RB-TASK-BUF RB-TASK-CAP allot
create RB-MODEL-BUF RB-MODEL-CAP allot
create RB-OUT-BUF RB-OUT-CAP allot
create RB-NUM-BUF 32 allot
create RB-SNIP1 256 allot
create RB-SNIP2 256 allot
create RB-SNIP3 256 allot
create RB-SNIP4 256 allot
create RB-EXE-BUF FS-PATH-CAP allot
create RB-LF-BUF 1 allot
RB-LF RB-LF-BUF c!

variable RB-K
variable RB-TASK-LEN
variable RB-MODEL-LEN
variable RB-OUT-LEN
variable RB-TASK-NEXT
variable RB-TASK-A
variable RB-TASK-U
variable RB-MODEL-NEXT
variable RB-MODEL-A
variable RB-MODEL-U
variable RB-TASK-ORDER
variable RB-SELECTED
variable RB-LIMIT
variable RB-MAX-REPAIRS
variable RB-RESUME
variable RB-OUT-FD
variable RB-PID

variable RB-TASKS-A
variable RB-TASKS-U
variable RB-OUT-A
variable RB-OUT-U
variable RB-RESULTS-A
variable RB-RESULTS-U
variable RB-PERF-A
variable RB-PERF-U
variable RB-SEED-A
variable RB-SEED-U
variable RB-MODEL-REG-A
variable RB-MODEL-REG-U
variable RB-MODEL-ONLY-A
variable RB-MODEL-ONLY-U
variable RB-TASK-IDS-A
variable RB-TASK-IDS-U
variable RB-FORTH-MODES-A
variable RB-FORTH-MODES-U
variable RB-FORTH-ARM-A
variable RB-FORTH-ARM-U
variable RB-ARRAY-ARMS-A
variable RB-ARRAY-ARMS-U
variable RB-SNIP1-U
variable RB-SNIP2-U
variable RB-SNIP3-U
variable RB-SNIP4-U
variable RB-EXE-U
variable RB-ROW-NEXT
variable RB-ROW-A
variable RB-ROW-U
variable RB-MODE-NEXT
variable RB-MODE-A
variable RB-MODE-U
variable RB-TRIAL

: RB-TRUE ( -- bool )
   0 0= ;

: RB-FALSE ( -- bool )
   RB-TRUE 0= ;

: RB-DIE ( ptr u8 n -- )
   RB-RUN-RC die ;

: RB-USAGE ( -- )
   s" usage: bench/llm/run-expanded-bench.f [k_trials] [out.jsonl]" RB-USAGE-RC die ;

TRUSTED: RB-SET$ ( ptr u8 n ptr n ptr n -- ) {: a:ptr u ap:ptr up:ptr :}
   a ap !
   u up ! ;

TRUSTED: RB-TASK-LINE! ( ptr u8 n -- )
   RB-TASK-U ! RB-TASK-A ! ;

TRUSTED: RB-MODEL-LINE! ( ptr u8 n -- )
   RB-MODEL-U ! RB-MODEL-A ! ;

TRUSTED: RB-ROW-LINE! ( ptr u8 n -- )
   RB-ROW-U ! RB-ROW-A ! ;

TRUSTED: RB-MODE! ( ptr u8 n -- )
   RB-MODE-U ! RB-MODE-A ! ;

: RB-TASKS! ( ptr u8 n -- )
   RB-TASKS-A RB-TASKS-U RB-SET$ ;

: RB-OUT! ( ptr u8 n -- )
   RB-OUT-A RB-OUT-U RB-SET$ ;

: RB-RESULTS! ( ptr u8 n -- )
   RB-RESULTS-A RB-RESULTS-U RB-SET$ ;

: RB-PERF! ( ptr u8 n -- )
   RB-PERF-A RB-PERF-U RB-SET$ ;

: RB-SEED! ( ptr u8 n -- )
   RB-SEED-A RB-SEED-U RB-SET$ ;

: RB-MODEL-REG! ( ptr u8 n -- )
   RB-MODEL-REG-A RB-MODEL-REG-U RB-SET$ ;

: RB-MODEL-ONLY! ( ptr u8 n -- )
   RB-MODEL-ONLY-A RB-MODEL-ONLY-U RB-SET$ ;

: RB-TASK-IDS! ( ptr u8 n -- )
   RB-TASK-IDS-A RB-TASK-IDS-U RB-SET$ ;

: RB-FORTH-MODES! ( ptr u8 n -- )
   RB-FORTH-MODES-A RB-FORTH-MODES-U RB-SET$ ;

: RB-FORTH-ARM! ( ptr u8 n -- )
   RB-FORTH-ARM-A RB-FORTH-ARM-U RB-SET$ ;

: RB-ARRAY-ARMS! ( ptr u8 n -- )
   RB-ARRAY-ARMS-A RB-ARRAY-ARMS-U RB-SET$ ;

TRUSTED: RB-TASKS$ ( -- ptr u8 n )
   RB-TASKS-A @ RB-TASKS-U @ ;

TRUSTED: RB-OUT$ ( -- ptr u8 n )
   RB-OUT-A @ RB-OUT-U @ ;

TRUSTED: RB-RESULTS$ ( -- ptr u8 n )
   RB-RESULTS-A @ RB-RESULTS-U @ ;

TRUSTED: RB-PERF$ ( -- ptr u8 n )
   RB-PERF-A @ RB-PERF-U @ ;

TRUSTED: RB-SEED$ ( -- ptr u8 n )
   RB-SEED-A @ RB-SEED-U @ ;

TRUSTED: RB-MODEL-REG$ ( -- ptr u8 n )
   RB-MODEL-REG-A @ RB-MODEL-REG-U @ ;

TRUSTED: RB-MODEL-ONLY$ ( -- ptr u8 n )
   RB-MODEL-ONLY-A @ RB-MODEL-ONLY-U @ ;

TRUSTED: RB-TASK-IDS$ ( -- ptr u8 n )
   RB-TASK-IDS-A @ RB-TASK-IDS-U @ ;

TRUSTED: RB-FORTH-MODES$ ( -- ptr u8 n )
   RB-FORTH-MODES-A @ RB-FORTH-MODES-U @ ;

TRUSTED: RB-FORTH-ARM$ ( -- ptr u8 n )
   RB-FORTH-ARM-A @ RB-FORTH-ARM-U @ ;

TRUSTED: RB-ARRAY-ARMS$ ( -- ptr u8 n )
   RB-ARRAY-ARMS-A @ RB-ARRAY-ARMS-U @ ;

: RB-PERF? ( -- bool )
   RB-PERF$ nip 0 > ;

TRUSTED: RB-MODE$ ( -- ptr u8 n )
   RB-MODE-A @ RB-MODE-U @ ;

: RB-ENV-OR ( ptr u8 n ptr u8 n -- ptr u8 n ) {: name:ptr nameu def:ptr defu :}
   name nameu GETENV dup 0= if 2drop def defu then ;

: RB-PARSE-U ( ptr u8 n -- n ) {: a:ptr u :}
   a u STR>NUMBER? 0= if s" run-expanded-bench: invalid number" RB-DIE then
   dup 0 < if s" run-expanded-bench: negative number" RB-DIE then ;

: RB-ENV-U-OR ( ptr u8 n n -- n ) {: name:ptr nameu def :}
   name nameu GETENV dup 0= if 2drop def exit then
   RB-PARSE-U ;

: RB-ENV-FLAG? ( ptr u8 n -- bool )
   GETENV dup 0= if 2drop RB-FALSE exit then
   RB-PARSE-U 0 = 0= ;

: RB-SB-U+ ( n -- ) {: n :}
   n 0 < if s" run-expanded-bench: negative format" RB-DIE then
   n RB-DEC >= if n RB-DEC / recurse then
   n RB-DEC mod RB-ZERO + SB-APPEND-C ;

: RB-PROC-ENV$ ( ptr u8 n ptr u8 n -- )
   PROC-ENV+ ;

: RB-PROC-ENV-U ( ptr u8 n n -- ) {: key:ptr keyu val :}
   SB-RESET
   val RB-SB-U+
   key keyu SB$ PROC-ENV+ ;

: RB-PATH$ ( -- ptr u8 n )
   s" PATH" GETENV dup 0= if
      2drop s" /usr/bin:/bin:/usr/local/bin:/opt/homebrew/bin"
   then ;

: RB-PROC-PATH ( -- )
   s" PATH" RB-PATH$ RB-PROC-ENV$ ;

: RB-RESOLVE-EXE ( ptr u8 n -- ptr u8 n )
   RB-PATH$ RB-EXE-BUF FIND-EXECUTABLE-IN-PATH 0= if
      drop 0 E-PROC-PATH throw
   then
   RB-EXE-U !
   RB-EXE-BUF RB-EXE-U @ ;

: RB-LOAD-FILES ( -- )
   RB-TASKS$ RB-TASK-BUF RB-TASK-CAP READ-ALL RB-TASK-LEN !
   RB-MODEL-REG$ RB-MODEL-BUF RB-MODEL-CAP READ-ALL RB-MODEL-LEN ! ;

TRUSTED: RB-TASK-LINE$ ( -- ptr u8 n )
   RB-TASK-A @ RB-TASK-U @ ;

TRUSTED: RB-MODEL-LINE$ ( -- ptr u8 n )
   RB-MODEL-A @ RB-MODEL-U @ ;

: RB-READ-TASK-LINE ( -- bool )
   RB-TASK-BUF RB-TASK-LEN @ RB-TASK-NEXT @ BM-LINE-NEXT if
      RB-TASK-NEXT !
      RB-TASK-LINE!
      RB-TRUE
   else
      drop 2drop RB-FALSE
   then ;

: RB-READ-MODEL-LINE ( -- bool )
   RB-MODEL-BUF RB-MODEL-LEN @ RB-MODEL-NEXT @ BM-LINE-NEXT if
      RB-MODEL-NEXT !
      RB-MODEL-LINE!
      RB-TRUE
   else
      drop 2drop RB-FALSE
   then ;

: RB-MODEL-ID$ ( -- ptr u8 n )
   RB-MODEL-LINE$ BM-M-ID BM-MODEL-FIELD$ ;

: RB-MODEL-SELECTED? ( -- bool )
   RB-MODEL-ONLY$ nip 0= if RB-TRUE exit then
   RB-MODEL-ID$ RB-MODEL-ONLY$ STR= ;

: RB-TASK-SELECTED? ( -- bool )
   RB-TASK-LINE$ RB-TASK-IDS$ BM-TASK-SELECTED? ;

: RB-LIMIT-REACHED? ( -- bool )
   RB-LIMIT @ 0 < if RB-FALSE exit then
   RB-SELECTED @ RB-LIMIT @ >= ;

: RB-TASK-FIELD$ ( n -- ptr u8 n )
   RB-TASK-LINE$ rot BM-TASK-FIELD$ ;

: RB-TASK-HARNESS$ ( -- ptr u8 n )
   BM-T-HARNESS RB-TASK-FIELD$ ;

: RB-TASK-CONV$ ( -- ptr u8 n )
   BM-T-CONV RB-TASK-FIELD$ ;

: RB-TASK-FORTH? ( -- bool )
   RB-TASK-HARNESS$ s" forth" STR= ;

: RB-TASK-ARRAY? ( -- bool )
   RB-TASK-HARNESS$ s" array" STR= ;

: RB-TASK-STDLIB-STACK? ( -- bool )
   RB-TASK-HARNESS$ s" stdlib" STR= if
      RB-TASK-CONV$ s" stack" STR= exit
   then
   RB-FALSE ;

: RB-TASK-STDLIB-FILE? ( -- bool )
   RB-TASK-HARNESS$ s" stdlib-file" STR= if
      RB-TASK-CONV$ s" run" STR= exit
   then
   RB-FALSE ;

: RB-TASK-STDLIB-FILE-NEGATIVE? ( -- bool )
   RB-TASK-HARNESS$ s" stdlib-negative" STR= if
      RB-TASK-CONV$ s" reject" STR= if
         BM-T-NAME RB-TASK-FIELD$ s" FS-READ-CAPACITY" STR= exit
      then
   then
   RB-FALSE ;

: RB-TASK-STDLIB-PROCESS? ( -- bool )
   RB-TASK-HARNESS$ s" stdlib-process" STR= if
      RB-TASK-CONV$ s" run" STR= exit
   then
   RB-FALSE ;

: RB-TASK-STDLIB-PROCESS-NEGATIVE? ( -- bool )
   RB-TASK-HARNESS$ s" stdlib-negative" STR= if
      RB-TASK-CONV$ s" reject" STR= if
         BM-T-NAME RB-TASK-FIELD$ s" PROC-CAPTURE-TIMEOUT" STR= if RB-TRUE exit then
         BM-T-NAME RB-TASK-FIELD$ s" PROC-CAPTURE-TRUNCATED" STR= exit
      then
   then
   RB-FALSE ;

: RB-TASK-STDLIB-PROPERTY? ( -- bool )
   RB-TASK-HARNESS$ s" stdlib-property" STR= if
      RB-TASK-CONV$ s" run" STR= exit
   then
   RB-FALSE ;

: RB-TASK-STDLIB-PROPERTY-NEGATIVE? ( -- bool )
   RB-TASK-HARNESS$ s" stdlib-negative" STR= if
      RB-TASK-CONV$ s" reject" STR= if
         BM-T-NAME RB-TASK-FIELD$ s" PROP-BAD-SEED" STR= exit
      then
   then
   RB-FALSE ;

: RB-TASK-STDLIB-BUILD? ( -- bool )
   RB-TASK-HARNESS$ s" stdlib-build" STR= if
      RB-TASK-CONV$ s" run" STR= exit
   then
   RB-FALSE ;

: RB-TASK-STDLIB-BUILD-NEGATIVE? ( -- bool )
   RB-TASK-HARNESS$ s" stdlib-negative" STR= if
      RB-TASK-CONV$ s" reject" STR= if
         BM-T-NAME RB-TASK-FIELD$ s" BUILD-STEP-STATUS" STR= if RB-TRUE exit then
         BM-T-NAME RB-TASK-FIELD$ s" BUILD-MISSING-ARTIFACT" STR= exit
      then
   then
   RB-FALSE ;

: RB-TASK-AOT? ( -- bool )
   RB-TASK-HARNESS$ s" aot" STR= if
      RB-TASK-CONV$ s" build-run" STR= exit
   then
   RB-FALSE ;

: RB-TASK-AOT-NEGATIVE? ( -- bool )
   RB-TASK-HARNESS$ s" aot-negative" STR= if
      RB-TASK-CONV$ s" reject" STR= exit
   then
   RB-FALSE ;

: RB-TASK-RUNNABLE? ( -- bool )
   RB-TASK-FORTH? if RB-TRUE exit then
   RB-TASK-ARRAY? if RB-TRUE exit then
   RB-TASK-STDLIB-STACK? if RB-TRUE exit then
   RB-TASK-STDLIB-FILE? if RB-TRUE exit then
   RB-TASK-STDLIB-FILE-NEGATIVE? if RB-TRUE exit then
   RB-TASK-STDLIB-PROCESS? if RB-TRUE exit then
   RB-TASK-STDLIB-PROCESS-NEGATIVE? if RB-TRUE exit then
   RB-TASK-STDLIB-PROPERTY? if RB-TRUE exit then
   RB-TASK-STDLIB-PROPERTY-NEGATIVE? if RB-TRUE exit then
   RB-TASK-STDLIB-BUILD? if RB-TRUE exit then
   RB-TASK-STDLIB-BUILD-NEGATIVE? if RB-TRUE exit then
   RB-TASK-AOT? if RB-TRUE exit then
   RB-TASK-AOT-NEGATIVE? ;

: RB-ARM-FOR-MODE$ ( ptr u8 n -- ptr u8 n ) {: mode:ptr modeu :}
   RB-FORTH-ARM$ nip 0 > if RB-FORTH-ARM$ exit then
   mode modeu s" repair" STR= if s" habu-forth" exit then
   mode modeu s" raw" STR= if s" habu-forth-raw" exit then
   mode modeu s" blind" STR= if s" habu-forth-blind" exit then
   s" run-expanded-bench: unknown BENCH_FORTH_MODES entry" RB-DIE
   s" " ;

: RB-ARRAY-HABU-ARM$ ( ptr u8 n -- ptr u8 n ) {: arm:ptr armu :}
   arm armu s" habu-a" STR= if s" a" exit then
   arm armu s" habu-lib" STR= if s" lib" exit then
   arm armu s" habu-stdlib" STR= if s" stdlib" exit then
   arm armu s" habu-skeleton" STR= if s" skeleton" exit then
   s" run-expanded-bench: unknown Habu array arm" RB-DIE
   s" " ;

: RB-ARRAY-HABU? ( ptr u8 n -- bool ) {: arm:ptr armu :}
   arm armu s" habu-a" STR= if RB-TRUE exit then
   arm armu s" habu-lib" STR= if RB-TRUE exit then
   arm armu s" habu-stdlib" STR= if RB-TRUE exit then
   arm armu s" habu-skeleton" STR= ;

: RB-ARRAY-SCRIPT$ ( ptr u8 n -- ptr u8 n ) {: arm:ptr armu :}
   arm armu RB-ARRAY-HABU? if s" bench/llm/drive-habu.sh" exit then
   arm armu s" js" STR= if s" bench/llm/drive-js.sh" exit then
   arm armu s" ts" STR= if s" bench/llm/drive-ts.sh" exit then
   arm armu s" rust" STR= if s" bench/llm/drive-rust.sh" exit then
   s" run-expanded-bench: unknown BENCH_ARRAY_ARMS entry" RB-DIE
   s" " ;

: RB-OUT-RESET ( -- )
   RB-OUT$ RB-LF-BUF 0 WRITE-ALL ;

: RB-RESULTS-RESET ( -- )
   RB-RESULTS$ RB-LF-BUF 0 WRITE-ALL ;

: RB-RUN-PREPARE ( -- )
   s" sh" RB-RESOLVE-EXE PROC-ARGV-PREPARE
   PROC-ENV-INHERIT-MISSING
   PROC-ENV-PREPARE
   RB-OUT$ OPEN-APPEND-FD RB-OUT-FD !
   -1 RB-OUT-FD @ -1 PROC-SPAWN-ARGV-ENV-RAW RB-PID !
   PROC-ARGV-ENV-RESET
   RB-OUT-FD @ close
   RB-PID @ 0 < if s" run-expanded-bench: spawn failed" RB-DIE then ;

: RB-RUN-APPEND ( -- n )
   RB-RUN-PREPARE
   RB-PID @ WAIT-RC ;

: RB-RUN-TO-RESULTS ( -- n )
   s" bin/hb" RB-RESOLVE-EXE PROC-ARGV-PREPARE
   PROC-ENV-INHERIT-MISSING
   PROC-ENV-PREPARE
   RB-RESULTS$ OPEN-APPEND-FD RB-OUT-FD !
   -1 RB-OUT-FD @ -1 PROC-SPAWN-ARGV-ENV-RAW RB-PID !
   PROC-ARGV-ENV-RESET
   RB-OUT-FD @ close
   RB-PID @ 0 < if s" run-expanded-bench: report spawn failed" RB-DIE then
   RB-PID @ WAIT-RC ;

: RB-RUN-HB-APPEND ( -- n )
   s" bin/hb" RB-RESOLVE-EXE PROC-ARGV-PREPARE
   PROC-ENV-INHERIT-MISSING
   PROC-ENV-PREPARE
   RB-OUT$ OPEN-APPEND-FD RB-OUT-FD !
   -1 RB-OUT-FD @ -1 PROC-SPAWN-ARGV-ENV-RAW RB-PID !
   PROC-ARGV-ENV-RESET
   RB-OUT-FD @ close
   RB-PID @ 0 < if s" run-expanded-bench: hb spawn failed" RB-DIE then
   RB-PID @ WAIT-RC ;

: RB-OUT-LOAD ( -- )
   RB-OUT$ FILE? if
      RB-OUT$ RB-OUT-BUF RB-OUT-CAP READ-ALL RB-OUT-LEN !
   else
      0 RB-OUT-LEN !
   then ;

: RB-SNIP-COPY ( ptr u8 ptr a -- ) {: dst:ptr lenp:ptr :}
   SB$ {: a:ptr u :}
   u 256 > if s" run-expanded-bench: row key too long" RB-DIE then
   a dst u BYTE-COPY
   u lenp ! ;

: RB-Q ( -- )
   [char] " SB-APPEND-C ;

: RB-SNIP-TASK ( ptr u8 n -- ) {: id:ptr idu :}
   SB-RESET
   RB-Q s" task_id" SB-APPEND RB-Q s" :" SB-APPEND
   id idu SB-APPEND
   s" ," SB-APPEND
   RB-SNIP1 RB-SNIP1-U RB-SNIP-COPY ;

: RB-SNIP-MODEL ( ptr u8 n -- ) {: model:ptr modelu :}
   SB-RESET
   RB-Q s" model_id" SB-APPEND RB-Q s" :" SB-APPEND RB-Q
   model modelu SB-APPEND
   RB-Q
   RB-SNIP2 RB-SNIP2-U RB-SNIP-COPY ;

: RB-SNIP-ARM ( ptr u8 n -- ) {: arm:ptr armu :}
   SB-RESET
   RB-Q s" arm" SB-APPEND RB-Q s" :" SB-APPEND RB-Q
   arm armu SB-APPEND
   RB-Q
   RB-SNIP3 RB-SNIP3-U RB-SNIP-COPY ;

: RB-SNIP-TRIAL ( n -- ) {: trial :}
   SB-RESET
   RB-Q s" trial" SB-APPEND RB-Q s" :" SB-APPEND
   trial RB-SB-U+
   s" ," SB-APPEND
   RB-SNIP4 RB-SNIP4-U RB-SNIP-COPY ;

TRUSTED: RB-ROW-LINE$ ( -- ptr u8 n )
   RB-ROW-A @ RB-ROW-U @ ;

: RB-READ-OUT-LINE ( -- bool )
   RB-OUT-BUF RB-OUT-LEN @ RB-ROW-NEXT @ BM-LINE-NEXT if
      RB-ROW-NEXT !
      RB-ROW-LINE!
      RB-TRUE
   else
      drop 2drop RB-FALSE
   then ;

: RB-ROW-HAS? ( ptr u8 n -- bool ) {: needle:ptr needleu :}
   RB-ROW-LINE$ needle needleu CONTAINS? ;

: RB-ROW-MATCH? ( -- bool )
   RB-SNIP1 RB-SNIP1-U @ RB-ROW-HAS? 0= if RB-FALSE exit then
   RB-SNIP2 RB-SNIP2-U @ RB-ROW-HAS? 0= if RB-FALSE exit then
   RB-SNIP3 RB-SNIP3-U @ RB-ROW-HAS? 0= if RB-FALSE exit then
   RB-SNIP4 RB-SNIP4-U @ RB-ROW-HAS? ;

: RB-ROW-DONE? {: id:ptr idu model:ptr modelu arm:ptr armu trial :} ( ptr u8 n ptr u8 n ptr u8 n n -- bool )
   id idu RB-SNIP-TASK
   model modelu RB-SNIP-MODEL
   arm armu RB-SNIP-ARM
   trial RB-SNIP-TRIAL
   RB-OUT-LOAD
   0 RB-ROW-NEXT !
   begin RB-READ-OUT-LINE while
      RB-ROW-MATCH? if RB-TRUE exit then
   repeat
   RB-FALSE ;

: RB-ADD-COMMON-ENV ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   RB-PROC-PATH
   s" MODEL_ID" model modelu RB-PROC-ENV$
   s" BENCH_TRIAL" trial RB-PROC-ENV-U
   s" BENCH_TASK_ORDER" RB-TASK-ORDER @ RB-PROC-ENV-U
   s" BENCH_K" RB-K @ RB-PROC-ENV-U
   s" BENCH_SEED" RB-SEED$ RB-PROC-ENV$
   s" BENCH_TASK_FAMILY" BM-T-CATEGORY RB-TASK-FIELD$ RB-PROC-ENV$
   s" MODEL_REGISTRY" RB-MODEL-REG$ RB-PROC-ENV$ ;

: RB-FORTH-ARGS ( ptr u8 n ptr u8 n n -- ) {: model:ptr modelu mode:ptr modeu trial :}
   PROC-ARGV-ENV-RESET
   model modelu trial RB-ADD-COMMON-ENV
   s" BENCH_FORTH_FEEDBACK" mode modeu RB-PROC-ENV$
   s" BENCH_FORTH_ARM" mode modeu RB-ARM-FOR-MODE$ RB-PROC-ENV$
   s" bench/llm/drive-forth.sh" PROC-ARGV+
   BM-T-ID RB-TASK-FIELD$ PROC-ARGV+
   BM-T-NAME RB-TASK-FIELD$ PROC-ARGV+
   RB-TASK-LINE$ BM-TASK-SIG$ PROC-ARGV+
   BM-T-CATEGORY RB-TASK-FIELD$ PROC-ARGV+
   BM-T-TESTS RB-TASK-FIELD$ PROC-ARGV+
   BM-T-SPEC RB-TASK-FIELD$ PROC-ARGV+
   SB-RESET
   RB-MAX-REPAIRS @ RB-SB-U+
   SB$ PROC-ARGV+ ;

: RB-ARRAY-ARGS ( ptr u8 n ptr u8 n n -- ) {: model:ptr modelu arm:ptr armu trial :}
   PROC-ARGV-ENV-RESET
   model modelu trial RB-ADD-COMMON-ENV
   arm armu RB-ARRAY-SCRIPT$ PROC-ARGV+
   BM-T-ID RB-TASK-FIELD$ PROC-ARGV+
   BM-T-NAME RB-TASK-FIELD$ PROC-ARGV+
   RB-TASK-LINE$ BM-TASK-SIG$ PROC-ARGV+
   BM-T-SPEC RB-TASK-FIELD$ PROC-ARGV+
   BM-T-CONV RB-TASK-FIELD$ PROC-ARGV+
   BM-T-VECTORS RB-TASK-FIELD$ PROC-ARGV+
   arm armu RB-ARRAY-HABU? if
      arm armu RB-ARRAY-HABU-ARM$ PROC-ARGV+
   then
   SB-RESET
   RB-MAX-REPAIRS @ RB-SB-U+
   SB$ PROC-ARGV+ ;

: RB-STDLIB-LOADS ( -- )
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+
   s" lib/fs-mutate.f" PROC-ARGV+
   s" lib/process.f" PROC-ARGV+
   s" lib/process-argv.f" PROC-ARGV+
   s" lib/process-env.f" PROC-ARGV+
   s" tools/argv.f" PROC-ARGV+
   s" tools/json.f" PROC-ARGV+
   s" bench/llm/manifest.f" PROC-ARGV+
   s" bench/llm/model.f" PROC-ARGV+
   s" bench/llm/parse-resp-lib.f" PROC-ARGV+
   s" bench/llm/model-run.f" PROC-ARGV+
   s" bench/llm/vectors.f" PROC-ARGV+
   s" lib/json-write.f" PROC-ARGV+
   s" src/core/sha256.f" PROC-ARGV+
   s" bench/llm/live-row.f" PROC-ARGV+
   s" bench/llm/drive-stdlib-lib.f" PROC-ARGV+ ;

: RB-STDLIB-TASK-ARGS ( -- )
   BM-T-ID RB-TASK-FIELD$ PROC-ARGV+
   BM-T-NAME RB-TASK-FIELD$ PROC-ARGV+
   RB-TASK-LINE$ BM-TASK-SIG$ PROC-ARGV+
   BM-T-CATEGORY RB-TASK-FIELD$ PROC-ARGV+
   BM-T-TESTS RB-TASK-FIELD$ PROC-ARGV+
   BM-T-SPEC RB-TASK-FIELD$ PROC-ARGV+
   SB-RESET
   RB-MAX-REPAIRS @ RB-SB-U+
   SB$ PROC-ARGV+ ;

: RB-STDLIB-ARGS ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   PROC-ARGV-ENV-RESET
   model modelu trial RB-ADD-COMMON-ENV
   RB-STDLIB-LOADS
   s" bench/llm/drive-stdlib.f" PROC-ARGV+
   s" --" PROC-ARGV+
   RB-STDLIB-TASK-ARGS ;

: RB-FILE-ARGS ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   PROC-ARGV-ENV-RESET
   model modelu trial RB-ADD-COMMON-ENV
   RB-STDLIB-LOADS
   s" bench/llm/driver-fixture-helpers.f" PROC-ARGV+
   s" bench/llm/drive-file-lib.f" PROC-ARGV+
   s" bench/llm/drive-file.f" PROC-ARGV+
   s" --" PROC-ARGV+
   RB-STDLIB-TASK-ARGS ;

: RB-PROCESS-ARGS ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   PROC-ARGV-ENV-RESET
   model modelu trial RB-ADD-COMMON-ENV
   RB-STDLIB-LOADS
   s" bench/llm/driver-fixture-helpers.f" PROC-ARGV+
   s" bench/llm/drive-process-lib.f" PROC-ARGV+
   s" bench/llm/drive-process.f" PROC-ARGV+
   s" --" PROC-ARGV+
   RB-STDLIB-TASK-ARGS ;

: RB-PROPERTY-ARGS ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   PROC-ARGV-ENV-RESET
   model modelu trial RB-ADD-COMMON-ENV
   RB-STDLIB-LOADS
   s" bench/llm/driver-token-helpers.f" PROC-ARGV+
   s" bench/llm/driver-fixture-helpers.f" PROC-ARGV+
   s" bench/llm/drive-property-lib.f" PROC-ARGV+
   s" bench/llm/drive-property.f" PROC-ARGV+
   s" --" PROC-ARGV+
   RB-STDLIB-TASK-ARGS ;

: RB-BUILD-ARGS ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   PROC-ARGV-ENV-RESET
   model modelu trial RB-ADD-COMMON-ENV
   RB-STDLIB-LOADS
   s" bench/llm/driver-token-helpers.f" PROC-ARGV+
   s" bench/llm/driver-fixture-helpers.f" PROC-ARGV+
   s" bench/llm/drive-build-lib.f" PROC-ARGV+
   s" bench/llm/drive-build.f" PROC-ARGV+
   s" --" PROC-ARGV+
   RB-STDLIB-TASK-ARGS ;

: RB-AOT-ARGS ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   PROC-ARGV-ENV-RESET
   model modelu trial RB-ADD-COMMON-ENV
   RB-STDLIB-LOADS
   s" bench/llm/driver-token-helpers.f" PROC-ARGV+
   s" bench/llm/drive-aot-lib.f" PROC-ARGV+
   s" bench/llm/drive-aot.f" PROC-ARGV+
   s" --" PROC-ARGV+
   RB-STDLIB-TASK-ARGS ;

: RB-RUN-FORTH-ONE ( ptr u8 n ptr u8 n n -- ) {: model:ptr modelu mode:ptr modeu trial :}
   mode modeu RB-ARM-FOR-MODE$ {: arm:ptr armu :}
   BM-T-ID RB-TASK-FIELD$ model modelu arm armu trial RB-ROW-DONE? RB-RESUME @ 0 <> and if exit then
   model modelu mode modeu trial RB-FORTH-ARGS
   RB-RUN-APPEND drop
   BM-T-ID RB-TASK-FIELD$ model modelu arm armu trial RB-ROW-DONE? 0= if
      s" run-expanded-bench: missing forth result row" RB-DIE
   then ;

: RB-RUN-FORTH-MODES ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   0 RB-MODE-NEXT !
   begin
      RB-FORTH-MODES$ RB-SPACE RB-MODE-NEXT @ SPLIT-NEXT
   while
      RB-MODE-NEXT !
      TRIM dup 0 > if
         RB-MODE!
         model modelu RB-MODE$ trial RB-RUN-FORTH-ONE
      else
         2drop
      then
   repeat
   drop 2drop ;

: RB-RUN-ARRAY-ONE ( ptr u8 n ptr u8 n n -- ) {: model:ptr modelu arm:ptr armu trial :}
   BM-T-ID RB-TASK-FIELD$ model modelu arm armu trial RB-ROW-DONE? RB-RESUME @ 0 <> and if exit then
   model modelu arm armu trial RB-ARRAY-ARGS
   RB-RUN-APPEND drop
   BM-T-ID RB-TASK-FIELD$ model modelu arm armu trial RB-ROW-DONE? 0= if
      s" run-expanded-bench: missing array result row" RB-DIE
   then ;

: RB-RUN-STDLIB-STACK-ONE ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   BM-T-ID RB-TASK-FIELD$ model modelu s" habu-stdlib" trial RB-ROW-DONE? RB-RESUME @ 0 <> and if exit then
   model modelu trial RB-STDLIB-ARGS
   RB-RUN-HB-APPEND drop
   BM-T-ID RB-TASK-FIELD$ model modelu s" habu-stdlib" trial RB-ROW-DONE? 0= if
      s" run-expanded-bench: missing stdlib stack result row" RB-DIE
   then ;

: RB-RUN-STDLIB-FILE-ONE ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   BM-T-ID RB-TASK-FIELD$ model modelu s" habu-stdlib-file" trial RB-ROW-DONE? RB-RESUME @ 0 <> and if exit then
   model modelu trial RB-FILE-ARGS
   RB-RUN-HB-APPEND drop
   BM-T-ID RB-TASK-FIELD$ model modelu s" habu-stdlib-file" trial RB-ROW-DONE? 0= if
      s" run-expanded-bench: missing stdlib file result row" RB-DIE
   then ;

: RB-RUN-STDLIB-PROCESS-ONE ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   BM-T-ID RB-TASK-FIELD$ model modelu s" habu-stdlib-process" trial RB-ROW-DONE? RB-RESUME @ 0 <> and if exit then
   model modelu trial RB-PROCESS-ARGS
   RB-RUN-HB-APPEND drop
   BM-T-ID RB-TASK-FIELD$ model modelu s" habu-stdlib-process" trial RB-ROW-DONE? 0= if
      s" run-expanded-bench: missing stdlib process result row" RB-DIE
   then ;

: RB-RUN-STDLIB-PROPERTY-ONE ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   BM-T-ID RB-TASK-FIELD$ model modelu s" habu-stdlib-property" trial RB-ROW-DONE? RB-RESUME @ 0 <> and if exit then
   model modelu trial RB-PROPERTY-ARGS
   RB-RUN-HB-APPEND drop
   BM-T-ID RB-TASK-FIELD$ model modelu s" habu-stdlib-property" trial RB-ROW-DONE? 0= if
      s" run-expanded-bench: missing stdlib property result row" RB-DIE
   then ;

: RB-RUN-STDLIB-BUILD-ONE ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   BM-T-ID RB-TASK-FIELD$ model modelu s" habu-stdlib-build" trial RB-ROW-DONE? RB-RESUME @ 0 <> and if exit then
   model modelu trial RB-BUILD-ARGS
   RB-RUN-HB-APPEND drop
   BM-T-ID RB-TASK-FIELD$ model modelu s" habu-stdlib-build" trial RB-ROW-DONE? 0= if
      s" run-expanded-bench: missing stdlib build result row" RB-DIE
   then ;

: RB-RUN-AOT-ONE ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   BM-T-ID RB-TASK-FIELD$ model modelu s" habu-aot" trial RB-ROW-DONE? RB-RESUME @ 0 <> and if exit then
   model modelu trial RB-AOT-ARGS
   RB-RUN-HB-APPEND drop
   BM-T-ID RB-TASK-FIELD$ model modelu s" habu-aot" trial RB-ROW-DONE? 0= if
      s" run-expanded-bench: missing AOT result row" RB-DIE
   then ;

: RB-RUN-ARRAY-ARMS ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   0 RB-MODE-NEXT !
   begin
      RB-ARRAY-ARMS$ RB-SPACE RB-MODE-NEXT @ SPLIT-NEXT
   while
      RB-MODE-NEXT !
      TRIM dup 0 > if
         RB-MODE!
         model modelu RB-MODE$ trial RB-RUN-ARRAY-ONE
      else
         2drop
      then
   repeat
   drop 2drop ;

: RB-RUN-TRIAL ( ptr u8 n n -- ) {: model:ptr modelu trial :}
   RB-TASK-STDLIB-STACK? if model modelu trial RB-RUN-STDLIB-STACK-ONE exit then
   RB-TASK-STDLIB-FILE? if model modelu trial RB-RUN-STDLIB-FILE-ONE exit then
   RB-TASK-STDLIB-FILE-NEGATIVE? if model modelu trial RB-RUN-STDLIB-FILE-ONE exit then
   RB-TASK-STDLIB-PROCESS? if model modelu trial RB-RUN-STDLIB-PROCESS-ONE exit then
   RB-TASK-STDLIB-PROCESS-NEGATIVE? if model modelu trial RB-RUN-STDLIB-PROCESS-ONE exit then
   RB-TASK-STDLIB-PROPERTY? if model modelu trial RB-RUN-STDLIB-PROPERTY-ONE exit then
   RB-TASK-STDLIB-PROPERTY-NEGATIVE? if model modelu trial RB-RUN-STDLIB-PROPERTY-ONE exit then
   RB-TASK-STDLIB-BUILD? if model modelu trial RB-RUN-STDLIB-BUILD-ONE exit then
   RB-TASK-STDLIB-BUILD-NEGATIVE? if model modelu trial RB-RUN-STDLIB-BUILD-ONE exit then
   RB-TASK-AOT? if model modelu trial RB-RUN-AOT-ONE exit then
   RB-TASK-AOT-NEGATIVE? if model modelu trial RB-RUN-AOT-ONE exit then
   RB-TASK-FORTH? if model modelu trial RB-RUN-FORTH-MODES exit then
   RB-TASK-ARRAY? if model modelu trial RB-RUN-ARRAY-ARMS exit then
   ;

: RB-FOR-MODELS-TRIALS ( -- )
   0 RB-MODEL-NEXT !
   RB-READ-MODEL-LINE 0= if s" run-expanded-bench: empty model registry" RB-DIE then
   RB-MODEL-LINE$ BM-REQUIRE-MODEL-HEADER
   begin RB-READ-MODEL-LINE while
      RB-MODEL-LINE$ BM-BLANK-OR-COMMENT? 0= if
         RB-MODEL-SELECTED? if
            1 begin dup RB-K @ <= while
               dup RB-TRIAL !
               RB-MODEL-ID$ RB-TRIAL @ RB-RUN-TRIAL
               1+
            repeat drop
         then
      then
   repeat ;

: RB-RUN-TASK ( -- )
   RB-TASK-RUNNABLE? 0= if exit then
   RB-TASK-ORDER @ 1+ RB-TASK-ORDER !
   RB-TASK-SELECTED? 0= if exit then
   RB-LIMIT-REACHED? if exit then
   RB-SELECTED @ 1+ RB-SELECTED !
   RB-FOR-MODELS-TRIALS ;

: RB-RUN-TASKS ( -- )
   0 RB-TASK-NEXT !
   RB-READ-TASK-LINE 0= if s" run-expanded-bench: empty task manifest" RB-DIE then
   RB-TASK-LINE$ BM-REQUIRE-TASK-HEADER
   0 RB-TASK-ORDER !
   0 RB-SELECTED !
   begin RB-READ-TASK-LINE while
      RB-TASK-LINE$ BM-BLANK-OR-COMMENT? 0= if RB-RUN-TASK then
   repeat ;

: RB-RUN-REPORT ( -- )
   RB-RESULTS-RESET
   PROC-ARGV-ENV-RESET
   RB-PROC-PATH
   s" --load" PROC-ARGV+
   s" lib/errors.f" PROC-ARGV+
   s" lib/string.f" PROC-ARGV+
   s" lib/fs.f" PROC-ARGV+
   s" lib/process.f" PROC-ARGV+
   s" lib/process-argv.f" PROC-ARGV+
   s" lib/process-env.f" PROC-ARGV+
   s" lib/time.f" PROC-ARGV+
   s" lib/date.f" PROC-ARGV+
   s" lib/argv.f" PROC-ARGV+
   s" tools/json.f" PROC-ARGV+
   s" bench/llm/expanded-report.f" PROC-ARGV+
   s" --" PROC-ARGV+
   RB-OUT$ PROC-ARGV+
   RB-PERF? if RB-PERF$ PROC-ARGV+ then
   RB-RUN-TO-RESULTS drop ;

: RB-CONFIG ( -- )
   s" bench/llm/run-expanded-bench.f [k_trials] [out.jsonl]" ARGV-USAGE!
   ARGV-PARSE
   ARGV-POS# 2 > if RB-USAGE then
   ARGV-POS# 0 > if 0 ARGV-POS$ RB-PARSE-U else 5 then RB-K !
   ARGV-POS# 1 > if 1 ARGV-POS$ else s" BENCH_OUT" s" bench/llm/results/run-expanded.jsonl" RB-ENV-OR then RB-OUT!
   s" BENCH_TASKS" s" bench/llm/tasks.tsv" RB-ENV-OR RB-TASKS!
   s" BENCH_RESULTS" s" bench/llm/RESULTS-expanded.md" RB-ENV-OR RB-RESULTS!
   s" BENCH_PERF_JSON" GETENV RB-PERF!
   s" BENCH_SEED" s" manifest" RB-ENV-OR RB-SEED!
   s" MODEL_REGISTRY" s" bench/llm/models.tsv" RB-ENV-OR RB-MODEL-REG!
   s" MODEL_ID" GETENV RB-MODEL-ONLY!
   s" BENCH_TASK_IDS" GETENV RB-TASK-IDS!
   s" BENCH_TASK_LIMIT" -1 RB-ENV-U-OR RB-LIMIT !
   s" BENCH_MAX_REPAIRS" 5 RB-ENV-U-OR RB-MAX-REPAIRS !
   s" BENCH_RESUME" RB-ENV-FLAG? RB-RESUME !
   s" BENCH_FORTH_ARM" GETENV RB-FORTH-ARM!
   s" BENCH_ARRAY_ARMS" GETENV dup 0 > if
      RB-ARRAY-ARMS!
   else
      2drop
      s" habu-a habu-lib habu-stdlib habu-skeleton js ts rust" RB-ARRAY-ARMS!
   then
   s" BENCH_FORTH_MODES" GETENV dup 0 > if
      RB-FORTH-MODES!
   else
      2drop
      RB-FORTH-ARM$ nip 0 > if s" repair" else s" repair raw blind" then RB-FORTH-MODES!
   then ;

: RB-MAIN ( -- )
   RB-CONFIG
   RB-LOAD-FILES
   RB-RESUME @ 0= if RB-OUT-RESET then
   RB-RUN-TASKS
   RB-RUN-REPORT ;

RB-MAIN
