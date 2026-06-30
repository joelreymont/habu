\ run-lib.f - resident native test suite implementation.
\
\ Load after test/run-support.f.

require test/run-support.f
require test/run-files.f

64 constant TR-USAGE-RC
65 constant TR-BUDGET-RC
66 constant TR-PROFILE-RC
70000 constant TR-DEFAULT-BUDGET-MS
4 constant TR-DEFAULT-NESTED-POOL-SLOTS
12 constant TR-TOP-POOL-MAX
600000 constant TR-TIMEOUT-MS
41 constant TR-PHASES
32 constant TR-NUM-CAP
$100 constant TR-HOST-CAP
$82 constant TR-UNDER-STAMP-U
$2 constant TR-CHECK-WARM-PHASES
$1E constant TR-EARLY-HOST-PHASES
$3 constant TR-LATE-PHASES
9 constant TR-UNDER-PREFIX-U
12 constant TR-TOOLS-WARM-SLOT
13 constant TR-CHECK-WARM-SLOT
14 constant TR-AOT-RUNNER-SLOT
0 constant TR-GROUP-SEQ
1 constant TR-GROUP-PAR
1 constant TR-PROFILE-MACOS-ARM64-12X2
2 constant TR-PROFILE-JETSON-ORIN-CLOCKS-4X2
3 constant TR-PROFILE-LINUX-ARM64-4X2

\ Longest post-warm phases first; this keeps ARM gates inside budget without
\ dropping coverage or raising the threshold.
create TR-CHECK-WARM-ORDER
$9 , $E ,

create TR-LATE-ORDER
$3 , $15 , $10 ,

create TR-EARLY-HOST-ORDER
$8 , $7 , $25 , $26 , $27 , $28 , $17 , $16 ,
$1B , $C , $11 , $24 , $1F , $23 , $B , $A ,
$1E , $20 , $22 ,
$5 , $2 , $1C , $1D , $1A , $21 , $D , $19 ,
$12 , $4 , $13 ,

create TR-WARM-BUF FS-PATH-CAP allot
create TR-TOOLS-BUF FS-PATH-CAP allot
create TR-TOOLS-TRUST-BUF FS-PATH-CAP allot
create TR-CHECK-BUF FS-PATH-CAP allot
create TR-CHECK-TRUST-BUF FS-PATH-CAP allot
create TR-BUILD-CACHE-BUF FS-PATH-CAP allot
create TR-PATH-BUF FS-PATH-CAP allot
create TR-UNDER-BUF FS-PATH-CAP allot
create TR-AOT-RUNNER-BUF FS-PATH-CAP allot
create TR-AOT-RUNNER-TRUST-BUF FS-PATH-CAP allot
create TR-AOT-RUNNER-STAMP-BUF FS-PATH-CAP allot
create TR-UNDER-HEX 64 allot
create TR-UNDER-KEY-HEX 80 allot
create TR-UNDER-ARG-BUF FS-PATH-CAP allot
create TR-UNDER-CACHE-BUF FS-PATH-CAP allot
create TR-UNDER-CACHE-TMP-BUF FS-PATH-CAP allot
create TR-UNDER-CACHE-LOCK-BUF FS-PATH-CAP allot
create TR-UNDER-CACHE-STAMP-BUF FS-PATH-CAP allot
create TR-UNDER-CACHE-STAMP-TMP-BUF FS-PATH-CAP allot
create TR-UNDER-NAME-BUF 80 allot
create TR-UNDER-STAMP-BUF TR-UNDER-STAMP-U allot
create TR-UNDER-STAMP-RD TR-UNDER-STAMP-U allot
create TR-AOT-RUNNER-KEY-HEX 80 allot
create TR-AOT-RUNNER-STAMP-RD 80 allot
create TR-PERSIST-BUF FS-PATH-CAP allot
create TR-NUM-BUF TR-NUM-CAP allot
create TR-HOST-BUF TR-HOST-CAP allot

variable TR-WARM-U
variable TR-TOOLS-U
variable TR-TOOLS-TRUST-U
variable TR-CHECK-U
variable TR-CHECK-TRUST-U
variable TR-BUILD-CACHE-U
variable TR-PATH-U
variable TR-UNDER-U
variable TR-AOT-RUNNER-U
variable TR-AOT-RUNNER-TRUST-U
variable TR-AOT-RUNNER-STAMP-U
variable TR-UNDER-ARG-U
variable TR-UNDER-CACHE-U
variable TR-UNDER-CACHE-TMP-U
variable TR-UNDER-CACHE-LOCK-U
variable TR-UNDER-CACHE-STAMP-U
variable TR-UNDER-CACHE-STAMP-TMP-U
variable TR-UNDER-NAME-U
variable TR-PERSIST-U
variable TR-GATE-START-NS
variable TR-TOOLS-WARM-READY
variable TR-CHECK-WARM-READY
variable TR-UNDER-READY
variable TR-AOT-RUNNER-READY
variable TR-UNDER-CACHE-HIT
variable TR-UNDER-CACHE-RC
variable TR-ARG-I
variable TR-BUDGET
variable TR-WALL-BUDGET
variable TR-BUDGET-USER
variable TR-WALL-BUDGET-USER
variable TR-NESTED-POOL
variable TR-TIMINGS
variable TR-COLD-CACHE
variable TR-PROFILE-ID
variable TR-NUM-U
variable TR-RESIDENT-ID
variable TR-PRE-TAIL
variable TR-PRE-AOT-POS
variable TR-PRE-AOT-NEG
variable TR-PRE-ARTIFACTS
variable TR-PRE-RUNTIME
variable TR-PRE-VALIDATE

: TR-WARM$ ( -- ptr u8 n )
   TR-WARM-BUF TR-WARM-U @ ;

: TR-PATH$ ( -- ptr u8 n )
   TR-PATH-BUF TR-PATH-U @ ;

: TR-TOOLS$ ( -- ptr u8 n )
   TR-TOOLS-BUF TR-TOOLS-U @ ;

: TR-TOOLS-TRUST$ ( -- ptr u8 n )
   TR-TOOLS-TRUST-BUF TR-TOOLS-TRUST-U @ ;

: TR-BUILD-CACHE$ ( -- ptr u8 n )
   TR-BUILD-CACHE-BUF TR-BUILD-CACHE-U @ ;

: TR-UNDER$ ( -- ptr u8 n )
   TR-UNDER-BUF TR-UNDER-U @ ;

: TR-UNDER-ARG$ ( -- ptr u8 n )
   TR-UNDER-ARG-BUF TR-UNDER-ARG-U @ ;

: TR-AOT-RUNNER$ ( -- ptr u8 n )
   TR-AOT-RUNNER-BUF TR-AOT-RUNNER-U @ ;

: TR-AOT-RUNNER-TRUST$ ( -- ptr u8 n )
   TR-AOT-RUNNER-TRUST-BUF TR-AOT-RUNNER-TRUST-U @ ;

: TR-AOT-RUNNER-STAMP$ ( -- ptr u8 n )
   TR-AOT-RUNNER-STAMP-BUF TR-AOT-RUNNER-STAMP-U @ ;

: TR-UNDER-CACHE$ ( -- ptr u8 n )
   TR-UNDER-CACHE-BUF TR-UNDER-CACHE-U @ ;

: TR-UNDER-CACHE-TMP$ ( -- ptr u8 n )
   TR-UNDER-CACHE-TMP-BUF TR-UNDER-CACHE-TMP-U @ ;

: TR-UNDER-CACHE-LOCK$ ( -- ptr u8 n )
   TR-UNDER-CACHE-LOCK-BUF TR-UNDER-CACHE-LOCK-U @ ;

: TR-UNDER-CACHE-STAMP$ ( -- ptr u8 n )
   TR-UNDER-CACHE-STAMP-BUF TR-UNDER-CACHE-STAMP-U @ ;

: TR-UNDER-CACHE-STAMP-TMP$ ( -- ptr u8 n )
   TR-UNDER-CACHE-STAMP-TMP-BUF TR-UNDER-CACHE-STAMP-TMP-U @ ;

: TR-UNDER-NAME$ ( -- ptr u8 n )
   TR-UNDER-NAME-BUF TR-UNDER-NAME-U @ ;

: TR-USAGE ( -- )
   s" usage: bin/hb --load libs test/run.f -- [--under PATH] [--perf-profile NAME|auto] [--pool-slots N] [--nested-pool-slots N] [--budget-ms N] [--wall-budget-ms N] [--cold-cache] [--timings]" TR-USAGE-RC die ;

: TR-ARG$ ( -- ptr u8 n )
   TR-ARG-I @ SCRIPT-ARGV$ ;

: TR-ARG-VALUE$ ( -- ptr u8 n )
   TR-ARG-I @ 1+ SCRIPT-ARGC >= if TR-USAGE then
   TR-ARG-I @ 1+ SCRIPT-ARGV$ ;

: TR-POS-NUM ( ptr u8 n -- n )
   STR>NUMBER? 0= if drop TR-USAGE then
   dup 1 < if drop TR-USAGE then ;

: TR-POOL-SLOTS-FAIL ( -- )
   s" --pool-slots must be between 1 and 12" TR-USAGE-RC die ;

: TR-TOP-POOL-CHECK ( n -- n ) {: n:n :}
   n 1 < if TR-POOL-SLOTS-FAIL then
   n TR-TOP-POOL-MAX > if TR-POOL-SLOTS-FAIL then
   n ;

: TR-TOP-POOL-SLOTS! ( n -- )
   TR-TOP-POOL-CHECK GT-POOL-SLOTS! ;

: TR-ADVANCE ( n -- )
   TR-ARG-I @ + TR-ARG-I ! ;

: TR-POOL-OPT ( -- )
   TR-ARG-VALUE$ TR-POS-NUM TR-TOP-POOL-SLOTS!
   2 TR-ADVANCE ;

: TR-NESTED-POOL-OPT ( -- )
   TR-ARG-VALUE$ TR-POS-NUM GT-POOL-CHECK-LIMIT TR-NESTED-POOL !
   2 TR-ADVANCE ;

: TR-BUDGET-OPT ( -- )
   TR-ARG-VALUE$ TR-POS-NUM TR-BUDGET !
   -1 TR-BUDGET-USER !
   2 TR-ADVANCE ;

: TR-WALL-BUDGET-OPT ( -- )
   TR-ARG-VALUE$ TR-POS-NUM TR-WALL-BUDGET !
   -1 TR-WALL-BUDGET-USER !
   2 TR-ADVANCE ;

: TR-TIMINGS-OPT ( -- )
   -1 TR-TIMINGS !
   1 TR-ADVANCE ;

: TR-COLD-CACHE-OPT ( -- )
   -1 TR-COLD-CACHE !
   1 TR-ADVANCE ;

: TR-PROFILE-FAIL ( ptr u8 n -- ) {: msg:ptr msgu:n :}
   msg msgu TR-PROFILE-RC die ;

: TR-HOST-READ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   a u EXISTS? 0= if s" missing host profile file" TR-PROFILE-FAIL then
   a u TR-HOST-BUF TR-HOST-CAP READ-ALL
   TR-HOST-BUF swap ;

: TR-JETSON-MODEL? ( -- bool )
   s" /proc/device-tree/model" TR-HOST-READ s" NVIDIA Jetson" CONTAINS? ;

: TR-JETSON-ONLINE? ( -- bool )
   s" /sys/devices/system/cpu/online" TR-HOST-READ TRIM s" 0-7" STR= ;

: TR-DETECT-PROFILE ( -- n )
   HB-TARGET-MACOS? if TR-PROFILE-MACOS-ARM64-12X2 exit then
   HB-TARGET-LINUX? if
      s" /proc/device-tree/model" EXISTS? if
         TR-JETSON-MODEL? if TR-PROFILE-JETSON-ORIN-CLOCKS-4X2 exit then
      then
      TR-PROFILE-LINUX-ARM64-4X2 exit
   then
   s" no supported timed host profile" TR-PROFILE-FAIL ;

: TR-PROFILE-ID? ( ptr u8 n -- n )
   2dup s" auto" STR= if 2drop TR-DETECT-PROFILE exit then
   2dup s" macos-arm64-12x2" STR= if 2drop TR-PROFILE-MACOS-ARM64-12X2 exit then
   2dup s" jetson-orin-clocks-4x2" STR= if 2drop TR-PROFILE-JETSON-ORIN-CLOCKS-4X2 exit then
   2dup s" linux-arm64-4x2" STR= if 2drop TR-PROFILE-LINUX-ARM64-4X2 exit then
   2drop TR-USAGE ;

: TR-PROFILE-APPLY ( n -- ) {: id:n :}
   id TR-PROFILE-ID !
   0 TR-BUDGET-USER !
   0 TR-WALL-BUDGET-USER !
   id case
      TR-PROFILE-MACOS-ARM64-12X2 of
         12 TR-TOP-POOL-SLOTS!
         2 TR-NESTED-POOL !
         40000 TR-BUDGET !
         45000 TR-WALL-BUDGET !
      endof
      TR-PROFILE-JETSON-ORIN-CLOCKS-4X2 of
         4 GT-POOL-SLOTS!
         2 TR-NESTED-POOL !
         100000 TR-BUDGET !
         110000 TR-WALL-BUDGET !
      endof
      TR-PROFILE-LINUX-ARM64-4X2 of
         4 GT-POOL-SLOTS!
         2 TR-NESTED-POOL !
         120000 TR-BUDGET !
         0 TR-WALL-BUDGET !
      endof
   endcase ;

: TR-ARGS-DEFAULTS ( -- )
   TR-DEFAULT-BUDGET-MS TR-BUDGET !
   0 TR-WALL-BUDGET !
   0 TR-BUDGET-USER !
   0 TR-WALL-BUDGET-USER !
   TR-DEFAULT-NESTED-POOL-SLOTS TR-NESTED-POOL !
   0 TR-TIMINGS !
   0 TR-COLD-CACHE !
   0 TR-UNDER-ARG-U !
   TR-DETECT-PROFILE TR-PROFILE-APPLY ;

: TR-COLD-BUDGET-MS ( -- n )
   TR-PROFILE-ID @ case
      TR-PROFILE-MACOS-ARM64-12X2 of 70000 endof
      TR-PROFILE-JETSON-ORIN-CLOCKS-4X2 of 150000 endof
      TR-PROFILE-LINUX-ARM64-4X2 of 150000 endof
      TR-BUDGET @ swap
   endcase ;

: TR-COLD-WALL-BUDGET-MS ( -- n )
   TR-PROFILE-ID @ case
      TR-PROFILE-MACOS-ARM64-12X2 of 70000 endof
      TR-PROFILE-JETSON-ORIN-CLOCKS-4X2 of 160000 endof
      TR-PROFILE-LINUX-ARM64-4X2 of 0 endof
      TR-WALL-BUDGET @ swap
   endcase ;

: TR-COLD-BUDGETS ( -- )
   TR-COLD-CACHE @ 0 = if exit then
   TR-BUDGET-USER @ 0= if TR-COLD-BUDGET-MS TR-BUDGET ! then
   TR-WALL-BUDGET-USER @ 0= if TR-COLD-WALL-BUDGET-MS TR-WALL-BUDGET ! then ;

: TR-MARK-COLD ( -- )
   TR-COLD-CACHE @ 0 <> if exit then
   -1 TR-COLD-CACHE !
   TR-COLD-BUDGETS ;

: TR-PERF-PROFILE-OPT ( -- )
   TR-ARG-VALUE$ TR-PROFILE-ID? TR-PROFILE-APPLY
   2 TR-ADVANCE ;

: TR-UNDER-ARG! ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0 < if E-FS-PATH throw then
   u FS-PATH-CAP > if E-FS-PATH throw then
   a TR-UNDER-ARG-BUF u BYTE-COPY
   u TR-UNDER-ARG-U ! ;

: TR-UNDER-OPT ( -- )
   TR-ARG-VALUE$ TR-UNDER-ARG!
   2 TR-ADVANCE ;

: TR-PARSE-ARG ( -- )
   TR-ARG$ s" full" STR= if
      s" test/run.f full retired; the native gate is test/run.f" TR-USAGE-RC die
   then
   TR-ARG$ s" --under" STR= if TR-UNDER-OPT exit then
   TR-ARG$ s" --pool-slots" STR= if TR-POOL-OPT exit then
   TR-ARG$ s" --nested-pool-slots" STR= if TR-NESTED-POOL-OPT exit then
   TR-ARG$ s" --budget-ms" STR= if TR-BUDGET-OPT exit then
   TR-ARG$ s" --wall-budget-ms" STR= if TR-WALL-BUDGET-OPT exit then
   TR-ARG$ s" --perf-profile" STR= if TR-PERF-PROFILE-OPT exit then
   TR-ARG$ s" --cold-cache" STR= if TR-COLD-CACHE-OPT exit then
   TR-ARG$ s" --timings" STR= if TR-TIMINGS-OPT exit then
   TR-USAGE ;

: TR-CHECK-ARGS ( -- )
   TR-ARGS-DEFAULTS
   0 TR-ARG-I !
   begin TR-ARG-I @ SCRIPT-ARGC < while
      TR-PARSE-ARG
   repeat
   TR-COLD-BUDGETS ;

: TR-TRUE ( -- bool )
   0 0= ;

: TR-FALSE ( -- bool )
   TR-TRUE 0= ;

: TR-GATE-START! ( -- )
   mono-ns TR-GATE-START-NS ! ;

: TR-GATE-ELAPSED-MS ( -- n )
   mono-ns TR-GATE-START-NS @ - PROC-NS-PER-MS / ;

: TR-BUDGET-MS ( -- n )
   TR-BUDGET @ ;

: TR-WALL-BUDGET-MS ( -- n )
   TR-WALL-BUDGET @ ;

: TR-WALL-BUDGET? ( -- bool )
   TR-WALL-BUDGET-MS 0 > ;

: TR-PROFILE$ ( -- ptr u8 n )
   TR-PROFILE-ID @ case
      TR-PROFILE-MACOS-ARM64-12X2 of s" macos-arm64-12x2" endof
      TR-PROFILE-JETSON-ORIN-CLOCKS-4X2 of s" jetson-orin-clocks-4x2" endof
      TR-PROFILE-LINUX-ARM64-4X2 of s" linux-arm64-4x2" endof
      s" unknown" rot
   endcase ;

: TR-CACHE-MODE$ ( -- ptr u8 n )
   TR-COLD-CACHE @ 0 <> if s" cold" exit then
   s" warm" ;

: TR-CHECK-MACOS-PROFILE ( -- )
   HB-TARGET-MACOS? 0= if s" macos-arm64-12x2 requires macOS target" TR-PROFILE-FAIL then ;

: TR-CHECK-JETSON-PROFILE ( -- )
   HB-TARGET-LINUX? 0= if s" jetson-orin-clocks-4x2 requires Linux target" TR-PROFILE-FAIL then
   TR-JETSON-MODEL? 0= if s" jetson-orin-clocks-4x2 requires NVIDIA Jetson model" TR-PROFILE-FAIL then
   TR-JETSON-ONLINE? 0= if s" jetson-orin-clocks-4x2 requires CPUs 0-7 online" TR-PROFILE-FAIL then ;

: TR-CHECK-LINUX-PROFILE ( -- )
   HB-TARGET-LINUX? 0= if s" linux-arm64-4x2 requires Linux target" TR-PROFILE-FAIL then ;

: TR-CHECK-PROFILE ( -- )
   TR-PROFILE-ID @ case
      TR-PROFILE-MACOS-ARM64-12X2 of TR-CHECK-MACOS-PROFILE endof
      TR-PROFILE-JETSON-ORIN-CLOCKS-4X2 of TR-CHECK-JETSON-PROFILE endof
      TR-PROFILE-LINUX-ARM64-4X2 of TR-CHECK-LINUX-PROFILE endof
      drop s" unknown perf profile" TR-PROFILE-FAIL
   endcase ;

: TR-PERSIST-TMP ( -- )
   s" TMPDIR" GETENV dup 0= if 2drop s" /tmp" then
   s" habu-gate-cache" TR-PERSIST-BUF JOIN-PATH TR-PERSIST-U ! ;

: TR-PERSIST-HOME ( ptr u8 n -- ) {: home:ptr homeu:n :}
   home homeu s" .cache/habu-gate" TR-PERSIST-BUF JOIN-PATH TR-PERSIST-U ! ;

: TR-PERSIST-XDG ( ptr u8 n -- ) {: root:ptr rootu:n :}
   root rootu s" habu-gate" TR-PERSIST-BUF JOIN-PATH TR-PERSIST-U ! ;

: TR-PERSIST-DEFAULT ( -- )
   s" XDG_CACHE_HOME" GETENV dup 0= if
      2drop
      s" HOME" GETENV dup 0= if 2drop TR-PERSIST-TMP exit then
      TR-PERSIST-HOME exit
   then
   TR-PERSIST-XDG ;

: TR-PERSIST-COLD ( -- )
   GT-ROOT s" cold-cache" TR-PERSIST-BUF JOIN-PATH TR-PERSIST-U ! ;

: TR-PERSIST-INIT ( -- )
   TR-COLD-CACHE @ 0 <> if TR-PERSIST-COLD exit then
   TR-PERSIST-DEFAULT ;

: TR-PERSIST? ( -- bool )
   TR-PERSIST-U @ 0 > ;

: TR-PERSIST$ ( -- ptr u8 n )
   TR-PERSIST? 0= if E-FS-PATH throw then
   TR-PERSIST-BUF TR-PERSIST-U @ ;

: TR-PERSIST-ENSURE ( -- )
   TR-PERSIST$ MAKE-DIRS ;

: TR-PERSIST-ENV+ ( -- )
   s" HABU_GATE_WARM_ROOT" >LEN TR-PERSIST$ >LEN PROC-ENV+ ;

: TR-BUDGET-FAIL ( n n -- ) {: elapsed:n budget:n :}
   s" FAIL: native test suite budget (" type
   elapsed GT-U-TYPE
   s" ms > " type
   budget GT-U-TYPE
   s" ms)" type cr
   s" native test suite budget exceeded" TR-BUDGET-RC die ;

: TR-WALL-BUDGET-FAIL ( n n -- ) {: elapsed:n budget:n :}
   s" FAIL: native test suite wall budget (" type
   elapsed GT-U-TYPE
   s" ms > " type
   budget GT-U-TYPE
   s" ms)" type cr
   s" native test suite wall budget exceeded" TR-BUDGET-RC die ;

: TR-PASS ( n n -- ) {: elapsed:n budget:n :}
   s" PASS: native test suite (fixpoint + engine suite + checked hb + repl + hb-build) (" type
   elapsed GT-U-TYPE
   s" ms <= " type
   budget GT-U-TYPE
   s" ms budget)" type cr ;

: TR-PERF-LINE ( -- )
   s" perf-profile: " type TR-PROFILE$ type
   s"  cache=" type TR-CACHE-MODE$ type
   s"  pool=" type GT-POOL-LIMIT @ GT-U-TYPE
   s"  nested=" type TR-NESTED-POOL @ GT-U-TYPE
   TR-WALL-BUDGET? if
      s"  wall-budget-ms=" type TR-WALL-BUDGET-MS GT-U-TYPE
   then
   cr ;

: TR-FINISH ( -- )
   TR-GATE-ELAPSED-MS {: elapsed:n :}
   TR-BUDGET-MS {: budget:n :}
   TR-PERF-LINE
   elapsed budget > if elapsed budget TR-BUDGET-FAIL then
   TR-WALL-BUDGET? if
      elapsed TR-WALL-BUDGET-MS > if elapsed TR-WALL-BUDGET-MS TR-WALL-BUDGET-FAIL then
   then
   elapsed budget TR-PASS ;

: TR-BUILD-CACHE-PATHS ( -- )
   TR-PERSIST$ s" hb-build-cache" TR-BUILD-CACHE-BUF JOIN-PATH TR-BUILD-CACHE-U !
   TR-BUILD-CACHE$ MAKE-DIRS ;

: TR-BUILD-CACHE-ENV ( -- )
   TR-BUILD-CACHE-PATHS
   s" HABU_BUILD_CACHE" >LEN TR-BUILD-CACHE$ >LEN PROC-ENV+ ;

: TR-DEFAULT+ ( ptr u8 n ptr u8 n -- ) {: name:ptr nameu:n val:ptr valu:n :}
   name nameu >LEN val valu >LEN PROC-ENV-DEFAULT+ ;

: TR-PERSIST-DEFAULT+ ( -- )
   s" HABU_GATE_WARM_ROOT" TR-PERSIST$ TR-DEFAULT+ ;

: TR-TMP-DEFAULT+ ( -- )
   s" HB_TMP" GT-ROOT TR-DEFAULT+ ;

: TR-BUILD-CACHE-DEFAULT+ ( -- )
   TR-BUILD-CACHE-PATHS
   s" HABU_BUILD_CACHE" TR-BUILD-CACHE$ TR-DEFAULT+ ;

: TR-STATS-DEFAULT+ ( -- )
   GS-ON? if s" HABU_GATE_STATS" GS-PATH$ TR-DEFAULT+ then ;

: TR-UNDER-PATHS ( -- )
   GT-ROOT s" hb-under-test" TR-UNDER-BUF JOIN-PATH TR-UNDER-U !
   TR-UNDER$ EXISTS? if TR-UNDER$ REMOVE-FILE then
   0 TR-UNDER-READY !
   0 TR-UNDER-CACHE-HIT !
   0 TR-AOT-RUNNER-READY ! ;

: TR-UNDER-ENV+ ( -- )
   s" HABU_UNDER_TEST" >LEN TR-UNDER$ >LEN PROC-ENV+ ;

: TR-POOL-PASS-SPAN ( ptr u8 n n -- ) {: label:ptr labelu:n ms:n :}
   label labelu ms GS-SPAN ;

: TR-INSTALL-POOL-HOOKS ( -- )
   [: TR-POOL-PASS-SPAN ;] is GT-POOL-PASS-HOOK ;

TR-INSTALL-POOL-HOOKS

: TR-START ( -- )
   GT-RESET
   CLEANUP-RESET
   s" HB_TMP" GETENV dup 0= if
      2drop
      s" hb-gate" TMPDIR-MKDIR GT-COPY-ROOT!
      GT-ROOT CLEANUP-TREE+
   else
      2dup MAKE-DIRS
      GT-COPY-ROOT!
   then
   TR-PERSIST-INIT
   TR-PERSIST-ENSURE
   TR-PERSIST$ CK-CACHE-ROOT!
   GT-ROOT GS-ROOT!
   TR-UNDER-PATHS ;

: TR-FAIL ( ptr u8 n -- ) {: label:ptr labelu:n :}
   s" FAIL: " type label labelu type cr
   GT-CLEANUP
   label labelu 1 die ;

: TR-UNDER-SHA! ( -- )
   TR-UNDER$ TR-UNDER-HEX SHA256-FILE-HEX 0 <> if
      s" failed to hash Habu-under-test" TR-FAIL
   then ;

: TR-UNDER-LINE ( -- )
   TR-UNDER-SHA!
   s" Habu-under-test: " type
   TR-UNDER$ type
   s"  sha256=" type
   TR-UNDER-HEX 64 type cr ;

: TR-EXPECT-UNDER ( -- )
   TR-UNDER$ EXECUTABLE? 0= if
      s" missing Habu-under-test: " type TR-UNDER$ type cr
      s" Habu-under-test not produced executable" TR-FAIL
   then
   -1 TR-UNDER-READY !
   s" candidate-ready" GS-EVENT
   TR-UNDER-LINE ;

: TR-UNDER-ARG? ( -- bool )
   TR-UNDER-ARG-U @ 0 > ;

: TR-UNDER-IMPORT ( -- )
   TR-UNDER-ARG? 0= if exit then
   TR-UNDER-ARG$ EXECUTABLE? 0= if s" --under executable missing" TR-FAIL then
   TR-UNDER-ARG$ TR-UNDER$ COPY-FILE-STREAM
   TR-UNDER$ CHMOD-X
   s" candidate-import" GS-EVENT
   -1 TR-UNDER-READY ! ;

: TR-BASE ( -- )
   PROC-ARGV-RESET
   PROC-ENV-RESET
   TR-PERSIST-ENSURE
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   TR-PERSIST-ENV+
   TR-BUILD-CACHE-ENV
   GS-ENV+
   PROC-ENV-INHERIT-MISSING
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/test/runner.f"  >LEN PROC-ARGV+ ;

: TR-SPAWN-CAPTURE ( -- )
   s" top-capture-spawn" GS-EVENT
   s" bin/hb" >LEN PROC-ARGV-CHECK-PATH
   PROC-CAPTURE-RESET
   TR-TIMEOUT-MS >MS PROC-CAPTURE-DEADLINE!
   PROC-SETUP-CAPTURE-FDS
   s" bin/hb" >LEN PROC-ARGV-PREPARE PROC-ENV-PREPARE PROC-SPAWN-ARGV-ENV-CAPTURE ;

: TR-PHASE-OK? ( -- bool )
   PROC-OUTCOME-KIND @ PROC-OUTCOME-EXIT =
   PROC-OUTCOME-CODE @ 0= and ;

: TR-RUN ( ptr u8 n -- ) {: label:ptr labelu:n :}
   label labelu GT-PROGRESS-RUN
   TR-SPAWN-CAPTURE
   label labelu GT-PROGRESS-CAPTURE-FLUSH
   PROC-CLOSE-CAPTURE-FDS
   TR-PHASE-OK? 0= if label labelu TR-FAIL then
   label labelu GT-PROGRESS-PASS ;

: TR-COMMON ( -- )
   s" test/gate-common.f"  >LEN PROC-ARGV+ ;

: TR-BUILD-ASSERT-LIBS ( -- )
   s" tools/json.f"  >LEN PROC-ARGV+
   s" tools/gate-json-assert-core.f"  >LEN PROC-ARGV+
   s" tools/aot-call-report-lib.f"  >LEN PROC-ARGV+ ;

: TR-CLEAN-WARM ( -- )
   GT-ROOT s" hb-check-warm" TR-WARM-BUF JOIN-PATH TR-WARM-U !
   TR-WARM$ FILE? if TR-WARM$ REMOVE-FILE then ;

: TR-SUFFIX! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n suf:ptr su:n dst:ptr lenp:ptr :}
   u su + FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   suf dst u + su BYTE-COPY
   u su + lenp ! ;

\ Tools-warm root: the content-keyed gate cache selected by TR-PERSIST-INIT.
\ Must match gate-stdlib.f SUITE-SET-ROOT so the baked image and HABU_WARM_TOOLS
\ resolve to the same place. Checker warm uses the same root through GE-WARM-ROOT
\ and validates with its own content stamp.
: TR-WARM-ROOT$ ( -- ptr u8 n )
   TR-PERSIST$ ;

: TR-TOOLS-PATHS ( -- )
   TR-WARM-ROOT$ s" hb-tools-warm" TR-TOOLS-BUF JOIN-PATH TR-TOOLS-U !
   TR-TOOLS$ s" .trust.f" TR-TOOLS-TRUST-BUF TR-TOOLS-TRUST-U TR-SUFFIX! ;

: TR-TOOLS-ENV ( -- )
   TR-TOOLS-PATHS
   s" HABU_WARM_TOOLS" >LEN TR-TOOLS$ >LEN PROC-ENV+
   s" HABU_WARM_TOOLS_TRUST" >LEN TR-TOOLS-TRUST$ >LEN PROC-ENV+ ;

: TR-CHECK$ ( -- ptr u8 n )
   TR-CHECK-BUF TR-CHECK-U @ ;

: TR-CHECK-TRUST$ ( -- ptr u8 n )
   TR-CHECK-TRUST-BUF TR-CHECK-TRUST-U @ ;

: TR-CHECK-PATHS ( -- )
   TR-WARM-ROOT$ s" hb-check-warm" TR-CHECK-BUF JOIN-PATH TR-CHECK-U !
   TR-CHECK$ s" .trust.f" TR-CHECK-TRUST-BUF TR-CHECK-TRUST-U TR-SUFFIX! ;

: TR-CHECK-ENV ( -- )
   TR-CHECK-PATHS
   s" HABU_WARM_CHECK" >LEN TR-CHECK$ >LEN PROC-ENV+
   s" HABU_WARM_CHECK_TRUST" >LEN TR-CHECK-TRUST$ >LEN PROC-ENV+ ;

: TR-TOOLS-DEFAULT+ ( -- )
   TR-TOOLS-PATHS
   TR-TOOLS$ EXECUTABLE? 0= if exit then
   TR-TOOLS-TRUST$ FILE? 0= if exit then
   s" HABU_WARM_TOOLS" TR-TOOLS$ TR-DEFAULT+
   s" HABU_WARM_TOOLS_TRUST" TR-TOOLS-TRUST$ TR-DEFAULT+
   TR-TOOLS$ TR-TOOLS-TRUST$ WR-TOOLS! ;

: TR-CHECK-DEFAULT+ ( -- )
   TR-CHECK-PATHS
   TR-CHECK$ EXECUTABLE? 0= if exit then
   TR-CHECK-TRUST$ FILE? 0= if exit then
   s" HABU_WARM_CHECK" TR-CHECK$ TR-DEFAULT+
   s" HABU_WARM_CHECK_TRUST" TR-CHECK-TRUST$ TR-DEFAULT+
   TR-CHECK$ TR-CHECK-TRUST$ WR-CHECK! ;

: TR-ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: TR-NUM$ ( n -- ptr u8 n )
   dup 0 < if E-TBL-FIELD throw then
   TR-NUM-CAP TR-NUM-U !
   dup 0= if
      drop
      TR-NUM-U @ 1- TR-NUM-U !
      STR-ZERO TR-NUM-BUF TR-NUM-U @ + c!
   else
      begin dup 0 > while
         TR-NUM-U @ 1- TR-NUM-U !
         dup 10 mod STR-ZERO + TR-NUM-BUF TR-NUM-U @ + c!
         10 /
      repeat drop
   then
   TR-NUM-BUF TR-NUM-U @ + TR-NUM-CAP TR-NUM-U @ - ;

: TR-NUM-ARG+ ( n -- )
   TR-NUM$ TR-ARG+ ;

: TR-POOL-ARG+ ( n -- )
   s" --pool-slots" TR-ARG+
   TR-NUM-ARG+ ;

: TR-AOT-RUNNER-PATHS ( -- )
   TR-WARM-ROOT$ s" hb-aot-warm" TR-AOT-RUNNER-BUF JOIN-PATH TR-AOT-RUNNER-U !
   TR-AOT-RUNNER$ s" .trust.f" TR-AOT-RUNNER-TRUST-BUF TR-AOT-RUNNER-TRUST-U TR-SUFFIX!
   TR-AOT-RUNNER$ s" .stamp" TR-AOT-RUNNER-STAMP-BUF TR-AOT-RUNNER-STAMP-U TR-SUFFIX! ;

: TR-KEY-FILE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CK-FILE+ ;

: TR-SNAPSHOT-LINUX-KEY ( -- )
   s" target:linux-aarch64" CK-TEXT+
   s" src/os/linux/layout.f" TR-KEY-FILE+
   s" src/os/linux/elf.f" TR-KEY-FILE+
   s" src/os/linux/sign.f" TR-KEY-FILE+ ;

: TR-SNAPSHOT-MACOS-KEY ( -- )
   s" target:macos-aarch64" CK-TEXT+
   s" src/os/macos/layout.f" TR-KEY-FILE+
   s" src/os/macos/macho.f" TR-KEY-FILE+
   s" src/os/macos/sign2.f" TR-KEY-FILE+ ;

: TR-SNAPSHOT-TARGET-KEY ( -- )
   HB-TARGET-LINUX? if TR-SNAPSHOT-LINUX-KEY exit then
   HB-TARGET-MACOS? if TR-SNAPSHOT-MACOS-KEY exit then
   s" warm image cache unknown target" TR-FAIL ;

: TR-SNAPSHOT-BUILDER-KEY ( -- )
   s" src/os/image-bytes.f" TR-KEY-FILE+
   TR-SNAPSHOT-TARGET-KEY
   s" src/habu/snap-lib.f" TR-KEY-FILE+
   s" src/habu/snap.f" TR-KEY-FILE+ ;

: TR-AOT-RUNNER-KEY-FILE+ ( ptr u8 n -- )
   TR-KEY-FILE+ ;

: TR-AOT-RUNNER-KEY-SUPPORT ( -- )
   [: TR-AOT-RUNNER-KEY-FILE+ ;] TR-AOT-RUNNER-SUPPORT-FILES ;

: TR-AOT-RUNNER-KEY! ( -- )
   CK-RESET
   s" hb-aot-runner-cache-v3" CK-TEXT+
   s" bin/hb" TR-AOT-RUNNER-KEY-FILE+
   s" test/run-lib.f" TR-AOT-RUNNER-KEY-FILE+
   s" test/run-files.f" TR-AOT-RUNNER-KEY-FILE+
   s" test/gate-stats.f" TR-AOT-RUNNER-KEY-FILE+
   s" tools/warm-image-lib.f" TR-AOT-RUNNER-KEY-FILE+
   s" tools/warm-image-gate-stats.f" TR-AOT-RUNNER-KEY-FILE+
   s" tools/warm-image.f" TR-AOT-RUNNER-KEY-FILE+
   TR-SNAPSHOT-BUILDER-KEY
   s" tools/public-signatures-core.f" TR-AOT-RUNNER-KEY-FILE+
   s" tools/public-signatures.f" TR-AOT-RUNNER-KEY-FILE+
   s" lib/content-key.f" TR-AOT-RUNNER-KEY-FILE+
   s" test/gate-aot-runner-entry.f" TR-AOT-RUNNER-KEY-FILE+
   TR-AOT-RUNNER-KEY-SUPPORT
   TR-AOT-RUNNER-KEY-HEX CK-FINAL-HEX ;

: TR-UNDER-SOURCE-KEY ( -- )
   [: TR-KEY-FILE+ ;] TR-UNDER-SOURCE-FILES ;

: TR-UNDER-LINUX-KEY ( -- )
   s" target:linux-aarch64" CK-TEXT+
   s" src/os/linux/target.f" TR-KEY-FILE+
   s" src/os/linux/layout.f" TR-KEY-FILE+
   s" src/os/linux/sys.f" TR-KEY-FILE+
   s" src/os/linux/elf.f" TR-KEY-FILE+
   s" src/os/linux/sign.f" TR-KEY-FILE+
   s" src/os/linux/repl-term.f" TR-KEY-FILE+ ;

: TR-UNDER-MACOS-KEY ( -- )
   s" target:macos-aarch64" CK-TEXT+
   s" src/os/macos/target.f" TR-KEY-FILE+
   s" src/os/macos/layout.f" TR-KEY-FILE+
   s" src/os/macos/sys.f" TR-KEY-FILE+
   s" src/os/macos/macho.f" TR-KEY-FILE+
   s" src/os/macos/sign2.f" TR-KEY-FILE+
   s" src/os/macos/repl-term.f" TR-KEY-FILE+ ;

: TR-UNDER-TARGET-KEY ( -- )
   HB-TARGET-LINUX? if TR-UNDER-LINUX-KEY exit then
   HB-TARGET-MACOS? if TR-UNDER-MACOS-KEY exit then
   s" Habu-under-test cache unknown target" TR-FAIL ;

: TR-UNDER-KEY! ( -- )
   CK-RESET
   s" hb-under-test-cache-v3" CK-TEXT+
   s" bin/hb" TR-KEY-FILE+
   TR-UNDER-SOURCE-KEY
   TR-UNDER-TARGET-KEY
   TR-UNDER-KEY-HEX CK-FINAL-HEX ;

: TR-UNDER-NAME! ( -- )
   s" hb-under-" {: p:ptr pu:n :}
   pu TR-UNDER-PREFIX-U <> if E-STR-BOUNDS throw then
   p TR-UNDER-NAME-BUF pu BYTE-COPY
   TR-UNDER-KEY-HEX TR-UNDER-NAME-BUF pu + 64 BYTE-COPY
   pu 64 + TR-UNDER-NAME-U ! ;

: TR-UNDER-CACHE-PATHS ( -- )
   TR-UNDER-NAME!
   TR-PERSIST$ MAKE-DIRS
   TR-PERSIST$ TR-UNDER-NAME$ TR-UNDER-CACHE-BUF JOIN-PATH TR-UNDER-CACHE-U !
   TR-UNDER-CACHE$ s" .tmp" TR-UNDER-CACHE-TMP-BUF TR-UNDER-CACHE-TMP-U TR-SUFFIX!
   TR-UNDER-CACHE$ s" .lock" TR-UNDER-CACHE-LOCK-BUF TR-UNDER-CACHE-LOCK-U TR-SUFFIX!
   TR-UNDER-CACHE$ s" .stamp" TR-UNDER-CACHE-STAMP-BUF TR-UNDER-CACHE-STAMP-U TR-SUFFIX!
   TR-UNDER-CACHE-STAMP$ s" .tmp" TR-UNDER-CACHE-STAMP-TMP-BUF TR-UNDER-CACHE-STAMP-TMP-U TR-SUFFIX! ;

: TR-UNDER-CACHE-KEY! ( -- )
   TR-UNDER-KEY!
   TR-UNDER-CACHE-PATHS ;

: TR-UNDER-STAMP$ ( -- ptr u8 n )
   TR-UNDER-STAMP-BUF TR-UNDER-STAMP-U ;

: TR-UNDER-CACHE-SHA! ( -- )
   TR-UNDER-CACHE$ TR-UNDER-HEX SHA256-FILE-HEX 0 <> if
      s" failed to hash cached Habu-under-test" TR-FAIL
   then ;

: TR-UNDER-STAMP! ( -- )
   TR-UNDER-KEY-HEX TR-UNDER-STAMP-BUF 64 BYTE-COPY
   $09 TR-UNDER-STAMP-BUF 64 + c!
   TR-UNDER-HEX TR-UNDER-STAMP-BUF 65 + 64 BYTE-COPY
   $0A TR-UNDER-STAMP-BUF 129 + c! ;

: TR-UNDER-CACHE-STAMP-MISSING? ( -- bool )
   TR-UNDER-CACHE-STAMP$ FILE? 0= ;

: TR-UNDER-CACHE-STAMP-OK? ( -- bool )
   TR-UNDER-CACHE-SHA!
   TR-UNDER-STAMP!
   TR-UNDER-CACHE-STAMP$ TR-UNDER-STAMP-RD TR-UNDER-STAMP-U READ-ALL {: got:n :}
   got TR-UNDER-STAMP-U <> if 0 0= 0= exit then
   TR-UNDER-STAMP-RD TR-UNDER-STAMP-U TR-UNDER-STAMP$ STR= ;

: TR-UNDER-CACHE-REMOVE ( -- )
   TR-UNDER-CACHE-TMP$ EXISTS? if TR-UNDER-CACHE-TMP$ REMOVE-FILE then
   TR-UNDER-CACHE-STAMP-TMP$ EXISTS? if TR-UNDER-CACHE-STAMP-TMP$ REMOVE-FILE then
   TR-UNDER-CACHE-STAMP$ EXISTS? if TR-UNDER-CACHE-STAMP$ REMOVE-FILE then
   TR-UNDER-CACHE$ EXISTS? if TR-UNDER-CACHE$ REMOVE-FILE then ;

: TR-UNDER-CACHE-CORRUPT ( -- )
   s" candidate-cache-corrupt" GS-EVENT
   s" Habu-under-test cache stamp mismatch" TR-FAIL ;

: TR-UNDER-CACHE-LOCK? ( -- bool )
   TR-UNDER-CACHE-LOCK$ FS-PATHZ FS-MUT-MODE-PRIVATE-DIR mkdir 0= if TR-TRUE exit then
   TR-UNDER-CACHE-LOCK$ DIR? if TR-FALSE exit then
   E-FS-IO throw ;

: TR-UNDER-CACHE-UNLOCK ( -- )
   TR-UNDER-CACHE-LOCK$ DIR? if TR-UNDER-CACHE-LOCK$ REMOVE-DIR then ;

: TR-UNDER-CACHE-RESTORE ( -- )
   TR-UNDER-READY @ 0 <> if exit then
   TR-PERSIST? 0= if exit then
   TR-UNDER-CACHE-KEY!
   TR-UNDER-CACHE$ EXECUTABLE? 0= if TR-MARK-COLD s" candidate-cache-miss" GS-EVENT exit then
   TR-UNDER-CACHE-STAMP-MISSING? if TR-MARK-COLD s" candidate-cache-miss" GS-EVENT exit then
   TR-UNDER-CACHE-STAMP-OK? 0= if TR-UNDER-CACHE-CORRUPT then
   s" candidate-cache-hit" GS-EVENT
   TR-UNDER-CACHE$ TR-UNDER$ COPY-FILE-STREAM
   TR-UNDER$ CHMOD-X
   -1 TR-UNDER-CACHE-HIT !
   -1 TR-UNDER-READY ! ;

: TR-UNDER-CACHE-INSTALL-LOCKED ( -- )
   TR-UNDER-CACHE$ EXECUTABLE? if
      TR-UNDER-CACHE-STAMP-MISSING? 0= if
         TR-UNDER-CACHE-STAMP-OK? if exit then
         TR-UNDER-CACHE-CORRUPT
      then
   then
   TR-UNDER-CACHE-REMOVE
   TR-UNDER$ TR-UNDER-CACHE-TMP$ COPY-FILE-STREAM
   TR-UNDER-CACHE-TMP$ CHMOD-X
   TR-UNDER-SHA!
   TR-UNDER-STAMP!
   TR-UNDER-CACHE-TMP$ TR-UNDER-CACHE$ RENAME-FILE
   TR-UNDER-CACHE-STAMP-TMP$ TR-UNDER-STAMP$ WRITE-ALL
   TR-UNDER-CACHE-STAMP-TMP$ TR-UNDER-CACHE-STAMP$ RENAME-FILE
   s" candidate-cache-install" GS-EVENT ;

: TR-UNDER-CACHE-INSTALL ( -- )
   TR-UNDER-ARG? if exit then
   TR-PERSIST? 0= if exit then
   TR-UNDER-CACHE-HIT @ 0 <> if exit then
   TR-UNDER-CACHE-KEY!
   TR-UNDER-CACHE$ EXECUTABLE? if
      TR-UNDER-CACHE-STAMP-MISSING? 0= if
         TR-UNDER-CACHE-STAMP-OK? if exit then
         TR-UNDER-CACHE-CORRUPT
      then
   then
   TR-UNDER-CACHE-LOCK? 0= if exit then
   [: TR-UNDER-CACHE-INSTALL-LOCKED ;] catch TR-UNDER-CACHE-RC !
   TR-UNDER-CACHE-UNLOCK
   TR-UNDER-CACHE-RC @ 0 <> if TR-UNDER-CACHE-RC @ throw then ;

: TR-AOT-RUNNER-CACHED? ( -- bool )
   TR-AOT-RUNNER$ EXECUTABLE? 0= if 0 0= 0= exit then
   TR-AOT-RUNNER-TRUST$ FILE? 0= if 0 0= 0= exit then
   TR-AOT-RUNNER-STAMP$ FILE? 0= if 0 0= 0= exit then
   TR-AOT-RUNNER-STAMP$ TR-AOT-RUNNER-STAMP-RD 80 READ-ALL
   dup 64 <> if drop 0 0= 0= exit then
   TR-AOT-RUNNER-STAMP-RD swap TR-AOT-RUNNER-KEY-HEX 64 STR= ;

: TR-AOT-RUNNER-TOOL-ARGV ( -- )
   PROC-ARGV-ENV-RESET
   s" --load" TR-ARG+
   s" tools/warm-image.f" TR-ARG+
   s" --" TR-ARG+
   TR-AOT-RUNNER$ TR-ARG+ ;

: TR-AOT-RUNNER-SUPPORT-ARGV ( -- )
   [: TR-ARG+ ;] TR-AOT-RUNNER-SUPPORT-FILES ;

: TR-AOT-RUNNER-START ( -- )
   TR-AOT-RUNNER-PATHS
   TR-AOT-RUNNER-KEY!
   TR-AOT-RUNNER-CACHED? if s" warm-cache-hit" GS-EVENT -1 TR-AOT-RUNNER-READY ! exit then
   TR-MARK-COLD
   s" warm-cache-miss" GS-EVENT
   s" warm-build" GS-EVENT
   s" gate-runner-build" GS-EVENT
   TR-AOT-RUNNER-TOOL-ARGV
   TR-AOT-RUNNER-SUPPORT-ARGV
   PROC-ENV-RESET
   s" HB_TMP" >LEN GT-ROOT >LEN PROC-ENV+
   TR-PERSIST-ENV+
   TR-BUILD-CACHE-ENV
   GS-ENV+
   PROC-ENV-INHERIT-MISSING
   s" bin/hb" s" native warm AOT gate runner image" TR-TIMEOUT-MS
   TR-AOT-RUNNER-SLOT >IDX GT-POOL-START-SLOT ;

: TR-AOT-RUNNER-EXPECT ( -- )
   TR-AOT-RUNNER$ EXECUTABLE? 0= if
      s" missing warm AOT gate runner image" TR-FAIL
   then
   TR-AOT-RUNNER-TRUST$ FILE? 0= if
      s" missing warm AOT gate runner trust file" TR-FAIL
   then
   TR-AOT-RUNNER-STAMP$ TR-AOT-RUNNER-KEY-HEX 64 WRITE-ALL
   -1 TR-AOT-RUNNER-READY ! ;

: TR-AOT-RUNNER-DONE? ( -- bool )
   TR-AOT-RUNNER-READY @ 0 <> if 0 0= exit then
   TR-AOT-RUNNER-SLOT >IDX GT-POOL-DONE@ 0= if 0 0= 0= exit then
   TR-AOT-RUNNER-EXPECT
   0 0= ;

: TR-DRAIN-UNTIL-AOT-RUNNER ( -- )
   begin TR-AOT-RUNNER-DONE? 0= while
      GT-POOL-STEP
   repeat ;

: TR-BUILD-COMMON ( -- )
   TR-COMMON
   TR-BUILD-ASSERT-LIBS
   s" test/gate-build-common.f"  >LEN PROC-ARGV+ ;

: TR-BUILD-LIB ( -- )
   s" lib/vector.f"  >LEN PROC-ARGV+
   s" lib/source.f"  >LEN PROC-ARGV+
   s" lib/build.f"  >LEN PROC-ARGV+
   s" lib/codesign.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+
   s" tools/lint/token.f"  >LEN PROC-ARGV+
   s" tools/lint/lib.f"  >LEN PROC-ARGV+
   s" tools/lint/json-writer.f"  >LEN PROC-ARGV+
   s" tools/lint/source-lex.f"  >LEN PROC-ARGV+
   s" tools/aot-lint-core.f"  >LEN PROC-ARGV+
   s" tools/signature-lint-core.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint.f"  >LEN PROC-ARGV+
   s" tools/warm-run.f"  >LEN PROC-ARGV+
   s" tools/hb-build-lib.f"  >LEN PROC-ARGV+
   s" tools/hb-build-direct-lints.f"  >LEN PROC-ARGV+ ;

: TR-BUILD-LIB-COMMON ( -- )
   TR-COMMON
   TR-BUILD-LIB
   TR-BUILD-ASSERT-LIBS
   s" test/gate-build-common.f"  >LEN PROC-ARGV+
   s" test/gate-build-hbb.f"  >LEN PROC-ARGV+ ;

: TR-STDLIB-ARGS ( -- )
   s" test/gate-pool.f"  >LEN PROC-ARGV+
   s" test/gate-stdlib.f"  >LEN PROC-ARGV+ ;

: TR-STDLIB-SLICE-ARGS ( ptr u8 n -- ) {: slice:ptr sliceu:n :}
   TR-STDLIB-ARGS
   s" --"  >LEN PROC-ARGV+
   slice sliceu  >LEN PROC-ARGV+ ;

: TR-STDLIB-WARM-ARGS ( -- )
   s" warm" TR-STDLIB-SLICE-ARGS ;

: TR-STDLIB-LINT-ARGS ( -- )
   s" lint" TR-STDLIB-SLICE-ARGS ;

: TR-STDLIB-LINT-TOOLS-ARGS ( -- )
   s" lint-tools" TR-STDLIB-SLICE-ARGS ;

: TR-STDLIB-LINT-MANIFEST-ARGS ( -- )
   s" tools/lint/text.f"  >LEN PROC-ARGV+
   s" tools/lint/token.f"  >LEN PROC-ARGV+
   s" tools/lint/lib.f"  >LEN PROC-ARGV+
   s" tools/stdlib-manifest-test.f"  >LEN PROC-ARGV+ ;

: TR-STDLIB-LINT-ARTIFACTS-ARGS ( -- )
   s" lint-artifacts" TR-STDLIB-SLICE-ARGS ;

: TR-STDLIB-LINT-LIBS-ARGS ( -- )
   s" lint-libs" TR-STDLIB-SLICE-ARGS ;

: TR-STDLIB-TOOL-ARGS ( -- )
   s" tool" TR-STDLIB-SLICE-ARGS ;

: TR-STDLIB-CHECK-CLI-ARGS ( -- )
   s" check-cli" TR-STDLIB-SLICE-ARGS ;

: TR-STDLIB-TAIL-ARGS ( -- )
   s" tail" TR-STDLIB-SLICE-ARGS ;

: TR-ENGINE-ARGS ( -- )
   TR-COMMON
   s" lib/build.f"  >LEN PROC-ARGV+
   s" lib/codesign.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint.f"  >LEN PROC-ARGV+
   s" test/gate-pool.f"  >LEN PROC-ARGV+
   s" test/gate-engine.f"  >LEN PROC-ARGV+ ;

: TR-ENGINE-SLICE-ARGS ( ptr u8 n -- ) {: slice:ptr sliceu:n :}
   TR-ENGINE-ARGS
   s" --"  >LEN PROC-ARGV+
   slice sliceu  >LEN PROC-ARGV+ ;

: TR-ENGINE-BUILD-ARGS ( -- )
   s" build" TR-ENGINE-SLICE-ARGS ;

: TR-ENGINE-FIXTURES-ARGS ( -- )
   s" fixtures" TR-ENGINE-SLICE-ARGS ;

: TR-ENGINE-REPAIR-ARGS ( -- )
   s" repair" TR-ENGINE-SLICE-ARGS ;

: TR-ENGINE-RUNTIME-ARGS ( -- )
   s" runtime" TR-ENGINE-SLICE-ARGS ;

: TR-ENGINE-VALIDATE-ARGS ( -- )
   s" validate" TR-ENGINE-SLICE-ARGS ;

: TR-DICTIONARY-ARGS ( -- )
   TR-COMMON
   s" lib/vector.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+
   s" tools/lint/token.f"  >LEN PROC-ARGV+
   s" tools/lint/lib.f"  >LEN PROC-ARGV+
   s" tools/lint/json-writer.f"  >LEN PROC-ARGV+
   s" tools/lint/source-lex.f"  >LEN PROC-ARGV+
   s" tools/check-all-errors-core.f"  >LEN PROC-ARGV+
   s" test/gate-dictionary.f"  >LEN PROC-ARGV+ ;

: TR-DIAGNOSTICS-ARGS ( -- )
   TR-COMMON
   s" test/gate-diagnostics.f"  >LEN PROC-ARGV+ ;

: TR-DIAG-SLICE-ARGS ( ptr u8 n -- ) {: slice:ptr sliceu:n :}
   TR-DIAGNOSTICS-ARGS
   s" --"  >LEN PROC-ARGV+
   slice sliceu  >LEN PROC-ARGV+ ;

: TR-DIAG-WARM-ARGS ( -- )
   s" warm" TR-DIAG-SLICE-ARGS ;

: TR-DIAG-REPAIR-ARGS ( -- )
   s" diag-repair" TR-DIAG-SLICE-ARGS ;

: TR-DIAG-UNDEF-PRIMARY-ARGS ( -- )
   s" diag-undef-primary" TR-DIAG-SLICE-ARGS ;

: TR-DIAG-ALL-STRICT-ARGS ( -- )
   s" diag-all-strict" TR-DIAG-SLICE-ARGS ;

: TR-DIAG-FILE-UNSAFE-ARGS ( -- )
   s" diag-file-unsafe" TR-DIAG-SLICE-ARGS ;

: TR-DEBUG-ARGS ( -- )
   TR-COMMON
   s" test/gate-debug.f"  >LEN PROC-ARGV+ ;

: TR-AOT-POSITIVE-ARGS ( -- )
   TR-BUILD-LIB-COMMON
   s" test/gate-aot-positive.f"  >LEN PROC-ARGV+ ;

: TR-AOT-NEGATIVE-ARGS ( -- )
   TR-BUILD-LIB-COMMON
   s" test/gate-aot-negative.f"  >LEN PROC-ARGV+ ;

: TR-STDLIB ( -- )
   TR-BASE
   TR-STDLIB-ARGS
   s" native lint/stdlib gate phase" TR-RUN ;

: TR-ENGINE ( -- )
   TR-BASE
   TR-ENGINE-ARGS
   s" native engine gate phase" TR-RUN ;

: TR-EXPECT-HB ( -- )
   s" bin/hb" EXECUTABLE? 0= if s" bin/hb not produced executable" TR-FAIL then ;

: TR-DICTIONARY ( -- )
   TR-BASE
   TR-DICTIONARY-ARGS
   s" native dictionary/checker gate phase" TR-RUN ;

: TR-DIAGNOSTICS ( -- )
   TR-BASE
   TR-DIAGNOSTICS-ARGS
   s" native checker diagnostics gate phase" TR-RUN ;

: TR-DIAG-WARM ( -- )
   TR-BASE
   TR-DIAG-WARM-ARGS
   s" native checker warm image gate phase" TR-RUN ;

: TR-DEBUG ( -- )
   TR-BASE
   TR-DEBUG-ARGS
   s" native prop/debug gate phase" TR-RUN ;

: TR-AOT-POSITIVE ( -- )
   TR-BASE
   TR-AOT-POSITIVE-ARGS
   s" native hb-build AOT positive gate phase" TR-RUN ;

: TR-AOT-NEGATIVE ( -- )
   TR-BASE
   TR-AOT-NEGATIVE-ARGS
   s" native hb-build AOT negative gate phase" TR-RUN ;

: TR-PHASE-LABEL ( idx -- ptr u8 n ) {: idx:idx :}
   idx IDX>N case
      0 of s" native stdlib tools warm image" endof
      1 of s" native checker warm image gate phase" endof
      2 of s" native stdlib trust tool slice" endof
      3 of s" native stdlib check-cli slice" endof
      4 of s" native stdlib tail slice" endof
      5 of s" native engine repair slice" endof
      6 of s" native prop/debug gate phase" endof
      7 of s" native hb-build AOT positive gate phase" endof
      8 of s" native hb-build AOT negative gate phase" endof
      9 of s" native engine fixture slice" endof
      10 of s" native checker diagnostics repair slice" endof
      11 of s" native checker diagnostics undef-primary slice" endof
      12 of s" native checker diagnostics all-strict slice" endof
      13 of s" native checker diagnostics file-unsafe slice" endof
      14 of s" native dictionary/checker gate phase" endof
      15 of s" native engine build slice" endof
      16 of s" native engine runtime slice" endof
      17 of s" native stdlib lint tools slice" endof
      18 of s" native stdlib lint manifest slice" endof
      19 of s" native stdlib lint artifacts slice" endof
      20 of s" native stdlib lint libs slice" endof
      21 of s" native engine candidate validation slice" endof
      22 of s" GROUP: stdlib/tool-repair [parallel]" endof
      23 of s" GROUP: stdlib/tool-doc [parallel]" endof
      24 of s" GROUP: stdlib/tool-lints [parallel]" endof
      25 of s" GROUP: stdlib/tool-typed-local [parallel]" endof
      26 of s" GROUP: stdlib/tail-fast [inprocess]" endof
      27 of s" GROUP: stdlib/tail-pure [inprocess]" endof
      28 of s" GROUP: stdlib/tail-runner [inprocess]" endof
      29 of s" GROUP: stdlib/tail-build [inprocess]" endof
      30 of s" GROUP: stdlib/tail-warm-image [inprocess]" endof
      31 of s" GROUP: stdlib/lint-libs/core [inprocess]" endof
      32 of s" GROUP: stdlib/lint-libs/ptx [inprocess]" endof
      33 of s" GROUP: stdlib/lint-libs/ptx-neg [inprocess]" endof
      34 of s" GROUP: stdlib/lint-libs/ptx-toolchain [inprocess]" endof
      35 of s" GROUP: stdlib/lint-artifacts/fast [inprocess]" endof
      36 of s" GROUP: stdlib/tail-process [inprocess]" endof
      37 of s" GROUP: stdlib/tool-lint/repl [parallel]" endof
      38 of s" GROUP: stdlib/tool-lint/aot-signature [parallel]" endof
      39 of s" GROUP: stdlib/tool-lint/names [parallel]" endof
      40 of s" GROUP: stdlib/tool-lint/bundle-json [parallel]" endof
      E-TBL-BOUNDS throw
   endcase ;

: TR-PHASE-DIR ( idx -- ptr u8 n ) {: idx:idx :}
   idx IDX>N case
      0 of s" gate-stdlib-warm" endof
      1 of s" gate-check-warm" endof
      2 of s" gate-stdlib-tool-trust" endof
      3 of s" gate-stdlib-check-cli" endof
      4 of s" gate-stdlib-tail" endof
      5 of s" gate-engine-repair" endof
      6 of s" gate-debug" endof
      7 of s" gate-aot-pos" endof
      8 of s" gate-aot-neg" endof
      9 of s" gate-engine-fixtures" endof
      10 of s" gate-diag-repair" endof
      11 of s" gate-diag-undef-primary" endof
      12 of s" gate-diag-all-strict" endof
      13 of s" gate-diag-file-unsafe" endof
      14 of s" gate-dict" endof
      15 of s" gate-engine-build" endof
      16 of s" gate-engine-runtime" endof
      17 of s" gate-stdlib-lint-tools" endof
      18 of s" gate-stdlib-lint-manifest" endof
      19 of s" gate-stdlib-lint-artifacts" endof
      20 of s" gate-stdlib-lint-libs" endof
      21 of s" gate-engine-validate" endof
      22 of s" gate-stdlib-tool-repair" endof
      23 of s" gate-stdlib-tool-doc" endof
      24 of s" gate-stdlib-tool-lints" endof
      25 of s" gate-stdlib-tool-typed" endof
      26 of s" gate-stdlib-tail-fast" endof
      27 of s" gate-stdlib-tail-pure" endof
      28 of s" gate-stdlib-tail-runner" endof
      29 of s" gate-stdlib-tail-build" endof
      30 of s" gate-stdlib-tail-warm" endof
      31 of s" gate-stdlib-lint-libs-core" endof
      32 of s" gate-stdlib-lint-libs-ptx" endof
      33 of s" gate-stdlib-lint-libs-ptx-neg" endof
      34 of s" gate-stdlib-lint-libs-ptx-tool" endof
      35 of s" gate-stdlib-lint-artifacts-fast" endof
      36 of s" gate-stdlib-tail-process" endof
      37 of s" gate-stdlib-tool-lint-repl" endof
      38 of s" gate-stdlib-tool-lint-aot" endof
      39 of s" gate-stdlib-tool-lint-names" endof
      40 of s" gate-stdlib-tool-lint-bundle" endof
      E-TBL-BOUNDS throw
   endcase ;

: TR-PHASE-ARGS ( idx -- ) {: idx:idx :}
   idx IDX>N case
      0 of TR-STDLIB-WARM-ARGS endof
      1 of TR-DIAG-WARM-ARGS endof
      2 of TR-STDLIB-TOOL-ARGS endof
      3 of TR-STDLIB-CHECK-CLI-ARGS endof
      4 of TR-STDLIB-TAIL-ARGS endof
      5 of TR-ENGINE-REPAIR-ARGS endof
      6 of TR-DEBUG-ARGS endof
      7 of TR-AOT-POSITIVE-ARGS endof
      8 of TR-AOT-NEGATIVE-ARGS endof
      9 of TR-ENGINE-FIXTURES-ARGS endof
      10 of TR-DIAG-REPAIR-ARGS endof
      11 of TR-DIAG-UNDEF-PRIMARY-ARGS endof
      12 of TR-DIAG-ALL-STRICT-ARGS endof
      13 of TR-DIAG-FILE-UNSAFE-ARGS endof
      14 of TR-DICTIONARY-ARGS endof
      15 of TR-ENGINE-BUILD-ARGS endof
      16 of TR-ENGINE-RUNTIME-ARGS endof
      17 of TR-STDLIB-LINT-TOOLS-ARGS endof
      18 of TR-STDLIB-LINT-MANIFEST-ARGS endof
      19 of TR-STDLIB-LINT-ARTIFACTS-ARGS endof
      20 of TR-STDLIB-LINT-LIBS-ARGS endof
      21 of TR-ENGINE-VALIDATE-ARGS endof
      22 of TR-STDLIB-TOOL-ARGS endof
      23 of TR-STDLIB-TOOL-ARGS endof
      24 of TR-STDLIB-TOOL-ARGS endof
      25 of TR-STDLIB-TOOL-ARGS endof
      26 of TR-STDLIB-TAIL-ARGS endof
      27 of TR-STDLIB-TAIL-ARGS endof
      28 of TR-STDLIB-TAIL-ARGS endof
      29 of TR-STDLIB-TAIL-ARGS endof
      30 of TR-STDLIB-TAIL-ARGS endof
      31 of TR-STDLIB-LINT-LIBS-ARGS endof
      32 of TR-STDLIB-LINT-LIBS-ARGS endof
      33 of TR-STDLIB-LINT-LIBS-ARGS endof
      34 of TR-STDLIB-LINT-LIBS-ARGS endof
      35 of TR-STDLIB-LINT-ARTIFACTS-ARGS endof
      36 of TR-STDLIB-TAIL-ARGS endof
      37 of TR-STDLIB-TOOL-ARGS endof
      38 of TR-STDLIB-TOOL-ARGS endof
      39 of TR-STDLIB-TOOL-ARGS endof
      40 of TR-STDLIB-TOOL-ARGS endof
      E-TBL-BOUNDS throw
   endcase ;

: TR-PHASE-RUNNER-TOKEN ( idx -- ptr u8 n ) {: idx:idx :}
   idx IDX>N case
      2 of s" tool" endof
      3 of s" check-cli" endof
      4 of s" tail" endof
      5 of s" repair" endof
      6 of s" debug" endof
      7 of s" aot-pos" endof
      8 of s" aot-neg" endof
      9 of s" fixtures" endof
      10 of s" diag-repair" endof
      11 of s" diag-undef-primary" endof
      12 of s" diag-all-strict" endof
      13 of s" diag-file-unsafe" endof
      14 of s" dictionary" endof
      16 of s" runtime" endof
      17 of s" lint-tools" endof
      18 of s" lint-manifest" endof
      19 of s" lint-artifacts" endof
      20 of s" lint-libs" endof
      21 of s" validate" endof
      22 of s" tool-repair" endof
      23 of s" tool-doc" endof
      24 of s" tool-lints" endof
      25 of s" tool-typed" endof
      26 of s" tail-fast" endof
      27 of s" tail-pure" endof
      28 of s" tail-runner" endof
      29 of s" tail-build" endof
      30 of s" tail-warm" endof
      31 of s" lint-libs-core" endof
      32 of s" lint-libs-ptx" endof
      33 of s" lint-libs-ptx-neg" endof
      34 of s" lint-libs-ptx-tool" endof
      35 of s" lint-artifacts-fast" endof
      36 of s" tail-process" endof
      37 of s" tool-lint-repl" endof
      38 of s" tool-lint-aot" endof
      39 of s" tool-lint-names" endof
      40 of s" tool-lint-bundle" endof
      E-TBL-BOUNDS throw
   endcase ;

: TR-PHASE-TMP! ( idx -- ) {: idx:idx :}
   GT-ROOT idx TR-PHASE-DIR TR-PATH-BUF JOIN-PATH TR-PATH-U !
   TR-PATH$ MAKE-DIRS ;

: TR-STDLIB-SLICE? ( idx -- bool ) {: idx:idx :}
   idx IDX>N case
      2 of TR-TRUE endof
      3 of TR-TRUE endof
      4 of TR-TRUE endof
      17 of TR-TRUE endof
      18 of TR-TRUE endof
      19 of TR-TRUE endof
      20 of TR-TRUE endof
      22 of TR-TRUE endof
      23 of TR-TRUE endof
      24 of TR-TRUE endof
      25 of TR-TRUE endof
      37 of TR-TRUE endof
      38 of TR-TRUE endof
      39 of TR-TRUE endof
      40 of TR-TRUE endof
      TR-FALSE swap
   endcase ;

: TR-TOOLS-PHASE? ( idx -- bool ) {: idx:idx :}
   idx TR-STDLIB-SLICE? if 0 0= exit then
   idx IDX>N 5 = ;

: TR-PHASE-POOL-ARGS ( idx -- ) {: idx:idx :}
   idx TR-STDLIB-SLICE? if
      TR-NESTED-POOL @ TR-POOL-ARG+
      exit
   then
   idx IDX>N case
      9 of TR-NESTED-POOL @ TR-POOL-ARG+ endof
   endcase ;

: TR-PHASE-TOOLS-ENV ( idx -- ) {: idx:idx :}
   idx TR-TOOLS-PHASE? if TR-TOOLS-ENV then
   idx IDX>N 3 = if TR-CHECK-ENV then ;

: TR-PHASE-UNDER? ( idx -- bool ) {: idx:idx :}
   idx IDX>N case
      3 of TR-TRUE endof
      14 of TR-TRUE endof
      16 of TR-TRUE endof
      21 of TR-TRUE endof
      TR-FALSE swap
   endcase ;

: TR-PHASE-UNDER-ENV? ( idx -- bool ) {: idx:idx :}
   TR-UNDER-READY @ 0= if TR-FALSE exit then
   idx TR-PHASE-UNDER? ;

: TR-PHASE-UNDER-EXE? ( idx -- bool ) {: idx:idx :}
   TR-UNDER-READY @ 0= if TR-FALSE exit then
   idx TR-PHASE-UNDER? ;

: TR-UNDER-DEFAULT+ ( idx -- ) {: idx:idx :}
   idx TR-PHASE-UNDER? if
      s" HABU_UNDER_TEST" TR-UNDER$ TR-DEFAULT+
      exit
   then ;

: TR-PHASE-RESIDENT? ( idx -- bool ) {: idx:idx :}
   idx IDX>N case
      0 of TR-FALSE endof
      1 of TR-FALSE endof
      4 of TR-FALSE endof
      7 of TR-FALSE endof
      8 of TR-FALSE endof
      15 of TR-FALSE endof
      16 of TR-FALSE endof
      19 of TR-FALSE endof
      20 of TR-FALSE endof
      21 of TR-FALSE endof
      TR-TRUE swap
   endcase ;

: TR-PHASE-AOT-RUNNER? ( idx -- bool ) {: idx:idx :}
   TR-AOT-RUNNER-READY @ 0= if 0 0= 0= exit then
   idx IDX>N case
      7 of TR-TRUE endof
      8 of TR-TRUE endof
      TR-FALSE swap
   endcase ;

: TR-PHASE-WARM-RUNNER? ( idx -- bool ) {: idx:idx :}
   idx TR-PHASE-AOT-RUNNER? ;

: TR-PHASE-SUBJECT ( idx -- ptr u8 n ) {: idx:idx :}
   idx IDX>N case
      0 of s" artifact" endof
      1 of s" artifact" endof
      7 of s" artifact" endof
      8 of s" artifact" endof
      15 of s" artifact" endof
      3 of s" candidate-cli" endof
      16 of s" candidate-cli" endof
      14 of s" candidate-source" endof
      21 of s" candidate-source" endof
      s" host-source" rot
   endcase ;

: TR-PHASE-RUNNER-KIND ( idx -- ptr u8 n ) {: idx:idx :}
   idx TR-PHASE-RESIDENT? if s" resident" exit then
   idx TR-PHASE-AOT-RUNNER? if s" aot-runner" exit then
   idx TR-PHASE-UNDER-EXE? if s" under" exit then
   s" bin" ;

: TR-PHASE-BOUNDARY ( idx -- ptr u8 n ) {: idx:idx :}
   idx TR-PHASE-RESIDENT? if s" resident-fork" exit then
   s" process" ;

: TR-PHASE-SHA ( idx -- ptr u8 n )
   drop
   TR-UNDER-READY @ 0= if s" -" exit then
   TR-UNDER-HEX 64 ;

: TR-TIMINGS-ARG+ ( -- )
   s" --timings" TR-ARG+ ;

: TR-PHASE-TIMINGS-ARGS ( idx -- ) {: idx:idx :}
   TR-TIMINGS @ 0= if exit then
   idx TR-STDLIB-SLICE? if TR-TIMINGS-ARG+ exit then ;

: TR-PHASE-TEST ( idx -- ) {: idx:idx :}
   idx TR-PHASE-LABEL
   idx TR-PHASE-SUBJECT
   idx TR-PHASE-RUNNER-KIND
   idx TR-PHASE-BOUNDARY
   idx TR-PHASE-SHA
   GS-TEST ;

: TR-PHASE-UNDER-ENV ( idx -- ) {: idx:idx :}
   idx TR-PHASE-UNDER-ENV? if
      s" under-env" GS-EVENT
      TR-UNDER-ENV+
   then ;

: TR-PHASE-EXE ( idx -- ptr u8 n ) {: idx:idx :}
   idx TR-PHASE-AOT-RUNNER? if TR-AOT-RUNNER$ exit then
   idx TR-PHASE-UNDER-EXE? if TR-UNDER$ exit then
   s" bin/hb" ;

: TR-PHASE-ARGV-COLD ( -- )
   s" --load"  >LEN PROC-ARGV+
   s" lib/errors.f"  >LEN PROC-ARGV+
   s" lib/string.f"  >LEN PROC-ARGV+
   s" lib/memory.f"  >LEN PROC-ARGV+
   s" lib/fs.f"  >LEN PROC-ARGV+
   s" lib/fs-mutate.f"  >LEN PROC-ARGV+
   s" lib/process.f"  >LEN PROC-ARGV+
   s" lib/process-argv.f"  >LEN PROC-ARGV+
   s" lib/process-env.f"  >LEN PROC-ARGV+
   s" lib/test/runner.f"  >LEN PROC-ARGV+ ;

: TR-PHASE-ARGV-AOT-RUNNER ( idx -- ) {: idx:idx :}
   s" --load"  >LEN PROC-ARGV+
   s" test/gate-aot-runner-entry.f"  >LEN PROC-ARGV+
   s" --"  >LEN PROC-ARGV+
   idx TR-PHASE-RUNNER-TOKEN >LEN PROC-ARGV+ ;

: TR-PHASE-BASE ( idx -- ) {: idx:idx :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   idx TR-PHASE-TMP!
   s" HB_TMP" >LEN TR-PATH$ >LEN PROC-ENV+
   TR-PERSIST-ENV+
   idx TR-PHASE-TOOLS-ENV
   TR-BUILD-CACHE-ENV
   GS-ENV+
   idx TR-PHASE-UNDER-ENV
   PROC-ENV-INHERIT-MISSING
   idx TR-PHASE-AOT-RUNNER? if
      idx TR-PHASE-ARGV-AOT-RUNNER
   else
      TR-PHASE-ARGV-COLD
   then ;

: TR-PHASE-START ( idx -- ) {: idx:idx :}
   idx TR-PHASE-BASE
   idx TR-PHASE-WARM-RUNNER? 0= if idx TR-PHASE-ARGS then
   idx TR-PHASE-POOL-ARGS
   idx TR-PHASE-TIMINGS-ARGS
   s" top-phase-spawn" GS-EVENT
   idx TR-PHASE-AOT-RUNNER? if s" runner-phase-spawn" GS-EVENT then
   idx TR-PHASE-UNDER-EXE? if s" under-phase-spawn" GS-EVENT then
   idx TR-PHASE-TEST
   idx TR-PHASE-EXE idx TR-PHASE-LABEL TR-TIMEOUT-MS GT-POOL-START ;

: TR-PHASE-START-SLOT ( idx idx -- ) {: idx:idx slot:idx :}
   idx TR-PHASE-BASE
   idx TR-PHASE-WARM-RUNNER? 0= if idx TR-PHASE-ARGS then
   idx TR-PHASE-POOL-ARGS
   idx TR-PHASE-TIMINGS-ARGS
   s" top-phase-spawn" GS-EVENT
   idx TR-PHASE-AOT-RUNNER? if s" runner-phase-spawn" GS-EVENT then
   idx TR-PHASE-UNDER-EXE? if s" under-phase-spawn" GS-EVENT then
   idx TR-PHASE-TEST
   idx TR-PHASE-EXE idx TR-PHASE-LABEL TR-TIMEOUT-MS slot GT-POOL-START-SLOT ;

: TR-GROUP-MODE ( idx -- n )
   drop TR-GROUP-PAR ;

: TR-GROUP-SEQ? ( idx -- bool )
   TR-GROUP-MODE TR-GROUP-SEQ = ;

: TR-WARM-READY-RESET ( -- )
   0 TR-TOOLS-WARM-READY !
   0 TR-CHECK-WARM-READY ! ;

: TR-WARM-READY-MARK ( -- )
   TR-TOOLS-WARM-READY @ 0= if
      TR-TOOLS-WARM-SLOT >IDX GT-POOL-DONE@ 0 <> if -1 TR-TOOLS-WARM-READY ! then
   then
   TR-CHECK-WARM-READY @ 0= if
      TR-CHECK-WARM-SLOT >IDX GT-POOL-DONE@ 0 <> if -1 TR-CHECK-WARM-READY ! then
   then ;

: TR-WARM-DONE? ( -- bool )
   TR-WARM-READY-MARK
   TR-TOOLS-WARM-READY @ 0 <>
   TR-CHECK-WARM-READY @ 0 <> and ;

: TR-UNDER-DONE? ( -- bool )
   TR-UNDER$ EXECUTABLE? ;

: TR-CHECK-WARM-DONE? ( -- bool )
   TR-WARM-READY-MARK
   TR-CHECK-WARM-READY @ 0 <> ;

: TR-DRAIN-UNTIL-UNDER ( -- )
   begin TR-UNDER-DONE? 0= while
      GT-POOL-STEP
   repeat
   TR-EXPECT-UNDER
   TR-UNDER-CACHE-INSTALL ;

: TR-DRAIN-UNTIL-WARM ( -- )
   begin TR-WARM-DONE? 0= while
      GT-POOL-STEP
   repeat ;

: TR-DRAIN-UNTIL-CHECK-WARM ( -- )
   begin TR-CHECK-WARM-DONE? 0= while
      GT-POOL-STEP
   repeat ;

: TR-CHECK-WARM-ORDER@ ( idx -- idx ) {: idx:idx :}
   idx IDX>N cells TR-CHECK-WARM-ORDER + @ >IDX ;

: TR-LATE-ORDER@ ( idx -- idx ) {: idx:idx :}
   idx IDX>N cells TR-LATE-ORDER + @ >IDX ;

: TR-EARLY-HOST-ORDER@ ( idx -- idx ) {: idx:idx :}
   idx IDX>N cells TR-EARLY-HOST-ORDER + @ >IDX ;

: TR-PRE-RESET ( -- )
   0 TR-PRE-TAIL !
   0 TR-PRE-AOT-POS !
   0 TR-PRE-AOT-NEG !
   0 TR-PRE-ARTIFACTS !
   0 TR-PRE-RUNTIME !
   0 TR-PRE-VALIDATE ! ;

: TR-PRE? ( idx -- bool ) {: idx:idx :}
   idx IDX>N case
      4 of TR-PRE-TAIL @ 0 <> endof
      7 of TR-PRE-AOT-POS @ 0 <> endof
      8 of TR-PRE-AOT-NEG @ 0 <> endof
      16 of TR-PRE-RUNTIME @ 0 <> endof
      19 of TR-PRE-ARTIFACTS @ 0 <> endof
      21 of TR-PRE-VALIDATE @ 0 <> endof
      TR-FALSE swap
   endcase ;

: TR-PRE-MARK ( idx -- ) {: idx:idx :}
   idx IDX>N case
      4 of -1 TR-PRE-TAIL ! endof
      7 of -1 TR-PRE-AOT-POS ! endof
      8 of -1 TR-PRE-AOT-NEG ! endof
      16 of -1 TR-PRE-RUNTIME ! endof
      19 of -1 TR-PRE-ARTIFACTS ! endof
      21 of -1 TR-PRE-VALIDATE ! endof
   endcase ;

: TR-PRE-START ( idx -- ) {: idx:idx :}
   idx TR-PHASE-START
   idx TR-PRE-MARK ;

: TR-TOOLS-CACHED? ( -- bool )
   TR-TOOLS-PATHS
   TR-TOOLS$ EXECUTABLE? 0= if TR-FALSE exit then
   TR-TOOLS-TRUST$ FILE? ;

: TR-PRE-TOOLS-START ( -- )
   TR-TOOLS-CACHED? 0= if exit then
   4 >IDX TR-PRE-START
   19 >IDX TR-PRE-START ;

: TR-PRE-AOT-START ( -- )
   TR-AOT-RUNNER-READY @ 0= if exit then
   8 >IDX TR-PRE-START
   7 >IDX TR-PRE-START ;

: TR-PRE-CANDIDATE-START ( -- )
   TR-UNDER-READY @ 0= if exit then
   21 >IDX TR-PRE-START
   16 >IDX TR-PRE-START ;

: TR-EARLY-EXTERNAL-START ( -- )
   GT-POOL-RESET
   TR-WARM-READY-RESET
   0 >IDX TR-TOOLS-WARM-SLOT >IDX TR-PHASE-START-SLOT
   1 >IDX TR-CHECK-WARM-SLOT >IDX TR-PHASE-START-SLOT
   TR-AOT-RUNNER-START
   TR-PRE-AOT-START
   TR-PRE-TOOLS-START
   TR-PRE-CANDIDATE-START
   TR-UNDER-READY @ 0= if
      15 >IDX TR-PHASE-START
   else
      s" candidate-build-skip" GS-EVENT
   then ;

: TR-PREPARE ( -- )
   TR-GATE-START!
   TR-CHECK-ARGS
   TR-CHECK-PROFILE
   TR-START
   TR-PRE-RESET
   TR-CLEAN-WARM
   TR-EXPECT-HB
   TR-UNDER-IMPORT
   TR-UNDER-CACHE-RESTORE ;

: TR-COMPLETE ( -- )
   GS-SUMMARY
   GT-CLEANUP
   TR-FINISH ;
