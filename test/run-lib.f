\ run-lib.f - resident native test suite implementation.
\
\ Load after test/run-support.f.

require lib/adt/option.f                 \ option<n> STR>NUMBER? consumer (switchover wave A)
require lib/test/budget.f
require test/run-support.f
require test/run-files.f
require test/run-result-cache.f

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
$2 constant TR-CANDIDATE-HOST-PHASES
$1B constant TR-EARLY-HOST-PHASES
$3 constant TR-LATE-PHASES
9 constant TR-UNDER-PREFIX-U
0 constant TR-GROUP-SEQ
1 constant TR-GROUP-PAR
1 constant TR-PROFILE-MACOS-ARM64-10X2
2 constant TR-PROFILE-JETSON-ORIN-CLOCKS-4X2
3 constant TR-PROFILE-LINUX-ARM64-4X2

\ Budget calibration: profile budget tables were tuned green on a reference
\ host; a startup spin probe measures this run's speed against the profile's
\ reference probe time and scales the timed budgets so load or downclocking
\ cannot fail a green tree. A profile with reference 0 is uncalibrated and
\ keeps its static budgets; user-supplied --budget-ms/--wall-budget-ms are
\ never scaled. The factor is clamped to [100%,300%] so a thrashing host
\ still trips the stop-line rather than stretching it without bound.
T-BUDGET-CAL-ITERS constant TR-CAL-ITERS             \ shared with lib/test/budget.f self-calibration
T-BUDGET-CAL-REF-MACOS-MS constant TR-CAL-REF-MACOS-MS
0 constant TR-CAL-REF-JETSON-MS
0 constant TR-CAL-REF-LINUX-MS
T-BUDGET-MIN-PCT constant TR-CAL-MIN-PCT
T-BUDGET-MAX-PCT constant TR-CAL-MAX-PCT

variable TR-CAL-SINK
variable TR-CAL-MEASURED-MS

\ Longest resident/direct phases first; this keeps ARM gates inside budget
\ without dropping coverage or raising the threshold.
create TR-CANDIDATE-HOST-ORDER
$9 , $E ,

create TR-LATE-ORDER
$3 , $15 , $10 ,

create TR-EARLY-HOST-ORDER
$8 , $7 , $25 , $26 , $27 , $28 , $17 , $16 ,
$1B , $C , $11 , $24 , $1F , $23 , $B , $A ,
$20 , $22 ,
$5 , $2 , $1C , $1D , $1A , $21 , $D , $19 ,
$12 ,

create TR-BUILD-CACHE-BUF FS-PATH-CAP allot
create TR-PATH-BUF FS-PATH-CAP allot
create TR-UNDER-BUF FS-PATH-CAP allot
create TR-UNDER-HEX 64 allot
create TR-UNDER-KEY-HEX 80 allot
create TR-RESULT-KEY-HEX 64 allot
create TR-UNDER-ARG-BUF FS-PATH-CAP allot
create TR-UNDER-CACHE-BUF FS-PATH-CAP allot
create TR-UNDER-CACHE-TMP-BUF FS-PATH-CAP allot
create TR-UNDER-CACHE-LOCK-BUF FS-PATH-CAP allot
create TR-UNDER-CACHE-STAMP-BUF FS-PATH-CAP allot
create TR-UNDER-CACHE-STAMP-TMP-BUF FS-PATH-CAP allot
create TR-UNDER-NAME-BUF 80 allot
create TR-UNDER-STAMP-BUF TR-UNDER-STAMP-U allot
create TR-UNDER-STAMP-RD TR-UNDER-STAMP-U allot
create TR-PERSIST-BUF FS-PATH-CAP allot
create TR-NUM-BUF TR-NUM-CAP allot
create TR-HOST-BUF TR-HOST-CAP allot

$8000 constant TR-RED-FILE-CAP
create TR-RED-FILE-BUF TR-RED-FILE-CAP allot
create TR-RED-LIST-PATH-BUF FS-PATH-CAP allot
create TR-RERUN-SET TR-PHASES cells allot

variable TR-BUILD-CACHE-U
variable TR-PATH-U
variable TR-UNDER-U
variable TR-UNDER-ARG-U
variable TR-UNDER-CACHE-U
variable TR-UNDER-CACHE-TMP-U
variable TR-UNDER-CACHE-LOCK-U
variable TR-UNDER-CACHE-STAMP-U
variable TR-UNDER-CACHE-STAMP-TMP-U
variable TR-UNDER-NAME-U
variable TR-PERSIST-U
variable TR-RED-FILE-U
variable TR-RED-LIST-PATH-U
variable TR-RERUN
variable TR-RERUN-N
variable TR-RERUN-POS
variable TR-GATE-START-NS
variable TR-UNDER-READY
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
variable TR-NO-RESULT-CACHE
variable TR-PROFILE-ID
variable TR-NUM-U
variable TR-RESIDENT-ID
variable TR-PRE-CHECK
variable TR-PRE-POST
variable TR-PRE-DICT
variable TR-PRE-TAIL
variable TR-PRE-ARTIFACTS
variable TR-PRE-REPAIR
variable TR-PRE-RUNTIME
variable TR-PRE-VALIDATE
variable TR-PRE-DIAG-GROUP
variable TR-PRE-DIAG-REPAIR
variable TR-PRE-DIAG-UNDEF
variable TR-PRE-DIAG-FILE

: TR-PATH$ ( -- ptr u8 n )
   TR-PATH-BUF TR-PATH-U @ ;

: TR-BUILD-CACHE$ ( -- ptr u8 n )
   TR-BUILD-CACHE-BUF TR-BUILD-CACHE-U @ ;

: TR-UNDER$ ( -- ptr u8 n )
   TR-UNDER-BUF TR-UNDER-U @ ;

: TR-UNDER-ARG$ ( -- ptr u8 n )
   TR-UNDER-ARG-BUF TR-UNDER-ARG-U @ ;

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
   s" usage: bin/hb --load libs test/run.f -- [--under PATH] [--perf-profile NAME|auto] [--pool-slots N] [--nested-pool-slots N] [--budget-ms N] [--wall-budget-ms N] [--cold-cache] [--no-result-cache] [--rerun-failed] [--timings]" TR-USAGE-RC die ;

: TR-ARG$ ( -- ptr u8 n )
   TR-ARG-I @ SCRIPT-ARGV$ ;

: TR-ARG-VALUE$ ( -- ptr u8 n )
   TR-ARG-I @ 1+ SCRIPT-ARGC >= if TR-USAGE then
   TR-ARG-I @ 1+ SCRIPT-ARGV$ ;

: TR-POS-NUM ( ptr u8 n -- n )
   STR>NUMBER? MATCH option
     none OF TR-USAGE ENDOF
     some OF ENDOF
   ;MATCH
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

: TR-NO-RESULT-CACHE-OPT ( -- )
   -1 TR-NO-RESULT-CACHE !
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
   HB-TARGET-MACOS? if TR-PROFILE-MACOS-ARM64-10X2 exit then
   HB-TARGET-LINUX? if
      s" /proc/device-tree/model" EXISTS? if
         TR-JETSON-MODEL? if TR-PROFILE-JETSON-ORIN-CLOCKS-4X2 exit then
      then
      TR-PROFILE-LINUX-ARM64-4X2 exit
   then
   s" no supported timed host profile" TR-PROFILE-FAIL ;

: TR-PROFILE-ID? ( ptr u8 n -- n )
   2dup s" auto" STR= if 2drop TR-DETECT-PROFILE exit then
   2dup s" macos-arm64-10x2" STR= if 2drop TR-PROFILE-MACOS-ARM64-10X2 exit then
   2dup s" jetson-orin-clocks-4x2" STR= if 2drop TR-PROFILE-JETSON-ORIN-CLOCKS-4X2 exit then
   2dup s" linux-arm64-4x2" STR= if 2drop TR-PROFILE-LINUX-ARM64-4X2 exit then
   2drop TR-USAGE ;

: TR-CAL-SPIN ( n -- n )
   T-BUDGET-CAL-SPIN ;

: TR-CALIBRATE ( -- )
   mono-ns {: t0:n :}
   TR-CAL-ITERS TR-CAL-SPIN TR-CAL-SINK !
   mono-ns t0 - PROC-NS-PER-MS / TR-CAL-MEASURED-MS ! ;

: TR-CAL-REF-MS ( -- n )
   TR-PROFILE-ID @ case
      TR-PROFILE-MACOS-ARM64-10X2 of TR-CAL-REF-MACOS-MS endof
      TR-PROFILE-JETSON-ORIN-CLOCKS-4X2 of TR-CAL-REF-JETSON-MS endof
      TR-PROFILE-LINUX-ARM64-4X2 of TR-CAL-REF-LINUX-MS endof
      0 swap
   endcase ;

: TR-CAL-CLAMP ( n -- n ) {: pct:n :}
   pct TR-CAL-MIN-PCT < if TR-CAL-MIN-PCT exit then
   pct TR-CAL-MAX-PCT > if TR-CAL-MAX-PCT exit then
   pct ;

: TR-CAL-PCT ( -- n )
   TR-CAL-REF-MS {: ref:n :}
   ref 0 <= if TR-CAL-MIN-PCT exit then
   TR-CAL-MEASURED-MS @ 0 <= if TR-CAL-MIN-PCT exit then
   TR-CAL-MEASURED-MS @ 100 * ref / TR-CAL-CLAMP ;

: TR-CAL-SCALED ( n -- n )
   TR-CAL-PCT * 100 / ;

: TR-PROFILE-APPLY ( n -- ) {: id:n :}
   id TR-PROFILE-ID !
   0 TR-BUDGET-USER !
   0 TR-WALL-BUDGET-USER !
   id case
      TR-PROFILE-MACOS-ARM64-10X2 of
         10 TR-TOP-POOL-SLOTS!
         2 TR-NESTED-POOL !
         40000 TR-CAL-SCALED TR-BUDGET !
         45000 TR-CAL-SCALED TR-WALL-BUDGET !
      endof
      TR-PROFILE-JETSON-ORIN-CLOCKS-4X2 of
         4 GT-POOL-SLOTS!
         2 TR-NESTED-POOL !
         100000 TR-CAL-SCALED TR-BUDGET !
         110000 TR-CAL-SCALED TR-WALL-BUDGET !
      endof
      TR-PROFILE-LINUX-ARM64-4X2 of
         4 GT-POOL-SLOTS!
         2 TR-NESTED-POOL !
         120000 TR-CAL-SCALED TR-BUDGET !
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
   0 TR-NO-RESULT-CACHE !
   0 TR-RERUN !
   0 TR-UNDER-ARG-U !
   TR-DETECT-PROFILE TR-PROFILE-APPLY ;

: TR-COLD-BUDGET-MS ( -- n )
   TR-PROFILE-ID @ case
      TR-PROFILE-MACOS-ARM64-10X2 of 70000 TR-CAL-SCALED endof
      TR-PROFILE-JETSON-ORIN-CLOCKS-4X2 of 150000 TR-CAL-SCALED endof
      TR-PROFILE-LINUX-ARM64-4X2 of 150000 TR-CAL-SCALED endof
      TR-BUDGET @ swap
   endcase ;

: TR-COLD-WALL-BUDGET-MS ( -- n )
   TR-PROFILE-ID @ case
      TR-PROFILE-MACOS-ARM64-10X2 of 70000 TR-CAL-SCALED endof
      TR-PROFILE-JETSON-ORIN-CLOCKS-4X2 of 160000 TR-CAL-SCALED endof
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

: TR-RERUN-OPT ( -- )
   -1 TR-RERUN !
   1 TR-ADVANCE ;

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
   TR-ARG$ s" --no-result-cache" STR= if TR-NO-RESULT-CACHE-OPT exit then
   TR-ARG$ s" --rerun-failed" STR= if TR-RERUN-OPT exit then
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
      TR-PROFILE-MACOS-ARM64-10X2 of s" macos-arm64-10x2" endof
      TR-PROFILE-JETSON-ORIN-CLOCKS-4X2 of s" jetson-orin-clocks-4x2" endof
      TR-PROFILE-LINUX-ARM64-4X2 of s" linux-arm64-4x2" endof
      s" unknown" rot
   endcase ;

: TR-CACHE-ROOT$ ( -- ptr u8 n )
   TR-COLD-CACHE @ 0 <> if s" scratch" exit then
   s" persistent" ;

: TR-CHECK-MACOS-PROFILE ( -- )
   HB-TARGET-MACOS? 0= if s" macos-arm64-10x2 requires macOS target" TR-PROFILE-FAIL then ;

: TR-CHECK-JETSON-PROFILE ( -- )
   HB-TARGET-LINUX? 0= if s" jetson-orin-clocks-4x2 requires Linux target" TR-PROFILE-FAIL then
   TR-JETSON-MODEL? 0= if s" jetson-orin-clocks-4x2 requires NVIDIA Jetson model" TR-PROFILE-FAIL then
   TR-JETSON-ONLINE? 0= if s" jetson-orin-clocks-4x2 requires CPUs 0-7 online" TR-PROFILE-FAIL then ;

: TR-CHECK-LINUX-PROFILE ( -- )
   HB-TARGET-LINUX? 0= if s" linux-arm64-4x2 requires Linux target" TR-PROFILE-FAIL then ;

: TR-CHECK-PROFILE ( -- )
   TR-PROFILE-ID @ case
      TR-PROFILE-MACOS-ARM64-10X2 of TR-CHECK-MACOS-PROFILE endof
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
   s"  cache-root=" type TR-CACHE-ROOT$ type
   s"  pool=" type GT-POOL-LIMIT @ GT-U-TYPE
   s"  nested=" type TR-NESTED-POOL @ GT-U-TYPE
   s"  cal-ms=" type TR-CAL-MEASURED-MS @ GT-U-TYPE
   s"  cal-factor=" type TR-CAL-PCT GT-U-TYPE s" %" type
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

: TR-TMP-DEFAULT+ ( -- )
   s" HB_TMP" GT-ROOT TR-DEFAULT+ ;

create TR-CAL-PCT-BUF 4 allot

: TR-CAL-PCT-DIGIT! ( n n -- ) {: d:n i:n :}
   d 48 + TR-CAL-PCT-BUF i + c! ;

\ The clamp guarantees 100..300, so the text is always exactly three digits.
: TR-PCT$ ( n -- ptr u8 n ) {: pct:n :}
   pct 100 / 0 TR-CAL-PCT-DIGIT!
   pct 10 / 10 mod 1 TR-CAL-PCT-DIGIT!
   pct 10 mod 2 TR-CAL-PCT-DIGIT!
   TR-CAL-PCT-BUF 3 ;

\ Structural pressure floor: startup calibration runs on an otherwise idle
\ box (cal-factor 100), but the gate's OWN pool oversubscribes it by the
\ nested factor, and in practice merge gating overlaps a SECOND full gate
\ (and often an install) on the same box - suites spawned inside that window
\ run several times slower than the calibration saw. The nested x 100 floor
\ (200%) was MEASURED MARGINAL: four incidents on 2026-07-07 alone killed
\ lib/process-test.f at exactly its 2x-floored 10s budget under merge+worker
\ overlap (throw -2502, WHY-THREW buffers far from caps every time), and the
\ 8000-program sweep experiment pushed past 2x as well. Any nested pool
\ therefore floors at TR-CAL-MAX-PCT (300%): the same worst case the clamp
\ already accepts for the wall budget, so a genuinely hung child still fails
\ within 3x its nominal budget - detection stays bounded. nested=1 setups
\ keep the measured cal-factor alone (no self-contention to cover).
: TR-POOL-PRESSURE-PCT ( -- n )
   TR-NESTED-POOL @ 1 > if TR-CAL-MAX-PCT exit then
   TR-CAL-MIN-PCT ;

: TR-LOAD-PCT-EXPORT ( -- n )
   TR-CAL-PCT {: cal:n :}
   TR-POOL-PRESSURE-PCT {: floor:n :}
   cal floor < if floor exit then
   cal ;

\ Export the load factor to spawned workers so suite budgets
\ (lib/test/budget.f T-BUDGET-MS) scale with the gate's measured calibration
\ and its structural pool pressure; forked/in-process suites read the cell
\ TR-PREPARE sets directly.
: TR-LOAD-PCT-DEFAULT+ ( -- )
   s" HB_LOAD_PCT" TR-LOAD-PCT-EXPORT TR-PCT$ TR-DEFAULT+ ;

: TR-BUILD-CACHE-DEFAULT+ ( -- )
   TR-BUILD-CACHE-PATHS
   s" HABU_BUILD_CACHE" TR-BUILD-CACHE$ TR-DEFAULT+ ;

: TR-STATS-DEFAULT+ ( -- )
   GS-ON? if s" HABU_GATE_STATS" GS-PATH$ TR-DEFAULT+ then ;

: TR-UNDER-PATHS ( -- )
   GT-ROOT s" hb-under-test" TR-UNDER-BUF JOIN-PATH TR-UNDER-U !
   TR-UNDER$ EXISTS? if TR-UNDER$ REMOVE-FILE then
   0 TR-UNDER-READY !
   0 TR-UNDER-CACHE-HIT ! ;

: TR-UNDER-ENV+ ( -- )
   s" HABU_UNDER_TEST" >LEN TR-UNDER$ >LEN PROC-ENV+ ;

: TR-POOL-PASS-SPAN ( ptr u8 n n -- ) {: label:ptr labelu:n ms:n :}
   label labelu ms GS-SPAN-AUTH ;

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
   TRC:RESET
   TR-PERSIST$ TRC:ROOT!
   GT-ROOT GS-ROOT!
   TR-UNDER-PATHS ;

: TR-KEPT-ROOT-LINE ( -- )
   s" capture root kept: " type GT-ROOT type cr ;

: TR-FAIL ( ptr u8 n -- ) {: label:ptr labelu:n :}
   s" FAIL: " type label labelu type cr
   GT-POOL-RED# 0 > if
      GT-POOL-RED-REPORT
      TR-KEPT-ROOT-LINE
      label labelu 1 die
   then
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
   GT-RC@ 0= ;

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

: TR-SUFFIX! ( ptr u8 n ptr u8 n ptr u8 ptr n -- )
   {: a:ptr u:n suf:ptr su:n dst:ptr lenp:ptr :}
   u su + FS-PATH-CAP > if E-FS-PATH throw then
   a dst u BYTE-COPY
   suf dst u + su BYTE-COPY
   u su + lenp ! ;

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

: TR-KEY-FILE+ ( ptr u8 n -- ) {: a:ptr u:n :}
   a u CK-FILE+ ;

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

: TR-BUILD-COMMON ( -- )
   TR-COMMON
   TR-BUILD-ASSERT-LIBS
   s" test/gate-build-common.f"  >LEN PROC-ARGV+ ;

: TR-BUILD-LIB ( -- )
   s" lib/vector.f"  >LEN PROC-ARGV+
   s" lib/source.f"  >LEN PROC-ARGV+
   s" lib/build.f"  >LEN PROC-ARGV+
   s" lib/codesign.f"  >LEN PROC-ARGV+
   s" lib/object.f"  >LEN PROC-ARGV+
   s" lib/object-cache.f"  >LEN PROC-ARGV+
   s" lib/object-index.f"  >LEN PROC-ARGV+
   s" lib/object-resolve.f"  >LEN PROC-ARGV+
   s" lib/object-link.f"  >LEN PROC-ARGV+
   s" tools/lint/text.f"  >LEN PROC-ARGV+
   s" tools/lint/token.f"  >LEN PROC-ARGV+
   s" tools/lint/lib.f"  >LEN PROC-ARGV+
   s" tools/lint/json-writer.f"  >LEN PROC-ARGV+
   s" tools/lint/source-lex.f"  >LEN PROC-ARGV+
   s" tools/aot-lint-core.f"  >LEN PROC-ARGV+
   s" tools/signature-lint-core.f"  >LEN PROC-ARGV+
   s" tools/build-fixpoint.f"  >LEN PROC-ARGV+
   s" tools/cli-run.f"  >LEN PROC-ARGV+
   s" tools/object-image.f"  >LEN PROC-ARGV+
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

: TR-UNSCHEDULED-PHASE ( -- )
   E-TBL-BOUNDS throw ;

: TR-PHASE-LABEL ( idx -- ptr u8 n ) {: idx:idx :}
   idx IDX>N case
      0 of s" unused legacy phase 0" endof
      1 of s" unused legacy phase 1" endof
      2 of s" native stdlib trust tool slice" endof
      3 of s" native stdlib check-cli slice" endof
      4 of s" native stdlib tail slice" endof
      5 of s" native engine repair slice" endof
      6 of s" native prop/debug gate phase" endof
      7 of s" native hb-build AOT positive gate phase" endof
      8 of s" native hb-build AOT negative gate phase" endof
      9 of s" native engine post-candidate group" endof
      10 of s" native checker diagnostics repair slice" endof
      11 of s" native checker diagnostics undef-primary slice" endof
      12 of s" native checker diagnostics group" endof
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
      30 of s" unused retired phase 30" endof
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
      0 of s" gate-unused-0" endof
      1 of s" gate-unused-1" endof
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
      30 of s" gate-unused-30" endof
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
      0 of TR-UNSCHEDULED-PHASE endof
      1 of TR-UNSCHEDULED-PHASE endof
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
      30 of TR-UNSCHEDULED-PHASE endof
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
      30 of TR-UNSCHEDULED-PHASE endof
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
      26 of TR-TRUE endof
      27 of TR-TRUE endof
      28 of TR-TRUE endof
      29 of TR-TRUE endof
      31 of TR-TRUE endof
      32 of TR-TRUE endof
      33 of TR-TRUE endof
      34 of TR-TRUE endof
      35 of TR-TRUE endof
      36 of TR-TRUE endof
      37 of TR-TRUE endof
      38 of TR-TRUE endof
      39 of TR-TRUE endof
      40 of TR-TRUE endof
      TR-FALSE swap
   endcase ;

\ Phases whose fork inherits the parent shared tool base: stdlib slices plus
\ the dictionary/checker and diagnostics families, whose require lists dedupe
\ against the base so only their gate-lib deltas load after the fork.
: TR-SHARED-BASE? ( idx -- bool ) {: idx:idx :}
   idx TR-STDLIB-SLICE? if TR-TRUE exit then
   idx IDX>N case
      10 of TR-TRUE endof
      11 of TR-TRUE endof
      12 of TR-TRUE endof
      13 of TR-TRUE endof
      14 of TR-TRUE endof
      TR-FALSE swap
   endcase ;

: TR-PHASE-POOL-ARGS ( idx -- ) {: idx:idx :}
   idx TR-STDLIB-SLICE? if
      TR-NESTED-POOL @ TR-POOL-ARG+
      exit
   then
   idx IDX>N case
      9 of TR-NESTED-POOL @ TR-POOL-ARG+ endof
   endcase ;

: TR-PHASE-TOOLS-ENV ( idx -- )
   drop ;

: TR-PHASE-UNDER? ( idx -- bool ) {: idx:idx :}
   idx IDX>N case
      3 of TR-TRUE endof
      14 of TR-TRUE endof
      16 of TR-TRUE endof
      21 of TR-TRUE endof
      TR-FALSE swap
   endcase ;

: TR-PHASE-UNDER-BUILD? ( idx -- bool ) {: idx:idx :}
   idx IDX>N 15 = ;

: TR-PHASE-UNDER-ENV? ( idx -- bool ) {: idx:idx :}
   idx TR-PHASE-UNDER-BUILD? if TR-TRUE exit then
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
      15 of TR-FALSE endof
      19 of TR-FALSE endof
      20 of TR-FALSE endof
      TR-TRUE swap
   endcase ;

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

: TR-PHASE-BASE ( idx -- ) {: idx:idx :}
   PROC-ARGV-RESET
   PROC-ENV-RESET
   idx TR-PHASE-TMP!
   s" HB_TMP" >LEN TR-PATH$ >LEN PROC-ENV+
   idx TR-PHASE-TOOLS-ENV
   TR-BUILD-CACHE-ENV
   GS-ENV+
   idx TR-PHASE-UNDER-ENV
   PROC-ENV-INHERIT-MISSING
   TR-PHASE-ARGV-COLD ;

: TR-PHASE-START ( idx -- ) {: idx:idx :}
   idx TR-PHASE-BASE
   idx TR-PHASE-ARGS
   idx TR-PHASE-POOL-ARGS
   idx TR-PHASE-TIMINGS-ARGS
   s" top-phase-spawn" GS-EVENT
   idx TR-PHASE-UNDER-EXE? if s" under-phase-spawn" GS-EVENT then
   idx TR-PHASE-TEST
   idx TR-PHASE-EXE idx TR-PHASE-LABEL TR-TIMEOUT-MS GT-POOL-START ;

: TR-PHASE-START-SLOT ( idx idx -- ) {: idx:idx slot:idx :}
   idx TR-PHASE-BASE
   idx TR-PHASE-ARGS
   idx TR-PHASE-POOL-ARGS
   idx TR-PHASE-TIMINGS-ARGS
   s" top-phase-spawn" GS-EVENT
   idx TR-PHASE-UNDER-EXE? if s" under-phase-spawn" GS-EVENT then
   idx TR-PHASE-TEST
   idx TR-PHASE-EXE idx TR-PHASE-LABEL TR-TIMEOUT-MS slot GT-POOL-START-SLOT ;

: TR-GROUP-MODE ( idx -- n )
   drop TR-GROUP-PAR ;

: TR-GROUP-SEQ? ( idx -- bool )
   TR-GROUP-MODE TR-GROUP-SEQ = ;

: TR-UNDER-DONE? ( -- bool )
   TR-UNDER$ EXECUTABLE? ;

: TR-UNDER-MISSING-FAIL ( -- )
   s" missing Habu-under-test after build pool drained: " type TR-UNDER$ type cr
   s" Habu-under-test build artifact missing" TR-FAIL ;

: TR-DRAIN-UNTIL-UNDER ( -- )
   begin TR-UNDER-DONE? 0= while
      GT-POOL-LIVE @ 0= if TR-UNDER-MISSING-FAIL then
      GT-POOL-STEP
   repeat
   TR-EXPECT-UNDER
   TR-UNDER-CACHE-INSTALL ;

: TR-CANDIDATE-HOST-ORDER@ ( idx -- idx ) {: idx:idx :}
   idx IDX>N cells TR-CANDIDATE-HOST-ORDER + @ >IDX ;

: TR-LATE-ORDER@ ( idx -- idx ) {: idx:idx :}
   idx IDX>N cells TR-LATE-ORDER + @ >IDX ;

: TR-EARLY-HOST-ORDER@ ( idx -- idx ) {: idx:idx :}
   idx IDX>N cells TR-EARLY-HOST-ORDER + @ >IDX ;

\ Per-phase content-keyed PASS-stamp cache. A phase with a declared file set
\ (test/run-files.f) keys (label, bin/hb, candidate sha for under phases,
\ declared files); a stamp hit skips the phase as PASS (cached). Misses are
\ recorded and stamped only after a fully green run; --cold-cache and
\ --no-result-cache bypass both sides.
: TR-RESULT-CACHE-ON? ( -- bool )
   TR-COLD-CACHE @ 0 <> if TR-FALSE exit then
   TR-NO-RESULT-CACHE @ 0 <> if TR-FALSE exit then
   TRC:ROOT? ;

: TR-RESULT-BASE-KEY ( -- )
   [: TR-KEY-FILE+ ;] TR-GATE-HARNESS-FILES
   [: TR-KEY-FILE+ ;] TR-GATE-COMMON-FILES ;

: TR-RESULT-KEY-FILES? ( idx -- bool ) {: idx:idx :}
   idx IDX>N case
      6 of TR-RESULT-BASE-KEY [: TR-KEY-FILE+ ;] TR-DEBUG-PHASE-FILES TR-TRUE endof
      8 of TR-RESULT-BASE-KEY [: TR-KEY-FILE+ ;] TR-AOT-NEG-PHASE-FILES TR-TRUE endof
      TR-FALSE swap
   endcase ;

: TR-RESULT-UNDER-KEY? ( idx -- bool ) {: idx:idx :}
   idx TR-PHASE-UNDER? 0= if TR-TRUE exit then
   TR-UNDER-READY @ 0= if TR-FALSE exit then
   TR-UNDER-SHA!
   TR-UNDER-HEX 64 CK-TEXT+
   TR-TRUE ;

: TR-RESULT-KEY? ( idx -- bool ) {: idx:idx :}
   CK-RESET
   s" gate-phase-pass-v1" CK-TEXT+
   idx TR-PHASE-LABEL CK-TEXT+
   s" bin/hb" TR-KEY-FILE+
   idx TR-RESULT-UNDER-KEY? 0= if TR-FALSE exit then
   idx TR-RESULT-KEY-FILES? 0= if TR-FALSE exit then
   TR-RESULT-KEY-HEX CK-FINAL-HEX
   TR-TRUE ;

: TR-RESULT-CACHED? ( idx -- bool ) {: idx:idx :}
   TR-RESULT-CACHE-ON? 0= if TR-FALSE exit then
   idx TR-RESULT-KEY? 0= if TR-FALSE exit then
   TR-RESULT-KEY-HEX TRC:HIT? if TR-TRUE exit then
   s" result-cache-miss" GS-EVENT
   idx IDX>N TR-RESULT-KEY-HEX TRC:PENDING+
   TR-FALSE ;

: TR-RESULT-SKIP ( idx -- ) {: idx:idx :}
   s" result-cache-hit" GS-EVENT
   s" PASS (cached): " type idx TR-PHASE-LABEL type cr ;

: TR-RESULT-STAMP-I ( n -- ) {: i:n :}
   i TRC:PENDING-PHASE >IDX TR-PHASE-LABEL i TRC:PENDING-KEY TRC:STAMP+ ;

: TR-RESULT-STAMPS ( -- )
   TR-RESULT-CACHE-ON? 0= if exit then
   GT-POOL-RED# 0 > if exit then
   0 begin dup TRC:PENDING# < while
      dup TR-RESULT-STAMP-I
      1+
   repeat drop ;

: TR-PRE-RESET ( -- )
   0 TR-PRE-CHECK !
   0 TR-PRE-POST !
   0 TR-PRE-DICT !
   0 TR-PRE-TAIL !
   0 TR-PRE-ARTIFACTS !
   0 TR-PRE-REPAIR !
   0 TR-PRE-RUNTIME !
   0 TR-PRE-VALIDATE !
   0 TR-PRE-DIAG-GROUP !
   0 TR-PRE-DIAG-REPAIR !
   0 TR-PRE-DIAG-UNDEF !
   0 TR-PRE-DIAG-FILE ! ;

: TR-PRE? ( idx -- bool ) {: idx:idx :}
   idx IDX>N case
      3 of TR-PRE-CHECK @ 0 <> endof
      4 of TR-PRE-TAIL @ 0 <> endof
      5 of TR-PRE-REPAIR @ 0 <> endof
      9 of TR-PRE-POST @ 0 <> endof
      10 of TR-PRE-DIAG-REPAIR @ 0 <> endof
      11 of TR-PRE-DIAG-UNDEF @ 0 <> endof
      12 of TR-PRE-DIAG-GROUP @ 0 <> endof
      13 of TR-PRE-DIAG-FILE @ 0 <> endof
      14 of TR-PRE-DICT @ 0 <> endof
      16 of TR-PRE-RUNTIME @ 0 <> endof
      19 of TR-PRE-ARTIFACTS @ 0 <> endof
      21 of TR-PRE-VALIDATE @ 0 <> endof
      TR-FALSE swap
   endcase ;

: TR-PRE-MARK ( idx -- ) {: idx:idx :}
   idx IDX>N case
      3 of -1 TR-PRE-CHECK ! endof
      4 of -1 TR-PRE-TAIL ! endof
      5 of -1 TR-PRE-REPAIR ! endof
      9 of -1 TR-PRE-POST ! -1 TR-PRE-REPAIR ! -1 TR-PRE-RUNTIME ! -1 TR-PRE-VALIDATE ! endof
      12 of -1 TR-PRE-DIAG-GROUP ! -1 TR-PRE-DIAG-REPAIR ! -1 TR-PRE-DIAG-UNDEF ! -1 TR-PRE-DIAG-FILE ! endof
      14 of -1 TR-PRE-DICT ! endof
      16 of -1 TR-PRE-RUNTIME ! endof
      19 of -1 TR-PRE-ARTIFACTS ! endof
      21 of -1 TR-PRE-VALIDATE ! endof
   endcase ;

: TR-PRE-START ( idx -- ) {: idx:idx :}
   idx TR-PHASE-START
   idx TR-PRE-MARK ;

: TR-PRE-TOOLS-START ( -- )
   ;

: TR-PRE-CANDIDATE-START ( -- )
   ;

\ --rerun-failed: a failing gate run persists its red top-level phases (phase
\ index plus the exact standalone repro command) under TR-PERSIST$; a later
\ --rerun-failed run reads that list and schedules only those phases.

: TR-RED-LIST$ ( -- ptr u8 n )
   TR-PERSIST$ s" gate-red-phases.txt" TR-RED-LIST-PATH-BUF JOIN-PATH TR-RED-LIST-PATH-U !
   TR-RED-LIST-PATH-BUF TR-RED-LIST-PATH-U @ ;

: TR-RED-FILE+ ( ptr u8 n -- )
   TR-RED-FILE-BUF TR-RED-FILE-CAP TR-RED-FILE-U BUF-APPEND ;

: TR-RED-FILE-C+ ( n -- )
   TR-RED-FILE-BUF TR-RED-FILE-CAP TR-RED-FILE-U BUF-APPEND-C ;

\ Rebuild a phase's argv into PROC-ARGV so the persisted repro line matches
\ what TR-PHASE-START would spawn; env-only temp paths stay out of the argv.
: TR-REPRO-BUILD ( idx -- ) {: idx:idx :}
   PROC-ARGV-RESET
   TR-PHASE-ARGV-COLD
   idx TR-PHASE-ARGS
   idx TR-PHASE-POOL-ARGS ;

\ PROC-ARGV entries are null-terminated and laid out back-to-back in
\ PROC-ARGV-BUF; render them as a space-separated command line by turning each
\ terminator into a space.
: TR-REPRO-ARGS+ ( -- )
   PROC-ARGV-OFF @ OFF>N {: off:n :}
   0 begin dup off < while
      PROC-ARGV-BUF over + c@
      dup 0= if drop $20 then TR-RED-FILE-C+
      1+
   repeat drop ;

: TR-RED-LINE+ ( idx -- ) {: idx:idx :}
   idx IDX>N TR-NUM$ TR-RED-FILE+
   $9 TR-RED-FILE-C+
   idx TR-PHASE-EXE TR-RED-FILE+
   $20 TR-RED-FILE-C+
   idx TR-REPRO-BUILD
   TR-REPRO-ARGS+
   $A TR-RED-FILE-C+ ;

: TR-LABEL>IDX ( ptr u8 n -- idx bool ) {: a:ptr u:n :}
   0 begin dup TR-PHASES < while
      dup >IDX TR-PHASE-LABEL a u STR= if >IDX TR-TRUE exit then
      1+
   repeat drop 0 >IDX TR-FALSE ;

: TR-RED-PERSIST-ENTRY ( n -- ) {: i:n :}
   i GT-POOL-RED-LABEL$ TR-LABEL>IDX if TR-RED-LINE+ else drop then ;

: TR-RED-PERSIST ( -- )
   TR-PERSIST? 0= if exit then
   TR-RED-FILE-U BUF-RESET
   0 begin dup GT-POOL-RED-DETAILED < while
      dup TR-RED-PERSIST-ENTRY
      1+
   repeat drop
   TR-RED-LIST$ TR-RED-FILE-BUF TR-RED-FILE-U @ LEN>N WRITE-ALL
   s" red phase list written: " type TR-RED-LIST$ type cr ;

: TR-RR-SLOT ( idx -- ptr a ) {: idx:idx :}
   idx IDX>N 0 < if E-TBL-BOUNDS throw then
   idx IDX>N TR-PHASES >= if E-TBL-BOUNDS throw then
   idx IDX>N cells TR-RERUN-SET + ;

: TR-RR-MARKED? ( idx -- bool )
   TR-RR-SLOT @ 0 <> ;

: TR-RR-MARK ( idx -- )
   -1 swap TR-RR-SLOT ! ;

: TR-RR-CLEAR ( -- )
   0 begin dup TR-PHASES < while
      0 over >IDX TR-RR-SLOT !
      1+
   repeat drop
   0 TR-RERUN-N ! ;

: TR-RERUN-SKIP? ( idx -- bool ) {: idx:idx :}
   TR-RERUN @ 0= if TR-FALSE exit then
   idx TR-RR-MARKED? 0= ;

: TR-FIELD0$ ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :}
   0 begin dup u < while
      dup a + c@ $9 = if a swap exit then
      1+
   repeat drop a u ;

: TR-RERUN-LINE ( ptr u8 n -- ) {: a:ptr u:n :}
   u 0= if exit then
   a u TR-FIELD0$ STR>NUMBER? MATCH option
     none OF exit ENDOF
     some OF ENDOF
   ;MATCH
   {: v:n :}
   v 0 < if exit then
   v TR-PHASES >= if exit then
   v >IDX TR-RR-MARKED? if exit then
   v >IDX TR-RR-MARK
   TR-RERUN-N @ 1+ TR-RERUN-N ! ;

: TR-RERUN-LINES ( ptr u8 n -- ) {: a:ptr u:n :}
   0 TR-RERUN-POS !
   begin TR-RERUN-POS @ u <= while
      a u $A TR-RERUN-POS @ SPLIT-NEXT drop {: fa:ptr fu:n next:n :}
      fa fu TR-RERUN-LINE
      next TR-RERUN-POS !
   repeat ;

: TR-RERUN-LOAD ( -- )
   TR-RR-CLEAR
   TR-RED-LIST$ EXISTS? 0= if
      s" --rerun-failed: no red phase list at " type TR-RED-LIST$ type cr
      s" no red phase list; run the gate first" TR-USAGE-RC die
   then
   TR-RED-LIST$ TR-RED-FILE-BUF TR-RED-FILE-CAP READ-ALL
   TR-RED-FILE-BUF swap TR-RERUN-LINES ;

: TR-RERUN-MAYBE-LOAD ( -- )
   TR-RERUN @ 0= if exit then
   TR-RERUN-LOAD
   s" --rerun-failed: " type TR-RERUN-N @ TR-NUM$ type
   s"  phase(s) from " type TR-RED-LIST$ type cr ;

: TR-EARLY-EXTERNAL-START ( -- )
   GT-POOL-RESET
   TR-PRE-TOOLS-START
   TR-PRE-CANDIDATE-START
   TR-UNDER-READY @ 0= if
      15 >IDX TR-PHASE-START
   else
      s" candidate-build-skip" GS-EVENT
   then ;

: TR-PREPARE ( -- )
   TR-CALIBRATE
   TR-GATE-START!
   TR-CHECK-ARGS
   TR-CHECK-PROFILE
   TR-LOAD-PCT-EXPORT T-BUDGET-PCT !
   TR-START
   TR-PRE-RESET
   TR-EXPECT-HB
   TR-UNDER-IMPORT
   TR-UNDER-CACHE-RESTORE
   TR-RERUN-MAYBE-LOAD ;

: TR-RED-COMPLETE ( -- )
   TR-RED-PERSIST
   GT-POOL-RED-REPORT
   TR-KEPT-ROOT-LINE
   s" native test suite phases failed" 1 die ;

: TR-COMPLETE ( -- )
   GS-SUMMARY
   GT-POOL-RED# 0 > if TR-RED-COMPLETE then
   GS-LABEL-DUP-GUARD
   TR-RESULT-STAMPS
   GT-CLEANUP
   TR-FINISH ;
