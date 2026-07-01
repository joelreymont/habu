\ spsc-motion-scenario.f - Habu-owned four-camera SPSC motion runner.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/render.f
require lib/float.f
require lib/time.f
require lib/task.f
require lib/process-argv.f
require odin/spsc-motion.f

package SPSCEN
private

64 constant SS-USAGE-RC
1024 constant SS-PATH-CAP
256 constant SS-TEXT-CAP
$10000 constant SS-READ-CAP
1000000000 constant SS-NS-PER-S
-1 constant SS-SKIPPED

create SS-ROOT-BUF SS-PATH-CAP allot
create SS-SCENARIO-BUF SS-TEXT-CAP allot
create SS-TAG-BUF SS-TEXT-CAP allot
create SS-OUTPUT-ID-BUF SS-TEXT-CAP allot
create SS-HB-BUF SS-PATH-CAP allot
create SS-OUTPUT-ROOT-BUF SS-PATH-CAP allot
create SS-BASELINE-ROOT-BUF SS-PATH-CAP allot
create SS-BASELINE-SUFFIX-BUF SS-TEXT-CAP allot
create SS-PRE-CUE-BUF SS-TEXT-CAP allot
create SS-P0 SS-PATH-CAP allot
create SS-P1 SS-PATH-CAP allot
create SS-P2 SS-PATH-CAP allot
create SS-READ-BUF SS-READ-CAP allot

create SS-CAPTURE-STATUS 4 cells allot
create SS-MOTION-STATUS 4 cells allot
create SS-CAPTURE-PIDS 4 cells allot

create SS-TH-ACC-RMS 1 cells allot
create SS-TH-GYR-RMS 1 cells allot
create SS-TH-ACC-RATIO 1 cells allot
create SS-TH-GYR-RATIO 1 cells allot

variable SS-ARG-I
variable SS-ROOT-U
variable SS-SCENARIO-U
variable SS-TAG-U
variable SS-OUTPUT-ID-U
variable SS-HB-U
variable SS-OUTPUT-ROOT-U
variable SS-BASELINE-ROOT-U
variable SS-BASELINE-SUFFIX-U
variable SS-PRE-CUE-U
variable SS-SAMPLES
variable SS-TIMEOUT-MS
variable SS-POLL-MS
variable SS-WINDOW-MS
variable SS-PRE-DELAY-S
variable SS-START-TIMER
variable SS-REQUIRE-READY
variable SS-DRY-RUN
variable SS-FIRST-FAILURE
variable SS-SUMMARY-STATUS
variable SS-HAVE-TH-ACC-RMS
variable SS-HAVE-TH-GYR-RMS
variable SS-HAVE-TH-ACC-RATIO
variable SS-HAVE-TH-GYR-RATIO
variable SS-I
variable SS-ACTIVE
variable SS-RUN-STATUS

: SS-TRUE ( -- bool ) true ;
: SS-FALSE ( -- bool ) false ;

: SS-COPY! ( ptr u8 n ptr u8 n ptr a -- )
   {: a:ptr u:n dst:ptr cap:n lenp:ptr :}
   u 0 <= if s" empty option value" SS-USAGE-RC die then
   u cap >= if E-STR-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: ROOT$ ( -- ptr u8 n ) SS-ROOT-BUF SS-ROOT-U @ ;
: SCENARIO$ ( -- ptr u8 n ) SS-SCENARIO-BUF SS-SCENARIO-U @ ;
: TAG$ ( -- ptr u8 n ) SS-TAG-BUF SS-TAG-U @ ;
: OUTPUT-ID$ ( -- ptr u8 n ) SS-OUTPUT-ID-BUF SS-OUTPUT-ID-U @ ;
: HB$ ( -- ptr u8 n ) SS-HB-BUF SS-HB-U @ ;
: OUTPUT-ROOT$ ( -- ptr u8 n ) SS-OUTPUT-ROOT-BUF SS-OUTPUT-ROOT-U @ ;
: BASELINE-ROOT$ ( -- ptr u8 n ) SS-BASELINE-ROOT-BUF SS-BASELINE-ROOT-U @ ;
: BASELINE-SUFFIX$ ( -- ptr u8 n ) SS-BASELINE-SUFFIX-BUF SS-BASELINE-SUFFIX-U @ ;
: PRE-CUE$ ( -- ptr u8 n ) SS-PRE-CUE-BUF SS-PRE-CUE-U @ ;

: SET-ROOT ( ptr u8 n -- ) SS-ROOT-BUF SS-PATH-CAP SS-ROOT-U SS-COPY! ;
: SET-SCENARIO ( ptr u8 n -- ) SS-SCENARIO-BUF SS-TEXT-CAP SS-SCENARIO-U SS-COPY! ;
: SET-TAG ( ptr u8 n -- ) SS-TAG-BUF SS-TEXT-CAP SS-TAG-U SS-COPY! ;
: SET-OUTPUT-ID ( ptr u8 n -- ) SS-OUTPUT-ID-BUF SS-TEXT-CAP SS-OUTPUT-ID-U SS-COPY! ;
: SET-HB ( ptr u8 n -- ) SS-HB-BUF SS-PATH-CAP SS-HB-U SS-COPY! ;
: SET-OUTPUT-ROOT ( ptr u8 n -- ) SS-OUTPUT-ROOT-BUF SS-PATH-CAP SS-OUTPUT-ROOT-U SS-COPY! ;
: SET-BASELINE-ROOT ( ptr u8 n -- ) SS-BASELINE-ROOT-BUF SS-PATH-CAP SS-BASELINE-ROOT-U SS-COPY! ;
: SET-BASELINE-SUFFIX ( ptr u8 n -- ) SS-BASELINE-SUFFIX-BUF SS-TEXT-CAP SS-BASELINE-SUFFIX-U SS-COPY! ;
: SET-PRE-CUE ( ptr u8 n -- ) SS-PRE-CUE-BUF SS-TEXT-CAP SS-PRE-CUE-U SS-COPY! ;

: A@ ( ptr a n -- n ) {: base:ptr ix:n :} base ix cells + @ ;
: A! ( n ptr a n -- ) {: v:n base:ptr ix:n :} v base ix cells + ! ;
: PID-A@ ( ptr a n -- pid ) A@ >PID ;
: PID-A! ( pid ptr a n -- ) {: pid:pid base:ptr ix:n :} pid PID>N base ix A! ;
: F!1 ( r ptr a -- ) F! ;
: F@1 ( ptr a -- r ) F@ ;

: CAMERA$ ( n -- ptr u8 n )
   dup 0 = if drop s" cam_a0" exit then
   dup 1 = if drop s" cam_a1" exit then
   dup 2 = if drop s" cam_b0" exit then
   drop s" cam_b1" ;

: DEVICE$ ( n -- ptr u8 n )
   dup 0 = if drop s" /dev/spsc_bmi0" exit then
   dup 1 = if drop s" /dev/spsc_bmi1" exit then
   dup 2 = if drop s" /dev/spsc_bmi2" exit then
   drop s" /dev/spsc_bmi3" ;

: NAME-CHAR? ( n -- bool )
   {: c:n :}
   c 48 >= c 57 <= and c 65 >= c 90 <= and or c 97 >= c 122 <= and or
   c 45 = or c 46 = or c 95 = or ;

: NAME? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u 0 <= if SS-FALSE exit then
   a c@ NAME-CHAR? 0= if SS-FALSE exit then
   0 SS-I !
   begin SS-I @ u < while
      a SS-I @ + c@ NAME-CHAR? 0= if SS-FALSE exit then
      SS-I @ 1+ SS-I !
   repeat
   SS-TRUE ;

: CHECK-NAME ( ptr u8 n -- )
   NAME? 0= if s" invalid scenario/tag/output name" SS-USAGE-RC die then ;

: OUT-C ( n -- )
   SS-OUTPUT-ID-U @ SS-TEXT-CAP >= if E-STR-CAPACITY throw then
   SS-OUTPUT-ID-BUF SS-OUTPUT-ID-U @ + c!
   SS-OUTPUT-ID-U @ 1+ SS-OUTPUT-ID-U ! ;

: OUT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   0 SS-I !
   begin SS-I @ u < while
      a SS-I @ + c@ OUT-C
      SS-I @ 1+ SS-I !
   repeat ;

: BUILD-OUTPUT-ID ( -- )
   0 SS-OUTPUT-ID-U !
   SCENARIO$ OUT+
   95 OUT-C
   TAG$ OUT+ ;

: DEFAULTS ( -- )
   0 SS-ARG-I !
   0 SS-SCENARIO-U !
   0 SS-OUTPUT-ID-U !
   0 SS-PRE-CUE-U !
   s" ../Odin" SET-ROOT
   s" ../habu/bin/hb" SET-HB
   s" manual" SET-TAG
   s" results/imu" SET-OUTPUT-ROOT
   s" results/imu" SET-BASELINE-ROOT
   s" spsc_static_20260624_0502_CEST" SET-BASELINE-SUFFIX
   1000 SS-SAMPLES !
   8000 SS-TIMEOUT-MS !
   50 SS-POLL-MS !
   250 SS-WINDOW-MS !
   0 SS-PRE-DELAY-S !
   1 SS-START-TIMER !
   0 SS-REQUIRE-READY !
   0 SS-DRY-RUN !
   0 SS-FIRST-FAILURE !
   0 SS-SUMMARY-STATUS !
   0 SS-HAVE-TH-ACC-RMS !
   0 SS-HAVE-TH-GYR-RMS !
   0 SS-HAVE-TH-ACC-RATIO !
   0 SS-HAVE-TH-GYR-RATIO !
   0 SS-I !
   begin SS-I @ 4 < while
      SS-SKIPPED SS-CAPTURE-STATUS SS-I @ A!
      SS-SKIPPED SS-MOTION-STATUS SS-I @ A!
      -1 >PID SS-CAPTURE-PIDS SS-I @ PID-A!
      SS-I @ 1+ SS-I !
   repeat ;

: ABSOLUTE? ( ptr u8 n -- bool ) {: a:ptr u:n :} u 0 > if a c@ 47 = else SS-FALSE then ;

: ROOTED+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u ABSOLUTE? 0= if
      ROOT$ SB-APPEND
      ROOT$ dup 0 > if 1- + c@ 47 <> if 47 SB-APPEND-C then else 2drop then
   then
   a u SB-APPEND ;

: ROOTED$ ( ptr u8 n -- ptr u8 n )
   SB-RESET ROOTED+ SB$ ;

: OUTPUT-ROOTED$ ( -- ptr u8 n ) OUTPUT-ROOT$ ROOTED$ ;
: BASELINE-ROOTED$ ( -- ptr u8 n ) BASELINE-ROOT$ ROOTED$ ;

: COPY-P0 ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :} a SS-P0 u BYTE-COPY SS-P0 u ;
: COPY-P1 ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :} a SS-P1 u BYTE-COPY SS-P1 u ;
: COPY-P2 ( ptr u8 n -- ptr u8 n ) {: a:ptr u:n :} a SS-P2 u BYTE-COPY SS-P2 u ;

: CAPTURE-DIR$ ( n -- ptr u8 n )
   {: ix:n :}
   SB-RESET
   OUTPUT-ROOT$ ROOTED+
   47 SB-APPEND-C
   ix CAMERA$ SB-APPEND
   s" _spsc_" SB-APPEND
   OUTPUT-ID$ SB-APPEND
   SB$ COPY-P0 ;

: MOTION-ROOT$ ( -- ptr u8 n )
   SB-RESET
   OUTPUT-ROOT$ ROOTED+
   s" /spsc_motion_" SB-APPEND
   OUTPUT-ID$ SB-APPEND
   SB$ COPY-P1 ;

: MOTION-DIR$ ( n -- ptr u8 n )
   {: ix:n :}
   MOTION-ROOT$ ix CAMERA$ SS-P2 JOIN-PATH SS-P2 swap ;

: BASELINE$ ( n -- ptr u8 n )
   {: ix:n :}
   SB-RESET
   BASELINE-ROOT$ ROOTED+
   47 SB-APPEND-C
   ix CAMERA$ SB-APPEND
   95 SB-APPEND-C
   BASELINE-SUFFIX$ SB-APPEND
   s" /imu.ndjson" SB-APPEND
   SB$ COPY-P2 ;

: IMU-NDJSON$ ( n -- ptr u8 n )
   CAPTURE-DIR$ s" imu.ndjson" SS-P2 JOIN-PATH SS-P2 swap ;

: READINESS$ ( n -- ptr u8 n )
   MOTION-DIR$ s" readiness.csv" SS-P2 JOIN-PATH SS-P2 swap ;

: RUNNER-SUMMARY$ ( -- ptr u8 n )
   MOTION-ROOT$ s" runner_summary.md" SS-P2 JOIN-PATH SS-P2 swap ;

: FIRST-FAIL ( n -- )
   dup SS-SKIPPED = if drop exit then
   dup 0 <> SS-FIRST-FAILURE @ 0= and if SS-FIRST-FAILURE ! else drop then ;

: CHECK-PREFLIGHT ( -- )
   HB$ EXECUTABLE? 0= if s" missing Habu engine" 2 die then
   s" odin/spsc-imu-cli.f" FILE? 0= if s" missing SPSC IMU CLI" 2 die then
   0 SS-I !
   begin SS-I @ 4 < while
      SS-I @ DEVICE$ EXISTS? 0= if s" missing SPSC device" 2 die then
      SS-I @ BASELINE$ FILE? 0= if s" missing SPSC baseline" 2 die then
      SS-I @ 1+ SS-I !
   repeat ;

: CONFIG-MOTION ( n -- )
   {: ix:n :}
   SPMOT:RESET
   ix IMU-NDJSON$ SPMOT:INPUT!
   ix BASELINE$ SPMOT:BASELINE!
   ix MOTION-DIR$ SPMOT:OUTPUT!
   SS-WINDOW-MS @ s>f SPMOT:WINDOW-MS!
   SS-HAVE-TH-ACC-RMS @ if SS-TH-ACC-RMS F@1 SPMOT:MAX-ACCEL-DYNAMIC-RMS! then
   SS-HAVE-TH-GYR-RMS @ if SS-TH-GYR-RMS F@1 SPMOT:MAX-GYRO-DYNAMIC-RMS! then
   SS-HAVE-TH-ACC-RATIO @ if SS-TH-ACC-RATIO F@1 SPMOT:MAX-ACCEL-DYNAMIC-RATIO! then
   SS-HAVE-TH-GYR-RATIO @ if SS-TH-GYR-RATIO F@1 SPMOT:MAX-GYRO-DYNAMIC-RATIO! then
   SS-REQUIRE-READY @ if SPMOT:REQUIRE-READY! then ;

: ARG+ ( ptr u8 n -- )
   >LEN PROC-ARGV+ ;

: ARG# ( n -- )
   RB-RESET RB# RB$ ARG+ ;

: CAPTURE-ARGS ( n -- )
   {: ix:n :}
   PROC-ARGV-RESET
   s" --load" ARG+
   s" odin/spsc-imu-cli.f" ARG+
   s" --" ARG+
   s" --device" ARG+ ix DEVICE$ ARG+
   s" --logical" ARG+ ix CAMERA$ ARG+
   s" --output" ARG+ ix CAPTURE-DIR$ ARG+
   s" --samples" ARG+ SS-SAMPLES @ ARG#
   s" --timeout-ms" ARG+ SS-TIMEOUT-MS @ ARG#
   s" --poll-ms" ARG+ SS-POLL-MS @ ARG#
   SS-START-TIMER @ if s" --start" else s" --no-start" then ARG+ ;

: START-CAPTURE ( n -- )
   {: ix:n :}
   ix CAPTURE-ARGS
   HB$ >LEN -1 >FD -1 >FD -1 >FD PROC-SPAWN-ARGV-IO
   SS-CAPTURE-PIDS ix PID-A! ;

: WAIT-CAPTURE ( n -- n )
   SS-CAPTURE-PIDS swap PID-A@ PROC-WAIT-RC RC>N ;

: RUN-MOTION ( n -- n )
   {: ix:n :}
   ix CONFIG-MOTION
   SPMOT:RUN ;

: CAPTURE-CALL ( -- )
   SS-ACTIVE @ START-CAPTURE
   0 SS-RUN-STATUS ! ;

: MOTION-CALL ( -- )
   SS-ACTIVE @ RUN-MOTION SS-RUN-STATUS ! ;

: CATCH-CAPTURE ( n -- n )
   SS-ACTIVE !
   SS-SKIPPED SS-RUN-STATUS !
   [: CAPTURE-CALL ;] catch dup 0= if drop SS-RUN-STATUS @ then ;

: CATCH-MOTION ( n -- n )
   SS-ACTIVE !
   SS-SKIPPED SS-RUN-STATUS !
   [: MOTION-CALL ;] catch dup 0= if drop SS-RUN-STATUS @ then ;

: DELAY-IF-REQUESTED ( -- )
   PRE-CUE$ dup 0 > if type cr else 2drop then
   SS-PRE-DELAY-S @ 0 <= if exit then
   TIME-MONO-NS SS-PRE-DELAY-S @ SS-NS-PER-S * + {: deadline:n :}
   begin TIME-MONO-NS deadline < while TASK:PAUSE repeat ;

: BUILD-SCENARIO-SUMMARY ( -- n )
   OUTPUT-ID$ MOTION-ROOT$ SPMOT:SCENARIO-RESET
   0 SS-I !
   begin SS-I @ 4 < while
      SS-I @ READINESS$ {: rpath:ptr rpathu:n :}
      rpath rpathu SS-READ-BUF SS-READ-CAP READ-ALL {: u:n :}
      SS-I @ CAMERA$ rpath rpathu SS-READ-BUF u SPMOT:SCENARIO-READINESS+
      SS-I @ 1+ SS-I !
   repeat
   MOTION-ROOT$ MAKE-DIRS
   MOTION-ROOT$ s" summary.csv" SS-P2 JOIN-PATH SS-P2 swap SPMOT:SCENARIO-CSV$ WRITE-ALL
   MOTION-ROOT$ s" summary.md" SS-P2 JOIN-PATH SS-P2 swap SPMOT:SCENARIO-MD$ WRITE-ALL
   SPMOT:SCENARIO-RESULT$ s" pass" STR= if 0 else 1 then ;

: SUMMARY-CALL ( -- )
   BUILD-SCENARIO-SUMMARY SS-RUN-STATUS ! ;

: CATCH-SUMMARY ( -- n )
   SS-SKIPPED SS-RUN-STATUS !
   [: SUMMARY-CALL ;] catch dup 0= if drop SS-RUN-STATUS @ then ;

: LIVE-RUN ( -- )
   CHECK-PREFLIGHT
   MOTION-ROOT$ MAKE-DIRS
   DELAY-IF-REQUESTED
   0 SS-I !
   begin SS-I @ 4 < while
      SS-I @ CATCH-CAPTURE SS-CAPTURE-STATUS SS-I @ A!
      SS-CAPTURE-STATUS SS-I @ A@ FIRST-FAIL
      SS-I @ 1+ SS-I !
   repeat
   0 SS-I !
   begin SS-I @ 4 < while
      SS-CAPTURE-STATUS SS-I @ A@ 0= if
         SS-I @ WAIT-CAPTURE SS-CAPTURE-STATUS SS-I @ A!
         SS-CAPTURE-STATUS SS-I @ A@ FIRST-FAIL
      then
      SS-I @ 1+ SS-I !
   repeat
   0 SS-I !
   begin SS-I @ 4 < while
      SS-CAPTURE-STATUS SS-I @ A@ 0= SS-I @ IMU-NDJSON$ FILE? and if
         SS-I @ CATCH-MOTION SS-MOTION-STATUS SS-I @ A!
         SS-MOTION-STATUS SS-I @ A@ FIRST-FAIL
      else
         SS-SKIPPED SS-MOTION-STATUS SS-I @ A!
      then
      SS-I @ 1+ SS-I !
   repeat
   CATCH-SUMMARY SS-SUMMARY-STATUS !
   SS-SUMMARY-STATUS @ FIRST-FAIL ;

: RB-STATUS ( n -- )
   dup SS-SKIPPED = if drop s" skipped" RB+ exit then
   RB# ;

: SUMMARY$ ( -- ptr u8 n )
   RB-RESET
   s" # SPSC Motion Scenario Runner" RB+ RB-NL RB-NL
   s" execution mode" SS-DRY-RUN @ if s" dry-run" else s" live" then MD-S
   s" scenario" OUTPUT-ID$ MD-S
   s" Odin root" ROOT$ MD-S
   s" Habu engine" HB$ MD-S
   s" output root" OUTPUT-ROOT$ MD-S
   s" baseline root" BASELINE-ROOT$ MD-S
   s" baseline suffix" BASELINE-SUFFIX$ MD-S
   s" motion root" MOTION-ROOT$ MD-S
   s" capture launch" s" concurrent Habu child captures" MD-S
   s" samples per device" SS-SAMPLES @ MD-N
   s" timeout ms" SS-TIMEOUT-MS @ MD-N
   s" window ms" SS-WINDOW-MS @ MD-N
   s" start timer" SS-START-TIMER @ if s" yes" else s" no" then MD-S
   s" readiness mode" SS-REQUIRE-READY @ if s" strict" else s" characterization" then MD-S
   s" pre-capture delay s" SS-PRE-DELAY-S @ MD-N
   s" pre-capture cue" PRE-CUE$ dup 0= if 2drop s" not supplied" then MD-S
   s" aggregate summary exit status" SS-SUMMARY-STATUS @ MD-N
   s" first nonzero exit status" SS-FIRST-FAILURE @ MD-N
   RB-NL
   s" ## Capture Status" RB+ RB-NL RB-NL
   0 SS-I !
   begin SS-I @ 4 < while
      s" - " RB+ SS-I @ CAMERA$ RB+ s"  (" RB+ SS-I @ DEVICE$ RB+ s" ): " RB+
      SS-CAPTURE-STATUS SS-I @ A@ RB-STATUS s"  (" RB+ SS-I @ CAPTURE-DIR$ RB+ s" )" RB+ RB-NL
      SS-I @ 1+ SS-I !
   repeat
   RB-NL s" ## Motion Analysis Status" RB+ RB-NL RB-NL
   0 SS-I !
   begin SS-I @ 4 < while
      s" - " RB+ SS-I @ CAMERA$ RB+ s" : " RB+ SS-MOTION-STATUS SS-I @ A@ RB-STATUS
      s"  (" RB+ SS-I @ MOTION-DIR$ RB+ s" )" RB+ RB-NL
      SS-I @ 1+ SS-I !
   repeat
   RB$ ;

: WRITE-RUNNER-SUMMARY ( -- )
   MOTION-ROOT$ MAKE-DIRS
   RUNNER-SUMMARY$ SUMMARY$ WRITE-ALL ;

: LINE ( ptr u8 n -- ) type cr ;
: USAGE ( -- )
   s" usage: odin/spsc-motion-scenario-cli.f -- <scenario-id> [options]" LINE
   s" options: --dry-run --samples N --timeout-ms N --window-ms N --tag VALUE" LINE
   s"          --habu PATH --output-root PATH --baseline-root PATH --baseline-suffix VALUE" LINE
   s"          --max-accel-dynamic-rms R --max-gyro-dynamic-rms R" LINE
   s"          --max-accel-dynamic-ratio R --max-gyro-dynamic-ratio R" LINE
   s"          --pre-capture-delay-s N --pre-capture-cue TEXT --require-ready --no-start" LINE
   s"          --odin-root PATH" LINE ;

: DIE-USAGE ( -- )
   USAGE s" spsc-motion-scenario usage" SS-USAGE-RC die ;

: ARG$ ( -- ptr u8 n ) SS-ARG-I @ SCRIPT-ARGV$ ;
: VALUE$ ( -- ptr u8 n )
   SS-ARG-I @ 1+ SCRIPT-ARGC >= if DIE-USAGE then
   SS-ARG-I @ 1+ SCRIPT-ARGV$ ;
: ADVANCE ( n -- ) SS-ARG-I @ + SS-ARG-I ! ;

: NUM ( ptr u8 n -- n )
   STR>NUMBER? 0= if drop DIE-USAGE then ;

: FLOAT-VALUE ( ptr u8 n -- r )
   STR>FLOAT 0= if drop DIE-USAGE then ;

: SET-SCENARIO-ONCE ( ptr u8 n -- )
   SS-SCENARIO-U @ 0 <> if 2drop DIE-USAGE then
   SET-SCENARIO ;

: OPTION ( -- )
   ARG$ s" -h" STR= if DIE-USAGE then
   ARG$ s" --help" STR= if DIE-USAGE then
   ARG$ s" --dry-run" STR= if 1 SS-DRY-RUN ! 1 ADVANCE exit then
   ARG$ s" --require-ready" STR= if 1 SS-REQUIRE-READY ! 1 ADVANCE exit then
   ARG$ s" --no-start" STR= if 0 SS-START-TIMER ! 1 ADVANCE exit then
   ARG$ s" --odin-root" STR= if VALUE$ SET-ROOT 2 ADVANCE exit then
   ARG$ s" --habu" STR= if VALUE$ SET-HB 2 ADVANCE exit then
   ARG$ s" --samples" STR= if VALUE$ NUM SS-SAMPLES ! 2 ADVANCE exit then
   ARG$ s" --timeout-ms" STR= if VALUE$ NUM SS-TIMEOUT-MS ! 2 ADVANCE exit then
   ARG$ s" --window-ms" STR= if VALUE$ NUM SS-WINDOW-MS ! 2 ADVANCE exit then
   ARG$ s" --tag" STR= if VALUE$ SET-TAG 2 ADVANCE exit then
   ARG$ s" --output-root" STR= if VALUE$ SET-OUTPUT-ROOT 2 ADVANCE exit then
   ARG$ s" --baseline-root" STR= if VALUE$ SET-BASELINE-ROOT 2 ADVANCE exit then
   ARG$ s" --baseline-suffix" STR= if VALUE$ SET-BASELINE-SUFFIX 2 ADVANCE exit then
   ARG$ s" --pre-capture-delay-s" STR= if VALUE$ NUM SS-PRE-DELAY-S ! 2 ADVANCE exit then
   ARG$ s" --pre-capture-cue" STR= if VALUE$ SET-PRE-CUE 2 ADVANCE exit then
   ARG$ s" --max-accel-dynamic-rms" STR= if VALUE$ FLOAT-VALUE SS-TH-ACC-RMS F!1 1 SS-HAVE-TH-ACC-RMS ! 2 ADVANCE exit then
   ARG$ s" --max-gyro-dynamic-rms" STR= if VALUE$ FLOAT-VALUE SS-TH-GYR-RMS F!1 1 SS-HAVE-TH-GYR-RMS ! 2 ADVANCE exit then
   ARG$ s" --max-accel-dynamic-ratio" STR= if VALUE$ FLOAT-VALUE SS-TH-ACC-RATIO F!1 1 SS-HAVE-TH-ACC-RATIO ! 2 ADVANCE exit then
   ARG$ s" --max-gyro-dynamic-ratio" STR= if VALUE$ FLOAT-VALUE SS-TH-GYR-RATIO F!1 1 SS-HAVE-TH-GYR-RATIO ! 2 ADVANCE exit then
   ARG$ s" --" STARTS-WITH? if DIE-USAGE then
   ARG$ SET-SCENARIO-ONCE
   1 ADVANCE ;

: DO-FINALIZE ( -- )
   SS-SCENARIO-U @ 0= if s" missing scenario" SS-USAGE-RC die then
   SCENARIO$ CHECK-NAME
   TAG$ CHECK-NAME
   SS-OUTPUT-ID-U @ 0= if BUILD-OUTPUT-ID then
   OUTPUT-ID$ CHECK-NAME ;

: PARSE ( -- )
   DEFAULTS
   SCRIPT-ARGC 0= if DIE-USAGE then
   begin SS-ARG-I @ SCRIPT-ARGC < while OPTION repeat
   DO-FINALIZE ;

: DO-LIVE ( -- n )
   LIVE-RUN
   WRITE-RUNNER-SUMMARY
   SS-FIRST-FAILURE @ ;

: RUN-MAIN ( -- )
   PARSE
   SS-DRY-RUN @ if SUMMARY$ type exit then
   DO-LIVE {: rc:n :}
   rc 0 <> if s" spsc motion scenario failed" rc die then ;

public

: RESET ( -- ) DEFAULTS ;
: SCENARIO! ( ptr u8 n -- ) SET-SCENARIO ;
: TAG! ( ptr u8 n -- ) SET-TAG ;
: OUTPUT-ID! ( ptr u8 n -- ) SET-OUTPUT-ID ;
: ODIN-ROOT! ( ptr u8 n -- ) SET-ROOT ;
: HABU! ( ptr u8 n -- ) SET-HB ;
: OUTPUT-ROOT! ( ptr u8 n -- ) SET-OUTPUT-ROOT ;
: BASELINE-ROOT! ( ptr u8 n -- ) SET-BASELINE-ROOT ;
: BASELINE-SUFFIX! ( ptr u8 n -- ) SET-BASELINE-SUFFIX ;
: SAMPLES! ( n -- ) SS-SAMPLES ! ;
: TIMEOUT-MS! ( n -- ) SS-TIMEOUT-MS ! ;
: WINDOW-MS! ( n -- ) SS-WINDOW-MS ! ;
: NO-START! ( -- ) 0 SS-START-TIMER ! ;
: REQUIRE-READY! ( -- ) 1 SS-REQUIRE-READY ! ;
: DRY-RUN! ( -- ) 1 SS-DRY-RUN ! ;
: FINALIZE ( -- ) DO-FINALIZE ;
: SUMMARY ( -- ptr u8 n ) SUMMARY$ ;
: LIVE ( -- n ) DO-LIVE ;
: MAIN ( -- ) RUN-MAIN ;

end-package
