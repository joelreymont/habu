\ user-assisted-plan.f - Habu-owned user-assisted validation batch planner.
\
\ Non-capturing planner for the four user-assisted Odin validation batches. It
\ checks static inputs and visible device nodes, then prints the operator plan
\ without emitting commands for non-Habu executables.

require lib/errors.f
require lib/string.f
require lib/fs.f

package UAP

64 constant UAP-USAGE-RC
256 constant UAP-TEXT-CAP

0 constant UAP-BATCH-NONE
1 constant UAP-BATCH-LATENCY
2 constant UAP-BATCH-SKY
3 constant UAP-BATCH-MOTION
4 constant UAP-BATCH-GEOMETRY

create UAP-ROOT-BUF FS-PATH-CAP allot
create UAP-LOGICAL-BUF UAP-TEXT-CAP allot
create UAP-START-BUF UAP-TEXT-CAP allot
create UAP-LEAD-BUF UAP-TEXT-CAP allot
create UAP-BASELINE-ROOT-BUF FS-PATH-CAP allot
create UAP-BASELINE-SUFFIX-BUF UAP-TEXT-CAP allot

variable UAP-BATCH
variable UAP-ARG-I
variable UAP-ROOT-U
variable UAP-LOGICAL-U
variable UAP-START-U
variable UAP-LEAD-U
variable UAP-BASELINE-ROOT-U
variable UAP-BASELINE-SUFFIX-U
variable UAP-LATENCY-DELAY
variable UAP-CAPTURE-DELAY
variable UAP-VIBRATION-DELAY
variable UAP-CHECK-HARDWARE
variable UAP-FAILURES
variable UAP-PENDING
variable UAP-I

: UAP-TRUE ( -- bool ) 0 0= ;
: UAP-FALSE ( -- bool ) UAP-TRUE 0= ;

: UAP-COPY! ( ptr u8 n ptr u8 n ptr a -- ) {: a:ptr u:n dst:ptr cap:n lenp:ptr :}
   u 0 <= if s" empty option value" UAP-USAGE-RC die then
   u cap > if E-STR-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp ! ;

: UAP-ROOT$ ( -- ptr u8 n ) UAP-ROOT-BUF UAP-ROOT-U @ ;
: UAP-LOGICAL$ ( -- ptr u8 n ) UAP-LOGICAL-BUF UAP-LOGICAL-U @ ;
: UAP-START$ ( -- ptr u8 n ) UAP-START-BUF UAP-START-U @ ;
: UAP-LEAD$ ( -- ptr u8 n ) UAP-LEAD-BUF UAP-LEAD-U @ ;
: UAP-BASELINE-ROOT$ ( -- ptr u8 n ) UAP-BASELINE-ROOT-BUF UAP-BASELINE-ROOT-U @ ;
: UAP-BASELINE-SUFFIX$ ( -- ptr u8 n ) UAP-BASELINE-SUFFIX-BUF UAP-BASELINE-SUFFIX-U @ ;

: UAP-ROOT! ( ptr u8 n -- ) UAP-ROOT-BUF FS-PATH-CAP UAP-ROOT-U UAP-COPY! ;
: UAP-LOGICAL! ( ptr u8 n -- ) UAP-LOGICAL-BUF UAP-TEXT-CAP UAP-LOGICAL-U UAP-COPY! ;
: UAP-START! ( ptr u8 n -- ) UAP-START-BUF UAP-TEXT-CAP UAP-START-U UAP-COPY! ;
: UAP-LEAD! ( ptr u8 n -- ) UAP-LEAD-BUF UAP-TEXT-CAP UAP-LEAD-U UAP-COPY! ;
: UAP-BASELINE-ROOT! ( ptr u8 n -- )
   UAP-BASELINE-ROOT-BUF FS-PATH-CAP UAP-BASELINE-ROOT-U UAP-COPY! ;
: UAP-BASELINE-SUFFIX! ( ptr u8 n -- )
   UAP-BASELINE-SUFFIX-BUF UAP-TEXT-CAP UAP-BASELINE-SUFFIX-U UAP-COPY! ;

: UAP-DEFAULTS ( -- )
   UAP-BATCH-NONE UAP-BATCH !
   s" ../Odin" UAP-ROOT!
   s" cam_a0" UAP-LOGICAL!
   s" not scheduled" UAP-START!
   s" 10-15 minutes" UAP-LEAD!
   s" results/imu" UAP-BASELINE-ROOT!
   s" spsc_static_20260624_0502_CEST" UAP-BASELINE-SUFFIX!
   30 UAP-LATENCY-DELAY !
   60 UAP-CAPTURE-DELAY !
   60 UAP-VIBRATION-DELAY !
   1 UAP-CHECK-HARDWARE !
   0 UAP-FAILURES !
   0 UAP-PENDING ! ;

: UAP-LINE ( ptr u8 n -- ) type cr ;
: UAP-SP ( -- ) 32 emit ;
: UAP-PIPE ( -- ) 124 emit ;
: UAP-CELL-SEP ( -- ) UAP-SP UAP-PIPE UAP-SP ;
: UAP-ROW-START ( -- ) UAP-PIPE UAP-SP ;
: UAP-ROW-END ( -- ) UAP-SP UAP-PIPE cr ;

: UAP-PRINT-U ( n -- )
   dup 10 < if 48 + emit exit then
   dup 10 / RECURSE
   10 mod 48 + emit ;

: UAP-PRINT-N ( n -- )
   dup 0 < if 45 emit negate then
   UAP-PRINT-U ;

: UAP-USAGE ( -- )
   s" usage: odin/user-assisted-plan.f -- <batch> [options]" UAP-LINE
   s" batches: latency_vibration | sky_low_light | motion | geometry_proxy" UAP-LINE
   s" options: --odin-root PATH --logical-name VALUE --start-window TEXT --lead-time TEXT" UAP-LINE
   s"          --latency-delay-s N --capture-delay-s N --vibration-delay-s N" UAP-LINE
   s"          --baseline-root PATH --baseline-suffix VALUE --skip-hardware-check" UAP-LINE ;

: UAP-DIE-USAGE ( -- )
   UAP-USAGE
   s" user-assisted-plan usage" UAP-USAGE-RC die ;

: UAP-ARG$ ( -- ptr u8 n )
   UAP-ARG-I @ SCRIPT-ARGV$ ;

: UAP-VALUE$ ( -- ptr u8 n )
   UAP-ARG-I @ 1+ SCRIPT-ARGC >= if UAP-DIE-USAGE then
   UAP-ARG-I @ 1+ SCRIPT-ARGV$ ;

: UAP-ADVANCE ( n -- )
   UAP-ARG-I @ + UAP-ARG-I ! ;

: UAP-PARSE-NONNEG ( ptr u8 n -- n )
   2dup STR-DIGITS? 0= if 2drop UAP-DIE-USAGE then
   STR>NUMBER? 0= if drop UAP-DIE-USAGE then ;

: UAP-BATCH-ID ( ptr u8 n -- n ) {: a:ptr u:n :}
   a u s" latency_vibration" STR= if UAP-BATCH-LATENCY exit then
   a u s" sky_low_light" STR= if UAP-BATCH-SKY exit then
   a u s" motion" STR= if UAP-BATCH-MOTION exit then
   a u s" geometry_proxy" STR= if UAP-BATCH-GEOMETRY exit then
   UAP-BATCH-NONE ;

: UAP-BATCH$ ( -- ptr u8 n )
   UAP-BATCH @ UAP-BATCH-LATENCY = if s" latency_vibration" exit then
   UAP-BATCH @ UAP-BATCH-SKY = if s" sky_low_light" exit then
   UAP-BATCH @ UAP-BATCH-MOTION = if s" motion" exit then
   UAP-BATCH @ UAP-BATCH-GEOMETRY = if s" geometry_proxy" exit then
   s" unknown" ;

: UAP-FIRST-NAME? ( n -- bool ) {: c:n :}
   c 48 >= c 57 <= and
   c 65 >= c 90 <= and or
   c 97 >= c 122 <= and or ;

: UAP-NAME-CHAR? ( n -- bool ) {: c:n :}
   c UAP-FIRST-NAME? if UAP-TRUE exit then
   c 46 = c 95 = or c 45 = or ;

: UAP-NAME? ( ptr u8 n -- bool ) {: a:ptr u:n :}
   u 0 <= if UAP-FALSE exit then
   a c@ UAP-FIRST-NAME? 0= if UAP-FALSE exit then
   1 UAP-I !
   begin UAP-I @ u < while
      a UAP-I @ + c@ UAP-NAME-CHAR? 0= if UAP-FALSE exit then
      UAP-I @ 1+ UAP-I !
   repeat
   UAP-TRUE ;

: UAP-CHECK-NAMES ( -- )
   UAP-LOGICAL$ UAP-NAME? 0= if s" invalid logical name" UAP-USAGE-RC die then
   UAP-BASELINE-SUFFIX$ UAP-NAME? 0= if s" invalid baseline suffix" UAP-USAGE-RC die then ;

: UAP-SET-BATCH ( ptr u8 n -- )
   UAP-BATCH @ UAP-BATCH-NONE <> if 2drop UAP-DIE-USAGE then
   UAP-BATCH-ID dup UAP-BATCH-NONE = if drop UAP-DIE-USAGE then
   UAP-BATCH ! ;

: UAP-OPTION ( -- )
   UAP-ARG$ s" -h" STR= if UAP-DIE-USAGE then
   UAP-ARG$ s" --help" STR= if UAP-DIE-USAGE then
   UAP-ARG$ s" --odin-root" STR= if UAP-VALUE$ UAP-ROOT! 2 UAP-ADVANCE exit then
   UAP-ARG$ s" --logical-name" STR= if UAP-VALUE$ UAP-LOGICAL! 2 UAP-ADVANCE exit then
   UAP-ARG$ s" --start-window" STR= if UAP-VALUE$ UAP-START! 2 UAP-ADVANCE exit then
   UAP-ARG$ s" --lead-time" STR= if UAP-VALUE$ UAP-LEAD! 2 UAP-ADVANCE exit then
   UAP-ARG$ s" --latency-delay-s" STR= if UAP-VALUE$ UAP-PARSE-NONNEG UAP-LATENCY-DELAY ! 2 UAP-ADVANCE exit then
   UAP-ARG$ s" --capture-delay-s" STR= if UAP-VALUE$ UAP-PARSE-NONNEG UAP-CAPTURE-DELAY ! 2 UAP-ADVANCE exit then
   UAP-ARG$ s" --vibration-delay-s" STR= if UAP-VALUE$ UAP-PARSE-NONNEG UAP-VIBRATION-DELAY ! 2 UAP-ADVANCE exit then
   UAP-ARG$ s" --baseline-root" STR= if UAP-VALUE$ UAP-BASELINE-ROOT! 2 UAP-ADVANCE exit then
   UAP-ARG$ s" --baseline-suffix" STR= if UAP-VALUE$ UAP-BASELINE-SUFFIX! 2 UAP-ADVANCE exit then
   UAP-ARG$ s" --skip-hardware-check" STR= if 0 UAP-CHECK-HARDWARE ! 1 UAP-ADVANCE exit then
   UAP-ARG$ s" --" STARTS-WITH? if UAP-DIE-USAGE then
   UAP-ARG$ UAP-SET-BATCH
   1 UAP-ADVANCE ;

: UAP-PARSE ( -- )
   UAP-DEFAULTS
   0 UAP-ARG-I !
   SCRIPT-ARGC 0= if UAP-DIE-USAGE then
   begin UAP-ARG-I @ SCRIPT-ARGC < while
      UAP-OPTION
   repeat
   UAP-BATCH @ UAP-BATCH-NONE = if UAP-DIE-USAGE then
   UAP-CHECK-NAMES ;

: UAP-JOIN$ ( ptr u8 n ptr u8 n -- ptr u8 n ) {: base:ptr baseu:n rel:ptr relu:n :}
   SB-RESET
   base baseu SB-APPEND
   baseu 0 > if base baseu 1- + c@ 47 <> if 47 SB-APPEND-C then then
   rel relu SB-APPEND
   SB$ ;

: UAP-ODIN-PATH$ ( ptr u8 n -- ptr u8 n ) {: rel:ptr relu:n :}
   UAP-ROOT$ rel relu UAP-JOIN$ ;

: UAP-BASELINE$ ( ptr u8 n -- ptr u8 n ) {: cam:ptr camu:n :}
   SB-RESET
   UAP-ROOT$ SB-APPEND
   47 SB-APPEND-C
   UAP-BASELINE-ROOT$ SB-APPEND
   47 SB-APPEND-C
   cam camu SB-APPEND
   95 SB-APPEND-C
   UAP-BASELINE-SUFFIX$ SB-APPEND
   s" /imu.ndjson" SB-APPEND
   SB$ ;

: UAP-DEVICE$ ( n -- ptr u8 n )
   dup 0 = if drop s" /dev/spsc_bmi0" exit then
   dup 1 = if drop s" /dev/spsc_bmi1" exit then
   dup 2 = if drop s" /dev/spsc_bmi2" exit then
   drop s" /dev/spsc_bmi3" ;

: UAP-CAMERA$ ( n -- ptr u8 n )
   dup 0 = if drop s" cam_a0" exit then
   dup 1 = if drop s" cam_a1" exit then
   dup 2 = if drop s" cam_b0" exit then
   drop s" cam_b1" ;

: UAP-ROW ( ptr u8 n ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n status:ptr statusu:n detail:ptr detailu:n :}
   UAP-ROW-START
   label labelu type UAP-CELL-SEP
   status statusu type UAP-CELL-SEP
   detail detailu type UAP-ROW-END ;

: UAP-FILE-CHECK ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n path:ptr pathu:n :}
   path pathu FILE? if
      label labelu s" pass" path pathu UAP-ROW
   else
      label labelu s" fail" path pathu UAP-ROW
      UAP-FAILURES @ 1+ UAP-FAILURES !
   then ;

: UAP-EXISTS-CHECK ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n path:ptr pathu:n :}
   path pathu EXISTS? if
      label labelu s" pass" path pathu UAP-ROW
   else
      label labelu s" fail" path pathu UAP-ROW
      UAP-FAILURES @ 1+ UAP-FAILURES !
   then ;

: UAP-PENDING-ROW ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n detail:ptr detailu:n :}
   label labelu s" pending" detail detailu UAP-ROW
   UAP-PENDING @ 1+ UAP-PENDING ! ;

: UAP-CHECK-ROOT-FILE ( ptr u8 n ptr u8 n -- ) {: label:ptr labelu:n rel:ptr relu:n :}
   label labelu rel relu UAP-ODIN-PATH$ UAP-FILE-CHECK ;

: UAP-BASELINE-CHECK ( ptr u8 n -- ) {: cam:ptr camu:n :}
   s" SPSC baseline" cam camu UAP-BASELINE$ UAP-FILE-CHECK ;

: UAP-DEVICE-CHECK ( n -- ) {: ix:n :}
   s" SPSC device" ix UAP-DEVICE$ UAP-EXISTS-CHECK ;

: UAP-BASIC-PREFLIGHT ( -- )
   s" camera config" s" configs/cameras.json" UAP-CHECK-ROOT-FILE ;

: UAP-LATENCY-PREFLIGHT ( -- )
   s" CameraOne latency runner CLI" s" odin/cameraone-latency-scenario-cli.f" UAP-FILE-CHECK
   s" CameraOne latency analyzer" s" odin/cameraone-latency.f" UAP-FILE-CHECK
   s" CameraOne capture backend" s" odin/capture-backend.f" UAP-FILE-CHECK
   s" SPSC capture backend" s" odin/spsc-imu.f" UAP-FILE-CHECK
   s" SPSC capture CLI" s" odin/spsc-imu-cli.f" UAP-FILE-CHECK
   s" SPSC motion analyzer" s" odin/spsc-motion.f" UAP-FILE-CHECK
   s" SPSC motion runner CLI" s" odin/spsc-motion-scenario-cli.f" UAP-FILE-CHECK ;

: UAP-SKY-PREFLIGHT ( -- )
   s" saved-image runner CLI" s" odin/saved-image-scenario-cli.f" UAP-FILE-CHECK
   s" saved-image analyzers" s" odin/saved-image-analyzers.f" UAP-FILE-CHECK
   s" bright sky exposure manifest" s" configs/exposure_manifests/bright_blue_sky.json" UAP-CHECK-ROOT-FILE
   s" dusk exposure manifest" s" configs/exposure_manifests/dusk_twilight.json" UAP-CHECK-ROOT-FILE
   s" outdoor dusk low-light manifest" s" configs/low_light_manifests/outdoor_dusk_twilight_auto.json" UAP-CHECK-ROOT-FILE ;

: UAP-MOTION-PREFLIGHT ( -- )
   s" saved-image runner CLI" s" odin/saved-image-scenario-cli.f" UAP-FILE-CHECK
   s" saved-image analyzers" s" odin/saved-image-analyzers.f" UAP-FILE-CHECK
   s" SPSC capture backend" s" odin/spsc-imu.f" UAP-FILE-CHECK
   s" SPSC capture CLI" s" odin/spsc-imu-cli.f" UAP-FILE-CHECK
   s" SPSC motion analyzer" s" odin/spsc-motion.f" UAP-FILE-CHECK
   s" SPSC motion runner CLI" s" odin/spsc-motion-scenario-cli.f" UAP-FILE-CHECK
   s" target motion manifest" s" configs/motion_manifests/target_motion_lateral_pass.json" UAP-CHECK-ROOT-FILE
   s" rig motion manifest" s" configs/motion_manifests/rig_motion_pan.json" UAP-CHECK-ROOT-FILE
   s" vibration manifest" s" configs/motion_manifests/vibration_bump.json" UAP-CHECK-ROOT-FILE ;

: UAP-GEOMETRY-PREFLIGHT ( -- )
   s" rig survey entrypoint" s" Habu physical survey runner required" UAP-PENDING-ROW
   s" proxy localization entrypoint" s" Habu proxy localization runner required" UAP-PENDING-ROW
   s" physical rig survey worksheet" s" configs/rig_survey.physical_template.csv" UAP-CHECK-ROOT-FILE
   s" factory intrinsics" s" results/rig_geometry/intrinsics.json" UAP-CHECK-ROOT-FILE
   s" localization manifest" s" configs/localization_manifest.example.json" UAP-CHECK-ROOT-FILE
   s" physical survey extrinsics" s" created after physical measurements" UAP-PENDING-ROW ;

: UAP-MAYBE-SPSC ( -- )
   UAP-BATCH @ UAP-BATCH-LATENCY = UAP-BATCH @ UAP-BATCH-MOTION = or 0= if exit then
   0 begin dup 4 < while
      dup UAP-CAMERA$ UAP-BASELINE-CHECK
      1+
   repeat drop
   UAP-CHECK-HARDWARE @ 0= if
      s" SPSC device check" s" skip" s" --skip-hardware-check" UAP-ROW
      exit
   then
   0 begin dup 4 < while
      dup UAP-DEVICE-CHECK
      1+
   repeat drop ;

: UAP-PREFLIGHT ( -- )
   s" ## Preflight" UAP-LINE cr
   s" | Check | Result | Detail |" UAP-LINE
   s" | --- | --- | --- |" UAP-LINE
   UAP-BASIC-PREFLIGHT
   UAP-BATCH @ UAP-BATCH-LATENCY = if UAP-LATENCY-PREFLIGHT then
   UAP-BATCH @ UAP-BATCH-SKY = if UAP-SKY-PREFLIGHT then
   UAP-BATCH @ UAP-BATCH-MOTION = if UAP-MOTION-PREFLIGHT then
   UAP-BATCH @ UAP-BATCH-GEOMETRY = if UAP-GEOMETRY-PREFLIGHT then
   UAP-MAYBE-SPSC ;

: UAP-PRINT-DELAY ( ptr u8 n n -- ) {: label:ptr labelu:n n:n :}
   s" - " type label labelu type s" : " type n UAP-PRINT-N s"  seconds" UAP-LINE ;

: UAP-LATENCY-SEQUENCE ( -- )
   s" ## Sequence: Latency + Vibration" UAP-LINE cr
   s" 1. CameraOne visible-motion plus IMU latency characterization." UAP-LINE
   s" 2. Separate Habu SPSC vibration/bump characterization." UAP-LINE cr
   s" Habu entrypoints are available for CameraOne latency and SPSC motion characterization." UAP-LINE cr
   s" ## User Actions" UAP-LINE cr
   s" - Before step 1: place a hand or target in the selected camera view." UAP-LINE
   s" - During step 1: make one visible motion/occlusion and one distinct rig tap." UAP-LINE
   s" - Before step 2: keep direct camera capture stopped." UAP-LINE
   s" - During step 2: apply the agreed vibration/bump profile." UAP-LINE ;

: UAP-SKY-SEQUENCE ( -- )
   s" ## Sequence: Sky + Low-Light" UAP-LINE cr
   s" 1. Bright or overcast sky exposure capture." UAP-LINE
   s" 2. Outdoor dusk/twilight low-light proxy capture." UAP-LINE cr
   s" Saved-image Habu analyzers are available for exposure, low-light, and sync." UAP-LINE cr
   s" ## User Actions" UAP-LINE cr
   s" - Before step 1: point the rig outside or through a window at the agreed sky scene." UAP-LINE
   s" - During step 1: keep the rig still for the measured capture." UAP-LINE
   s" - Before step 2: point at the dusk/twilight scene and place the agreed target/proxy." UAP-LINE
   s" - After each run: record sky condition, time, target range/size, and contrast notes." UAP-LINE ;

: UAP-MOTION-SEQUENCE ( -- )
   s" ## Sequence: Motion" UAP-LINE cr
   s" 1. Target lateral-pass motion blur capture." UAP-LINE
   s" 2. Controlled rig-pan motion capture." UAP-LINE
   s" 3. Image vibration/bump capture." UAP-LINE
   s" 4. Separate SPSC vibration/bump characterization." UAP-LINE cr
   s" Saved-image Habu analyzers are available for motion-blur and sync; Habu SPSC motion characterization is available separately." UAP-LINE cr
   s" ## User Actions" UAP-LINE cr
   s" - Before step 1: measure target physical size, range, path, and expected speed." UAP-LINE
   s" - During step 1: move the target across the agreed path during the capture window." UAP-LINE
   s" - During step 2: pan the rig at the agreed angular rate." UAP-LINE
   s" - During steps 3 and 4: apply the agreed bump/vibration profile." UAP-LINE ;

: UAP-GEOMETRY-SEQUENCE ( -- )
   s" ## Sequence: Geometry + Proxy Localization" UAP-LINE cr
   s" 1. Fill or refresh the physical survey worksheet." UAP-LINE
   s" 2. Validate the measured physical geometry." UAP-LINE
   s" 3. Run proxy localization after capture and annotation exist." UAP-LINE cr
   s" Required Habu entrypoints: physical survey runner; proxy localization runner." UAP-LINE cr
   s" ## User Actions" UAP-LINE cr
   s" - Fill every camera-to-rig and rig-to-truck measurement before geometry validation." UAP-LINE
   s" - Capture a controlled proxy target scene before localization." UAP-LINE
   s" - Annotate at least one visible proxy target before requiring visible labels." UAP-LINE ;

: UAP-SEQUENCE ( -- )
   cr
   s" latency countdown" UAP-LATENCY-DELAY @ UAP-PRINT-DELAY
   s" saved-image countdown" UAP-CAPTURE-DELAY @ UAP-PRINT-DELAY
   s" SPSC countdown" UAP-VIBRATION-DELAY @ UAP-PRINT-DELAY
   cr
   UAP-BATCH @ UAP-BATCH-LATENCY = if UAP-LATENCY-SEQUENCE then
   UAP-BATCH @ UAP-BATCH-SKY = if UAP-SKY-SEQUENCE then
   UAP-BATCH @ UAP-BATCH-MOTION = if UAP-MOTION-SEQUENCE then
   UAP-BATCH @ UAP-BATCH-GEOMETRY = if UAP-GEOMETRY-SEQUENCE then ;

: UAP-HEADER ( -- )
   s" # User-Assisted Validation Batch Plan" UAP-LINE cr
   s" - batch: " type UAP-BATCH$ UAP-LINE
   s" - Odin root: " type UAP-ROOT$ UAP-LINE
   s" - logical camera: " type UAP-LOGICAL$ UAP-LINE
   s" - start window: " type UAP-START$ UAP-LINE
   s" - operator lead time: " type UAP-LEAD$ UAP-LINE
   s" - capture/analyzer processes started by this planner: none" UAP-LINE cr ;

: UAP-FINAL ( -- )
   cr
   UAP-FAILURES @ 0 <> if
      s" Preflight result: fail (" type UAP-FAILURES @ UAP-PRINT-N s"  issue(s)); do not schedule yet." UAP-LINE
      exit
   then
   UAP-PENDING @ 0 <> if
      s" Preflight result: input pass; " type UAP-PENDING @ UAP-PRINT-N
      s"  Habu execution entrypoint(s) pending before this batch can run." UAP-LINE
      exit
   then
   s" Preflight result: pass; ready to schedule with the user." UAP-LINE ;

: UAP-MAIN ( -- )
   UAP-PARSE
   UAP-HEADER
   UAP-PREFLIGHT
   UAP-SEQUENCE
   UAP-FINAL ;

end-package
