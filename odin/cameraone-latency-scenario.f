\ cameraone-latency-scenario.f - Habu-owned CameraOne latency scenario runner.
\
\ Captures one CameraOne stream with saved frames plus IMAGE-time IMU sensor rows,
\ then runs the Habu CameraOne latency analyzer. Dry-run mode validates option and
\ path plumbing without touching camera hardware.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/fs-mutate.f
require lib/render.f
require lib/float.f
require odin/capture-backend.f
require odin/cameraone-latency.f

package C1LAT
private

64 constant C1-USAGE-RC
-1 constant C1-SKIPPED
256 constant C1-TEXT-CAP
1024 constant C1-PATH-CAP
1000000 constant C1-NS-PER-MS

create C1-ROOT-BUF C1-PATH-CAP allot
create C1-OUTPUT-ROOT-BUF C1-PATH-CAP allot
create C1-CAPTURE-ABI-BUF C1-PATH-CAP allot
create C1-CONFIG-BUF C1-PATH-CAP allot
create C1-SCENARIO-BUF C1-TEXT-CAP allot
create C1-TAG-BUF C1-TEXT-CAP allot
create C1-OUTPUT-ID-BUF C1-TEXT-CAP allot
create C1-LOGICAL-BUF C1-TEXT-CAP allot
create C1-SERIAL-BUF C1-TEXT-CAP allot
create C1-CAMERA-BUF C1-TEXT-CAP allot
create C1-RESOLUTION-BUF C1-TEXT-CAP allot
create C1-FPS-BUF C1-TEXT-CAP allot
create C1-DURATION-BUF C1-TEXT-CAP allot
create C1-WARMUP-BUF C1-TEXT-CAP allot
create C1-METADATA-BUF C1-TEXT-CAP allot
create C1-SAVE-EVERY-BUF C1-TEXT-CAP allot
create C1-MAX-SAVED-BUF C1-TEXT-CAP allot
create C1-MANUAL-EXPOSURE-BUF C1-TEXT-CAP allot
create C1-MANUAL-GAIN-BUF C1-TEXT-CAP allot
create C1-PRE-DELAY-BUF C1-TEXT-CAP allot
create C1-PRE-CUE-BUF C1-TEXT-CAP allot
create C1-P0 C1-PATH-CAP allot
create C1-P1 C1-PATH-CAP allot
create C1-P2 C1-PATH-CAP allot
create C1-P3 C1-PATH-CAP allot

variable C1-ROOT-U
variable C1-OUTPUT-ROOT-U
variable C1-CAPTURE-ABI-U
variable C1-CONFIG-U
variable C1-SCENARIO-U
variable C1-TAG-U
variable C1-OUTPUT-ID-U
variable C1-LOGICAL-U
variable C1-SERIAL-U
variable C1-CAMERA-U
variable C1-RESOLUTION-U
variable C1-FPS-U
variable C1-DURATION-U
variable C1-WARMUP-U
variable C1-METADATA-U
variable C1-SAVE-EVERY-U
variable C1-MAX-SAVED-U
variable C1-MANUAL-EXPOSURE-U
variable C1-MANUAL-GAIN-U
variable C1-PRE-DELAY-U
variable C1-PRE-CUE-U
variable C1-P0-U
variable C1-P1-U
variable C1-P2-U
variable C1-ARG-I
variable C1-I
variable C1-DRY-RUN
variable C1-ALLOW-CHARACTERIZATION
variable C1-THRESHOLD-DELTA
variable C1-CAMERA-MIN-SPACING-NS
variable C1-IMU-THRESHOLD-MILLI
variable C1-IMU-MIN-SPACING-NS
variable C1-MAX-JITTER-NS
variable C1-OFFSET-NS
variable C1-HAVE-OFFSET
variable C1-CAPTURE-STATUS
variable C1-ANALYZE-STATUS
variable C1-FIRST-FAILURE

: C1-TRUE ( -- bool ) 0 0= ;
: C1-FALSE ( -- bool ) C1-TRUE 0= ;

: C1-COPY! ( ptr u8 n ptr u8 n ptr a -- )
   {: src:ptr u:n dst:ptr cap:n lenp:ptr :}
   u 0 < if E-STR-CAPACITY throw then
   u cap >= if E-STR-CAPACITY throw then
   src dst u BYTE-COPY
   u lenp ! ;

: C1-NONEMPTY-COPY! ( ptr u8 n ptr u8 n ptr a -- )
   {: src:ptr u:n dst:ptr cap:n lenp:ptr :}
   u 0 <= if s" empty option value" C1-USAGE-RC die then
   src u dst cap lenp C1-COPY! ;

: C1-ROOT$ ( -- ptr u8 n ) C1-ROOT-BUF C1-ROOT-U @ ;
: C1-OUTPUT-ROOT$ ( -- ptr u8 n ) C1-OUTPUT-ROOT-BUF C1-OUTPUT-ROOT-U @ ;
: C1-CAPTURE-ABI$ ( -- ptr u8 n ) C1-CAPTURE-ABI-BUF C1-CAPTURE-ABI-U @ ;
: C1-CONFIG$ ( -- ptr u8 n ) C1-CONFIG-BUF C1-CONFIG-U @ ;
: C1-SCENARIO$ ( -- ptr u8 n ) C1-SCENARIO-BUF C1-SCENARIO-U @ ;
: C1-TAG$ ( -- ptr u8 n ) C1-TAG-BUF C1-TAG-U @ ;
: C1-OUTPUT-ID$ ( -- ptr u8 n ) C1-OUTPUT-ID-BUF C1-OUTPUT-ID-U @ ;
: C1-LOGICAL$ ( -- ptr u8 n ) C1-LOGICAL-BUF C1-LOGICAL-U @ ;
: C1-SERIAL$ ( -- ptr u8 n ) C1-SERIAL-BUF C1-SERIAL-U @ ;
: C1-CAMERA$ ( -- ptr u8 n ) C1-CAMERA-BUF C1-CAMERA-U @ ;
: C1-RESOLUTION$ ( -- ptr u8 n ) C1-RESOLUTION-BUF C1-RESOLUTION-U @ ;
: C1-FPS$ ( -- ptr u8 n ) C1-FPS-BUF C1-FPS-U @ ;
: C1-DURATION$ ( -- ptr u8 n ) C1-DURATION-BUF C1-DURATION-U @ ;
: C1-WARMUP$ ( -- ptr u8 n ) C1-WARMUP-BUF C1-WARMUP-U @ ;
: C1-METADATA$ ( -- ptr u8 n ) C1-METADATA-BUF C1-METADATA-U @ ;
: C1-SAVE-EVERY$ ( -- ptr u8 n ) C1-SAVE-EVERY-BUF C1-SAVE-EVERY-U @ ;
: C1-MAX-SAVED$ ( -- ptr u8 n ) C1-MAX-SAVED-BUF C1-MAX-SAVED-U @ ;
: C1-MANUAL-EXPOSURE$ ( -- ptr u8 n ) C1-MANUAL-EXPOSURE-BUF C1-MANUAL-EXPOSURE-U @ ;
: C1-MANUAL-GAIN$ ( -- ptr u8 n ) C1-MANUAL-GAIN-BUF C1-MANUAL-GAIN-U @ ;
: C1-PRE-DELAY$ ( -- ptr u8 n ) C1-PRE-DELAY-BUF C1-PRE-DELAY-U @ ;
: C1-PRE-CUE$ ( -- ptr u8 n ) C1-PRE-CUE-BUF C1-PRE-CUE-U @ ;

: C1-ROOT! ( ptr u8 n -- ) C1-ROOT-BUF C1-PATH-CAP C1-ROOT-U C1-NONEMPTY-COPY! ;
: C1-OUTPUT-ROOT! ( ptr u8 n -- ) C1-OUTPUT-ROOT-BUF C1-PATH-CAP C1-OUTPUT-ROOT-U C1-NONEMPTY-COPY! ;
: C1-CAPTURE-ABI! ( ptr u8 n -- ) C1-CAPTURE-ABI-BUF C1-PATH-CAP C1-CAPTURE-ABI-U C1-NONEMPTY-COPY! ;
: C1-CONFIG! ( ptr u8 n -- ) C1-CONFIG-BUF C1-PATH-CAP C1-CONFIG-U C1-NONEMPTY-COPY! ;
: C1-SCENARIO! ( ptr u8 n -- ) C1-SCENARIO-BUF C1-TEXT-CAP C1-SCENARIO-U C1-NONEMPTY-COPY! ;
: C1-TAG! ( ptr u8 n -- ) C1-TAG-BUF C1-TEXT-CAP C1-TAG-U C1-NONEMPTY-COPY! ;
: C1-OUTPUT-ID! ( ptr u8 n -- ) C1-OUTPUT-ID-BUF C1-TEXT-CAP C1-OUTPUT-ID-U C1-NONEMPTY-COPY! ;
: C1-LOGICAL! ( ptr u8 n -- ) C1-LOGICAL-BUF C1-TEXT-CAP C1-LOGICAL-U C1-NONEMPTY-COPY! ;
: C1-SERIAL! ( ptr u8 n -- ) C1-SERIAL-BUF C1-TEXT-CAP C1-SERIAL-U C1-NONEMPTY-COPY! ;
: C1-CAMERA-COPY! ( ptr u8 n -- ) C1-CAMERA-BUF C1-TEXT-CAP C1-CAMERA-U C1-NONEMPTY-COPY! ;
: C1-RESOLUTION! ( ptr u8 n -- ) C1-RESOLUTION-BUF C1-TEXT-CAP C1-RESOLUTION-U C1-NONEMPTY-COPY! ;
: C1-FPS! ( ptr u8 n -- ) C1-FPS-BUF C1-TEXT-CAP C1-FPS-U C1-NONEMPTY-COPY! ;
: C1-DURATION! ( ptr u8 n -- ) C1-DURATION-BUF C1-TEXT-CAP C1-DURATION-U C1-NONEMPTY-COPY! ;
: C1-WARMUP! ( ptr u8 n -- ) C1-WARMUP-BUF C1-TEXT-CAP C1-WARMUP-U C1-NONEMPTY-COPY! ;
: C1-METADATA! ( ptr u8 n -- ) C1-METADATA-BUF C1-TEXT-CAP C1-METADATA-U C1-NONEMPTY-COPY! ;
: C1-SAVE-EVERY! ( ptr u8 n -- ) C1-SAVE-EVERY-BUF C1-TEXT-CAP C1-SAVE-EVERY-U C1-NONEMPTY-COPY! ;
: C1-MAX-SAVED! ( ptr u8 n -- ) C1-MAX-SAVED-BUF C1-TEXT-CAP C1-MAX-SAVED-U C1-NONEMPTY-COPY! ;
: C1-MANUAL-EXPOSURE! ( ptr u8 n -- ) C1-MANUAL-EXPOSURE-BUF C1-TEXT-CAP C1-MANUAL-EXPOSURE-U C1-NONEMPTY-COPY! ;
: C1-MANUAL-GAIN! ( ptr u8 n -- ) C1-MANUAL-GAIN-BUF C1-TEXT-CAP C1-MANUAL-GAIN-U C1-NONEMPTY-COPY! ;
: C1-PRE-DELAY! ( ptr u8 n -- ) C1-PRE-DELAY-BUF C1-TEXT-CAP C1-PRE-DELAY-U C1-NONEMPTY-COPY! ;
: C1-PRE-CUE! ( ptr u8 n -- ) C1-PRE-CUE-BUF C1-TEXT-CAP C1-PRE-CUE-U C1-NONEMPTY-COPY! ;

: C1-NUM ( ptr u8 n -- n )
   STR>NUMBER? 0= if drop s" expected integer option" C1-USAGE-RC die then ;

: C1-MS>NS ( ptr u8 n -- n )
   STR>FLOAT {: ok:bool :}
   ok 0= if drop s" expected millisecond number" C1-USAGE-RC die then
   C1-NS-PER-MS s>f f* f>s ;

: C1-G>MG ( ptr u8 n -- n )
   STR>FLOAT {: ok:bool :}
   ok 0= if drop s" expected IMU threshold number" C1-USAGE-RC die then
   1000.0 f* f>s ;

: C1-DEFAULTS ( -- )
   0 C1-ARG-I !
   0 C1-SCENARIO-U !
   0 C1-OUTPUT-ID-U !
   0 C1-LOGICAL-U !
   0 C1-SERIAL-U !
   0 C1-CAMERA-U !
   0 C1-MANUAL-EXPOSURE-U !
   0 C1-MANUAL-GAIN-U !
   0 C1-PRE-CUE-U !
   s" ../Odin" C1-ROOT!
   s" results/latency_calibration/cameraone_scenarios" C1-OUTPUT-ROOT!
   s" libodin_zed_capture.so" C1-CAPTURE-ABI!
   s" configs/cameras.json" C1-CONFIG!
   s" SVGA" C1-RESOLUTION!
   s" 60" C1-FPS!
   s" 5000" C1-DURATION!
   s" 2000" C1-WARMUP!
   s" 120" C1-METADATA!
   s" 1" C1-SAVE-EVERY!
   s" 360" C1-MAX-SAVED!
   s" 0" C1-PRE-DELAY!
   s" manual" C1-TAG!
   0 C1-DRY-RUN !
   0 C1-ALLOW-CHARACTERIZATION !
   50 C1-THRESHOLD-DELTA !
   0 C1-CAMERA-MIN-SPACING-NS !
   10500 C1-IMU-THRESHOLD-MILLI !
   25 C1-NS-PER-MS * C1-IMU-MIN-SPACING-NS !
   500000 C1-MAX-JITTER-NS !
   0 C1-OFFSET-NS !
   0 C1-HAVE-OFFSET ! ;

: C1-LINE ( ptr u8 n -- )
   type cr ;

: C1-USAGE ( -- )
   s" usage: odin/cameraone-latency-scenario-cli.f -- <scenario-id> [options]" C1-LINE
   s" options: --dry-run --logical-name NAME --serial SERIAL --camera SERIAL:NAME" C1-LINE
   s"          --resolution VALUE --fps VALUE --duration-ms N --warmup-ms N" C1-LINE
   s"          --metadata-every N --save-every N --max-saved-frames N" C1-LINE
   s"          --manual-exposure-us N --manual-gain N --threshold-delta N" C1-LINE
   s"          --camera-min-spacing-ms N --imu-threshold N --imu-min-spacing-ms N" C1-LINE
   s"          --offset-ns N --max-jitter-ms N --allow-characterization" C1-LINE
   s"          --pre-capture-delay-s N --pre-capture-cue TEXT --tag VALUE" C1-LINE
   s"          --output-id VALUE --output-root PATH --odin-root PATH" C1-LINE ;

: C1-DIE-USAGE ( -- )
   C1-USAGE
   s" cameraone-latency-scenario usage" C1-USAGE-RC die ;

: C1-ARG$ ( -- ptr u8 n )
   C1-ARG-I @ SCRIPT-ARGV$ ;

: C1-VALUE$ ( -- ptr u8 n )
   C1-ARG-I @ 1+ SCRIPT-ARGC >= if C1-DIE-USAGE then
   C1-ARG-I @ 1+ SCRIPT-ARGV$ ;

: C1-ADVANCE ( n -- )
   C1-ARG-I @ + C1-ARG-I ! ;

: C1-FIRST-NAME? ( n -- bool )
   {: c:n :}
   c 48 >= c 57 <= and
   c 65 >= c 90 <= and or
   c 97 >= c 122 <= and or ;

: C1-NAME-CHAR? ( n -- bool )
   {: c:n :}
   c C1-FIRST-NAME? if C1-TRUE exit then
   c 46 = c 95 = or c 45 = or ;

: C1-NAME? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u 0 <= if C1-FALSE exit then
   a c@ C1-FIRST-NAME? 0= if C1-FALSE exit then
   1 C1-I !
   begin C1-I @ u < while
      a C1-I @ + c@ C1-NAME-CHAR? 0= if C1-FALSE exit then
      C1-I @ 1+ C1-I !
   repeat
   C1-TRUE ;

: C1-CHECK-NAME ( ptr u8 n -- )
   C1-NAME? 0= if s" invalid scenario/tag/output name" C1-USAGE-RC die then ;

: C1-OUT-C ( n -- )
   C1-OUTPUT-ID-U @ C1-TEXT-CAP >= if E-STR-CAPACITY throw then
   C1-OUTPUT-ID-BUF C1-OUTPUT-ID-U @ + c!
   C1-OUTPUT-ID-U @ 1+ C1-OUTPUT-ID-U ! ;

: C1-OUT+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   0 C1-I !
   begin C1-I @ u < while
      a C1-I @ + c@ C1-OUT-C
      C1-I @ 1+ C1-I !
   repeat ;

: C1-BUILD-OUTPUT-ID ( -- )
   0 C1-OUTPUT-ID-U !
   C1-SCENARIO$ C1-OUT+
   95 C1-OUT-C
   C1-TAG$ C1-OUT+ ;

: C1-CAMERA-SELECTOR! ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u 58 INDEX-OF {: sep:n :}
   sep 0 <= if C1-DIE-USAGE then
   sep u 1- >= if C1-DIE-USAGE then
   C1-SERIAL-U @ 0= if a sep C1-SERIAL! then
   C1-LOGICAL-U @ 0= if a sep 1+ + u sep 1+ - C1-LOGICAL! then ;

: C1-CAMERA! ( ptr u8 n -- )
   2dup C1-CAMERA-COPY!
   C1-CAMERA-SELECTOR! ;

: C1-REQUIRE-SELECTOR ( -- )
   C1-LOGICAL-U @ 0= C1-SERIAL-U @ 0= and if
      s" supply --camera, --logical-name, or --serial" C1-USAGE-RC die
   then ;

: C1-FINALIZE ( -- )
   C1-SCENARIO-U @ 0= if C1-DIE-USAGE then
   C1-SCENARIO$ C1-CHECK-NAME
   C1-TAG$ C1-CHECK-NAME
   C1-OUTPUT-ID-U @ 0= if C1-BUILD-OUTPUT-ID then
   C1-OUTPUT-ID$ C1-CHECK-NAME
   C1-MANUAL-EXPOSURE-U @ 0 > C1-MANUAL-GAIN-U @ 0= and if s" 1000" C1-MANUAL-GAIN! then
   C1-REQUIRE-SELECTOR ;

: C1-SET-SCENARIO ( ptr u8 n -- )
   C1-SCENARIO-U @ 0 <> if 2drop C1-DIE-USAGE then
   C1-SCENARIO! ;

: C1-OPTION ( -- )
   C1-ARG$ s" -h" STR= if C1-DIE-USAGE then
   C1-ARG$ s" --help" STR= if C1-DIE-USAGE then
   C1-ARG$ s" --dry-run" STR= if 1 C1-DRY-RUN ! 1 C1-ADVANCE exit then
   C1-ARG$ s" --allow-characterization" STR= if 1 C1-ALLOW-CHARACTERIZATION ! 1 C1-ADVANCE exit then
   C1-ARG$ s" --odin-root" STR= if C1-VALUE$ C1-ROOT! 2 C1-ADVANCE exit then
   C1-ARG$ s" --output-root" STR= if C1-VALUE$ C1-OUTPUT-ROOT! 2 C1-ADVANCE exit then
   C1-ARG$ s" --capture-abi" STR= if C1-VALUE$ C1-CAPTURE-ABI! 2 C1-ADVANCE exit then
   C1-ARG$ s" --config" STR= if C1-VALUE$ C1-CONFIG! 2 C1-ADVANCE exit then
   C1-ARG$ s" --logical-name" STR= if C1-VALUE$ C1-LOGICAL! 2 C1-ADVANCE exit then
   C1-ARG$ s" --serial" STR= if C1-VALUE$ C1-SERIAL! 2 C1-ADVANCE exit then
   C1-ARG$ s" --camera" STR= if C1-VALUE$ C1-CAMERA! 2 C1-ADVANCE exit then
   C1-ARG$ s" --resolution" STR= if C1-VALUE$ C1-RESOLUTION! 2 C1-ADVANCE exit then
   C1-ARG$ s" --fps" STR= if C1-VALUE$ C1-FPS! 2 C1-ADVANCE exit then
   C1-ARG$ s" --duration-ms" STR= if C1-VALUE$ C1-DURATION! 2 C1-ADVANCE exit then
   C1-ARG$ s" --warmup-ms" STR= if C1-VALUE$ C1-WARMUP! 2 C1-ADVANCE exit then
   C1-ARG$ s" --metadata-every" STR= if C1-VALUE$ C1-METADATA! 2 C1-ADVANCE exit then
   C1-ARG$ s" --save-every" STR= if C1-VALUE$ C1-SAVE-EVERY! 2 C1-ADVANCE exit then
   C1-ARG$ s" --max-saved-frames" STR= if C1-VALUE$ C1-MAX-SAVED! 2 C1-ADVANCE exit then
   C1-ARG$ s" --manual-exposure-us" STR= if C1-VALUE$ C1-MANUAL-EXPOSURE! 2 C1-ADVANCE exit then
   C1-ARG$ s" --manual-gain" STR= if C1-VALUE$ C1-MANUAL-GAIN! 2 C1-ADVANCE exit then
   C1-ARG$ s" --threshold-delta" STR= if C1-VALUE$ C1-NUM C1-THRESHOLD-DELTA ! 2 C1-ADVANCE exit then
   C1-ARG$ s" --camera-min-spacing-ms" STR= if C1-VALUE$ C1-MS>NS C1-CAMERA-MIN-SPACING-NS ! 2 C1-ADVANCE exit then
   C1-ARG$ s" --imu-threshold" STR= if C1-VALUE$ C1-G>MG C1-IMU-THRESHOLD-MILLI ! 2 C1-ADVANCE exit then
   C1-ARG$ s" --imu-min-spacing-ms" STR= if C1-VALUE$ C1-MS>NS C1-IMU-MIN-SPACING-NS ! 2 C1-ADVANCE exit then
   C1-ARG$ s" --max-jitter-ms" STR= if C1-VALUE$ C1-MS>NS C1-MAX-JITTER-NS ! 2 C1-ADVANCE exit then
   C1-ARG$ s" --offset-ns" STR= if C1-VALUE$ C1-NUM C1-OFFSET-NS ! 1 C1-HAVE-OFFSET ! 2 C1-ADVANCE exit then
   C1-ARG$ s" --pre-capture-delay-s" STR= if C1-VALUE$ C1-PRE-DELAY! 2 C1-ADVANCE exit then
   C1-ARG$ s" --pre-capture-cue" STR= if C1-VALUE$ C1-PRE-CUE! 2 C1-ADVANCE exit then
   C1-ARG$ s" --tag" STR= if C1-VALUE$ C1-TAG! 2 C1-ADVANCE exit then
   C1-ARG$ s" --output-id" STR= if C1-VALUE$ C1-OUTPUT-ID! 2 C1-ADVANCE exit then
   C1-ARG$ s" --" STARTS-WITH? if C1-DIE-USAGE then
   C1-ARG$ C1-SET-SCENARIO
   1 C1-ADVANCE ;

: C1-PARSE ( -- )
   C1-DEFAULTS
   SCRIPT-ARGC 0= if C1-DIE-USAGE then
   begin C1-ARG-I @ SCRIPT-ARGC < while C1-OPTION repeat
   C1-FINALIZE ;

: C1-ABSOLUTE? ( ptr u8 n -- bool )
   {: a:ptr u:n :}
   u 0 > if a c@ 47 = else C1-FALSE then ;

: C1-ROOTED-SB+ ( ptr u8 n -- )
   {: a:ptr u:n :}
   a u C1-ABSOLUTE? 0= if
      C1-ROOT$ SB-APPEND
      C1-ROOT$ dup 0 > if 1- + c@ 47 <> if 47 SB-APPEND-C then else 2drop then
   then
   a u SB-APPEND ;

: C1-ROOTED$ ( ptr u8 n -- ptr u8 n )
   SB-RESET
   C1-ROOTED-SB+
   SB$ ;

: C1-RUN-ROOT$ ( -- ptr u8 n )
   SB-RESET
   C1-OUTPUT-ROOT$ C1-ROOTED-SB+
   47 SB-APPEND-C
   C1-OUTPUT-ID$ SB-APPEND
   SB$ ;

: C1-PATH-COPY ( ptr u8 n ptr u8 ptr a -- ptr u8 n )
   {: a:ptr u:n dst:ptr lenp:ptr :}
   u C1-PATH-CAP >= if E-STR-CAPACITY throw then
   a dst u BYTE-COPY
   u lenp !
   dst u ;

: C1-P0! ( ptr u8 n -- ptr u8 n ) C1-P0 C1-P0-U C1-PATH-COPY ;
: C1-P1! ( ptr u8 n -- ptr u8 n ) C1-P1 C1-P1-U C1-PATH-COPY ;
: C1-P2! ( ptr u8 n -- ptr u8 n ) C1-P2 C1-P2-U C1-PATH-COPY ;

: C1-RUN-ROOT-P0$ ( -- ptr u8 n ) C1-RUN-ROOT$ C1-P0! ;
: C1-CAPTURE-P1$ ( -- ptr u8 n ) C1-RUN-ROOT$ s" capture" C1-P1 JOIN-PATH C1-P1 swap ;
: C1-COMBINED-P2$ ( -- ptr u8 n ) C1-CAPTURE-P1$ s" combined.ndjson" C1-P2 JOIN-PATH C1-P2 swap ;
: C1-SUMMARY-P2$ ( -- ptr u8 n ) C1-RUN-ROOT$ s" summary.md" C1-P2 JOIN-PATH C1-P2 swap ;
: C1-CAMERAONE-IMU$ ( -- ptr u8 n ) C1-RUN-ROOT$ s" cameraone_imu" C1-P2 JOIN-PATH C1-P2 swap ;
: C1-CAMERA-EVENTS$ ( -- ptr u8 n ) C1-RUN-ROOT$ s" latency/camera_events" C1-P2 JOIN-PATH C1-P2 swap ;
: C1-IMU-EVENTS$ ( -- ptr u8 n ) C1-RUN-ROOT$ s" latency/imu_events" C1-P2 JOIN-PATH C1-P2 swap ;
: C1-LATENCY$ ( -- ptr u8 n ) C1-RUN-ROOT$ s" latency/latency_calibration" C1-P2 JOIN-PATH C1-P2 swap ;

: C1-MAYBE$ ( ptr u8 n -- ptr u8 n )
   dup 0= if 2drop s" not supplied" then ;

: C1-YESNO$ ( bool -- ptr u8 n )
   if s" yes" else s" no" then ;

: C1-MD-S ( ptr u8 n ptr u8 n -- )
   MD-S ;

: C1-MD-N ( ptr u8 n n -- )
   MD-N ;

: C1-MD-B ( ptr u8 n bool -- )
   {: label:ptr labelu:n b:bool :}
   label labelu b C1-YESNO$ MD-S ;

: C1-MD-PATH ( ptr u8 n ptr u8 n -- )
   {: label:ptr labelu:n a:ptr u:n :}
   label labelu a u C1-MAYBE$ MD-S ;

: C1-MD-STATUS ( ptr u8 n n -- )
   {: label:ptr labelu:n st:n :}
   st C1-SKIPPED = if label labelu s" skipped" MD-S else label labelu st MD-N then ;

: C1-SUMMARY$ ( -- ptr u8 n )
   RB-RESET
   s" # CameraOne Latency Scenario" RB+ RB-NL RB-NL
   s" execution mode" C1-DRY-RUN @ if s" dry-run" else s" live-requested" then C1-MD-S
   s" scenario" C1-SCENARIO$ C1-MD-S
   s" output id" C1-OUTPUT-ID$ C1-MD-S
   s" Odin root" C1-ROOT$ C1-MD-S
   s" output root" C1-OUTPUT-ROOT$ C1-MD-S
   s" run root" C1-RUN-ROOT$ C1-MD-S
   s" capture" C1-CAPTURE-P1$ C1-MD-S
   s" CameraOne IMU" C1-CAMERAONE-IMU$ C1-MD-S
   s" camera events" C1-CAMERA-EVENTS$ C1-MD-S
   s" IMU events" C1-IMU-EVENTS$ C1-MD-S
   s" latency calibration" C1-LATENCY$ C1-MD-S
   s" capture ABI" C1-CAPTURE-ABI$ C1-MD-S
   s" camera config" C1-CONFIG$ C1-MD-S
   s" capture camera spec" C1-CAMERA$ C1-MD-PATH
   s" selected serial" C1-SERIAL$ C1-MD-PATH
   s" selected logical name" C1-LOGICAL$ C1-MD-PATH
   s" resolution" C1-RESOLUTION$ C1-MD-S
   s" fps" C1-FPS$ C1-MD-S
   s" duration ms" C1-DURATION$ C1-MD-S
   s" warmup ms" C1-WARMUP$ C1-MD-S
   s" metadata every" C1-METADATA$ C1-MD-S
   s" save every" C1-SAVE-EVERY$ C1-MD-S
   s" max saved frames" C1-MAX-SAVED$ C1-MD-S
   s" pre-capture delay s" C1-PRE-DELAY$ C1-MD-S
   s" pre-capture cue" C1-PRE-CUE$ C1-MD-PATH
   s" manual exposure us" C1-MANUAL-EXPOSURE$ C1-MD-PATH
   s" manual gain" C1-MANUAL-GAIN$ C1-MD-PATH
   s" threshold delta" C1-THRESHOLD-DELTA @ C1-MD-N
   s" camera min spacing ns" C1-CAMERA-MIN-SPACING-NS @ C1-MD-N
   s" IMU threshold milli" C1-IMU-THRESHOLD-MILLI @ C1-MD-N
   s" IMU min spacing ns" C1-IMU-MIN-SPACING-NS @ C1-MD-N
   s" max jitter ns" C1-MAX-JITTER-NS @ C1-MD-N
   s" offset ns" C1-OFFSET-NS @ C1-MD-N
   s" offset provided" C1-HAVE-OFFSET @ C1-MD-B
   s" allow characterization" C1-ALLOW-CHARACTERIZATION @ C1-MD-B
   s" capture exit status" C1-CAPTURE-STATUS @ C1-MD-STATUS
   s" analyzer exit status" C1-ANALYZE-STATUS @ C1-MD-STATUS
   s" first nonzero exit status" C1-FIRST-FAILURE @ C1-MD-N
   RB-NL
   C1-DRY-RUN @ if
      s" Capture/analyzer processes started: none." RB+ RB-NL
      s" Live execution will use Habu CameraOne capture with IMAGE sensor records." RB+ RB-NL
   else
      s" Capture backend: Habu CameraOne capture with IMAGE sensor records." RB+ RB-NL
      s" Analyzer execution: Habu CameraOne latency analyzer." RB+ RB-NL
   then
   s" Generated artifacts are local results and should not be committed unless explicitly approved." RB+ RB-NL
   RB$ ;

: C1-STATUS-RESET ( -- )
   C1-SKIPPED C1-CAPTURE-STATUS !
   C1-SKIPPED C1-ANALYZE-STATUS !
   0 C1-FIRST-FAILURE ! ;

: C1-RECORD-STATUS ( n -- )
   dup C1-SKIPPED = if drop exit then
   dup 0 <> C1-FIRST-FAILURE @ 0= and if C1-FIRST-FAILURE ! else drop then ;

: C1-CAPTURE-RUN ( -- )
   HCAP:RESET
   C1-CAPTURE-ABI$ C1-ROOTED$ HCAP:ABI!
   C1-CONFIG$ C1-ROOTED$ HCAP:CONFIG!
   C1-CAPTURE-P1$ HCAP:OUTPUT!
   C1-RESOLUTION$ HCAP:RESOLUTION!
   C1-FPS$ HCAP:FPS!
   C1-DURATION$ HCAP:DURATION-MS!
   C1-WARMUP$ HCAP:WARMUP-MS!
   C1-METADATA$ HCAP:METADATA-EVERY!
   C1-SAVE-EVERY$ HCAP:SAVE-EVERY!
   C1-MAX-SAVED$ HCAP:MAX-SAVED-FRAMES!
   C1-MANUAL-EXPOSURE$ HCAP:MANUAL-EXPOSURE!
   C1-MANUAL-GAIN$ HCAP:MANUAL-GAIN!
   C1-CAMERA-U @ 0 > if C1-CAMERA$ HCAP:CAMERA+ then
   HCAP:INCLUDE-IMAGE-SENSOR!
   HCAP:RUN ;

: C1-CONFIG-COLAT ( -- )
   COLAT:RESET
   C1-LOGICAL-U @ 0 > if C1-LOGICAL$ COLAT:LOGICAL! then
   C1-SERIAL-U @ 0 > if C1-SERIAL$ COLAT:SERIAL! then
   C1-THRESHOLD-DELTA @ COLAT:THRESHOLD-DELTA!
   C1-CAMERA-MIN-SPACING-NS @ COLAT:CAMERA-MIN-SPACING-NS!
   C1-IMU-THRESHOLD-MILLI @ COLAT:IMU-THRESHOLD-MILLI!
   C1-IMU-MIN-SPACING-NS @ COLAT:IMU-MIN-SPACING-NS!
   C1-MAX-JITTER-NS @ COLAT:MAX-JITTER-NS!
   C1-HAVE-OFFSET @ 0 <> if C1-OFFSET-NS @ COLAT:OFFSET-NS! then
   C1-ALLOW-CHARACTERIZATION @ 0 <> if COLAT:ALLOW-CHARACTERIZATION! then ;

: C1-ANALYZE-RUN ( -- n )
   C1-CONFIG-COLAT
   C1-COMBINED-P2$ C1-CAPTURE-P1$ C1-RUN-ROOT-P0$ COLAT:ANALYZE ;

: C1-CALL-ANALYZE ( -- )
   C1-ANALYZE-RUN C1-ANALYZE-STATUS ! ;

: C1-CAPTURE-READY? ( -- bool )
   C1-CAPTURE-STATUS @ 0= if C1-COMBINED-P2$ FILE? else C1-FALSE then ;

: C1-WRITE-SUMMARY ( -- )
   C1-RUN-ROOT-P0$ MAKE-DIRS
   C1-SUMMARY-P2$ C1-SUMMARY$ WRITE-ALL ;

: C1-LIVE-RUN ( -- )
   C1-STATUS-RESET
   C1-RUN-ROOT-P0$ MAKE-DIRS
   [: C1-CAPTURE-RUN ;] catch C1-CAPTURE-STATUS !
   C1-CAPTURE-STATUS @ C1-RECORD-STATUS
   C1-CAPTURE-READY? if
      [: C1-CALL-ANALYZE ;] catch dup 0 <> if C1-ANALYZE-STATUS ! else drop then
      C1-ANALYZE-STATUS @ C1-RECORD-STATUS
   else
      C1-CAPTURE-STATUS @ 0= if 2 C1-ANALYZE-STATUS ! 2 C1-RECORD-STATUS then
   then
   C1-WRITE-SUMMARY
   s" done: " type C1-RUN-ROOT$ type cr
   C1-FIRST-FAILURE @ 0 <> if s" cameraone latency scenario failed" C1-FIRST-FAILURE @ die then ;

: C1-RUN ( -- )
   C1-STATUS-RESET
   C1-SUMMARY$ type
   C1-DRY-RUN @ 0= if C1-LIVE-RUN then ;

: C1-MAIN ( -- )
   C1-PARSE
   C1-RUN ;

end-package
