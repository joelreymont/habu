\ cameraone-latency-scenario-test.f - focused CameraOne latency scenario tests.

require lib/errors.f
require lib/string.f
require lib/test.f
require odin/cameraone-latency-scenario.f

package C1LAT

: C1-TEST-ID-PATHS ( -- )
   C1-DEFAULTS
   s" tap_cam_a0" C1-SCENARIO!
   s" TESTTAG" C1-TAG!
   s" 306885122:cam_a0" C1-CAMERA!
   C1-FINALIZE
   C1-OUTPUT-ID$ s" tap_cam_a0_TESTTAG" T$=
   C1-RUN-ROOT$ s" ../Odin/results/latency_calibration/cameraone_scenarios/tap_cam_a0_TESTTAG" T$=
   C1-CAPTURE-P1$ s" ../Odin/results/latency_calibration/cameraone_scenarios/tap_cam_a0_TESTTAG/capture" T$=
   C1-COMBINED-P2$ s" ../Odin/results/latency_calibration/cameraone_scenarios/tap_cam_a0_TESTTAG/capture/combined.ndjson" T$= ;

: C1-TEST-MANUAL-GAIN ( -- )
   C1-DEFAULTS
   s" tap_cam_a0" C1-SCENARIO!
   s" cam_a0" C1-LOGICAL!
   s" 8000" C1-MANUAL-EXPOSURE!
   C1-FINALIZE
   C1-MANUAL-GAIN$ s" 1000" T$= ;

: C1-TEST-SUMMARY ( -- )
   C1-DEFAULTS
   1 C1-DRY-RUN !
   s" demo" C1-SCENARIO!
   s" 123:cam_a0" C1-CAMERA!
   C1-FINALIZE
   C1-STATUS-RESET
   C1-SUMMARY$ s" # CameraOne Latency Scenario

- execution mode: dry-run
- scenario: demo
- output id: demo_manual
- Odin root: ../Odin
- output root: results/latency_calibration/cameraone_scenarios
- run root: ../Odin/results/latency_calibration/cameraone_scenarios/demo_manual
- capture: ../Odin/results/latency_calibration/cameraone_scenarios/demo_manual/capture
- CameraOne IMU: ../Odin/results/latency_calibration/cameraone_scenarios/demo_manual/cameraone_imu
- camera events: ../Odin/results/latency_calibration/cameraone_scenarios/demo_manual/latency/camera_events
- IMU events: ../Odin/results/latency_calibration/cameraone_scenarios/demo_manual/latency/imu_events
- latency calibration: ../Odin/results/latency_calibration/cameraone_scenarios/demo_manual/latency/latency_calibration
- capture ABI: libodin_zed_capture.so
- camera config: configs/cameras.json
- capture camera spec: 123:cam_a0
- selected serial: 123
- selected logical name: cam_a0
- resolution: SVGA
- fps: 60
- duration ms: 5000
- warmup ms: 2000
- metadata every: 120
- save every: 1
- max saved frames: 360
- pre-capture delay s: 0
- pre-capture cue: not supplied
- manual exposure us: not supplied
- manual gain: not supplied
- threshold delta: 50
- camera min spacing ns: 0
- IMU threshold milli: 10500
- IMU min spacing ns: 25000000
- max jitter ns: 500000
- offset ns: 0
- offset provided: no
- allow characterization: no
- capture exit status: skipped
- analyzer exit status: skipped
- first nonzero exit status: 0

Capture/analyzer processes started: none.
Live execution will use Habu CameraOne capture with IMAGE sensor records.
Generated artifacts are local results and should not be committed unless explicitly approved.
" T$= ;

: C1-TEST-RUN ( -- )
   T-RESET
   C1-TEST-ID-PATHS
   C1-TEST-MANUAL-GAIN
   C1-TEST-SUMMARY ;

C1-TEST-RUN
T-REPORT

end-package
