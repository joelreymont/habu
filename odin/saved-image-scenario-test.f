\ saved-image-scenario-test.f - focused saved-image scenario runner tests.

require lib/errors.f
require lib/string.f
require lib/fs.f
require lib/render.f
require lib/test.f
require odin/saved-image-scenario.f

package SAVEDIMG

: SIS-TEST-ID-PATHS ( -- )
   SIS-DEFAULTS
   s" sky_bright_blue" SIS-SCENARIO!
   s" 20260701_TEST" SIS-TAG!
   SIS-FINALIZE
   SIS-OUTPUT-ID$ s" sky_bright_blue_hd1200_60_20260701_TEST" T$=
   SIS-CAPTURE-DIR$ s" ../Odin/results/capture/sky_bright_blue_hd1200_60_20260701_TEST" T$=
   SIS-EXPOSURE-DIR$ s" ../Odin/results/exposure_adaptation/sky_bright_blue_hd1200_60_20260701_TEST" T$= ;

: SIS-TEST-MANUAL-GAIN ( -- )
   SIS-DEFAULTS
   s" dusk_proxy" SIS-SCENARIO!
   s" 20260701_TEST" SIS-TAG!
   s" 8000" SIS-MANUAL-EXPOSURE!
   SIS-FINALIZE
   SIS-MANUAL-GAIN$ s" 1000" T$= ;

: SIS-TEST-SUMMARY ( -- )
   SIS-DEFAULTS
   1 SIS-DRY-RUN !
   s" target_motion" SIS-SCENARIO!
   s" TESTTAG" SIS-TAG!
   s" SVGA" SIS-RESOLUTION!
   s" 30" SIS-FPS!
   s" 9000" SIS-DURATION!
   s" 1000" SIS-WARMUP!
   s" 5" SIS-SAVE-EVERY!
   s" 12" SIS-MAX-SAVED!
   s" operator cue" SIS-PRE-CUE!
   0 SIS-RUN-EXPOSURE !
   0 SIS-RUN-LOW-LIGHT !
   1 SIS-RUN-MOTION-BLUR !
   0 SIS-REQUIRE-SYNC !
   1 SIS-REQUIRE-PAIRING !
   SIS-FINALIZE
   SIS-SUMMARY$ s" # Saved-Image Scenario Runner

- execution mode: dry-run
- scenario: target_motion
- output id: target_motion_svga_30_TESTTAG
- Odin root: ../Odin
- results root: results
- capture ABI: libodin_zed_capture.so
- camera config: configs/cameras.json
- capture: ../Odin/results/capture/target_motion_svga_30_TESTTAG
- exposure: ../Odin/results/exposure_adaptation/target_motion_svga_30_TESTTAG
- low-light: ../Odin/results/low_light/target_motion_svga_30_TESTTAG
- motion-blur: ../Odin/results/motion_blur/target_motion_svga_30_TESTTAG
- sync: ../Odin/results/timestamp_sync/target_motion_svga_30_TESTTAG
- resolution: SVGA
- fps: 30
- duration ms: 9000
- warmup ms: 1000
- metadata every: 120
- save every: 5
- max saved frames: 12
- pre-capture delay s: 0
- pre-capture cue: operator cue
- manual exposure us: not supplied
- manual gain: not supplied
- exposure manifest: not supplied
- low-light manifest: not supplied
- motion manifest: not supplied
- run exposure: no
- run low-light: no
- run motion-blur: yes
- run sync: yes
- strict sync: no
- require timestamp pairing: yes

Capture/analyzer processes started: none.
Live execution will use the Habu camera-capture backend.
" T$= ;

: SIS-TEST-RUN ( -- )
   T-RESET
   SIS-TEST-ID-PATHS
   SIS-TEST-MANUAL-GAIN
   SIS-TEST-SUMMARY ;

SIS-TEST-RUN
T-REPORT

end-package
