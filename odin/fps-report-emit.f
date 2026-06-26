\ fps-report-emit.f - emits the FPS sweep report markdown + metrics CSV to stdout
\ for a byte-exact diff against the src/fps_sweep.zig test-699 ohsnap blocks. Feeds
\ the exact test fixture (two "all" cases, two cameras each).

package FPS
private
: FEED ( -- )
   FCR-RESET
   s" all_HD1200_60" s" all" s" HD1200" s" cam_a0+cam_a1" FCR-CASE
   60 2 1000 0 0 FCR-NUMS
   s" 100" s" cam_a0" 10 0 s" pass" FCR-CAM
   s" 200" s" cam_a1" 60 0 s" pass" FCR-CAM
   s" all_HD1080_30" s" all" s" HD1080" s" cam_a0+cam_a1" FCR-CASE
   30 2 1000 0 0 FCR-NUMS
   s" 100" s" cam_a0" 30 0 s" pass" FCR-CAM
   s" 200" s" cam_a1" 29 1 s" pass" FCR-CAM
   FCR-FINISH ;

: SETUP-CAMS ( -- )                              \ the test-679 four-camera fixture
   MC-RESET
   s" cam_a0" 0 MC-ADD  s" cam_a1" 0 MC-ADD
   s" cam_b0" 1 MC-ADD  s" cam_b1" 1 MC-ADD ;

: MARK ( ptr u8 n -- ) type 10 emit ;

FEED  SETUP-CAMS
s" <<<MD>>>"  MARK   FR-MD  type
s" <<<CSV>>>" MARK   FR-CSV type
s" <<<MANIFEST>>>" MARK  FR-MANIFEST type
s" <<<END>>>" MARK
end-package
