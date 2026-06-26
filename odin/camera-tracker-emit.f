\ camera-tracker-emit.f - generates the src/camera_tracker.zig test stream (60 frames
\ at 60 Hz, a target at x=100+5f / y=200, detections every 3rd frame, heartbeats every
\ frame with +20 ms processing latency, confirm_hits defaults to 2) and prints the
\ summary markdown for a byte-exact diff against test "snap: camera-rate summary".
\ Part of the CAMTRACK module: reopens `package CAMTRACK` and drives the tracker by
\ its unqualified names. (When `include` lands this folds into one package block.)

package CAMTRACK
private

variable GF  variable GTS
: GEN ( -- )
   RESET
   0 GF ! begin GF @ 60 < while
      GF @ 16666667 * 1000000000 + GTS !
      GF @ 3 mod 0= if  GF @ 5 * 100 + s>f  200.0  GTS @  DET  then
      GTS @  GTS @ 20000000 +  HB
      GF @ 1+ GF !
   repeat
   FINISH ;

GEN
MD type

end-package
