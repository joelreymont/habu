\ fps-sweep.f - per-camera FPS quality metrics, ported from src/fps_sweep.zig
\ (achievedFpsMilli + summarizeCaseQuality + the ratio comparators). This is the
\ multi-camera FPS conclusion metric Odin mandates: simultaneous per-camera
\ achieved FPS, camera FPS spread, cameras-at-target count, and worst per-camera
\ drop rate (as an exact num/den ratio, never a rounded float).
\
\ A "case" is one capture run; FQ-RESET sets its fps target + duration, FQ-ADD
\ folds in one camera's (frames_seen, frames_dropped), FQ-FINISH computes spread.
\ Drop rates compare by cross-multiplication (ratioGreater) to stay exact.
\ Signatures use type keywords only. Counts fit i64 for realistic runs (the .zig
\ widens fps-milli to u128; here frames*1e6 stays in i64 for sane frame counts).
\ The mode-ranking decision (allModeBetter, MODE-BETTER?) is tested in
\ fps-sweep-test.f; the byte-exact report + manifest renderers live in
\ odin/fps-report.f.
\ Depends on lib/errors.f lib/string.f.

package FPS
private
1000000 constant FPS-SCALE      \ frames/ms -> milli-fps numerator
1000 constant MS-PER-S

: MIN2 ( n n -- n ) {: a:n b:n :} a b < if a else b then ;
: MAX2 ( n n -- n ) {: a:n b:n :} a b > if a else b then ;

\ achieved FPS in milli-fps: frames_seen * 1e6 / duration_ms (achievedFpsMilli)
public
: FPS-MILLI ( n n -- n ) {: frames:n dur:n :}
   dur 0= if 0 else frames FPS-SCALE * dur / then ;

\ exact ratio comparisons by cross-multiplication (a_num/a_den vs b_num/b_den)
: RATIO> ( n n n n -- bool ) {: an:n ad:n bn:n bd:n :} an bd *  bn ad *  > ;
: RATIO< ( n n n n -- bool ) {: an:n ad:n bn:n bd:n :} an bd *  bn ad *  < ;
: RATIO= ( n n n n -- bool ) {: an:n ad:n bn:n bd:n :} an bd *  bn ad *  = ;

variable FQ-FPS  variable FQ-DUR  variable FQ-HAVE
variable FQ-MIN  variable FQ-MAX  variable FQ-SPREAD  variable FQ-ATTGT
variable FQ-DROPN variable FQ-DROPD

: FQ-RESET ( n n -- ) {: fps:n dur:n :}             \ fps target, duration_ms
   fps FQ-FPS !  dur FQ-DUR !  0 FQ-HAVE !
   0 FQ-MIN !  0 FQ-MAX !  0 FQ-SPREAD !  0 FQ-ATTGT !  0 FQ-DROPN !  1 FQ-DROPD ! ;

\ fold one camera's (frames_seen, frames_dropped) into the case quality
: FQ-ADD ( n n -- ) {: seen:n dropped:n :}
   seen FQ-DUR @ FPS-MILLI {: fpsm:n :}
   FQ-HAVE @ 0= if
      fpsm FQ-MIN !  fpsm FQ-MAX !  -1 FQ-HAVE !
   else
      fpsm FQ-MIN @ MIN2 FQ-MIN !   fpsm FQ-MAX @ MAX2 FQ-MAX !
   then
   seen MS-PER-S *  FQ-FPS @ FQ-DUR @ *  >= if FQ-ATTGT @ 1+ FQ-ATTGT ! then
   seen dropped + 0 > if
      dropped  seen dropped +  FQ-DROPN @ FQ-DROPD @  RATIO> if
         dropped FQ-DROPN !  seen dropped + FQ-DROPD !
      then
   then ;

: FQ-FINISH ( -- ) FQ-MAX @ FQ-MIN @ - FQ-SPREAD ! ;

: FQ-MIN@    ( -- n ) FQ-MIN @ ;
: FQ-MAX@    ( -- n ) FQ-MAX @ ;
: FQ-SPREAD@ ( -- n ) FQ-SPREAD @ ;
: FQ-ATTGT@  ( -- n ) FQ-ATTGT @ ;
: FQ-DROPN@  ( -- n ) FQ-DROPN @ ;
: FQ-DROPD@  ( -- n ) FQ-DROPD @ ;
\ all cameras hit target FPS? (allCamerasAtTarget: cameras_at_target >= camera_count)
: FQ-ALL-AT-TARGET? ( n -- bool ) {: cc:n :} FQ-ATTGT @ cc >= ;

\ --- mode ranking (allModeBetter) ---------------------------------------------
\ Two case-quality slots A (candidate) and B (incumbent); MODE-BETTER? returns
\ whether A should replace B, by the .zig lexicographic order: all-cameras-at-target,
\ then cameras_at_target, then min-fps/fps ratio, then smaller spread, then smaller
\ worst-drop ratio, then lower fps. Fields: cameras_at_target, min_fps_milli, fps,
\ spread_milli, drop_num, drop_den, camera_count.
variable CQ-A-CAT variable CQ-A-MINF variable CQ-A-FPS variable CQ-A-SPREAD
variable CQ-A-DN variable CQ-A-DD variable CQ-A-CC
variable CQ-B-CAT variable CQ-B-MINF variable CQ-B-FPS variable CQ-B-SPREAD
variable CQ-B-DN variable CQ-B-DD variable CQ-B-CC

: MODE-BETTER? ( -- bool )
   CQ-A-CAT @ CQ-A-CC @ >= {: aat:bool :}
   CQ-B-CAT @ CQ-B-CC @ >= {: bat:bool :}
   aat if bat 0= if 0 0= exit then then          \ A all-at-target, B not -> A better
   aat 0= if bat if 0 0= 0= exit then then        \ B all-at-target, A not -> A worse
   CQ-A-CAT @ CQ-B-CAT @ <> if CQ-A-CAT @ CQ-B-CAT @ > exit then
   CQ-A-MINF @ CQ-A-FPS @ CQ-B-MINF @ CQ-B-FPS @ RATIO= 0= if
      CQ-A-MINF @ CQ-A-FPS @ CQ-B-MINF @ CQ-B-FPS @ RATIO> exit then
   CQ-A-SPREAD @ CQ-B-SPREAD @ <> if CQ-A-SPREAD @ CQ-B-SPREAD @ < exit then
   CQ-A-DN @ CQ-A-DD @ CQ-B-DN @ CQ-B-DD @ RATIO= 0= if
      CQ-A-DN @ CQ-A-DD @ CQ-B-DN @ CQ-B-DD @ RATIO< exit then
   CQ-A-FPS @ CQ-B-FPS @ < ;
end-package
