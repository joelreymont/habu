\ fps-sweep-test.f - per-camera FPS quality oracle, derived from src/fps_sweep.zig
\ achievedFpsMilli / summarizeCaseQuality / ratio comparators.
\ Run: cat lib/errors.f lib/string.f lib/test.f odin/fps-sweep.f odin/fps-sweep-test.f | bin/hb

package FPS
private
: FS-RUN ( -- )
   T-RESET
   \ achievedFpsMilli: frames*1e6/duration_ms
   60000 60 1000 FPS-MILLI T=          \ 60 frames in 1000ms -> 60000 milli-fps
   0     0  1000 FPS-MILLI T=
   0     60 0    FPS-MILLI T=          \ duration 0 -> 0
   \ exact ratio comparators
   5 60 0 1 RATIO> TTRUE               \ 5/60 > 0/1
   0 60 5 60 RATIO< TTRUE
   1 2 2 4 RATIO= TTRUE                \ 1/2 == 2/4

   \ a case at fps target 60 over 1000ms with two cameras:
   \ cam0 = 60 seen / 0 dropped (at target), cam1 = 55 seen / 5 dropped (below).
   60 1000 FQ-RESET
   60 0 FQ-ADD
   55 5 FQ-ADD
   FQ-FINISH
   55000 FQ-MIN@ T=                    \ slowest camera 55000 milli-fps
   60000 FQ-MAX@ T=
   5000  FQ-SPREAD@ T=                 \ spread
   1     FQ-ATTGT@ T=                  \ one camera at target
   5     FQ-DROPN@ T=                  \ worst drop 5/60 (cam1)
   60    FQ-DROPD@ T=
   2 FQ-ALL-AT-TARGET? TFALSE          \ not all 2 cameras at target
   1 FQ-ALL-AT-TARGET? TTRUE ;         \ >= 1 is satisfied

\ slots: cat min_fps fps spread drop_num drop_den camera_count
: SET-A ( n n n n n n n -- ) {: cat:n minf:n fps:n spread:n dn:n dd:n cc:n :}
   cat CQ-A-CAT !  minf CQ-A-MINF !  fps CQ-A-FPS !  spread CQ-A-SPREAD !  dn CQ-A-DN !  dd CQ-A-DD !  cc CQ-A-CC ! ;
: SET-B ( n n n n n n n -- ) {: cat:n minf:n fps:n spread:n dn:n dd:n cc:n :}
   cat CQ-B-CAT !  minf CQ-B-MINF !  fps CQ-B-FPS !  spread CQ-B-SPREAD !  dn CQ-B-DN !  dd CQ-B-DD !  cc CQ-B-CC ! ;
: MODE-RUN ( -- )   \ allModeBetter lexicographic order, one assert per tiebreak level
   T-RESET
   2 60000 60 0 0 1 2 SET-A   1 55000 60 0 0 1 2 SET-B   MODE-BETTER? TTRUE   \ A all-at-target, B not
   1 55000 60 0 0 1 2 SET-A   2 60000 60 0 0 1 2 SET-B   MODE-BETTER? TFALSE  \ B all-at-target, A not
   1 0 60 0 0 1 2 SET-A       0 0 60 0 0 1 2 SET-B        MODE-BETTER? TTRUE   \ more cameras-at-target
   1 60000 60 100 0 1 2 SET-A 1 60000 60 500 0 1 2 SET-B  MODE-BETTER? TTRUE   \ smaller spread
   1 30000 60 0 1 4 2 SET-A   1 30000 60 0 2 4 2 SET-B    MODE-BETTER? TTRUE   \ smaller worst-drop ratio
   0 0 30 0 0 1 2 SET-A       0 0 60 0 0 1 2 SET-B        MODE-BETTER? TTRUE ; \ lower fps fallback

FS-RUN
MODE-RUN
T-REPORT
end-package
