\ fps-report.f - the FPS sweep report model + byte-exact renderers, ported from
\ writeSweepReports + summarizeCaseQuality in src/fps_sweep.zig. A run is a set of
\ "cases"; each case carries metadata (name/subset/resolution/fps/cameras/duration)
\ plus its per-camera capture summaries (serial/logical_name/frames_seen/dropped/
\ result). FCR-FINISH computes the per-case quality (min/max/spread achieved FPS in
\ milli, cameras-at-target, worst per-camera drop ratio, totals). The renderers emit
\ the markdown summary and the metrics CSV exactly as the Zig (writeMilli3/
\ writeFixed3, the all-camera + full-sweep tables, and the best-all-case narrative).
\ Per-camera index-gap and SDK/host jitter come from the timing report in the Zig;
\ this capture-summary model carries them as 0 (the fixture has no timing records),
\ matching the Zig's not-found default. Depends on lib/errors.f lib/string.f
\ lib/prelude.f lib/render.f odin/fps-sweep.f (FPS-MILLI/RATIO>/MIN2/MAX2).

package FPS
private
64 constant PCR-MAX                       \ max cameras across all cases
16 constant FCR-MAX                       \ max cases
-6211 constant E-FCR-FULL
-6212 constant E-PCR-FULL

\ per-camera table (global, cases index ranges into it)
create PCR-SER-A PCR-MAX cells allot  create PCR-SER-N PCR-MAX cells allot
create PCR-LNA-A PCR-MAX cells allot  create PCR-LNA-N PCR-MAX cells allot
create PCR-RES-A PCR-MAX cells allot  create PCR-RES-N PCR-MAX cells allot
create PCR-SEEN  PCR-MAX cells allot  create PCR-DROP  PCR-MAX cells allot
create PCR-ACH   PCR-MAX cells allot      \ achieved milli-fps (computed)
variable PCR-N

\ per-case table
create FCR-NA-A FCR-MAX cells allot  create FCR-NA-N FCR-MAX cells allot   \ name
create FCR-SB-A FCR-MAX cells allot  create FCR-SB-N FCR-MAX cells allot   \ subset
create FCR-RS-A FCR-MAX cells allot  create FCR-RS-N FCR-MAX cells allot   \ resolution
create FCR-CM-A FCR-MAX cells allot  create FCR-CM-N FCR-MAX cells allot   \ cameras string
create FCR-FPS  FCR-MAX cells allot  create FCR-CC   FCR-MAX cells allot
create FCR-DUR  FCR-MAX cells allot  create FCR-REGR FCR-MAX cells allot
create FCR-FATAL FCR-MAX cells allot
create FCR-CST  FCR-MAX cells allot  create FCR-CN   FCR-MAX cells allot   \ camera range [start,count)
create FCR-MINF FCR-MAX cells allot  create FCR-MAXF FCR-MAX cells allot
create FCR-SPR  FCR-MAX cells allot  create FCR-ATT  FCR-MAX cells allot
create FCR-WDN  FCR-MAX cells allot  create FCR-WDD  FCR-MAX cells allot
create FCR-TSN  FCR-MAX cells allot  create FCR-TDR  FCR-MAX cells allot
variable FCR-N

public
: FCR-RESET ( -- ) 0 FCR-N !  0 PCR-N ! ;

\ start a new case with its metadata strings ( name$ subset$ res$ cameras$ -- )
: FCR-CASE ( ptr u8 n ptr u8 n ptr u8 n ptr u8 n -- )
   {: na:ptr nn:n sa:ptr sn:n ra:ptr rn:n ca:ptr cn:n :}
   FCR-N @ FCR-MAX >= if E-FCR-FULL throw then
   FCR-N @ {: ix:n :}
   na FCR-NA-A ix cells + !  nn FCR-NA-N ix cells + !
   sa FCR-SB-A ix cells + !  sn FCR-SB-N ix cells + !
   ra FCR-RS-A ix cells + !  rn FCR-RS-N ix cells + !
   ca FCR-CM-A ix cells + !  cn FCR-CM-N ix cells + !
   PCR-N @ FCR-CST ix cells + !   0 FCR-CN ix cells + !
   ix 1+ FCR-N ! ;

\ set the current case's numeric fields ( fps camcount dur regr fatal -- )
: FCR-NUMS ( n n n n n -- ) {: fps:n cc:n dur:n regr:n fatal:n :}
   FCR-N @ 1- {: ix:n :}
   fps FCR-FPS ix cells + !  cc FCR-CC ix cells + !  dur FCR-DUR ix cells + !
   regr FCR-REGR ix cells + !  fatal FCR-FATAL ix cells + ! ;

\ append a camera to the current case ( serial$ lname$ seen dropped result$ -- )
: FCR-CAM ( ptr u8 n ptr u8 n n n ptr u8 n -- ) {: sa:ptr sn:n la:ptr ln:n seen:n drp:n ra:ptr rn:n :}
   PCR-N @ PCR-MAX >= if E-PCR-FULL throw then
   sa PCR-SER-A PCR-N @ cells + !  sn PCR-SER-N PCR-N @ cells + !
   la PCR-LNA-A PCR-N @ cells + !  ln PCR-LNA-N PCR-N @ cells + !
   ra PCR-RES-A PCR-N @ cells + !  rn PCR-RES-N PCR-N @ cells + !
   seen PCR-SEEN PCR-N @ cells + !  drp PCR-DROP PCR-N @ cells + !
   0 PCR-ACH PCR-N @ cells + !
   FCR-CN FCR-N @ 1- cells + dup @ 1+ swap !
   PCR-N @ 1+ PCR-N ! ;

\ compute per-case quality over its cameras (summarizeCaseQuality).
\ No locals inside the loops (Habu rejects mid-control locals); the case index
\ FCR-CI, camera index FCR-KJ, and achieved-fps FCR-ACH are module vars.
variable FCR-CI  variable FCR-KJ  variable FCR-K0  variable FCR-K1  variable FCR-ACH
: FCR-FINISH ( -- )
   0 FCR-CI !
   begin FCR-CI @ FCR-N @ < while
      FCR-CST FCR-CI @ cells + @ FCR-K0 !
      FCR-K0 @ FCR-CN FCR-CI @ cells + @ + FCR-K1 !
      0 FCR-ATT FCR-CI @ cells + !   0 FCR-WDN FCR-CI @ cells + !   1 FCR-WDD FCR-CI @ cells + !
      0 FCR-TSN FCR-CI @ cells + !   0 FCR-TDR FCR-CI @ cells + !
      0 FCR-MINF FCR-CI @ cells + !  0 FCR-MAXF FCR-CI @ cells + !
      FCR-K0 @ FCR-KJ !
      begin FCR-KJ @ FCR-K1 @ < while
         PCR-SEEN FCR-KJ @ cells + @  FCR-DUR FCR-CI @ cells + @  FPS-MILLI  FCR-ACH !
         FCR-ACH @ PCR-ACH FCR-KJ @ cells + !
         FCR-KJ @ FCR-K0 @ = if
            FCR-ACH @ FCR-MINF FCR-CI @ cells + !  FCR-ACH @ FCR-MAXF FCR-CI @ cells + !
         else
            FCR-ACH @ FCR-MINF FCR-CI @ cells + @ MIN2 FCR-MINF FCR-CI @ cells + !
            FCR-ACH @ FCR-MAXF FCR-CI @ cells + @ MAX2 FCR-MAXF FCR-CI @ cells + !
         then
         PCR-SEEN FCR-KJ @ cells + @ 1000 *  FCR-FPS FCR-CI @ cells + @ FCR-DUR FCR-CI @ cells + @ *  >= if
            FCR-ATT FCR-CI @ cells + dup @ 1+ swap ! then
         PCR-SEEN FCR-KJ @ cells + @ PCR-DROP FCR-KJ @ cells + @ + 0 > if
            PCR-DROP FCR-KJ @ cells + @   PCR-SEEN FCR-KJ @ cells + @ PCR-DROP FCR-KJ @ cells + @ +
            FCR-WDN FCR-CI @ cells + @ FCR-WDD FCR-CI @ cells + @ RATIO> if
               PCR-DROP FCR-KJ @ cells + @ FCR-WDN FCR-CI @ cells + !
               PCR-SEEN FCR-KJ @ cells + @ PCR-DROP FCR-KJ @ cells + @ + FCR-WDD FCR-CI @ cells + !
            then
         then
         FCR-TSN FCR-CI @ cells + dup @  PCR-SEEN FCR-KJ @ cells + @ +  swap !
         FCR-TDR FCR-CI @ cells + dup @  PCR-DROP FCR-KJ @ cells + @ +  swap !
         FCR-KJ @ 1+ FCR-KJ !
      repeat
      FCR-MAXF FCR-CI @ cells + @ FCR-MINF FCR-CI @ cells + @ - FCR-SPR FCR-CI @ cells + !
      FCR-CI @ 1+ FCR-CI !
   repeat ;

\ --- case accessors ( case-index -- value ) ---
private
: FCR-NAME@  ( n -- ptr u8 n ) {: ix:n :} FCR-NA-A ix cells + @  FCR-NA-N ix cells + @ ;
: FCR-SUB@   ( n -- ptr u8 n ) {: ix:n :} FCR-SB-A ix cells + @  FCR-SB-N ix cells + @ ;
: FCR-RES@   ( n -- ptr u8 n ) {: ix:n :} FCR-RS-A ix cells + @  FCR-RS-N ix cells + @ ;
: FCR-CAMS@  ( n -- ptr u8 n ) {: ix:n :} FCR-CM-A ix cells + @  FCR-CM-N ix cells + @ ;
: FCR-FPS@   ( n -- n ) cells FCR-FPS + @ ;
: FCR-CC@    ( n -- n ) cells FCR-CC + @ ;
: FCR-DUR@   ( n -- n ) cells FCR-DUR + @ ;
: FCR-REGR@  ( n -- n ) cells FCR-REGR + @ ;
: FCR-FATAL@ ( n -- n ) cells FCR-FATAL + @ ;
: FCR-CST@   ( n -- n ) cells FCR-CST + @ ;
: FCR-CN@    ( n -- n ) cells FCR-CN + @ ;
: FCR-MINF@  ( n -- n ) cells FCR-MINF + @ ;
: FCR-MAXF@  ( n -- n ) cells FCR-MAXF + @ ;
: FCR-SPR@   ( n -- n ) cells FCR-SPR + @ ;
: FCR-ATT@   ( n -- n ) cells FCR-ATT + @ ;
: FCR-WDN@   ( n -- n ) cells FCR-WDN + @ ;
: FCR-WDD@   ( n -- n ) cells FCR-WDD + @ ;
: FCR-TSN@   ( n -- n ) cells FCR-TSN + @ ;
: FCR-TDR@   ( n -- n ) cells FCR-TDR + @ ;
: FCR-COUNT  ( -- n ) FCR-N @ ;
: FCR-ALL?   ( n -- bool ) FCR-SUB@ s" all" STR= ;
\ --- camera accessors ( camera-index -- value ) ---
: PCR-SER@ ( n -- ptr u8 n ) {: kk:n :} PCR-SER-A kk cells + @  PCR-SER-N kk cells + @ ;
: PCR-LNA@ ( n -- ptr u8 n ) {: kk:n :} PCR-LNA-A kk cells + @  PCR-LNA-N kk cells + @ ;
: PCR-RES@ ( n -- ptr u8 n ) {: kk:n :} PCR-RES-A kk cells + @  PCR-RES-N kk cells + @ ;
: PCR-SEEN@ ( n -- n ) cells PCR-SEEN + @ ;
: PCR-DROP@ ( n -- n ) cells PCR-DROP + @ ;
: PCR-ACH@  ( n -- n ) cells PCR-ACH + @ ;

\ ===========================================================================
\ best all-case selection (allModeBetter) for the report narrative
\ ===========================================================================
\ is case a strictly better than case b, by the Zig lexicographic order?
: CASE-BETTER? ( n n -- bool ) {: a:n b:n :}
   a FCR-ATT@ a FCR-CC@ >= {: aat:bool :}
   b FCR-ATT@ b FCR-CC@ >= {: bat:bool :}
   aat if bat 0= if true exit then then
   aat 0= if bat if false exit then then
   a FCR-ATT@ b FCR-ATT@ <> if a FCR-ATT@ b FCR-ATT@ > exit then
   a FCR-MINF@ a FCR-FPS@ b FCR-MINF@ b FCR-FPS@ RATIO= 0= if
      a FCR-MINF@ a FCR-FPS@ b FCR-MINF@ b FCR-FPS@ RATIO> exit then
   a FCR-SPR@ b FCR-SPR@ <> if a FCR-SPR@ b FCR-SPR@ < exit then
   a FCR-WDN@ a FCR-WDD@ b FCR-WDN@ b FCR-WDD@ RATIO= 0= if
      a FCR-WDN@ a FCR-WDD@ b FCR-WDN@ b FCR-WDD@ RATIO< exit then
   a FCR-FPS@ b FCR-FPS@ < ;

\ eligible all-camera case: subset "all", no fatal/regression, some frames
: ELIGIBLE? ( n -- bool ) {: ci:n :}
   ci FCR-ALL? if
      ci FCR-FATAL@ 0= if
         ci FCR-REGR@ 0= if
            ci FCR-TSN@ ci FCR-TDR@ + 0 > exit
         then
      then
   then false ;

variable FRB-BEST  variable FRB-I
: FR-BEST ( -- n )
   -1 FRB-BEST !  0 FRB-I !
   begin FRB-I @ FCR-N @ < while
      FRB-I @ ELIGIBLE? if
         FRB-BEST @ 0 < if FRB-I @ FRB-BEST !
         else FRB-I @ FRB-BEST @ CASE-BETTER? if FRB-I @ FRB-BEST ! then
         then
      then
      FRB-I @ 1+ FRB-I !
   repeat
   FRB-BEST @ ;

\ ===========================================================================
\ markdown + CSV renderers
\ ===========================================================================
\ per-camera FPS list for a case: "lname=milli3; lname=milli3"
variable FR-MK
: FR-PCFPS ( n -- ) {: ci:n :}
   FCR-CST ci cells + @ FR-MK !
   begin FR-MK @  FCR-CST ci cells + @ FCR-CN ci cells + @ +  < while
      FR-MK @ PCR-LNA@ RB+  61 RB-C  FR-MK @ PCR-ACH@ RB-MILLI3
      FR-MK @ 1+  FCR-CST ci cells + @ FCR-CN ci cells + @ +  < if s" ; " RB+ then
      FR-MK @ 1+ FR-MK !
   repeat ;

\ verdict text (writeVerdict)
: FR-VERDICT ( n -- ) {: ci:n :}
   ci FCR-ATT@ ci FCR-CC@ < if s" rejected: below target" RB+ exit then
   ci FCR-SPR@ 0 <> if s" rejected: uneven FPS" RB+ exit then
   ci FCR-TDR@ 0 <> if s" rejected: drops present" RB+ exit then
   ci FCR-WDN@ 0 <> if s" rejected: drops present" RB+ exit then
   s" candidate" RB+ ;

\ all-camera suitability table row (only for subset "all" cases)
: FR-MDROW-ALL ( n -- ) {: ci:n :}
   LBAR ci FCR-NAME@ RB+
   BAR ci FCR-FPS@ RB#
   BAR ci FR-PCFPS
   BAR ci FCR-MINF@ RB-MILLI3
   BAR ci FCR-MAXF@ RB-MILLI3
   BAR ci FCR-SPR@ RB-MILLI3
   BAR ci FCR-ATT@ RB# 47 RB-C ci FCR-CC@ RB#
   BAR ci FCR-TDR@ RB#
   BAR ci FCR-WDN@ ci FCR-WDD@ RB-FIXED3
   BAR ci FR-VERDICT
   RBAR RB-NL ;

\ full sweep metrics table row (all cases). index gap / jitter are 0 (no timing).
: FR-MDROW-FULL ( n -- ) {: ci:n :}
   LBAR ci FCR-NAME@ RB+
   BAR ci FCR-CAMS@ RB+
   BAR ci FCR-FPS@ RB#
   BAR ci FCR-MINF@ RB-MILLI3
   BAR ci FCR-MAXF@ RB-MILLI3
   BAR ci FCR-SPR@ RB-MILLI3
   BAR ci FCR-ATT@ RB# 47 RB-C ci FCR-CC@ RB#
   BAR ci FCR-TSN@ ci FCR-DUR@ FPS-MILLI RB-MILLI3
   BAR ci FCR-TSN@ RB#
   BAR ci FCR-TDR@ RB#
   BAR ci FCR-TDR@  ci FCR-TSN@ ci FCR-TDR@ +  RB-FIXED3
   BAR ci FCR-WDN@ ci FCR-WDD@ RB-FIXED3
   BAR 48 RB-C
   BAR 48 RB-C
   BAR 48 RB-C
   BAR ci FCR-REGR@ RB#
   RBAR RB-NL ;

variable FR-VIA
: FR-NARRATIVE ( -- )
   FR-BEST {: best:n :}
   best 0 < if exit then
   0 FR-VIA !
   best FCR-ATT@ best FCR-CC@ >= if best FCR-SPR@ 0= if best FCR-WDN@ 0= if -1 FR-VIA ! then then then
   RB-NL
   FR-VIA @ 0 <> if s" All-camera candidate: " RB+
   else s" No all-camera mode sustained the requested per-camera FPS on every camera with equal achieved FPS and zero drops. Best observed by per-camera target attainment: " RB+ then
   96 RB-C  best FCR-NAME@ RB+  96 RB-C
   s"  with min/max per-camera FPS " RB+  best FCR-MINF@ RB-MILLI3  47 RB-C  best FCR-MAXF@ RB-MILLI3
   s" , spread " RB+  best FCR-SPR@ RB-MILLI3
   s" , cameras at target " RB+  best FCR-ATT@ RB#  47 RB-C  best FCR-CC@ RB#
   s" , worst camera drop rate " RB+  best FCR-WDN@ best FCR-WDD@ RB-FIXED3
   46 RB-C
   FR-VIA @ 0 <> if s"  Treat this as a candidate, not a sustained recommendation, until repeated runs and resource telemetry confirm stability." RB+
   else s"  Do not treat this as a sustained operating-mode recommendation until the capture pipeline can keep every camera at the target FPS with low spread and minimal drops." RB+ then
   RB-NL ;

variable FR-MI
public
: FR-MD ( -- ptr u8 n )
   RB-RESET
   s" # Four-Camera FPS Sweep" RB+ RB-NL  RB-NL
   s" The acceptance metric is simultaneous per-camera FPS, not aggregate FPS." RB+ RB-NL
   s" A viable mode must keep every camera at the requested FPS with low" RB+ RB-NL
   s" per-camera spread and minimal drops." RB+ RB-NL  RB-NL
   s" Capture-backed sweep execution is owned by the Habu runner layer; this report consumes sweep case records and renders suitability evidence." RB+ RB-NL  RB-NL
   s" ## All-Camera Suitability" RB+ RB-NL  RB-NL
   s" | mode | target FPS/camera | per-camera FPS | min FPS | max FPS | spread | cameras at target | drops | worst drop rate | verdict |" RB+ RB-NL
   s" | --- | ---: | --- | ---: | ---: | ---: | ---: | ---: | ---: | --- |" RB+ RB-NL
   0 FR-MI ! begin FR-MI @ FCR-N @ < while  FR-MI @ FCR-ALL? if FR-MI @ FR-MDROW-ALL then  FR-MI @ 1+ FR-MI ! repeat
   RB-NL
   s" ## Full Sweep Metrics" RB+ RB-NL  RB-NL
   s" | case | cameras | target FPS/camera | min FPS/camera | max FPS/camera | FPS spread | cameras at target | total FPS | frames | drops | total drop rate | worst camera drop rate | max index gap | SDK jitter max ns | host jitter max ns | regressions |" RB+ RB-NL
   s" | --- | --- | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: | ---: |" RB+ RB-NL
   0 FR-MI ! begin FR-MI @ FCR-N @ < while  FR-MI @ FR-MDROW-FULL  FR-MI @ 1+ FR-MI ! repeat
   FR-NARRATIVE
   RB$ ;

\ metrics CSV: one row per camera per case
private
: FR-CSV-HDR ( -- )
   s" case_name,subset,resolution,fps,camera_count,cameras,duration_ms,total_frames,total_drops," RB+
   s" total_drop_rate,total_achieved_fps,target_camera_fps,min_camera_achieved_fps,max_camera_achieved_fps," RB+
   s" camera_fps_spread,cameras_at_target,worst_camera_drop_rate,timestamp_regressions,fatal_errors," RB+
   s" camera_serial,logical_name,camera_frames,camera_drops,camera_drop_rate,camera_achieved_fps," RB+
   s" max_index_gap_drop,sdk_jitter_max_abs_ns,host_jitter_max_abs_ns,result" RB+ RB-NL ;

: FR-CSVROW ( n n -- ) {: ci:n kk:n :}
   ci FCR-NAME@ RB+ CM  ci FCR-SUB@ RB+ CM  ci FCR-RES@ RB+ CM  ci FCR-FPS@ RB# CM
   ci FCR-CC@ RB# CM  ci FCR-CAMS@ RB+ CM  ci FCR-DUR@ RB# CM
   ci FCR-TSN@ RB# CM  ci FCR-TDR@ RB# CM
   ci FCR-TDR@ ci FCR-TSN@ ci FCR-TDR@ + RB-FIXED3 CM
   ci FCR-TSN@ ci FCR-DUR@ FPS-MILLI RB-MILLI3 CM
   ci FCR-FPS@ RB# CM
   ci FCR-MINF@ RB-MILLI3 CM  ci FCR-MAXF@ RB-MILLI3 CM  ci FCR-SPR@ RB-MILLI3 CM
   ci FCR-ATT@ RB# CM
   ci FCR-WDN@ ci FCR-WDD@ RB-FIXED3 CM
   ci FCR-REGR@ RB# CM  ci FCR-FATAL@ RB# CM
   kk PCR-SER@ RB+ CM  kk PCR-LNA@ RB+ CM
   kk PCR-SEEN@ RB# CM  kk PCR-DROP@ RB# CM
   kk PCR-DROP@ kk PCR-SEEN@ kk PCR-DROP@ + RB-FIXED3 CM
   kk PCR-ACH@ RB-MILLI3 CM
   48 RB-C CM  48 RB-C CM  48 RB-C CM
   kk PCR-RES@ RB+ RB-NL ;

variable FR-CCI  variable FR-CKK
public
: FR-CSV ( -- ptr u8 n )
   RB-RESET  FR-CSV-HDR
   0 FR-CCI ! begin FR-CCI @ FCR-N @ < while
      FCR-CST FR-CCI @ cells + @ FR-CKK !
      begin FR-CKK @  FCR-CST FR-CCI @ cells + @ FCR-CN FR-CCI @ cells + @ +  < while
         FR-CCI @ FR-CKK @ FR-CSVROW
         FR-CKK @ 1+ FR-CKK !
      repeat
      FR-CCI @ 1+ FR-CCI !
   repeat
   RB$ ;

\ ===========================================================================
\ sweep matrix manifest (buildSweepMatrix + renderManifestCsv). Generates the
\ case rows directly: per mode (resolution,fps) it enumerates each camera single,
\ then group_a, group_b, cross_a0_b0, all. name = "{subset}_{resolution}_{fps}",
\ cameras = the subset's logical names joined by '+'. Group 0 = A, 1 = B.
\ ===========================================================================
create MC-NA-A 8 cells allot  create MC-NA-N 8 cells allot  create MC-GRP 8 cells allot
variable MC-N
: MC-RESET ( -- ) 0 MC-N ! ;
: MC-ADD ( ptr u8 n n -- ) {: na:ptr nn:n grp:n :}
   na MC-NA-A MC-N @ cells + !  nn MC-NA-N MC-N @ cells + !  grp MC-GRP MC-N @ cells + !
   MC-N @ 1+ MC-N ! ;
private
: MC-NAME@ ( n -- ptr u8 n ) {: kk:n :} MC-NA-A kk cells + @  MC-NA-N kk cells + @ ;

variable MR-J  variable MR-CNT  variable MR-FX
: MC-FIRST@ ( n -- ptr u8 n ) {: grp:n :}          \ first camera name in group grp
   0 MR-FX !
   begin MR-FX @ MC-N @ < while
      MC-GRP MR-FX @ cells + @ grp = if MR-FX @ MC-NAME@ exit then
      MR-FX @ 1+ MR-FX !
   repeat  0 MC-NAME@ ;
: COUNT-GROUP ( n -- n ) {: grp:n :}
   0 MR-CNT !  0 MR-J !
   begin MR-J @ MC-N @ < while
      MC-GRP MR-J @ cells + @ grp = if MR-CNT @ 1+ MR-CNT ! then
      MR-J @ 1+ MR-J !
   repeat  MR-CNT @ ;
: JOIN-GROUP ( n -- ) {: grp:n :}                 \ emit '+'-joined names of group grp
   0 MR-CNT !  0 MR-J !
   begin MR-J @ MC-N @ < while
      MC-GRP MR-J @ cells + @ grp = if
         MR-CNT @ 0 > if 43 RB-C then
         MR-J @ MC-NAME@ RB+  MR-CNT @ 1+ MR-CNT !
      then
      MR-J @ 1+ MR-J !
   repeat ;
: JOIN-ALL ( -- )                               \ emit '+'-joined names of all cameras
   0 MR-J ! begin MR-J @ MC-N @ < while
      MR-J @ 0 > if 43 RB-C then  MR-J @ MC-NAME@ RB+  MR-J @ 1+ MR-J ! repeat ;

\ emit "subset_res_fps,subset,res,fps,count," (cameras + newline follow)
: CASE-HEAD ( ptr u8 n ptr u8 n n n -- ) {: sa:ptr sn:n ra:ptr rn:n fps:n cnt:n :}
   sa sn RB+ 95 RB-C ra rn RB+ 95 RB-C fps RB#  CM
   sa sn RB+ CM  ra rn RB+ CM  fps RB# CM  cnt RB# CM ;

variable MR-CI
: MODE-ROWS ( ptr u8 n n -- ) {: ra:ptr rn:n fps:n :}    \ all subset rows for one mode
   0 MR-CI ! begin MR-CI @ MC-N @ < while            \ singles
      MR-CI @ MC-NAME@ ra rn fps 1 CASE-HEAD  MR-CI @ MC-NAME@ RB+ RB-NL
      MR-CI @ 1+ MR-CI !
   repeat
   0 COUNT-GROUP 0 > if
      s" group_a" ra rn fps 0 COUNT-GROUP CASE-HEAD  0 JOIN-GROUP RB-NL then
   1 COUNT-GROUP 0 > if
      s" group_b" ra rn fps 1 COUNT-GROUP CASE-HEAD  1 JOIN-GROUP RB-NL then
   0 COUNT-GROUP 0 > if 1 COUNT-GROUP 0 > if
      s" cross_a0_b0" ra rn fps 2 CASE-HEAD
      0 MC-FIRST@ RB+ 43 RB-C 1 MC-FIRST@ RB+ RB-NL then then
   s" all" ra rn fps MC-N @ CASE-HEAD  JOIN-ALL RB-NL ;

public
: FR-MANIFEST ( -- ptr u8 n )
   RB-RESET
   s" case_name,subset,resolution,fps,camera_count,cameras" RB+ RB-NL
   s" HD1200" 60 MODE-ROWS   s" HD1200" 30 MODE-ROWS
   s" HD1080" 60 MODE-ROWS   s" HD1080" 30 MODE-ROWS
   s" SVGA" 60 MODE-ROWS     s" SVGA" 120 MODE-ROWS
   RB$ ;
end-package
