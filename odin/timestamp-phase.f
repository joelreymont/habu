\ timestamp-phase.f - frame phase-offset analysis, ported from
\ src/timestamp_metrics.zig (buildFramePhaseOffsets / bestFramePhaseOffset /
\ phaseOffsetStats). Layered on timestamp-metrics.f (reuses TM-N camera count,
\ MIN2/MAX2/PCTL) + lib/hashmap.f + lib/sort.f.
\
\ Frame samples (camera-index, frame-index, sdk timestamp) are fed with FS-ADD and
\ indexed in an O(1) hash keyed by (frame_index<<4 | camera_index) - camera_index
\ < 16 - so the per-(camera,frame) timestamp lookup the offset sweep needs is O(1),
\ not the .zig's linear scan. For each camera vs the reference (camera 0), the sweep
\ tries frame offsets -2..2, scoring each by the |sdk - sdk| skew distribution
\ (mean/p95/p99/max, nearest-rank), and keeps the offset with the smallest p99
\ (ties broken by more matched frames). same_index_sdk_skew_p99 records offset 0.
\ Sentinels: AFO/FS-LOOKUP return -1 for "none" (frames/timestamps are non-negative).

package CAMSYNC
private
2048 constant FS-MAX
4096 constant FS-CAP
create FS-CAMV FS-MAX cells allot      \ per-sample camera index
create FS-IDXV FS-MAX cells allot      \ per-sample frame index
create FS-SDKV FS-MAX cells allot      \ per-sample sdk timestamp
create FS-K FS-CAP cells allot         \ hash key (frame<<4|cam)
create FS-U FS-CAP cells allot         \ hash used flag
create FS-V FS-CAP cells allot         \ hash value: sdk timestamp
variable FS-N  variable FS-SLOT  variable FS-SI

create PHO-OFF    TM-MAX cells allot    \ per-camera best frame offset
create PHO-MATCH  TM-MAX cells allot    \ matched frames
create PHO-MEAN   TM-MAX cells allot
create PHO-P95    TM-MAX cells allot
create PHO-P99    TM-MAX cells allot
create PHO-MAX    TM-MAX cells allot
create PHO-SAME99 TM-MAX cells allot    \ offset-0 p99
variable PHO-SI

create PO-SKEW FS-MAX cells allot       \ skew samples for one (cam, offset)
variable PO-N  variable PO-SUM  variable PO-TF  variable PO-TSDK  variable PO-SK
variable PS-MATCH variable PS-MEAN variable PS-P95 variable PS-P99 variable PS-MAX
variable PB-SET variable PB-OFF variable PB-MATCH variable PB-MEAN
variable PB-P95 variable PB-P99 variable PB-MAX variable PB-SAME99 variable PB-OI

: ABS-I ( n -- n ) {: x:n :} x 0 < if 0 x - else x then ;
: FS-KEY ( n n -- n ) {: cam:n fidx:n :} fidx 4 lshift cam or ;

public
: FS-RESET ( -- ) 0 FS-N !  FS-U FS-CAP HM-CLEAR ;
-6204 constant E-FS-FULL
: FS-ADD ( n n n -- ) {: cam:n fidx:n sdk:n :}
   FS-N @ FS-MAX >= if E-FS-FULL throw then        \ guard: never silently drop a sample
   cam FS-CAMV FS-N @ cells + !  fidx FS-IDXV FS-N @ cells + !  sdk FS-SDKV FS-N @ cells + !
   FS-N @ 1+ FS-N !
   FS-K FS-U FS-CAP  cam fidx FS-KEY  HM-PROBE FS-SLOT !
   FS-U FS-SLOT @ cells + @ 0= if          \ first sample for this (cam,frame) wins
      cam fidx FS-KEY FS-K FS-SLOT @ cells + !  -1 FS-U FS-SLOT @ cells + !  sdk FS-V FS-SLOT @ cells + !
   then ;
private
: FS-LOOKUP ( n n -- n ) {: cam:n fidx:n :}    \ sdk timestamp, or -1 if absent
   FS-K FS-U FS-CAP cam fidx FS-KEY HM-PROBE {: s:n :}
   FS-U s cells + @ 0= if -1 else FS-V s cells + @ then ;

: AFO ( n n -- n ) {: fidx:n ofs:n :}          \ frame_index + offset, or -1 if it underflows
   ofs 0 >= if fidx ofs + exit then
   fidx 0 ofs - < if -1 exit then
   fidx 0 ofs - - ;

\ skew stats for reference vs cam at a given frame offset -> PS-* vars
: PO-STATS ( n n n -- ) {: refcam:n cam:n ofs:n :}
   0 PO-N !  0 PO-SUM !  0 FS-SI !
   begin FS-SI @ FS-N @ < while
      FS-CAMV FS-SI @ cells + @ refcam = if
         FS-IDXV FS-SI @ cells + @ ofs AFO PO-TF !
         PO-TF @ -1 <> if
            cam PO-TF @ FS-LOOKUP PO-TSDK !
            PO-TSDK @ -1 <> if
               PO-TSDK @ FS-SDKV FS-SI @ cells + @ - ABS-I PO-SK !
               PO-SK @ PO-SUM @ + PO-SUM !
               PO-SK @ PO-SKEW PO-N @ cells + !
               PO-N @ 1+ PO-N !
            then
         then
      then
      FS-SI @ 1+ FS-SI !
   repeat
   PO-N @ 0= if
      0 PS-MATCH !  0 PS-MEAN !  0 PS-P95 !  0 PS-P99 !  0 PS-MAX !
   else
      PO-N @ PS-MATCH !
      PO-SUM @ PO-N @ / PS-MEAN !
      PO-SKEW PO-N @ [: < ;] SORT!
      PO-SKEW PO-N @ 95 PCTL PS-P95 !
      PO-SKEW PO-N @ 99 PCTL PS-P99 !
      PO-N @ 1- cells PO-SKEW + @ PS-MAX !
   then ;

\ sweep offsets -2..2 for cam vs reference, store best into PHO-* at camrow
: PO-BEST ( n n n -- ) {: refcam:n cam:n camrow:n :}
   0 PB-SET !  0 PB-SAME99 !  0 PB-OFF !  0 PB-MATCH !  0 PB-MEAN !
   0 PB-P95 !  0 PB-P99 !  0 PB-MAX !
   -2 PB-OI !
   begin PB-OI @ 3 < while
      refcam cam PB-OI @ PO-STATS
      PB-OI @ 0= if PS-P99 @ PB-SAME99 ! then
      PS-MATCH @ 0 > if
         PB-SET @ 0=
         PS-P99 @ PB-P99 @ <  or
         PS-P99 @ PB-P99 @ =  PS-MATCH @ PB-MATCH @ >  and  or
         if
            -1 PB-SET !
            PB-OI @ PB-OFF !  PS-MATCH @ PB-MATCH !  PS-MEAN @ PB-MEAN !
            PS-P95 @ PB-P95 !  PS-P99 @ PB-P99 !  PS-MAX @ PB-MAX !
         then
      then
      PB-OI @ 1+ PB-OI !
   repeat
   PB-OFF @ PHO-OFF camrow cells + !       PB-MATCH @ PHO-MATCH camrow cells + !
   PB-MEAN @ PHO-MEAN camrow cells + !     PB-P95 @ PHO-P95 camrow cells + !
   PB-P99 @ PHO-P99 camrow cells + !       PB-MAX @ PHO-MAX camrow cells + !
   PB-SAME99 @ PHO-SAME99 camrow cells + ! ;

\ build a phase-offset row for every camera vs the reference (camera 0)
public
: PHO-BUILD ( -- )
   0 PHO-SI !
   begin PHO-SI @ TM-N @ < while
      0 PHO-SI @ PHO-SI @ PO-BEST
      PHO-SI @ 1+ PHO-SI !
   repeat ;

: PHO-OFF@    ( n -- n ) cells PHO-OFF + @ ;
: PHO-MATCH@  ( n -- n ) cells PHO-MATCH + @ ;
: PHO-MEAN@   ( n -- n ) cells PHO-MEAN + @ ;
: PHO-P95@    ( n -- n ) cells PHO-P95 + @ ;
: PHO-P99@    ( n -- n ) cells PHO-P99 + @ ;
: PHO-MAX@    ( n -- n ) cells PHO-MAX + @ ;
: PHO-SAME99@ ( n -- n ) cells PHO-SAME99 + @ ;

\ ---------------------------------------------------------------------------
\ Timestamp pairing (buildTimestampPairing): for each reference sample, the target
\ camera's sample whose SDK timestamp is NEAREST (ties -> smaller frame index).
\ Nearest-by-value is not a keyed lookup, so it scans the target camera's samples
\ (faithful to the .zig); the skew distribution is mean/p95/p99/max over matches,
\ with duplicate_target_matches counting consecutive repeats of the matched frame.
\ ---------------------------------------------------------------------------

variable PN-FIDX  variable PN-SDK  variable PN-SKEW  variable PN-CUR  variable FN-SI
create TP-PREF   TM-MAX cells allot     \ reference_frames
create TP-PMATCH TM-MAX cells allot     \ matched_frames
create TP-PDUP   TM-MAX cells allot     \ duplicate_target_matches
create TP-PMEAN  TM-MAX cells allot
create TP-PP95   TM-MAX cells allot
create TP-PP99   TM-MAX cells allot
create TP-PMAX   TM-MAX cells allot
create TP-SKEW   FS-MAX cells allot
variable TP-REF variable TP-MATCH variable TP-DUP variable TP-N variable TP-SUM
variable TP-PREV variable TP-NSDK variable TP-NFIDX variable TP-SK variable TP-RI

\ nearest target-camera sample to a timestamp -> ( frame_index sdk ), or ( -1 -1 )
private
: FS-NEAREST ( n n -- n n ) {: cam:n target:n :}    \ own index FN-SI (called inside FS-SI loops)
   -1 PN-FIDX !  -1 PN-SDK !  -1 PN-SKEW !
   0 FN-SI !
   begin FN-SI @ FS-N @ < while
      FS-CAMV FN-SI @ cells + @ cam = if
         FS-SDKV FN-SI @ cells + @ target - ABS-I PN-CUR !
         PN-SKEW @ -1 =
         PN-CUR @ PN-SKEW @ <  or
         PN-CUR @ PN-SKEW @ =  FS-IDXV FN-SI @ cells + @ PN-FIDX @ <  and  or
         if
            FS-IDXV FN-SI @ cells + @ PN-FIDX !  FS-SDKV FN-SI @ cells + @ PN-SDK !  PN-CUR @ PN-SKEW !
         then
      then
      FN-SI @ 1+ FN-SI !
   repeat
   PN-FIDX @ PN-SDK @ ;

\ pairing row for cam vs reference -> TP-P* arrays at camrow
: TP-ONE ( n n n -- ) {: refcam:n cam:n camrow:n :}
   0 TP-REF !  0 TP-MATCH !  0 TP-DUP !  0 TP-N !  0 TP-SUM !  -1 TP-PREV !
   0 FS-SI !
   begin FS-SI @ FS-N @ < while
      FS-CAMV FS-SI @ cells + @ refcam = if
         TP-REF @ 1+ TP-REF !
         cam FS-SDKV FS-SI @ cells + @ FS-NEAREST  TP-NSDK !  TP-NFIDX !
         TP-NFIDX @ -1 <> if
            TP-MATCH @ 1+ TP-MATCH !
            TP-NFIDX @ TP-PREV @ = if TP-DUP @ 1+ TP-DUP ! then
            TP-NFIDX @ TP-PREV !
            TP-NSDK @ FS-SDKV FS-SI @ cells + @ - ABS-I TP-SK !
            TP-SK @ TP-SUM @ + TP-SUM !
            TP-SK @ TP-SKEW TP-N @ cells + !
            TP-N @ 1+ TP-N !
         then
      then
      FS-SI @ 1+ FS-SI !
   repeat
   TP-REF @ TP-PREF camrow cells + !  TP-MATCH @ TP-PMATCH camrow cells + !  TP-DUP @ TP-PDUP camrow cells + !
   TP-N @ 0= if
      0 TP-PMEAN camrow cells + !  0 TP-PP95 camrow cells + !  0 TP-PP99 camrow cells + !  0 TP-PMAX camrow cells + !
   else
      TP-SUM @ TP-N @ / TP-PMEAN camrow cells + !
      TP-SKEW TP-N @ [: < ;] SORT!
      TP-SKEW TP-N @ 95 PCTL TP-PP95 camrow cells + !
      TP-SKEW TP-N @ 99 PCTL TP-PP99 camrow cells + !
      TP-N @ 1- cells TP-SKEW + @ TP-PMAX camrow cells + !
   then ;

public
: TP-BUILD ( -- )
   0 TP-RI !
   begin TP-RI @ TM-N @ < while
      0 TP-RI @ TP-RI @ TP-ONE
      TP-RI @ 1+ TP-RI !
   repeat ;

: TP-PREF@   ( n -- n ) cells TP-PREF + @ ;
: TP-PMATCH@ ( n -- n ) cells TP-PMATCH + @ ;
: TP-PDUP@   ( n -- n ) cells TP-PDUP + @ ;
: TP-PMEAN@  ( n -- n ) cells TP-PMEAN + @ ;
: TP-PP95@   ( n -- n ) cells TP-PP95 + @ ;
: TP-PP99@   ( n -- n ) cells TP-PP99 + @ ;
: TP-PMAX@   ( n -- n ) cells TP-PMAX + @ ;

\ ---------------------------------------------------------------------------
\ Timestamp frame pairs (buildTimestampFramePairs): the per-(camera, reference-
\ sample) detail rows the pairing aggregate summarizes - reference frame/ts, the
\ nearest target frame/ts, the skew, and a consecutive-duplicate flag.
\ ---------------------------------------------------------------------------

2048 constant FP-MAX
create FP-REFFIDX FP-MAX cells allot
create FP-REFSDK  FP-MAX cells allot
create FP-CAMIDX  FP-MAX cells allot
create FP-MFIDX   FP-MAX cells allot
create FP-MSDK    FP-MAX cells allot
create FP-SKEW    FP-MAX cells allot
create FP-DUP     FP-MAX cells allot
variable FP-N variable FP-CI variable FP-SI2 variable FP-PREV variable FP-TFIDX variable FP-TSDK
-6202 constant E-FP-FULL

: FP-BUILD ( -- )
   0 FP-N !  0 FP-CI !
   begin FP-CI @ TM-N @ < while
      -1 FP-PREV !  0 FP-SI2 !
      begin FP-SI2 @ FS-N @ < while
         FS-CAMV FP-SI2 @ cells + @ 0= if              \ reference = camera 0
            FP-CI @  FS-SDKV FP-SI2 @ cells + @  FS-NEAREST  FP-TSDK !  FP-TFIDX !
            FP-TFIDX @ -1 <> if
               FP-N @ FP-MAX >= if E-FP-FULL throw then
               FS-IDXV FP-SI2 @ cells + @  FP-REFFIDX FP-N @ cells + !
               FS-SDKV FP-SI2 @ cells + @  FP-REFSDK FP-N @ cells + !
               FP-CI @                     FP-CAMIDX FP-N @ cells + !
               FP-TFIDX @                  FP-MFIDX FP-N @ cells + !
               FP-TSDK @                   FP-MSDK FP-N @ cells + !
               FP-TSDK @ FS-SDKV FP-SI2 @ cells + @ - ABS-I  FP-SKEW FP-N @ cells + !
               FP-TFIDX @ FP-PREV @ = if 1 else 0 then  FP-DUP FP-N @ cells + !
               FP-TFIDX @ FP-PREV !
               FP-N @ 1+ FP-N !
            then
         then
         FP-SI2 @ 1+ FP-SI2 !
      repeat
      FP-CI @ 1+ FP-CI !
   repeat ;

: FP-N@       ( -- n ) FP-N @ ;
: FP-REFFIDX@ ( n -- n ) cells FP-REFFIDX + @ ;
: FP-REFSDK@  ( n -- n ) cells FP-REFSDK + @ ;
: FP-CAMIDX@  ( n -- n ) cells FP-CAMIDX + @ ;
: FP-MFIDX@   ( n -- n ) cells FP-MFIDX + @ ;
: FP-MSDK@    ( n -- n ) cells FP-MSDK + @ ;
: FP-SKEW@    ( n -- n ) cells FP-SKEW + @ ;
: FP-DUP@     ( n -- n ) cells FP-DUP + @ ;

\ ---------------------------------------------------------------------------
\ Readiness gates (buildSyncReadiness / buildTimestampPairingReadiness): pass/fail
\ verdicts from the collated report + ReadinessOptions defaults (max_sdk_skew
\ 500000 ns, min_complete_ratio 0.99, require_common_lifecycle). Ratios are checked
\ by cross-multiplication (a/b >= 99/100  <=>  a*100 >= b*99), so no float math.
\ ---------------------------------------------------------------------------

500000 constant RD-MAXSKEW
99 constant RD-MINNUM  100 constant RD-MINDEN          \ min_complete_ratio = 99/100

variable RD-DROPS variable RD-DUPS variable RD-REGR variable RD-CI

\ --- sync readiness ---
variable SR-CAMOK variable SR-RATIOOK variable SR-COMMONOK variable SR-SKEWOK
variable SR-DROPOK variable SR-DUPOK variable SR-REGROK variable SR-RESULT
: SR-BUILD ( -- )
   0 RD-DROPS !  0 RD-DUPS !  0 RD-REGR !  0 RD-CI !
   begin RD-CI @ TM-N @ < while
      RD-DROPS @ RD-CI @ TM-FDROP@ + RD-DROPS !
      RD-DUPS  @ RD-CI @ TM-DUP@   + RD-DUPS !          \ duplicate_flags
      RD-REGR  @ RD-CI @ TM-REGR@  + RD-REGR !
      RD-CI @ 1+ RD-CI !
   repeat
   TM-N @ 2 >= SR-CAMOK !
   TX-FSETS@ 0= if 0 0= 0= SR-RATIOOK ! else
      TX-COMPLETE@ RD-MINDEN *  TX-FSETS@ RD-MINNUM *  >= SR-RATIOOK !
   then
   TX-COMMON@ SR-COMMONOK !
   RD-MAXSKEW TX-SP99@ >= SR-SKEWOK !
   RD-DROPS @ 0= SR-DROPOK !
   RD-DUPS @ 0= SR-DUPOK !
   RD-REGR @ 0= SR-REGROK !
   SR-CAMOK @ SR-RATIOOK @ and SR-COMMONOK @ and SR-SKEWOK @ and
   SR-DROPOK @ and SR-DUPOK @ and SR-REGROK @ and  SR-RESULT ! ;
: SR-RESULT@ ( -- bool ) SR-RESULT @ 0 <> ;

\ --- timestamp pairing readiness ---
variable TPR-CAMOK variable TPR-MATCHOK variable TPR-DUPOK variable TPR-COMMONOK
variable TPR-SKEWOK variable TPR-DROPOK variable TPR-FDUPOK variable TPR-REGROK
variable TPR-RESULT variable TPR-CI variable TPR-P99
: TPR-BUILD ( -- )
   0 RD-DROPS !  0 RD-DUPS !  0 RD-REGR !  0 RD-CI !
   begin RD-CI @ TM-N @ < while
      RD-DROPS @ RD-CI @ TM-FDROP@ + RD-DROPS !
      RD-DUPS  @ RD-CI @ TM-DUP@   + RD-DUPS !
      RD-REGR  @ RD-CI @ TM-REGR@  + RD-REGR !
      RD-CI @ 1+ RD-CI !
   repeat
   \ match ratio ok (all pairings matched*100 >= ref*99) + dup ratio ok (dup*100 <= ref) + max p99
   0 0= TPR-MATCHOK !  0 0= TPR-DUPOK !  0 TPR-P99 !  0 TPR-CI !
   begin TPR-CI @ TM-N @ < while
      TPR-CI @ TP-PMATCH@ RD-MINDEN *  TPR-CI @ TP-PREF@ RD-MINNUM *  <  if 0 0= 0= TPR-MATCHOK ! then
      TPR-CI @ TP-PDUP@ RD-MINDEN *  TPR-CI @ TP-PREF@  >  if 0 0= 0= TPR-DUPOK ! then
      TPR-CI @ TP-PP99@ TPR-P99 @ MAX2 TPR-P99 !
      TPR-CI @ 1+ TPR-CI !
   repeat
   TM-N @ 2 >= TPR-CAMOK !
   TX-COMMON@ TPR-COMMONOK !
   RD-MAXSKEW TPR-P99 @ >= TPR-SKEWOK !
   RD-DROPS @ 0= TPR-DROPOK !
   RD-DUPS @ 0= TPR-FDUPOK !
   RD-REGR @ 0= TPR-REGROK !
   TPR-CAMOK @ TPR-MATCHOK @ and TPR-DUPOK @ and TPR-COMMONOK @ and TPR-SKEWOK @ and
   TPR-DROPOK @ and TPR-FDUPOK @ and TPR-REGROK @ and  TPR-RESULT ! ;
: TPR-RESULT@ ( -- bool ) TPR-RESULT @ 0 <> ;
end-package
