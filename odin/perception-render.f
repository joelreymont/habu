\ perception-render.f - renderMetricsCsv for the perception-latency Report, ported
\ from src/perception_latency.zig. The analyze() kernel lives in
\ odin/perception-latency.f; this holds the full Report as fields (optionals carry a
\ present flag) plus the Options thresholds, and emits the metric,value,threshold,pass
\ CSV byte-for-byte. Optional values render empty when absent (writeOptionalF64/U64),
\ pass renders yes/no/empty (writeOptionalPass), result() gates characterization ->
\ threshold checks -> pass/fail. Verified against the .zig renderMetricsCsv run under
\ zig 0.16 on a constructed Report. Depends on lib/errors.f lib/string.f lib/float.f
\ odin/float-cell.f lib/render.f.

\ scalar fields
package PERCEPTION
private
variable PR-RECORDS variable PR-DETREC variable PR-INFTICK variable PR-TRKTICK
variable PR-CAMERAS variable PR-TARGETS variable PR-LATSAMP
variable PR-LP50 variable PR-LP95 variable PR-LP99 variable PR-LMAX
variable PR-QSAMP variable PR-TRKUPD variable PR-TLSAMP
\ optionals: value + present flag
variable PR-QMAX  variable PR-QMAXP        variable PR-QMEAN variable PR-QMEANP
variable PR-DRMIN variable PR-DRMINP  variable PR-DRMEAN variable PR-DRMEANP  variable PR-DRMAX variable PR-DRMAXP
variable PR-IRMIN variable PR-IRMINP  variable PR-IRMEAN variable PR-IRMEANP  variable PR-IRMAX variable PR-IRMAXP
variable PR-TRHZ variable PR-TRHZP  variable PR-TRMIN variable PR-TRMINP  variable PR-TRMEAN variable PR-TRMEANP  variable PR-TRMAX variable PR-TRMAXP
variable PR-TLP50 variable PR-TLP50P  variable PR-TLP95 variable PR-TLP95P  variable PR-TLP99 variable PR-TLP99P  variable PR-TLMAX variable PR-TLMAXP
\ four timing summaries: samples + p50/p95/p99/max (present = samples>0)
create TG-SAMP 4 cells allot
create TG-P50 4 cells allot  create TG-P95 4 cells allot  create TG-P99 4 cells allot  create TG-MX 4 cells allot
\ options
variable OPT-MLP95 variable OPT-MLP95P  variable OPT-MLP99 variable OPT-MLP99P
variable OPT-MQ variable OPT-MQP  variable OPT-MDR variable OPT-MDRP

public
: PR-RESET ( -- )
   0 PR-QMAXP !  0 PR-QMEANP !  0 PR-DRMINP !  0 PR-DRMEANP !  0 PR-DRMAXP !
   0 PR-IRMINP !  0 PR-IRMEANP !  0 PR-IRMAXP !  0 PR-TRHZP !  0 PR-TRMINP !  0 PR-TRMEANP !  0 PR-TRMAXP !
   0 PR-TLP50P !  0 PR-TLP95P !  0 PR-TLP99P !  0 PR-TLMAXP !
   0 OPT-MLP95P !  0 OPT-MLP99P !  0 OPT-MQP !  0 OPT-MDRP ! ;

\ --- line emitters ---
private
: L-U ( ptr u8 n n -- ) {: na:ptr nl:n v:n :} na nl RB+ CM v RB# CM CM RB-NL ;            \ "name,N,,"
: L-F ( ptr u8 n r -- ) {: na:ptr nl:n x:r :} na nl RB+ CM x RB-FFIX3 CM CM RB-NL ;       \ "name,X.XXX,,"
: OPTV-F ( n r -- ) {: p:n x:r :} p 0 <> if x RB-FFIX3 then ;                          \ optional f64 value
: OPTV-U ( n n -- ) {: p:n v:n :} p 0 <> if v RB# then ;                               \ optional u64 value
: L-OF ( ptr u8 n n r -- ) {: na:ptr nl:n p:n x:r :} na nl RB+ CM p x OPTV-F CM CM RB-NL ;   \ optional f64, no threshold

: PL-RESULT ( -- ptr u8 n )       \ result(options)
   OPT-MLP95P @ OPT-MLP99P @ + OPT-MQP @ + OPT-MDRP @ + 0= if s" characterization" exit then
   OPT-MLP95P @ 0 <> if PR-LP95 F@ OPT-MLP95 F@ f> if s" fail" exit then then
   OPT-MLP99P @ 0 <> if PR-LP99 F@ OPT-MLP99 F@ f> if s" fail" exit then then
   OPT-MQP @ 0 <> if PR-QMAXP @ 0= if s" fail" exit then  PR-QMAX @ OPT-MQ @ > if s" fail" exit then then
   OPT-MDRP @ 0 <> if
      PR-IRMINP @ 0 <> if  PR-IRMIN F@ OPT-MDR F@ f< if s" fail" exit then
      else PR-DRMINP @ 0 <> if  PR-DRMIN F@ OPT-MDR F@ f< if s" fail" exit then
      else s" fail" exit then then
   then
   s" pass" ;

\ optional f64 with threshold + pass: "name,<val>,<thresh f64>,<yes/no>"
: L-FTP ( ptr u8 n n r n r n bool -- )
   {: na:ptr nl:n vp:n v:r tp:n t:r pp:n pass:bool :}
   na nl RB+ CM  vp v OPTV-F  CM  tp 0 <> if t RB-FFIX3 then  CM  pp 0 <> if pass if s" yes" RB+ else s" no" RB+ then then  RB-NL ;
\ optional u64 with threshold(u64) + pass: "name,<val u64>,<thresh u64>,<yes/no>"
: L-UTP ( ptr u8 n n n n n n bool -- )
   {: na:ptr nl:n vp:n v:n tp:n t:n pp:n pass:bool :}
   na nl RB+ CM  vp 0 <> if v RB# then  CM  tp 0 <> if t RB# then  CM  pp 0 <> if pass if s" yes" RB+ else s" no" RB+ then then  RB-NL ;

\ min detector rate = inference_rate_min orelse detector_rate_min (for the pass gate)
: DET-MINRATE ( -- r ) PR-IRMINP @ 0 <> if PR-IRMIN F@ else PR-DRMIN F@ then ;

public
: PL-RENDER ( -- ptr u8 n )
   RB-RESET
   s" metric,value,threshold,pass" RB+ RB-NL
   s" records" PR-RECORDS @ L-U   s" detection_records" PR-DETREC @ L-U
   s" inference_ticks" PR-INFTICK @ L-U   s" tracker_ticks" PR-TRKTICK @ L-U
   s" cameras" PR-CAMERAS @ L-U   s" targets" PR-TARGETS @ L-U
   s" latency_samples" PR-LATSAMP @ L-U
   s" latency_ms_p50" PR-LP50 F@ L-F
   s" latency_ms_p95" -1 PR-LP95 F@  OPT-MLP95P @ OPT-MLP95 F@  OPT-MLP95P @ PR-LP95 F@ OPT-MLP95 F@ f> 0=  L-FTP
   s" latency_ms_p99" -1 PR-LP99 F@  OPT-MLP99P @ OPT-MLP99 F@  OPT-MLP99P @ PR-LP99 F@ OPT-MLP99 F@ f> 0=  L-FTP
   s" latency_ms_max" PR-LMAX F@ L-F
   s" detector_rate_min_hz" PR-DRMINP @ PR-DRMIN F@  OPT-MDRP @ OPT-MDR F@  OPT-MDRP @  DET-MINRATE OPT-MDR F@ f< 0=  L-FTP
   s" detector_rate_mean_hz" PR-DRMEANP @ PR-DRMEAN F@ L-OF
   s" detector_rate_max_hz" PR-DRMAXP @ PR-DRMAX F@ L-OF
   s" inference_rate_min_hz" PR-IRMINP @ PR-IRMIN F@  OPT-MDRP @ OPT-MDR F@  OPT-MDRP @ PR-IRMIN F@ OPT-MDR F@ f< 0=  L-FTP
   s" inference_rate_mean_hz" PR-IRMEANP @ PR-IRMEAN F@ L-OF
   s" inference_rate_max_hz" PR-IRMAXP @ PR-IRMAX F@ L-OF
   s" queue_depth_samples" PR-QSAMP @ L-U
   s" queue_depth_max" PR-QMAXP @ PR-QMAX @  OPT-MQP @ OPT-MQ @  OPT-MQP @ PR-QMAX @ OPT-MQ @ > 0=  L-UTP
   s" queue_depth_mean" PR-QMEANP @ PR-QMEAN F@ L-OF
   s" tracker_updates" PR-TRKUPD @ L-U
   s" tracker_rate_hz" PR-TRHZP @ PR-TRHZ F@ L-OF
   s" tracker_rate_min_hz" PR-TRMINP @ PR-TRMIN F@ L-OF
   s" tracker_rate_mean_hz" PR-TRMEANP @ PR-TRMEAN F@ L-OF
   s" tracker_rate_max_hz" PR-TRMAXP @ PR-TRMAX F@ L-OF
   s" tracker_latency_samples" PR-TLSAMP @ L-U
   s" tracker_latency_ms_p50" PR-TLP50P @ PR-TLP50 F@ L-OF
   s" tracker_latency_ms_p95" PR-TLP95P @ PR-TLP95 F@ L-OF
   s" tracker_latency_ms_p99" PR-TLP99P @ PR-TLP99 F@ L-OF
   s" tracker_latency_ms_max" PR-TLMAXP @ PR-TLMAX F@ L-OF
   s" schedule_lag_samples" TG-SAMP 0 cells + @ L-U
   s" schedule_lag_ms_p50" TG-SAMP 0 cells + @ TG-P50 0 cells + F@ L-OF
   s" schedule_lag_ms_p95" TG-SAMP 0 cells + @ TG-P95 0 cells + F@ L-OF
   s" schedule_lag_ms_p99" TG-SAMP 0 cells + @ TG-P99 0 cells + F@ L-OF
   s" schedule_lag_ms_max" TG-SAMP 0 cells + @ TG-MX 0 cells + F@ L-OF
   s" tensor_retrieve_samples" TG-SAMP 1 cells + @ L-U
   s" tensor_retrieve_ms_p50" TG-SAMP 1 cells + @ TG-P50 1 cells + F@ L-OF
   s" tensor_retrieve_ms_p95" TG-SAMP 1 cells + @ TG-P95 1 cells + F@ L-OF
   s" tensor_retrieve_ms_p99" TG-SAMP 1 cells + @ TG-P99 1 cells + F@ L-OF
   s" tensor_retrieve_ms_max" TG-SAMP 1 cells + @ TG-MX 1 cells + F@ L-OF
   s" detector_run_samples" TG-SAMP 2 cells + @ L-U
   s" detector_run_ms_p50" TG-SAMP 2 cells + @ TG-P50 2 cells + F@ L-OF
   s" detector_run_ms_p95" TG-SAMP 2 cells + @ TG-P95 2 cells + F@ L-OF
   s" detector_run_ms_p99" TG-SAMP 2 cells + @ TG-P99 2 cells + F@ L-OF
   s" detector_run_ms_max" TG-SAMP 2 cells + @ TG-MX 2 cells + F@ L-OF
   s" detector_cycle_samples" TG-SAMP 3 cells + @ L-U
   s" detector_cycle_ms_p50" TG-SAMP 3 cells + @ TG-P50 3 cells + F@ L-OF
   s" detector_cycle_ms_p95" TG-SAMP 3 cells + @ TG-P95 3 cells + F@ L-OF
   s" detector_cycle_ms_p99" TG-SAMP 3 cells + @ TG-P99 3 cells + F@ L-OF
   s" detector_cycle_ms_max" TG-SAMP 3 cells + @ TG-MX 3 cells + F@ L-OF
   s" result,,," RB+ PL-RESULT RB+ RB-NL
   RB$ ;
end-package
