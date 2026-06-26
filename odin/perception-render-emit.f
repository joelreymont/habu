\ perception-render-emit.f - populates a Report mirroring the src/perception_latency.zig
\ oracle harness (all optionals set + thresholds) and prints renderMetricsCsv for a
\ byte-exact diff.

package PERCEPTION
private
: POP ( -- )
   PR-RESET
   100 PR-RECORDS ! 80 PR-DETREC ! 60 PR-INFTICK ! 40 PR-TRKTICK !
   2 PR-CAMERAS ! 3 PR-TARGETS ! 80 PR-LATSAMP !
   1.234 PR-LP50 F! 5.678 PR-LP95 F! 9.012 PR-LP99 F! 12.345 PR-LMAX F!
   50 PR-QSAMP !  8 PR-QMAX ! -1 PR-QMAXP !  3.5 PR-QMEAN F! -1 PR-QMEANP !
   29.0 PR-DRMIN F! -1 PR-DRMINP !  30.0 PR-DRMEAN F! -1 PR-DRMEANP !  31.0 PR-DRMAX F! -1 PR-DRMAXP !
   58.0 PR-IRMIN F! -1 PR-IRMINP !  59.0 PR-IRMEAN F! -1 PR-IRMEANP !  60.0 PR-IRMAX F! -1 PR-IRMAXP !
   40 PR-TRKUPD !  39.0 PR-TRHZ F! -1 PR-TRHZP !  38.0 PR-TRMIN F! -1 PR-TRMINP !  39.0 PR-TRMEAN F! -1 PR-TRMEANP !  40.0 PR-TRMAX F! -1 PR-TRMAXP !
   40 PR-TLSAMP !  0.5 PR-TLP50 F! -1 PR-TLP50P !  0.8 PR-TLP95 F! -1 PR-TLP95P !  0.9 PR-TLP99 F! -1 PR-TLP99P !  1.0 PR-TLMAX F! -1 PR-TLMAXP !
   10 TG-SAMP 0 cells + !  0.1 TG-P50 0 cells + F!  0.2 TG-P95 0 cells + F!  0.3 TG-P99 0 cells + F!  0.4 TG-MX 0 cells + F!
   11 TG-SAMP 1 cells + !  0.11 TG-P50 1 cells + F!  0.21 TG-P95 1 cells + F!  0.31 TG-P99 1 cells + F!  0.41 TG-MX 1 cells + F!
   12 TG-SAMP 2 cells + !  0.12 TG-P50 2 cells + F!  0.22 TG-P95 2 cells + F!  0.32 TG-P99 2 cells + F!  0.42 TG-MX 2 cells + F!
   13 TG-SAMP 3 cells + !  0.13 TG-P50 3 cells + F!  0.23 TG-P95 3 cells + F!  0.33 TG-P99 3 cells + F!  0.43 TG-MX 3 cells + F!
   6.0 OPT-MLP95 F! -1 OPT-MLP95P !  10.0 OPT-MLP99 F! -1 OPT-MLP99P !  10 OPT-MQ ! -1 OPT-MQP !  25.0 OPT-MDR F! -1 OPT-MDRP ! ;
POP
PL-RENDER type
end-package
