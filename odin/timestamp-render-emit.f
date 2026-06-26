\ timestamp-render-emit.f - emits the four timestamp-sync renders to stdout for a
\ byte-for-byte diff against the src/timestamp_metrics.zig test-1774 ohsnap blocks
\ (the oracle). Feeds the exact sample_ndjson field values (same fixture as
\ timestamp-metrics-test.f), then prints each render between <<<...>>> markers so
\ an external diff can split them. This is the checked Habu side of the byte-exact
\ check for the Zig renderFrameSyncCsv/Json + renderCrossCameraSyncCsv/Json.

package CAMSYNC
private
: A+ ( n n n n n n n -- ) {: fps:n fidx:n sdk:n host:n drp:n dupf:n regr:n :}
   s" 306885122" s" cam_a0" fps fidx sdk host drp dupf regr TM-ADD
   fidx sdk host TG-ADD ;
: B+ ( n n n n n n n -- ) {: fps:n fidx:n sdk:n host:n drp:n dupf:n regr:n :}
   s" 309091258" s" cam_a1" fps fidx sdk host drp dupf regr TM-ADD
   fidx sdk host TG-ADD ;

: FEED ( -- )
   TM-RESET  TX-RESET
   s" capture-null-multi" TX-SCHEMA
   60 0 1000000000 2000000000 0 0 0 A+
   60 1 1016666667 2016666667 0 0 0 A+
   60 2 1033333334 2033333334 0 0 0 A+
   60 0 3000000000 4000000000 0 0 0 B+
   60 2 3033333334 4033333334 1 0 0 B+
   s" 309091258" 4 TM-SUMMARY
   TM-FINISH  2 TX-BUILD ;

: MARK ( ptr u8 n -- ) type 10 emit ;

FEED
s" <<<CSV>>>"   MARK   TS-CSV  type
s" <<<XCSV>>>"  MARK   TX-CSV  type
s" <<<JSON>>>"  MARK   TS-JSON type
s" <<<XJSON>>>" MARK   TX-JSON type
s" <<<END>>>"   MARK
end-package
