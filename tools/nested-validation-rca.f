\ nested-validation-rca.f - CLI for the resident validation process-tree RCA.
\ Run with HABU_GATE_STATS set to a fresh file:
\   HABU_GATE_STATS=/tmp/gate-stats.tsv bin/hb --load \
\     tools/nested-validation-rca.f -- /path/to/hb-under-test

require tools/nested-validation-rca-core.f

package NESTED-VALIDATION-RCA-CLI

64 constant USAGE-RC

: USAGE ( -- )
   s" usage: tools/nested-validation-rca.f -- CANDIDATE" USAGE-RC die ;

: MAIN ( -- )
   SCRIPT-ARGC 1 <> if USAGE then
   0 SCRIPT-ARGV$ NESTED-VALIDATION-RCA:RUN
   s" nested-validation-rca: ok" type cr ;

MAIN

;package
