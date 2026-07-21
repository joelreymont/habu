\ nested-validation-rca-test.f - focused topology and step-evidence coverage.
\ Run: bin/hb --load test/nested-validation-rca-test.f

require lib/test.f
require tools/nested-validation-rca-core.f

package NESTED-VALIDATION-RCA-TEST

create ROOT-BUF FS-PATH-CAP allot
variable ROOT-U

: ROOT$ ( -- ptr u8 n )
   ROOT-BUF ROOT-U @ ;

: SETUP ( -- )
   s" habu-nested-validation-rca-test" TMPDIR-MKDIR {: a:ptr u:n :}
   a ROOT-BUF u BYTE-COPY
   u ROOT-U !
   ROOT$ GS-ROOT! ;

: HAS? ( ptr u8 n -- bool )
   GS-BUF GS-U @ 2swap CONTAINS? ;

: CHECK-STEPS ( -- )
   GS-READ
   s" root step carries generation" T-LABEL
   s" nested-validation-step	gen=0	pid=" HAS? TTRUE
   s" resident step carries child generation" T-LABEL
   s" gen=0-1	pid=" HAS? TTRUE
   s" nested validation step carries grandchild generation" T-LABEL
   s" gen=0-1-2	pid=" HAS? TTRUE
   s" process-group evidence is durable" T-LABEL
   s" 	pgrp=" HAS? TTRUE
   s" terminal foreground-group evidence is durable" T-LABEL
   s" 	tpgid=" HAS? TTRUE
   s" validation entry is durable" T-LABEL
   s" stage=validation-enter" HAS? TTRUE
   s" validation completion is durable" T-LABEL
   s" stage=validation-leave" HAS? TTRUE ;

: MAIN ( -- )
   T-RESET
   SETUP
   NESTED-VALIDATION-RCA:PROBE
   CHECK-STEPS
   ROOT$ CLEANUP-TREE+
   CLEANUP-RUN
   T-REPORT
   s" nested-validation-rca-test: ok" type cr ;

MAIN

;package
