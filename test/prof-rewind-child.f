split-str
\ Run in a child: a caught failed text keeps its words' ordinals and code, so a
\ later word gets its own, and each report charges only the word that ran.

require src/core/exit-status.f
require src/lib/test/eval.f

variable TRIM-IX
variable TRIM-PC

: FAIL-TEXT ( str -- )
   TEST-EVAL:RC 73 <> if
      s" profiler rewind: text did not fail" EXIT-STATUS:FAILURE die then ;

\ A retired word keeps its index entry, so the failed text's pc names its record.
: VERIFY-RETIRED ( -- )
   TRIM-PC @ prof-pc>rec TRIM-IX @ <> if
      s" profiler trim: a retired word's pc names another record" EXIT-STATUS:FAILURE die then ;
: VERIFY-TRIM ( -- )
   s" PROF-TRIM-NEW" XREF-FIND-CURRENT-INDEX {: new:n :}
   new TRIM-IX @ = if
      s" profiler trim: ordinal was reused" EXIT-STATUS:FAILURE die then
   prof-json
   s" PROF-TRIM-NEW" XREF-FIND XREF-START prof-pc>rec new <> if
      s" profiler trim: new pc has no live record" EXIT-STATUS:FAILURE die then
   new prof-row ;
S\" : PROF-TRIM-OLD ( -- ) 100000000 0 ?do i drop loop ;\ns\" PROF-TRIM-OLD\" XREF-FIND-CURRENT-INDEX TRIM-IX !\ns\" PROF-TRIM-OLD\" XREF-FIND XREF-START TRIM-PC !\n0 prof-on\nPROF-TRIM-OLD\nprof-off\n73 throw\n" FAIL-TEXT
S\" : PROF-TRIM-NEW ( -- n ) 7 ;\nVERIFY-TRIM\n73 throw\n" FAIL-TEXT
prof-json
VERIFY-RETIRED

\ A failed text's word keeps the samples it took before the report names them.
S\" 0 prof-on\n: PROF-LATE-TRIM ( -- ) 100000000 0 ?do i drop loop ;\nPROF-LATE-TRIM\nprof-off\n73 throw\n" FAIL-TEXT
prof-json

\ The donor's code stays; the word defined after it gets code of its own and
\ the next report charges it, not the donor.
variable REUSE-PC
S\" : PROF-RECLAIM-DONOR ( -- ) 100000000 0 ?do i drop loop ;\ns\" PROF-RECLAIM-DONOR\" XREF-FIND XREF-START REUSE-PC !\n0 prof-on\n73 throw\n" FAIL-TEXT
: PROF-RECLAIM-TARGET ( -- n ) 100000000 0 ?do i drop loop 42 ;
: VERIFY-RECLAIM ( -- )
   s" PROF-RECLAIM-TARGET" XREF-FIND XREF-START REUSE-PC @ <= if
      s" profiler reclaim: code address was reused" EXIT-STATUS:FAILURE die then
   PROF-RECLAIM-TARGET 42 <> if
      s" profiler reclaim: target returned wrong value" EXIT-STATUS:FAILURE die then
   prof-off
   prof-json ;
VERIFY-RECLAIM
