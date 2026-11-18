split-str
\ Run in a child: a rollback gives back ordinals and code addresses for reuse,
\ and each report must charge only the live owner.

require src/core/exit-status.f
require src/lib/test/eval.f

variable TRIM-IX
variable TRIM-PC

\ Every word the checks call is defined before the first text, so each
\ replacement takes the ordinal and code its rollback gave back.
: ROLLBACK ( str -- )
   TEST-EVAL:RC 73 <> if
      s" profiler rewind: text was not rolled back" EXIT-STATUS:FAILURE die then ;

\ A rollback lowers NDICT through ndict!. The retired word's identity must
\ leave the profiler's index before reuse, and the replacement starts fresh.
: VERIFY-HIDDEN ( -- )
   TRIM-PC @ prof-pc>rec -1 <> if
      s" profiler trim: hidden pc has a live record" EXIT-STATUS:FAILURE die then ;
: VERIFY-TRIM ( -- )
   s" PROF-TRIM-NEW" XREF-FIND-INDEX TRIM-IX @ <> if
      s" profiler trim: ordinal was not reused" EXIT-STATUS:FAILURE die then
   prof-json
   s" PROF-TRIM-NEW" XREF-FIND XREF-START prof-pc>rec TRIM-IX @ <> if
      s" profiler trim: new pc has no live record" EXIT-STATUS:FAILURE die then
   TRIM-IX @ prof-row ;
S\" : PROF-TRIM-OLD ( -- ) 100000000 0 ?do i drop loop ;\ns\" PROF-TRIM-OLD\" XREF-FIND-INDEX TRIM-IX !\ns\" PROF-TRIM-OLD\" XREF-FIND XREF-START TRIM-PC !\n0 prof-on\nPROF-TRIM-OLD\nprof-off\n73 throw\n" ROLLBACK
S\" : PROF-TRIM-NEW ( -- n ) 7 ;\nVERIFY-TRIM\n73 throw\n" ROLLBACK
prof-json
VERIFY-HIDDEN

\ A rollback must name pending samples before the dictionary rows go.
\ Otherwise the next report loses the only owner of these deferred PCs.
S\" 0 prof-on\n: PROF-LATE-TRIM ( -- ) 100000000 0 ?do i drop loop ;\nPROF-LATE-TRIM\nprof-off\n73 throw\n" ROLLBACK
prof-json

\ A rollback releases the donor's code; the word that reuses its address must
\ not charge the donor's identity in the next report.
variable REUSE-PC
S\" : PROF-RECLAIM-DONOR ( -- ) 100000000 0 ?do i drop loop ;\ns\" PROF-RECLAIM-DONOR\" XREF-FIND XREF-START REUSE-PC !\n0 prof-on\n73 throw\n" ROLLBACK
: PROF-RECLAIM-TARGET ( -- n ) 100000000 0 ?do i drop loop 42 ;
: VERIFY-RECLAIM ( -- )
   s" PROF-RECLAIM-TARGET" XREF-FIND XREF-START REUSE-PC @ <> if
      s" profiler reclaim: code address was not reused" EXIT-STATUS:FAILURE die then
   PROF-RECLAIM-TARGET 42 <> if
      s" profiler reclaim: target returned wrong value" EXIT-STATUS:FAILURE die then
   prof-off
   prof-json ;
VERIFY-RECLAIM
