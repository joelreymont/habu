\ ir-id-concurrency.f - isolated process-wide allocator concurrency fixture.

require lib/task.f
require src/compiler/ir/id.f

package IR-ID-CONCURRENCY
private

4 constant WORKER#
32 constant PER-WORKER
WORKER# PER-WORKER * constant ID#
$4C constant OVERLAP-RC

\ Test-only observation erases an already checked owner. It is private, cannot
\ mint an identity, and is absent from every production compiler package.
CAST: MODULE>SERIAL ( IR-ID:ir-module-id -- n )

create SERIALS ID# cells allot
here CELL 1- and CELL swap - CELL 1- and allot
variable DONE
variable READY
variable GO
variable MODE
variable BODY-RC
variable CLEAN-RC
variable KILL-RC

TASK:MIN-STACK TASK:TASK WORKER-0
TASK:MIN-STACK TASK:TASK WORKER-1
TASK:MIN-STACK TASK:TASK WORKER-2
TASK:MIN-STACK TASK:TASK WORKER-3

: WAIT-CELL ( ptr n n -- ) {: cell:ptr want:n :}
   begin
      cell atomic@ want >= if exit then
      TASK:PAUSE
   again ;

: SERIAL-PTR ( n -- ptr n )
   cells SERIALS + ;

: USE-BARRIER? ( -- bool )
   MODE @ 0 <> ;

: FILL ( n -- ) {: start:n :}
   USE-BARRIER? if
      1 READY atomic-add drop
      GO 1 WAIT-CELL
   then
   PER-WORKER 0 ?do
      IR-ID:NEW-MODULE
         {: key:IR-ID:ir-module-key owner:IR-ID:ir-module-id :}
      key 0 IR-ID:PACK-SOURCE IR-ID:SOURCE-OWNER
         owner IR-ID:MODULE-SAME? 0= if OVERLAP-RC throw then
      owner MODULE>SERIAL start i + SERIAL-PTR !
   loop
   1 DONE atomic-add drop ;

: WORK-0 ( -- ) 0 FILL ;
: WORK-1 ( -- ) PER-WORKER FILL ;
: WORK-2 ( -- ) PER-WORKER 2 * FILL ;
: WORK-3 ( -- ) PER-WORKER 3 * FILL ;

: SERIAL@ ( n -- n )
   SERIAL-PTR @ ;

: NOTE-KILL-RC ( n -- ) {: rc:n :}
   rc 0= if exit then
   KILL-RC @ 0= if rc KILL-RC ! then ;

: TASKS-KILL ( -- )
   0 KILL-RC !
   [: WORKER-0 TASK:KILL ;] catch NOTE-KILL-RC
   [: WORKER-1 TASK:KILL ;] catch NOTE-KILL-RC
   [: WORKER-2 TASK:KILL ;] catch NOTE-KILL-RC
   [: WORKER-3 TASK:KILL ;] catch NOTE-KILL-RC
   KILL-RC @ dup 0 <> if throw then drop ;

: VERIFY-OVERLAP ( -- )
   READY atomic@ WORKER# <> if OVERLAP-RC throw then
   GO atomic@ 1 <> if OVERLAP-RC throw then ;

: VERIFY-SERIALS ( -- )
   ID# 0 ?do
      i SERIAL@ 0 <= if OVERLAP-RC throw then
      ID# i 1+ ?do
         i SERIAL@ j SERIAL@ = if OVERLAP-RC throw then
      loop
   loop ;

: BODY ( -- )
   0 DONE !
   0 READY !
   0 GO !
   ['] WORK-0 WORKER-0 TASK:ACTIVATE
   MODE @ 2 = if ['] WORK-0 WORKER-0 TASK:ACTIVATE then
   ['] WORK-1 WORKER-1 TASK:ACTIVATE
   ['] WORK-2 WORKER-2 TASK:ACTIVATE
   ['] WORK-3 WORKER-3 TASK:ACTIVATE
   USE-BARRIER? if
      READY WORKER# WAIT-CELL
      1 GO atomic!
   then
   DONE WORKER# WAIT-CELL
   VERIFY-OVERLAP
   VERIFY-SERIALS ;

public

: RUN ( n -- )
   MODE !
   [: BODY ;] catch BODY-RC !
   [: TASKS-KILL ;] catch CLEAN-RC !
   BODY-RC @ dup 0 <> if throw then drop
   CLEAN-RC @ dup 0 <> if throw then drop ;

: CLEANUP-REUSE ( -- )
   [: 2 RUN ;] catch E-TASK-STATE <> if OVERLAP-RC throw then
   1 RUN ;

;package
