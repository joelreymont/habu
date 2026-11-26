\ The task control block and the offsets the engine's task entry loads from it,
\ shared by the engine and the TASK library.
package TASK-ABI
public

0 constant EMPTY
1 constant CONSTRUCTED
2 constant RUNNING
3 constant DONE
4 constant HALT-REQ

private
BEGIN-STRUCTURE SEMAPHORE-SIZE
   CELL +FIELD SEM.GUARD
   \ Linux sem_t or Darwin Mach port storage.
   $20 +FIELD SEM.OBJ
END-STRUCTURE

public
NULL-PTR SEM.GUARD NULL-PTR - constant SEM-GUARD-OFF
NULL-PTR SEM.OBJ NULL-PTR - constant SEM-OBJ-OFF
SEMAPHORE-SIZE constant SEMAPHORE-BYTES

private
BEGIN-STRUCTURE TCB-SIZE
public
   CELL +FIELD TCB.SIZE
   CELL +FIELD TCB.XT-CELL
   CELL +FIELD TCB.THREAD
   PTR-FIELD: TCB.STACK
   CELL +FIELD TCB.STACK-U
   PTR-FIELD: TCB.REGION
   CELL +FIELD TCB.REGION-U
   CELL +FIELD TCB.DBASE
   CELL +FIELD TCB.NDICT
   CELL +FIELD TCB.CP
   CELL +FIELD TCB.STATUS
   CELL +FIELD TCB.STOP
   CELL +FIELD TCB.RET
   CELL +FIELD TCB.USER-XT-CELL
   PTR-FIELD: TCB.RSTACK
   CELL +FIELD TCB.RSTACK-U
   PTR-FIELD: TCB.LSTACK
   CELL +FIELD TCB.LSTACK-U
   CELL +FIELD TCB.THROW
   \ The task's own wake-up semaphore.
   SEMAPHORE-BYTES +FIELD TCB.PARK
   \ The task's queue: its epoll descriptor in the low 32 bits and its wake
   \ eventfd in the high 32, 0 for none. Never closed.
   CELL +FIELD TCB.QUEUE
   CELL +FIELD TCB.RESULT
   CELL +FIELD TCB.RESULT-SET
   CELL +FIELD TCB.JOINER
   \ The head of the task's declared cleanups: 0 for none, else the row plus one.
   CELL +FIELD TCB.EXIT-SLOT
   SEMAPHORE-BYTES +FIELD TCB.DONE
   \ The head of its run cleanups, the same way.
   CELL +FIELD TCB.EXIT-RUN
END-STRUCTURE

NULL-PTR TCB.XT-CELL NULL-PTR - constant XT-OFF
NULL-PTR TCB.STACK NULL-PTR - constant STACK-OFF
NULL-PTR TCB.REGION NULL-PTR - constant REGION-OFF
NULL-PTR TCB.REGION-U NULL-PTR - constant REGION-U-OFF
NULL-PTR TCB.DBASE NULL-PTR - constant DBASE-OFF
NULL-PTR TCB.NDICT NULL-PTR - constant NDICT-OFF
NULL-PTR TCB.CP NULL-PTR - constant CP-OFF
NULL-PTR TCB.STATUS NULL-PTR - constant STATUS-OFF
NULL-PTR TCB.PARK NULL-PTR - constant PARK-OFF
NULL-PTR TCB.QUEUE NULL-PTR - constant QUEUE-OFF
NULL-PTR TCB.EXIT-SLOT NULL-PTR - constant EXIT-SLOT-OFF
TCB-SIZE constant TCB-BYTES

;package
