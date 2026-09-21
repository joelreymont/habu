\ Fixed C-to-Habu task entry contract, shared by the engine and TASK library.
package TASK-ABI
public

0 constant EMPTY
1 constant CONSTRUCTED
2 constant RUNNING
3 constant DONE
4 constant HALT-REQ

0 constant SIZE-OFF
$8 constant XT-OFF
$10 constant THREAD-OFF
$18 constant STACK-OFF
$20 constant STACK-U-OFF
$28 constant REGION-OFF
$30 constant REGION-U-OFF
$38 constant DBASE-OFF
$40 constant NDICT-OFF
$48 constant CP-OFF
$50 constant STATUS-OFF
$58 constant STOP-OFF
$60 constant RET-OFF
$68 constant USER-XT-OFF
\ A task's return stack and DO/LOOP frame stack are guarded mappings of its
\ own (src/habu/stack-abi.f); their bases are published into the task's
\ region copy at construction and the mappings released with the task.
$70 constant RSTACK-OFF
$78 constant RSTACK-U-OFF
$80 constant LSTACK-OFF
$88 constant LSTACK-U-OFF
\ The task runner records the code of an uncaught throw from the worker body
\ here before the entry marks the task DONE; the entry itself never reads it.
$90 constant THROW-OFF
\ The task's one-cell mailbox: the unread message, the TCB of whoever sent it,
\ the pending flag MSG? reads, and the two semaphores that make a send block
\ while the cell is full and a get block while it is empty (lib/task.f). Each
\ semaphore record is a guard cell followed by one sem_t.
$98 constant MSG-OFF
$A0 constant MSG-SENDER-OFF
$A8 constant MSG-PENDING-OFF
$B0 constant MSG-FREE-OFF
$D8 constant MSG-FULL-OFF
\ The task's own wake-up: TASK:STOP waits on this semaphore and TASK:WAKE posts
\ it, so a loop that completes work for many tasks needs no semaphore per waiter.
\ Same shape and lifetime as the two mailbox records above - a guard cell then
\ one sem_t, created with the task and destroyed with its memory.
$100 constant PARK-OFF
\ The task's outcome, read by TASK:JOIN after the task has ended: the cell
\ TASK:RETURN stores and the flag that says it did, the joiner's claim on that
\ one answer, the index of the task's cleanup quotation (0 for none, else the
\ slot plus one) and the semaphore the ending task signals. The result rows
\ outlive the task's memory, like the throw slot above them.
$128 constant RESULT-OFF
$130 constant RESULT-SET-OFF
$138 constant JOINER-OFF
$140 constant EXIT-SLOT-OFF
$148 constant DONE-OFF
$170 constant TCB-BYTES

;package
