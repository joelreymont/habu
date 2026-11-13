\ Fixed C-to-Habu task entry contract, shared by the engine and TASK library.
package TASK-ABI
public

0 constant EMPTY
1 constant CONSTRUCTED
2 constant RUNNING
3 constant DONE
4 constant HALT-REQ

$8 constant XT-OFF
$18 constant STACK-OFF
$28 constant REGION-OFF
$30 constant REGION-U-OFF
$38 constant DBASE-OFF
$40 constant NDICT-OFF
$48 constant CP-OFF
$50 constant STATUS-OFF
\ The task's own wake-up: TASK:WAIT waits on this semaphore and TASK:WAKE posts
\ it. A guard cell then one sem_t, it lives as long as its TCB in this process
\ and the capture sweep destroys it; ACTIVATE drains it, so every run opens at
\ zero.
$98 constant PARK-OFF
\ The head of the task's cleanup chain: 0 for none, else the row plus one.
$D8 constant EXIT-SLOT-OFF
$108 constant TCB-BYTES

;package
