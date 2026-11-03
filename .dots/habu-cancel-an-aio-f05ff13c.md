---
title: Cancel an AIO operation when the completion ring is full
status: open
priority: 2
issue-type: task
created-at: "2026-11-02T19:14:25.564519+02:00"
---

Problem: on Linux, with REC-LIVE at CQ-ENTRIES-N (512, AIO-CQ-ENTRIES src/lib/aio.f:85), URING-CLAIM refuses E-AIO-FULL (aio.f:922-924), so CANCEL-STAGE (:1003) cannot claim the record its cancel SQE needs. TRY-CANCEL (:1033) drops that code, both in a halted wait's finally and in SCRUB-ONE (:1036): the halted wait's operation is never cancelled and stays STATE-FORGET, the ring stays full, main's next AIO:CANCEL gets E-AIO-FULL (-9293) and AIO:STOP stays E-AIO-BUSY. Measured on spark (Linux 6.17, arm64) with the case below: asserts 1-37 pass (the join answers the halt, the catch does not run), assert 38 (FULL-DROP's first cancel) fails, with and without the halt fix of habu-aio-waits-keep-127bb200, so the defect predates it. Candidate: IORING_REGISTER_SYNC_CANCEL (Linux 6.0; spark 6.17, pulsar 6.18) cancels without an SQE or CQE of its own, so a cancel never needs ring room. The ring's fixed 512 entries is also a preset limit; DECISIONS 2026-10-31 'a PostgreSQL limit' says a library takes its limits from what it speaks, so the fix states which limit, if any, the kernel imposes. Acceptance: a cancel of an in-flight operation succeeds whatever the ring's fill; a halted wait's cleanup cancels its operation with the ring full; AIO:STOP goes idle after. Regression: the CASE-HALTED-FULL diff below (Linux only), failing at assert 38 before. Files: src/lib/aio.f, src/lib/aio-test.f. Verify: hb --load src/lib/aio-test.f on spark and on Darwin, rc 0. Depends: habu-aio-waits-keep-127bb200 landed. Ownership: carl. Claim: unassigned.

Regression case (apply to src/lib/aio-test.f after the aio halt lane lands):

```diff
diff --git a/src/lib/aio-test.f b/src/lib/aio-test.f
index d7d0b41884..2982936b72 100644
--- a/src/lib/aio-test.f
+++ b/src/lib/aio-test.f
@@ -41,6 +41,10 @@
 : AIO-TEST:HANDLE-IDX ( n -- n )
    HANDLE>IDX ;
 
+\ The records a claim can still take before E-AIO-FULL.
+: AIO-TEST:ROOM ( -- n )
+   CQ-ENTRIES-N @ REC-LIVE @ - ;
+
 ;package
 
 package AIO-TEST
@@ -68,6 +72,8 @@
 $10 constant SOCKADDR-N              \ sockaddr_in, as src/lib/net/udp4.f lays it out
 4 constant STREAM-BACKLOG
 $10 constant REUSE-N                  \ the bytes the reused socket's peer sends
+$400 constant FULL-CAP               \ above the completion ring's entries
+10000 constant FILL-MS               \ a filler deadline no case outlives
 
 : AIO-TEST-ALIGN8 ( -- )
    here FFI:>CELL 7 and 8 swap - 7 and allot ;
@@ -86,6 +92,9 @@
 variable FAN-DURING
 variable ANY-CANCELLED
 variable HALT-PARKED
+variable FULL-PARKED
+variable FULL-SEEN
+variable FULL-N
 variable P-R
 variable P-W
 variable XFER-PATH-U
@@ -117,10 +126,12 @@
 FAN-N TYPED-BUFFER FAN-TICKETS AIO:ticket
 ANY-N TYPED-BUFFER ANY-TICKETS AIO:ticket
 GROW-N TYPED-BUFFER GROW-TICKETS AIO:ticket
+FULL-CAP TYPED-BUFFER FULL-TICKETS AIO:ticket
 AIO-TEST-ALIGN8
 create GROW-IDX GROW-N cells allot
 TYPED-VARIABLE ANY-CUR AIO:ticket
 TYPED-VARIABLE LONE-TICKET AIO:ticket
+TYPED-VARIABLE FULL-CUR AIO:ticket
 TYPED-VARIABLE LONE-XFER AIO:xfer
 TYPED-VARIABLE THIEF-TICKET AIO:ticket
 TYPED-VARIABLE STALE-TICKET AIO:ticket  \ a handle whose record has been reused,
@@ -163,6 +174,7 @@
 TASK:MIN-STACK TASK:TASK TAKEOVER-TASK
 TASK:MIN-STACK TASK:TASK ABANDON-TASK
 TASK:MIN-STACK TASK:TASK HALT-TASK
+TASK:MIN-STACK TASK:TASK FULL-TASK
 TASK:MIN-STACK TASK:TASK XFER-OWNER-TASK
 TASK:MIN-STACK TASK:TASK XFER-FORGET-TASK
 TASK:MIN-STACK TASK:TASK OWN-EXIT-TASK
@@ -935,6 +947,49 @@
    STOP-LOOP
    P-R P-W PIPE-CLOSE ;
 
+\ The halt that ends FULL-WORK reaches no code after its catch, so FULL-SEEN
+\ keeps 1 unless the body itself saw an error.
+: FULL-WORK ( -- )
+   [: P-R @ >FD AIO:READABLE -1 >MS AIO:POLL LONE-TICKET !
+      1 FULL-PARKED atomic!
+      LONE-TICKET @ AWAIT>N drop ;] catch FULL-SEEN ! ;
+
+: FULL-FILL ( -- )
+   FULL-N @ 0 ?do FILL-MS >MS AIO:TIMEOUT i FULL-TICKETS ! loop ;
+
+\ Stops at the first refused cancel and never awaits a filler it left live.
+: FULL-DROP ( -- )
+   FULL-N @ 0 ?do
+      i FULL-TICKETS @ FULL-CUR !
+      [: FULL-CUR @ AIO:CANCEL ;] catch dup 0 T= 0<> if unloop exit then
+      FULL-CUR @ AWAIT>N MARK-CANCELLED T=
+   loop ;
+
+\ A halt while the wait's own record fills the completion ring: the join answers
+\ the halt and not E-AIO-FULL, and the ring still goes idle.
+: CASE-HALTED-FULL ( -- )
+   0 FULL-PARKED !
+   1 FULL-SEEN !
+   AIO-TEST:ROOM 1 - FULL-N !
+   FULL-N @ FULL-CAP <= TTRUE
+   FULL-N @ FULL-CAP > if exit then
+   P-R P-W PIPE-OPEN
+   FULL-FILL
+   AIO-TEST:ROOM 1 T=
+   ['] FULL-WORK FULL-TASK TASK:ACTIVATE
+   FULL-PARKED 1 REACH
+   SETTLE-MS TASK:SLEEP
+   AIO-TEST:ROOM 0 T=
+   FULL-TASK TASK:HALT
+   FULL-TASK AWAIT-END
+   FULL-TASK TASK:THROW@ 0 T=
+   FULL-SEEN @ 1 T=
+   FULL-TASK TASK:KILL
+   FULL-DROP
+   STOP-LOOP
+   P-R P-W PIPE-CLOSE
+   AIO:START ;
+
 \ XC connected to the listener; the accepted end comes back.
 : STREAM-PAIR ( -- n )
    OPEN-SOCKET
@@ -1118,6 +1173,7 @@
    HB-TARGET-LINUX-KERNEL? if
       CASE-PLANTED-TIMER
       CASE-PLANTED-POLL
+      CASE-HALTED-FULL
    then
    CASE-AWAIT-ANY
    CASE-FAN
```
