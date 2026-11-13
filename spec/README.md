# Specs

This directory holds TLA+ specs of Habu's state machines. Each spec is kept
with its `.cfg`, and is checked before a change to the code it models lands.
They are plain TLA+ with standard modules only, checked by TLC from TLA+
tools 1.7.4 on spark, never on the Mac: `~/.local/bin/tlc` copies the spec's `.tla` and `.cfg` files there and
runs TLC with the arguments given.

## ModelCall

`ModelCall.tla` models one task that owns several MODEL calls, starts them and
serves them as they answer, with CURL's loop serving their transfers and
another task free to cancel or stop any call at any time. It proves that a
started call has one armed or answered read until it lands, while its owner
neither unwinds nor has ended, nor is giving the call's transfer up after a
failed take (LAND-FAULT's CURL:CLEANUP waits for the loop to leave the record,
while another task's SERVE or AWAIT gets E-STATE and its CANCEL only marks the
call cancelled); that the loop never copies a reply into memory its owner has
freed; that CLOSE frees only a call with no transfer out and no record in use;
that nothing wakes an owner after it has ended; that a cancelled call lands
unless its owner ends first; that a halted owner ends; that an ended owner's
transfers are all given back; and that an owner that follows the header's
caller rule ends with no call started. A take may throw: SERVE or AWAIT then
lands the call as LAND-FAULT does, whose CURL:CLEANUP gives up a record still
in the loop, as a halted owner does, before the call lands. SERVE and AWAIT
run a started call's receive deferred: a halt while AWAIT waits gives up the
record and lands the call before the owner unwinds, even when its read was
answered meanwhile, since NEXT-TAKE checks the halt first; a halted owner's
SERVE that lands nothing leaves the call started and the owner unwinding. A
landed call's recorder and journal put (ACCOUNT) run after the deferred
section: a halt pending while they wait unwinds the owner there, and the call
still ends landed. OwnerEnds, with the put's end fair only while no halt is
pending, shows that a halt does not wait for the recorder or put to end. With
TIER-RESEND!, a landed call whose send ended E-TIER is started again once by
its starter inside SERVE or AWAIT, uncounted in its starts, never when it is
stopped or was cancelled as it landed; the same properties hold.
Whether the owner follows the caller rule is fixed for a run: one that does
unwinds through a finally that CANCELs, then AWAITs, each call still started,
catching each AWAIT's fault; one that does not may end with calls still
started, whose records are abandoned and freed by the loop. It leaves out
another task's SERVE or AWAIT, which REQUIRE-STARTER refuses with the call
unchanged, and a later task on an ended owner's TCB, whose OWNED-INDEX refuses
that owner's record. The bounds are two calls, each started once, and a reply
of at most two chunks. The actions stand for these words: Start, Dispatch,
Serve and Await for MODEL's START, SERVE and AWAIT, Close for CLOSE, Cancel
and Stop for CANCEL and REQUEST-STOP; the takes and re-arms in Serve and Await
for CURL's NEXT-ARM, NEXT-READY? and NEXT-TAKE, the halted wait and
CleanupStep for ABANDON, PutDone and PutCut for ACCOUNT's recorder and
journal put ending or cut by a halt, PutDone's end for TIER-STEP and
TIER-MACHINE, Resend for TIER-STANDARD's dispatch by SEND-BEGUN after
RESEND-LOCKED, the owner's end for OWNER-END, and Add, Produce,
Deliver, Finish, CancelRun and LeaveRun for SERVICE-ONE and the finish of a
transfer; Halt, the halt handling, the unwind and FinallyStep for TASK's HALT,
DEFER-ENTER, DEFER-LEAVE, HALT-UNWIND and PAUSE and the caller's finally; and
the owner's park and its counted hints for TASK's WAIT and WAKE.

Check it from the repository root:

```sh
tlc -workers auto -metadir /tmp/habu-tlc -config spec/ModelCall.cfg spec/ModelCall.tla
```

A pass ends "Model checking completed. No error has been found."

## ProfClock

`ProfClock.tla` models the profiler clock's handshake in `src/habu/prof.f`:
the SIGALRM handler EMIT-PROF against the public control words prof-on,
prof-off, prof-reset and prof-report, run by two threads, main and a task,
either of which takes a SIGALRM at any step of its own code. A handler counts
itself into PROF-IN, reads PROF-ARMED again, and only then tests ARN-OWNER:
the owner samples, any other thread forwards to ARN-OWNER-TID while still
counted. A control word takes PROF-CTL, a ticket lock (CtlNext, CtlServe),
leaves ARMED-RUNS by a store, waits until PROF-IN reads 0, works, and arms
again when it is prof-on or held a running clock. It proves that no handler
samples while a control word works, that control words work one at a time,
that only the owner samples, that a forward reads ARN-OWNER-TID only while
neither prof-on nor prof-off is changing it, that every started control word
ends, and that a delivery pending on the live owner of a running clock runs
its handler. A thread's own code is strongly fair, since a SIGALRM may arrive
again as each handler returns; handler steps and delivery are weakly fair.
The header says why one atomic step per release store, acquire load or atomic
add is sound on ARMv8. The bounds are two threads and tickets modulo NTickets
= 3.

Check it from the repository root:

```sh
tlc -workers auto -metadir /tmp/habu-tlc -config spec/ProfClock.cfg spec/ProfClock.tla
```

A pass ends "Model checking completed. No error has been found."

Two mutants, each a copy of `ProfClock.cfg` with one constant changed, must
fail: `Wait = FALSE`, a controller that does not wait for PROF-IN, violates
HandlerExcludesControl in 18 states; `OwnerFirst = TRUE`, a handler that tests
the owner before it counts itself in, violates ForwardHandleStable in 18
states, and OnlyOwnerCounts in 21 with ForwardHandleStable left out of the
copy.
