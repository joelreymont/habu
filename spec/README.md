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
in the loop, as a halted owner does, before the call lands. Such a call never
rests, so TakeFault leaves `resend.out` FALSE, as Dispatch set it: a receive
throws only E-STATE (MODEL's CALL-HANDLE, CHAT-CHUNK and LOST; CURL's
NEXT-ARM, NEXT-READY?, NEXT-TAKE and CANCEL), CURL's E-OPERAND (NEXT-ARM), or a
buffer's, span's or memory's fault while it copies a chunk. None is in
RESENDS?'s class, so REST-OUT? keeps the call landed. A failed transfer is no
throw: NEXT-TAKE answers it as an end, which Take lands. SERVE and AWAIT
run a started call's receive deferred: a halt while AWAIT waits gives up the
record and lands the call before the owner unwinds, even when its read was
answered meanwhile, since NEXT-TAKE checks the halt first; a halted owner's
SERVE that lands nothing leaves the call started and the owner unwinding. A
landed call's recorder and journal put (ACCOUNT) run after the deferred
section: a halt pending while they wait unwinds the owner there, and the call
still ends landed. OwnerEnds, with the put's end fair only while no halt is
pending, shows that a halt does not wait for the recorder or put to end. Each
owner step is one hold of a call's gate, or a step another task cannot tell
from one, so another task's CANCEL or stop may come between any two; a
machine's next state is stored only once its transition's action has
returned, so a CANCEL or a halt inside that action finds the old state. With
TIER-RESEND!, a landed call whose send ended E-TIER is started again once by
its starter inside SERVE or AWAIT, uncounted in its starts, never when it is
stopped or was cancelled as or after it landed; the same properties hold. With
RESEND-WAIT!, a landed send in REST-OUT?'s class rests: one that failed in
RESENDS?'s class once its request left, after its record and the tier
machine's step, or whose key read failed at dispatch. The spec chooses each
landed send's ending and takes the class from it; that a failed journal
lookup or a journal put's fault never rests, and that a journal put's E-TIER
is never sent again, are facts about REST-OUT? and TIER-OUT?, which the spec
does not check.
SENT-SENSE chooses the rest, BEGIN-REST then publishes it under
the gate, and REST-SENSE senses the rest just begun before SERVE or AWAIT
returns. A resting call is started with no transfer, record or read, and its
resend machine holds a rest that has ended only inside the action that ends
it or after a halt cut that action's ACCOUNT (RestIdle). The tier machine's
resend comes first. A rest keeps the tier its send went out at: a fact about
the code that src/lib/net/model-test.f shows and the spec, which does not
model RECEIPT-SENT, does not check. SERVE of a resting call answers false
until it is due, then sends it again, and a send that lands at once (a
journal hit, a failed lookup or a dispatch fault) is sensed in the same step;
AWAIT parks until it is due or a hint wakes it. A halt due in a rest
(Pending, TASK:HALT-DUE?) unwinds the owner with the call still resting, at
SERVE-HALT's HALT-UNWIND when the call is due. One due when a send that ended
E-TIER is to be sent again stops the tier machine, and the call lands with
that send's outcome, or E-CANCELLED when a CANCEL kept it from being sent
again. NoHaltedSend shows that no send a rest or the tier machine starts
begins while a halt of the owner is due; inside DEFER-ENTER, where a halt is
not served, both go on. A CANCEL
or stop during a rest posts the owner's park, and RestStops shows that it
lands the call E-CANCELLED unless the owner ends first; CancelLands shows the
same of a CANCEL that answered true. As AWAIT makes a call idle, CancelEnds
checks what it shows: a CANCEL that answered true ends the call E-CANCELLED;
an E-CANCELLED call's last CANCEL answered true, or it was stopped with no
CANCEL answered, or a halt
cut its recorder; and unless a halt cut it, an E-CANCELLED call whose request
was dispatched has exactly one cancelled record, any other none. Once a CANCEL
has run since a call's START, NoSendAfterCancel shows that no send of it begins
until the next START: a resend that lands at once is to be sent again
(RESEND-NEXT?) while its machine is still resuming, so a CANCEL then marks it
and its sense lands it E-CANCELLED. A CANCEL of a landed send that would not
be sent again leaves its outcome. The caller rule's park begins only after a
round in which every SERVE answered false, so with no resting call stopped or
cancelled; one stopped or cancelled after the
round posts the park, which then returns at once. The park may time out while
a call rests. Time is not modelled: a resting call may be found due at any
SERVE or AWAIT. MODEL puts no cap on resends, so an AWAIT on a call whose
provider stays away never returns; the spec bounds a start's rests, past which
every send lands outside the class. Whether the owner follows the caller rule
is fixed for a run: one that does unwinds through a finally that CANCELs, then
AWAITs, each call still started, catching each AWAIT's fault; one that does
not may end with calls still started, whose records are abandoned and freed by
the loop. It leaves out another task's SERVE or AWAIT, which REQUIRE-STARTER
refuses with the call unchanged, a later task on an ended owner's TCB, whose
OWNED-INDEX refuses that owner's record, and a dispatch fault after the
transfer has started (ARM's): a send lands at once only before START-LOCKED.
The bounds are two calls, each started once and resting at most once a start,
and a reply of at most two chunks. The actions stand for these words: Start,
Serve and Await for MODEL's START, SERVE and AWAIT, Close for CLOSE, Cancel
and Stop for CANCEL and REQUEST-STOP; Dispatch for SEND-BEGUN's SEND-MACHINE
(LOOK-UP, DISPATCH, START-LOCKED) up to END-RUN, after START's BEGIN-CALL,
TIER-STANDARD's RESEND-LOCKED or RESEND's RESUME-LOCKED, and, for a cut send
START or SEND-STANDARD began, on through LAND-CUT and SENT-SENSE's settle;
LandCut for a cut send SEND-AGAIN began: LAND-CUT, SEND-AGAIN's return,
RESEND-MOVE and SENT-SENSE's settle; the takes and
re-arms in Serve and Await for CURL's NEXT-ARM, NEXT-READY? and NEXT-TAKE, the
halted wait and CleanupStep for ABANDON, PutDone and PutCut for ACCOUNT's
recorder and journal put ending or cut by a halt (LANDING-END), PutDone's end
for TIER-STEP, TIER-MACHINE, TIER-STANDARD and SENT-SENSE after a receive, or
for the move of the RESEND-MACHINE whose CANCEL-RECORD ran ACCOUNT; Resent for
RESEND-MOVE's store after SEND-AGAIN's send landed at once or RESEND's begin
faulted; Sense for SENT-SENSE of a send that landed at once; Publish for
BEGIN-REST; BegunSense for REST-SENSE of a rest just begun; Cut for
CANCEL-RECORD after LAND-CANCELLED's or RESUME-LOCKED's REST-END-LOCKED, whose
gate hold stored the outcome E-CANCELLED; RestServe and RestAwait for SERVE's
and AWAIT's RESEND-STEP of a resting call (REST-SENSE, SERVE-HALT's
HALT-UNWIND, RESEND's RESUME-LOCKED) and AWAIT's REST-PARK; the owner's end
for OWNER-END, and Add, Produce, Deliver, Finish, CancelRun and LeaveRun for
SERVICE-ONE and the
finish of a transfer; Halt, the halt handling, the unwind and FinallyStep for
TASK's HALT, DEFER-ENTER, DEFER-LEAVE, HALT-UNWIND and PAUSE and the caller's
finally; and the owner's park and its counted hints for TASK's WAIT and WAKE.

Check it from the repository root, with both configurations: `ModelCall.cfg`
runs two calls with one rest each; `ModelCall-rests.cfg` runs one call through
two rests, which a resend that rests again needs (NoSendAfterCancel).

```sh
tlc -workers auto -metadir /tmp/habu-tlc -config spec/ModelCall.cfg spec/ModelCall.tla
tlc -workers auto -metadir /tmp/habu-tlc -config spec/ModelCall-rests.cfg spec/ModelCall.tla
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
