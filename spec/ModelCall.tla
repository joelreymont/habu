----------------------------- MODULE ModelCall -----------------------------
(* MODEL's call (src/lib/net/model.f: START, SERVE, AWAIT, CLOSE, CANCEL,  *)
(* REQUEST-STOP, ACCOUNT), over CURL's record and request machines         *)
(* (src/lib/net/curl.f: NEXT-ARM, NEXT-READY?, NEXT-TAKE, ABANDON,         *)
(* CLEANUP, OWNER-END, SERVICE-ONE) and TASK's halt and wait               *)
(* (src/lib/task.f: HALT, PAUSE, HALT-UNWIND, DEFER-ENTER, DEFER-LEAVE,    *)
(* WAIT, WAKE). One owner task owns every call, the CURL loop serves their *)
(* records, and another task may CANCEL or REQUEST-STOP any call at any    *)
(* time. Whether the owner follows the header's caller rule (model.f: it   *)
(* CANCELs, then AWAITs, its busy calls in a finally) is fixed at Init.    *)
EXTENDS Naturals

CONSTANTS Calls, MaxChunks, MaxStarts

VARIABLES
    lc, rest, cancelled, stopped, transport, span, starts,
    opc, hints, halted, deferred, unwound,
    rec, req, left, held, ends,
    wroteFreed, wokeEnded, rule

vars == <<lc, rest, cancelled, stopped, transport, span, starts,
          opc, hints, halted, deferred, unwound,
          rec, req, left, held, ends, wroteFreed, wokeEnded, rule>>

Lifecycles == {"idle", "started", "landed", "closed"}
Records == {"none", "claimed", "running", "cancel", "done",
            "abandoned", "leaving", "left"}
Requests == {"none", "waiting", "chunk", "response", "failed"}
Answered == {"chunk", "response", "failed"}
Places == {<<"free">>, <<"parked">>, <<"halting">>, <<"unwinding">>,
           <<"finally">>, <<"ended">>}
          \cup ({"dispatch", "await", "abandon"} \X Calls)
          \cup ({"cleanup", "account"} \X Calls \X {"serve", "await"})

TypeOK ==
    /\ lc \in [Calls -> Lifecycles]
    /\ rest \in [Calls -> {"reading", "dropping"}]
    /\ cancelled \in [Calls -> BOOLEAN]
    /\ stopped \in [Calls -> BOOLEAN]
    /\ transport \in [Calls -> BOOLEAN]
    /\ span \in [Calls -> {"live", "freed"}]
    /\ starts \in [Calls -> 0..MaxStarts]
    /\ opc \in Places
    /\ hints \in Nat
    /\ halted \in BOOLEAN
    /\ deferred \in BOOLEAN
    /\ unwound \in BOOLEAN
    /\ rec \in [Calls -> Records]
    /\ req \in [Calls -> Requests]
    /\ left \in [Calls -> 0..MaxChunks]
    /\ held \in [Calls -> BOOLEAN]
    /\ ends \in [Calls -> {"ok", "fail"}]
    /\ wroteFreed \in BOOLEAN
    /\ wokeEnded \in BOOLEAN
    /\ rule \in BOOLEAN

Init ==
    /\ lc = [c \in Calls |-> "idle"]
    /\ rest = [c \in Calls |-> "reading"]
    /\ cancelled = [c \in Calls |-> FALSE]
    /\ stopped = [c \in Calls |-> FALSE]
    /\ transport = [c \in Calls |-> FALSE]
    /\ span = [c \in Calls |-> "live"]
    /\ starts = [c \in Calls |-> 0]
    /\ opc = <<"free">>
    /\ hints = 0
    /\ halted = FALSE
    /\ deferred = FALSE
    /\ unwound = FALSE
    /\ rec = [c \in Calls |-> "none"]
    /\ req = [c \in Calls |-> "none"]
    /\ left = [c \in Calls |-> 0]
    /\ held = [c \in Calls |-> FALSE]
    /\ ends = [c \in Calls |-> "ok"]
    /\ wroteFreed = FALSE
    /\ wokeEnded = FALSE
    /\ rule \in BOOLEAN

-----------------------------------------------------------------------------
(* Shared steps *)

(* TASK:WAKE of the owner by the loop: a hint, counted. *)
Wake ==
    /\ hints' = hints + 1
    /\ wokeEnded' = (wokeEnded \/ opc = <<"ended">>)

(* CURL:CANCEL: CANCEL-MARK moves only a claimed or running record. *)
Marked(c) == IF rec[c] \in {"claimed", "running"} THEN "cancel" ELSE rec[c]

CancelMark(c) == rec' = [rec EXCEPT ![c] = Marked(c)]

(* NEXT-ARM: a done record with nothing held answers at once. *)
ArmFor(c, r) ==
    IF r = "done" /\ ~held[c]
    THEN IF ends[c] = "fail" THEN "failed" ELSE "response"
    ELSE "waiting"

(* What a receive that throws leaves of the read and the record, as        *)
(* <<request, record>>.                                                    *)
(* Before the take (the request still answered):                           *)
(*   CALL-HANDLE: E-STATE, the call holds no handle.                       *)
(*   NEXT-READY?, NEXT-TAKE: E-STATE, no owned record or no armed read.    *)
(* After a chunk's take (the request taken, none armed):                   *)
(*   CHAT-CHUNK: E-STATE, the chat holds no decoder.                       *)
(*   NEXT-ARM in REARM or REARM-DROP: E-STATE, no owned streamed record or *)
(*   a read still armed; E-OPERAND, a span under one byte.                 *)
(*   CURL:CANCEL in DROP-REPLY: E-STATE, no record; when it marked the     *)
(*   record, REARM-DROP's NEXT-ARM may still throw.                        *)
(* An end's take releases the record and has nothing after it that throws. *)
Throws(c) ==
    {<<req[c], rec[c]>>}
    \cup (IF req[c] = "chunk" THEN {<<"none", rec[c]>>} ELSE {})
    \cup (IF req[c] = "chunk" /\ rest[c] = "reading"
          THEN {<<"none", Marked(c)>>} ELSE {})

(* A receive that throws, caught by SERVE and AWAIT and landed by          *)
(* LAND-FAULT: KEEP-FAULT, then SETTLE, whose SETTLE-SENSE ends the       *)
(* transfer (END-TRANSFER), frees the handle (CURL:CLEANUP) and lands the  *)
(* call (END-CALL). The record is the caller's, so CLEANUP's OWNED-INDEX   *)
(* finds it and ABANDON gives it up: a DONE one is freed at once           *)
(* (LEAVE-DONE, REC-RELEASE) and the call lands; any other is marked       *)
(* LEAVING and the owner waits in ABANDON (CleanupStep) before the call    *)
(* lands.                                                                  *)
TakeFault(c) ==
    \E t \in Throws(c) :
        /\ transport' = [transport EXCEPT ![c] = FALSE]
        /\ IF t[2] = "done"
           THEN /\ rec' = [rec EXCEPT ![c] = "none"]
                /\ req' = [req EXCEPT ![c] = "none"]
                /\ held' = [held EXCEPT ![c] = FALSE]
                /\ lc' = [lc EXCEPT ![c] = "landed"]
           ELSE /\ rec' = [rec EXCEPT ![c] = "leaving"]
                /\ req' = [req EXCEPT ![c] = t[1]]
                /\ UNCHANGED <<held, lc>>
        /\ UNCHANGED rest

(* NEXT-TAKE of an answered request (STREAM-RESULT-TAKE), as SERVE and     *)
(* AWAIT do it: a chunk is fed and the next read armed at once, or it      *)
(* spoils the reply (DROP: CURL:CANCEL, rest dropping, re-arm); an end     *)
(* releases the record, ends the transfer (END-TRANSFER) and lands; or the *)
(* receive throws (TakeFault).                                             *)
Take(c) ==
    \/ /\ req[c] = "chunk"
       /\ \/ /\ req' = [req EXCEPT ![c] = ArmFor(c, rec[c])]
             /\ UNCHANGED <<rec, rest>>
          \/ /\ rest[c] = "reading"
             /\ rest' = [rest EXCEPT ![c] = "dropping"]
             /\ CancelMark(c)
             /\ req' = [req EXCEPT ![c] = ArmFor(c, rec'[c])]
       /\ UNCHANGED <<lc, transport, held>>
    \/ /\ req[c] \in {"response", "failed"}
       /\ rec' = [rec EXCEPT ![c] = "none"]
       /\ req' = [req EXCEPT ![c] = "none"]
       /\ transport' = [transport EXCEPT ![c] = FALSE]
       /\ lc' = [lc EXCEPT ![c] = "landed"]
       /\ UNCHANGED <<rest, held>>
    \/ TakeFault(c)

(* CANCEL-LOCKED under the gate (model.f): a started call is cut; any     *)
(* other call only has its cancelled flag cleared.                         *)
CancelBody(c) ==
    IF lc[c] = "started"
    THEN /\ cancelled' = [cancelled EXCEPT ![c] = TRUE]
         /\ IF transport[c]
            THEN /\ transport' = [transport EXCEPT ![c] = FALSE]
                 /\ CancelMark(c)
            ELSE UNCHANGED <<transport, rec>>
    ELSE /\ cancelled' = [cancelled EXCEPT ![c] = FALSE]
         /\ UNCHANGED <<transport, rec>>

(* OWNER-END (curl.f), the owner's AT-EXIT: OWNER-MARK abandons every     *)
(* record that is not FREE.                                                *)
OwnerEnd ==
    /\ rec' = [c \in Calls |-> IF rec[c] = "none" THEN "none" ELSE "abandoned"]
    /\ opc' = <<"ended">>

(* Where the unwind starts: the rule's finally, else the cleanups. *)
Unwind == IF rule THEN <<"finally">> ELSE <<"unwinding">>

(* Where an AWAIT returns to. *)
Back ==
    IF unwound THEN Unwind ELSE IF deferred THEN <<"halting">> ELSE <<"free">>

(* HALT-UNWIND's test (task.f): a halt whose unwind has not begun          *)
(* (TCB-HALT-PENDING? reads TCB.STOP), outside every DEFER-ENTER.          *)
Pending == halted /\ ~unwound /\ ~deferred

(* The end of SERVE's RECEIVE-STARTED (model.f) after DEFER-LEAVE and *)
(* ACCOUNT: 0 TASK:HALT-UNWIND starts the unwind on a pending halt. *)
ServeEnd ==
    IF Pending
    THEN /\ unwound' = TRUE
         /\ opc' = Unwind
    ELSE /\ opc' = <<"free">>
         /\ UNCHANGED unwound

(* NEXT-TAKE's TASK:PAUSE after ABANDON runs inside RECEIVE-STARTED's      *)
(* DEFER-ENTER, so it only yields: the take answers failed CURLE_ABORTED,  *)
(* TAKE-WAIT or DISCARD-WAIT answers done and the receive lands the call.  *)
PauseAfter(c) ==
    /\ lc' = [lc EXCEPT ![c] = "landed"]
    /\ transport' = [transport EXCEPT ![c] = FALSE]
    /\ opc' = <<"await", c>>
    /\ UNCHANGED unwound

-----------------------------------------------------------------------------
(* The owner *)

Free == opc = <<"free">> /\ ~halted

Start(c) ==
    /\ Free
    /\ lc[c] = "idle"
    /\ starts[c] < MaxStarts
    /\ ~stopped[c]                     \* BEGIN-LOCKED: a stopped call is E-CANCELLED
    /\ lc' = [lc EXCEPT ![c] = "started"]
    /\ cancelled' = [cancelled EXCEPT ![c] = FALSE]
    /\ starts' = [starts EXCEPT ![c] = @ + 1]
    /\ opc' = <<"dispatch", c>>
    /\ UNCHANGED <<rest, stopped, transport, span, hints, halted, deferred,
                   unwound, rec, req, left, held, ends, wroteFreed, wokeEnded,
                   rule>>

(* START-LOCKED (model.f): a cancel since begin means no transfer. *)
Dispatch(c) ==
    /\ opc = <<"dispatch", c>>
    /\ opc' = <<"free">>
    /\ IF cancelled[c]
       THEN /\ lc' = [lc EXCEPT ![c] = "landed"]
            /\ UNCHANGED <<rest, transport, rec, req, left, held, ends>>
       ELSE /\ rec' = [rec EXCEPT ![c] = "claimed"]
            /\ transport' = [transport EXCEPT ![c] = TRUE]
            /\ req' = [req EXCEPT ![c] = "waiting"]
            /\ rest' = [rest EXCEPT ![c] = "reading"]
            /\ held' = [held EXCEPT ![c] = FALSE]
            /\ \E e \in {"ok", "fail"}, n \in 0..MaxChunks:
                  /\ ends' = [ends EXCEPT ![c] = e]
                  /\ left' = [left EXCEPT ![c] = n]
            /\ UNCHANGED lc
    /\ UNCHANGED <<cancelled, stopped, span, starts, hints, halted, deferred,
                   unwound, wroteFreed, wokeEnded, rule>>

(* SERVE's started arm (RECEIVE-STARTED) as one step: the receive only     *)
(* polls, deferred; a take that throws may wait in CLEANUP's ABANDON; then *)
(* DEFER-LEAVE, and a landed call's recorder and journal put (ACCOUNT)     *)
(* before HALT-UNWIND. A halted owner's poll that takes nothing leaves the *)
(* call started and the owner unwinding.                                   *)
(* REQUIRE-STARTER passes: the one owner starts every call.                *)
(* OWNED-INDEX's refusal of a left owner's record never fires: no TCB is   *)
(* reused.                                                                 *)
Serve(c) ==
    /\ opc = <<"free">>
    /\ lc[c] = "started"
    /\ \/ /\ req[c] \in Answered
          /\ Take(c)
       \/ /\ halted
          /\ req[c] = "waiting"
          /\ UNCHANGED <<lc, rest, transport, rec, req, held>>
    /\ IF rec'[c] = "leaving"
       THEN /\ opc' = <<"cleanup", c, "serve">>
            /\ UNCHANGED unwound
       ELSE IF lc'[c] = "landed"
            THEN /\ opc' = <<"account", c, "serve">>
                 /\ UNCHANGED unwound
            ELSE ServeEnd
    /\ UNCHANGED <<cancelled, stopped, span, starts, hints, halted,
                   deferred, left, ends, wroteFreed, wokeEnded, rule>>

WaitAny ==
    /\ Free
    /\ \E c \in Calls : lc[c] = "started"
    /\ \A c \in Calls : lc[c] = "started" => req[c] \notin Answered
    /\ opc' = <<"parked">>
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   hints, halted, deferred, unwound, rec, req, left, held,
                   ends, wroteFreed, wokeEnded, rule>>

(* TASK:WAIT returns by taking one hint, whoever posted it. *)
Resume ==
    /\ opc = <<"parked">>
    /\ hints > 0
    /\ hints' = hints - 1
    /\ opc' = <<"free">>
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   halted, deferred, unwound, rec, req, left, held, ends,
                   wroteFreed, wokeEnded, rule>>

(* AWAIT: a landed call goes idle; a started one enters RECEIVE-STARTED.   *)
(* REQUIRE-STARTER passes: the one owner starts every call.                *)
(* OWNED-INDEX's refusal of a left owner's record never fires: no TCB is   *)
(* reused.                                                                 *)
Await(c) ==
    /\ Free
    /\ \/ /\ lc[c] = "landed"
          /\ lc' = [lc EXCEPT ![c] = "idle"]
          /\ UNCHANGED opc
       \/ /\ lc[c] = "started"
          /\ opc' = <<"await", c>>
          /\ UNCHANGED lc
    /\ UNCHANGED <<rest, cancelled, stopped, transport, span, starts, hints,
                   halted, deferred, unwound, rec, req, left, held, ends,
                   wroteFreed, wokeEnded, rule>>

(* AWAIT's started arm, deferred: take what is answered, else NEXT-TAKE's  *)
(* TASK:WAIT and its TASK:HALT-PENDING? check, which starts ABANDON        *)
(* (curl.f) on a halt whose unwind has not begun; in the unwind it waits   *)
(* as an unhalted owner does. After TASK:WAIT, NEXT-TAKE checks the halt   *)
(* before an answered read, so an owner with a halt pending gives the      *)
(* record up even when its read was answered. Once the call lands,         *)
(* DEFER-LEAVE, then its recorder and journal put (ACCOUNT).               *)
AwaitStep(c) ==
    /\ opc = <<"await", c>>
    /\ \/ /\ lc[c] = "landed"
          /\ opc' = <<"account", c, "await">>
          /\ UNCHANGED <<lc, rest, transport, hints, unwound, rec, req,
                         held>>
       \/ /\ lc[c] = "started"
          /\ req[c] \in Answered
          /\ Take(c)
          /\ opc' = IF rec'[c] = "leaving" THEN <<"cleanup", c, "await">> ELSE opc
          /\ UNCHANGED <<hints, unwound>>
       \/ /\ lc[c] = "started"
          /\ req[c] = "waiting" \/ (halted /\ ~unwound /\ req[c] \in Answered)
          /\ hints > 0
          /\ hints' = hints - 1
          /\ IF ~(halted /\ ~unwound)
             THEN UNCHANGED <<lc, rest, transport, opc, unwound, rec, req, held>>
             ELSE IF rec[c] = "done"
                  \* ABANDON: a DONE record is out of the multi handle; freed here.
                  THEN /\ rec' = [rec EXCEPT ![c] = "none"]
                       /\ req' = [req EXCEPT ![c] = "none"]
                       /\ held' = [held EXCEPT ![c] = FALSE]
                       /\ PauseAfter(c)
                       /\ UNCHANGED rest
                  ELSE /\ rec' = [rec EXCEPT ![c] = "leaving"]
                       /\ opc' = <<"abandon", c>>
                       /\ UNCHANGED <<lc, rest, transport, unwound, req, held>>
    /\ UNCHANGED <<cancelled, stopped, span, starts, halted, deferred, left,
                   ends, wroteFreed, wokeEnded, rule>>

(* ABANDON's wait: the span is the owner's again only once the loop has   *)
(* let go (LEFT); the owner frees the record itself.                       *)
AbandonStep(c) ==
    /\ opc = <<"abandon", c>>
    /\ \/ /\ rec[c] = "left"
          /\ rec' = [rec EXCEPT ![c] = "none"]
          /\ req' = [req EXCEPT ![c] = "none"]
          /\ held' = [held EXCEPT ![c] = FALSE]
          /\ PauseAfter(c)
          /\ UNCHANGED hints
       \/ /\ rec[c] /= "left"
          /\ hints > 0
          /\ hints' = hints - 1
          /\ UNCHANGED <<lc, transport, opc, unwound, rec, req, held>>
    /\ UNCHANGED <<rest, cancelled, stopped, span, starts, halted, deferred,
                   left, ends, wroteFreed, wokeEnded, rule>>

(* CLEANUP's ABANDON in LAND-FAULT's SETTLE-SENSE: the same wait; once the *)
(* owner has freed the record, END-CALL lands the call, then its recorder  *)
(* and journal put run (ACCOUNT).                                          *)
CleanupStep(c) ==
    \E f \in {"serve", "await"} :
        /\ opc = <<"cleanup", c, f>>
        /\ \/ /\ rec[c] = "left"
              /\ rec' = [rec EXCEPT ![c] = "none"]
              /\ req' = [req EXCEPT ![c] = "none"]
              /\ held' = [held EXCEPT ![c] = FALSE]
              /\ lc' = [lc EXCEPT ![c] = "landed"]
              /\ opc' = <<"account", c, f>>
              /\ UNCHANGED <<hints, unwound>>
           \/ /\ rec[c] /= "left"
              /\ hints > 0
              /\ hints' = hints - 1
              /\ UNCHANGED <<lc, opc, unwound, rec, req, held>>
        /\ UNCHANGED <<rest, cancelled, stopped, transport, span, starts,
                       halted, deferred, left, ends, wroteFreed, wokeEnded,
                       rule>>

(* ACCOUNT (model.f): a landed call's recorder and journal put, which may  *)
(* wait, run after RECEIVE-STARTED's DEFER-LEAVE. The put ends: SERVE's    *)
(* arm ends (ServeEnd); AWAIT's starts the unwind on a pending halt with   *)
(* the call landed, else makes the call idle and returns.                  *)
PutDone(c) ==
    \E f \in {"serve", "await"} :
        /\ opc = <<"account", c, f>>
        /\ IF f = "serve"
           THEN /\ ServeEnd
                /\ UNCHANGED lc
           ELSE IF Pending
                THEN /\ unwound' = TRUE
                     /\ opc' = Unwind
                     /\ UNCHANGED lc
                ELSE /\ lc' = [lc EXCEPT ![c] = "idle"]
                     /\ opc' = Back
                     /\ UNCHANGED unwound
        /\ UNCHANGED <<rest, cancelled, stopped, transport, span, starts,
                       hints, halted, deferred, rec, req, left, held, ends,
                       wroteFreed, wokeEnded, rule>>

(* A halt served while the recorder or put waits: the task unwinds there,  *)
(* and LANDING-END leaves the call landed (outcome E-CANCELLED).           *)
PutCut(c) ==
    \E f \in {"serve", "await"} :
        /\ opc = <<"account", c, f>>
        /\ Pending
        /\ unwound' = TRUE
        /\ opc' = Unwind
        /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                       hints, halted, deferred, rec, req, left, held, ends,
                       wroteFreed, wokeEnded, rule>>

(* CLOSE (model.f) frees an idle or landed call's arena; a started one is  *)
(* refused.                                                                *)
CloseBody(c) ==
    /\ lc[c] \in {"idle", "landed"}
    /\ lc' = [lc EXCEPT ![c] = "closed"]
    /\ span' = [span EXCEPT ![c] = "freed"]

Close(c) ==
    /\ Free
    /\ CloseBody(c)
    /\ UNCHANGED <<rest, cancelled, stopped, transport, starts, opc, hints,
                   halted, deferred, unwound, rec, req, left, held, ends,
                   wroteFreed, wokeEnded, rule>>

(* An owner that does not follow the rule ends its body with calls still   *)
(* started (a fault no cleanup awaits): CLOSE refuses those, so their      *)
(* spans stay; OWNER-END abandons their records. This is the case          *)
(* OWNER-END exists for.                                                   *)
Quit ==
    /\ opc = <<"free">>
    /\ ~rule
    /\ OwnerEnd
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   hints, halted, deferred, unwound, req, left, held, ends,
                   wroteFreed, wokeEnded, rule>>

(* The serve loop finds the halt: DEFER-ENTER, then cancel and await      *)
(* every call.                                                             *)
HaltEnter ==
    /\ opc = <<"free">>
    /\ halted
    /\ deferred' = TRUE
    /\ opc' = <<"halting">>
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   hints, halted, unwound, rec, req, left, held, ends,
                   wroteFreed, wokeEnded, rule>>

(* One step of the owner's cleanup of its calls, in halt handling or in   *)
(* the unwind: cancel a started call, await a cancelled one, await a       *)
(* landed one.                                                             *)
Settle(c) ==
    \/ /\ lc[c] = "started"
       /\ ~cancelled[c] \/ transport[c]
       /\ CancelBody(c)
       /\ UNCHANGED <<lc, opc>>
    \/ /\ lc[c] = "started"
       /\ cancelled[c] /\ ~transport[c]
       /\ opc' = <<"await", c>>
       /\ UNCHANGED <<lc, cancelled, transport, rec>>
    \/ /\ lc[c] = "landed"
       /\ lc' = [lc EXCEPT ![c] = "idle"]
       /\ UNCHANGED <<opc, cancelled, transport, rec>>

Unsettled == \E c \in Calls : lc[c] \in {"started", "landed"}

HaltStep ==
    /\ opc = <<"halting">>
    /\ \/ /\ \E c \in Calls : Settle(c)
          /\ UNCHANGED <<deferred, unwound>>
       \/ /\ ~Unsettled
          /\ deferred' = FALSE                \* DEFER-LEAVE
          /\ unwound' = TRUE                  \* PAUSE starts the unwind
          /\ opc' = Unwind
          /\ UNCHANGED <<lc, cancelled, transport, rec>>
    /\ UNCHANGED <<rest, stopped, span, starts, hints, halted, req, left,
                   held, ends, wroteFreed, wokeEnded, rule>>

(* The unwind's cleanups of an owner that does not follow the rule (their *)
(* PAUSE only yields): settle every call, close every call, then          *)
(* OWNER-END.                                                              *)
UnwindStep ==
    /\ opc = <<"unwinding">>
    /\ \/ /\ \E c \in Calls : Settle(c)
          /\ UNCHANGED span
       \/ /\ ~Unsettled
          /\ \E c \in Calls : CloseBody(c)
          /\ UNCHANGED <<opc, cancelled, transport, rec>>
       \/ /\ \A c \in Calls : lc[c] = "closed"
          /\ OwnerEnd
          /\ UNCHANGED <<lc, cancelled, transport, span>>
    /\ UNCHANGED <<rest, stopped, starts, hints, halted, deferred, unwound,
                   req, left, held, ends, wroteFreed, wokeEnded, rule>>

(* The unwind of an owner that follows the rule runs its finally: for each *)
(* call still started, CANCEL (CANCEL-LOCKED), then AWAIT, catching        *)
(* AWAIT's fault (E-CANCELLED for a call it just cancelled), whose started *)
(* arm (AwaitStep) runs inside the unwind; then OWNER-END.                 *)
FinallyStep ==
    /\ opc = <<"finally">>
    /\ \/ \E c \in Calls :
             /\ lc[c] = "started"
             /\ CancelBody(c)
             /\ opc' = <<"await", c>>
       \/ /\ \A c \in Calls : lc[c] /= "started"
          /\ OwnerEnd
          /\ UNCHANGED <<cancelled, transport>>
    /\ UNCHANGED <<lc, rest, stopped, span, starts, hints, halted, deferred,
                   unwound, req, left, held, ends, wroteFreed, wokeEnded,
                   rule>>

-----------------------------------------------------------------------------
(* The environment and another task *)

(* TASK:HALT: one CAS, then the park is posted; an ended task is left alone. *)
Halt ==
    /\ ~halted
    /\ opc /= <<"ended">>
    /\ halted' = TRUE
    /\ hints' = hints + 1
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   opc, deferred, unwound, rec, req, left, held, ends,
                   wroteFreed, wokeEnded, rule>>

(* CANCEL: E-STATE on a closed call (its arena is gone). *)
Cancel(c) ==
    /\ lc[c] /= "closed"
    /\ CancelBody(c)
    /\ UNCHANGED <<lc, rest, stopped, span, starts, opc, hints, halted,
                   deferred, unwound, req, left, held, ends, wroteFreed,
                   wokeEnded, rule>>

Stop(c) ==
    /\ lc[c] /= "closed"
    /\ stopped' = [stopped EXCEPT ![c] = TRUE]
    /\ CancelBody(c)
    /\ UNCHANGED <<lc, rest, span, starts, opc, hints, halted, deferred,
                   unwound, req, left, held, ends, wroteFreed, wokeEnded,
                   rule>>

-----------------------------------------------------------------------------
(* The CURL loop: SERVICE-ONE and the finish of a transfer *)

Add(c) ==
    /\ rec[c] = "claimed"
    /\ rec' = [rec EXCEPT ![c] = "running"]
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   opc, hints, halted, deferred, unwound, req, left, held,
                   ends, wroteFreed, wokeEnded, rule>>

Produce(c) ==
    /\ rec[c] = "running"
    /\ left[c] > 0
    /\ ~held[c]
    /\ held' = [held EXCEPT ![c] = TRUE]
    /\ left' = [left EXCEPT ![c] = @ - 1]
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   opc, hints, halted, deferred, unwound, rec, req, ends,
                   wroteFreed, wokeEnded, rule>>

(* STREAM-DELIVER, reached only past SERVICE-ONE's CANCEL and LEAVING?     *)
(* checks: a held chunk is copied into the armed span, or a done record    *)
(* with nothing held answers its end.                                      *)
Deliver(c) ==
    /\ req[c] = "waiting"
    /\ \/ /\ rec[c] \in {"running", "done"}
          /\ held[c]
          /\ wroteFreed' = (wroteFreed \/ span[c] = "freed")
          /\ held' = [held EXCEPT ![c] = FALSE]
          /\ req' = [req EXCEPT ![c] = "chunk"]
       \/ /\ rec[c] = "done"
          /\ ~held[c]
          /\ req' = [req EXCEPT ![c] = IF ends[c] = "fail" THEN "failed" ELSE "response"]
          /\ UNCHANGED <<held, wroteFreed>>
    /\ Wake
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   opc, halted, deferred, unwound, rec, left, ends, rule>>

(* STREAM-FINISH, then SETTLE: a streamed record is not woken here. *)
Finish(c) ==
    /\ rec[c] = "running"
    /\ (left[c] = 0 /\ ~held[c]) \/ ends[c] = "fail"
    /\ rec' = [rec EXCEPT ![c] = "done"]
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   opc, hints, halted, deferred, unwound, req, left, held,
                   ends, wroteFreed, wokeEnded, rule>>

(* CANCEL-RUN: FAIL-RUN drops what is held, then SETTLE-FAILED answers a  *)
(* waiting request.                                                        *)
CancelRun(c) ==
    /\ rec[c] = "cancel"
    /\ held' = [held EXCEPT ![c] = FALSE]
    /\ rec' = [rec EXCEPT ![c] = "done"]
    /\ ends' = [ends EXCEPT ![c] = "fail"]
    /\ IF req[c] = "waiting"
       THEN /\ req' = [req EXCEPT ![c] = "failed"]
            /\ Wake
       ELSE UNCHANGED <<req, hints, wokeEnded>>
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   opc, halted, deferred, unwound, left, wroteFreed, rule>>

(* LEAVE-RUN, then LEAVE-DONE: only a LEAVING owner, still waiting in      *)
(* ABANDON, is woken; an abandoned record's owner has ended.               *)
LeaveRun(c) ==
    /\ rec[c] \in {"abandoned", "leaving"}
    /\ held' = [held EXCEPT ![c] = FALSE]
    /\ IF rec[c] = "leaving"
       THEN /\ rec' = [rec EXCEPT ![c] = "left"]
            /\ Wake
            /\ UNCHANGED req
       ELSE /\ rec' = [rec EXCEPT ![c] = "none"]
            /\ req' = [req EXCEPT ![c] = "none"]
            /\ UNCHANGED <<hints, wokeEnded>>
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   opc, halted, deferred, unwound, left, ends, wroteFreed,
                   rule>>

-----------------------------------------------------------------------------

(* The owner has ended for good: TLC reads no deadlock here. *)
Ended ==
    /\ opc = <<"ended">>
    /\ UNCHANGED vars

OwnerStep ==
    \/ \E c \in Calls :
          Start(c) \/ Dispatch(c) \/ Serve(c) \/ Await(c) \/ AwaitStep(c)
          \/ AbandonStep(c) \/ CleanupStep(c) \/ PutDone(c) \/ PutCut(c)
          \/ Close(c)
    \/ WaitAny \/ Resume \/ Quit \/ HaltEnter \/ HaltStep \/ UnwindStep
    \/ FinallyStep

LoopStep ==
    \E c \in Calls :
        Add(c) \/ Produce(c) \/ Deliver(c) \/ Finish(c) \/ CancelRun(c)
        \/ LeaveRun(c)

Next ==
    \/ OwnerStep
    \/ LoopStep
    \/ Halt
    \/ \E c \in Calls : Cancel(c) \/ Stop(c)
    \/ Ended

Fairness ==
    /\ \A c \in Calls :
          /\ WF_vars(Start(c)) /\ WF_vars(Dispatch(c)) /\ WF_vars(Serve(c))
          /\ WF_vars(Await(c)) /\ WF_vars(AwaitStep(c))
          /\ WF_vars(AbandonStep(c)) /\ WF_vars(CleanupStep(c))
          /\ WF_vars(PutDone(c) /\ ~Pending) /\ WF_vars(PutCut(c))
          /\ WF_vars(Close(c))
          /\ WF_vars(Add(c)) /\ WF_vars(Produce(c)) /\ WF_vars(Deliver(c))
          /\ WF_vars(Finish(c)) /\ WF_vars(CancelRun(c))
          /\ WF_vars(LeaveRun(c))
    /\ WF_vars(WaitAny) /\ WF_vars(Resume) /\ WF_vars(HaltEnter)
    /\ WF_vars(HaltStep) /\ WF_vars(UnwindStep) /\ WF_vars(FinallyStep)

Spec == Init /\ [][Next]_vars /\ Fairness

-----------------------------------------------------------------------------
(* Properties *)

SpanSafe == ~wroteFreed

Unwinding == unwound \/ opc = <<"ended">>

OneRequest ==
    \A c \in Calls :
        (lc[c] = "started" /\ ~Unwinding /\ opc /= <<"dispatch", c>>
             /\ \A f \in {"serve", "await"} : opc /= <<"cleanup", c, f>>)
            => /\ rec[c] /= "none"
               /\ req[c] \in {"waiting"} \cup Answered

NoWakeAfterEnd == ~wokeEnded

(* A closed call has no transfer out and no record in use. *)
ClosedFreed ==
    \A c \in Calls : lc[c] = "closed" => ~transport[c] /\ rec[c] = "none"

(* An owner that follows the rule ends with no call started. *)
RuleLands ==
    (rule /\ opc = <<"ended">>) => \A c \in Calls : lc[c] /= "started"

(* Every unwind lands every call before OWNER-END: only Quit ends an owner *)
(* with a call still started.                                              *)
CancelLands ==
    \A c \in Calls :
        (lc[c] = "started" /\ cancelled[c])
            ~> (lc[c] /= "started" \/ opc = <<"ended">>)

RecordsFreed == (opc = <<"ended">>) ~> (\A c \in Calls : rec[c] = "none")

OwnerEnds == halted ~> (opc = <<"ended">>)
=============================================================================
