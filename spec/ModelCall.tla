----------------------------- MODULE ModelCall -----------------------------
(* MODEL's call (src/lib/net/model.f: START, SERVE, AWAIT, CLOSE, CANCEL,  *)
(* REQUEST-STOP, ACCOUNT, the resend TIER-RESEND! turns on and the rest   *)
(* and resend RESEND-WAIT! turns on), over                                 *)
(* CURL's record and request machines                                      *)
(* (src/lib/net/curl.f: NEXT-ARM, NEXT-READY?, NEXT-TAKE, ABANDON,         *)
(* CLEANUP, OWNER-END, SERVICE-ONE) and TASK's halt and wait               *)
(* (src/lib/task.f: HALT, PAUSE, HALT-UNWIND, DEFER-ENTER, DEFER-LEAVE,    *)
(* WAIT, WAKE). One owner task owns every call, the CURL loop serves their *)
(* records, and another task may CANCEL or REQUEST-STOP any call at any    *)
(* time. Whether the owner follows the header's caller rule (model.f: it   *)
(* CANCELs, then AWAITs, its busy calls in a finally) is fixed at Init.    *)
(* Each step of the owner is one hold of the call's gate, or a step no     *)
(* other task can tell apart from one: another task's CANCEL or stop may   *)
(* come between any two.                                                   *)
EXTENDS Naturals

CONSTANTS Calls, MaxChunks, MaxStarts, MaxRests

(* tier is the call's TIER-MACHINE state, kept in its receipt; tierOut,    *)
(* whether its landed send ended E-TIER with TIER-SENSE's other conditions *)
(* holding (TIER-RESEND! on, a tier named, its record kept). A send that   *)
(* ended E-TIER without them steps as one that did not. resend is the      *)
(* call's RESEND-MACHINE, kept in its receipt: step, its state, which      *)
(* RESEND-MOVE stores only once the transition's action has returned; out, *)
(* whether its landed send is in REST-OUT?'s class: its land machine's     *)
(* ending, chosen as it lands (Take, Dispatch), is failed, or unsent after *)
(* its key read failed, its fault is in RESENDS?'s class with RESEND-WAIT! *)
(* on, and its record is kept. It holds through the rest the send begins,  *)
(* as the receipt does, until the next send is claimed; rests, how many    *)
(* times this start has rested. A send outside the class                   *)
(* steps as out = FALSE. due, whether the call is resting: RECEIPT-DUE is  *)
(* not 0. Time is not modelled: a resting call may be found due at any     *)
(* SERVE or AWAIT, and a park while a call rests may time out.             *)
(* What the caller sees of a call's start: ecan, whether its outcome is    *)
(* E-CANCELLED; said, the answer of the last CANCEL since START; crecs,    *)
(* how many cancelled accounting records ACCOUNT has written; sent,        *)
(* whether the request of its last send was dispatched (LEDGER-DISPATCHED, *)
(* which a record needs); hcut, whether a halt cut its recorder, which may *)
(* or may not have written the record.                                     *)
VARIABLES
    lc, rest, cancelled, stopped, transport, span, starts,
    opc, hints, halted, deferred, unwound,
    rec, req, left, held, ends,
    wroteFreed, wokeEnded, rule, tier, tierOut, resend, due,
    ecan, said, crecs, sent, hcut

vars == <<lc, rest, cancelled, stopped, transport, span, starts,
          opc, hints, halted, deferred, unwound,
          rec, req, left, held, ends, wroteFreed, wokeEnded, rule,
          tier, tierOut, resend, due, ecan, said, crecs, sent, hcut>>

Lifecycles == {"idle", "started", "landed", "closed"}
Records == {"none", "claimed", "running", "cancel", "done",
            "abandoned", "leaving", "left"}
Requests == {"none", "waiting", "chunk", "response", "failed"}
Answered == {"chunk", "response", "failed"}
Tiers == {"preferred", "standard", "done"}
Steps == {"sending", "begun", "resting", "resuming", "done"}
Rests == [step : Steps, out : BOOLEAN, rests : 0..MaxRests]
Sending == [step |-> "sending", out |-> FALSE, rests |-> 0]
(* MaxRests bounds the rests of one start as MaxStarts bounds the starts: *)
(* past it every send lands outside RESENDS?'s class.                      *)
CanRest(c) == resend[c].rests < MaxRests
(* BEGIN-REST and the move to begun. *)
Rested(c) ==
    [resend EXCEPT ![c].step = "begun", ![c].rests = @ + 1]
Settled(c) ==
    [resend EXCEPT ![c].step = "done", ![c].out = FALSE]
(* A landed send is to be sent again by the tier machine (TIER-OUT? at    *)
(* preferred, while the resend machine sends) or by a rest (REST-OUT?     *)
(* while it sends or resumes: a resend landed at once or refused, its     *)
(* move on not yet stored). A landed call resting came only from          *)
(* LAND-CANCELLED, whose E-CANCELLED outcome CANCEL-LOCKED's own check    *)
(* marks (ecan), so RESEND-NEXT? answers false there.                     *)
TierClass(c) == tier[c] = "preferred" /\ tierOut[c]
RestClass(c) ==
    resend[c].step \in {"sending", "resuming"} /\ resend[c].out
(* Who runs a call's send: START, or SERVE's or AWAIT's RECEIVE-STARTED. *)
Froms == {"start", "serve", "await"}
(* Who began it: START (new), the tier machine (tier) or a rest (rest).   *)
Kinds == {"new", "tier", "rest"}
Takers == {"serve", "await"}
(* The owner's places. A place naming a call is a step of that call's      *)
(* send: dispatch, SEND-BEGUN's dispatch of a send of a kind; landcut, a   *)
(* cut send SEND-AGAIN began, landed by END-RUN, LAND-CUT not yet run;     *)
(* resent, a rest's resend landed at once or its begin faulted, the move   *)
(* to sending not yet stored; sense, SENT-SENSE of a send landed at once;  *)
(* publish, BEGIN-REST chosen and not yet published; begun, REST-SENSE of  *)
(* a rest just published; cut, LAND-CANCELLED or a refused RESEND,         *)
(* E-CANCELLED stored as the outcome under the gate, past the gate and     *)
(* before CANCEL-RECORD; account, ACCOUNT of a landed receive or of        *)
(* CANCEL-RECORD's cancelled record.                                       *)
Moves == {"landcut", "resent", "sense", "publish", "begun", "cut"}
Places == {<<"free">>, <<"parked">>, <<"halting">>, <<"unwinding">>,
           <<"finally">>, <<"ended">>}
          \cup ({"await"} \X Calls)
          \cup ({"dispatch"} \X Calls \X Froms \X Kinds)
          \cup (Moves \X Calls \X Froms)
          \cup ({"abandon", "cleanup"} \X Calls \X Takers)
          \cup ({"account"} \X Calls \X Takers \X {"receive", "cancel"})

(* The owner is inside a step of c's send. *)
Acting(c) ==
    opc \in ({"dispatch"} \X {c} \X Froms \X Kinds)
            \cup (Moves \X {c} \X Froms)
            \cup ({"account"} \X {c} \X Takers \X {"receive", "cancel"})

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
    /\ tier \in [Calls -> Tiers]
    /\ tierOut \in [Calls -> BOOLEAN]
    /\ resend \in [Calls -> Rests]
    /\ due \in [Calls -> BOOLEAN]
    /\ ecan \in [Calls -> BOOLEAN]
    /\ said \in [Calls -> {"none", "yes", "no"}]
    /\ crecs \in [Calls -> 0..2]
    /\ sent \in [Calls -> BOOLEAN]
    /\ hcut \in [Calls -> BOOLEAN]

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
    /\ tier = [c \in Calls |-> "preferred"]
    /\ tierOut = [c \in Calls |-> FALSE]
    /\ resend = [c \in Calls |-> Sending]
    /\ due = [c \in Calls |-> FALSE]
    /\ ecan = [c \in Calls |-> FALSE]
    /\ said = [c \in Calls |-> "none"]
    /\ crecs = [c \in Calls |-> 0]
    /\ sent = [c \in Calls |-> FALSE]
    /\ hcut = [c \in Calls |-> FALSE]

seen == <<ecan, said, crecs, sent, hcut>>

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
(*   CHAT-CHUNK, EMBED-CHUNK: a buffer's, span's or memory's fault while   *)
(*   it copies the chunk.                                                  *)
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
(* call (END-RUN), E-CANCELLED if a CANCEL cut it (LAND-CUT). The record   *)
(* is the caller's, so CLEANUP's OWNED-INDEX finds it and ABANDON gives it *)
(* up: a DONE one is freed at once (LEAVE-DONE, REC-RELEASE) and the call  *)
(* lands; any other is marked LEAVING and the owner waits in ABANDON       *)
(* (CleanupStep) before the call lands.                                    *)
FaultAs(c, t) ==
    /\ transport' = [transport EXCEPT ![c] = FALSE]
    /\ IF t[2] = "done"
       THEN /\ rec' = [rec EXCEPT ![c] = "none"]
            /\ req' = [req EXCEPT ![c] = "none"]
            /\ held' = [held EXCEPT ![c] = FALSE]
            /\ lc' = [lc EXCEPT ![c] = "landed"]
            /\ ecan' = [ecan EXCEPT ![c] = cancelled[c]]
       ELSE /\ rec' = [rec EXCEPT ![c] = "leaving"]
            /\ req' = [req EXCEPT ![c] = t[1]]
            /\ UNCHANGED <<held, lc, ecan>>
    /\ UNCHANGED <<rest, tierOut, resend>>

TakeFault(c) == \E t \in Throws(c) : FaultAs(c, t)

(* NEXT-TAKE of an answered request (STREAM-RESULT-TAKE), as SERVE and     *)
(* AWAIT do it: a chunk is fed and the next read armed at once, or it      *)
(* spoils the reply (DROP: CURL:CANCEL, rest dropping, re-arm); an end     *)
(* releases the record, ends the transfer (END-TRANSFER) and lands; or the *)
(* receive throws (TakeFault). END-RUN reads the cancel: a cut call lands  *)
(* E-CANCELLED (cut, or cut-failed after a failure the send met first,     *)
(* whose line it keeps). Only an end the provider sent may land E-TIER,    *)
(* and only when the call was not cut: whether it does is chosen here, and *)
(* so are the land machine's ending, kept in the receipt (served, refused  *)
(* or failed; E-TIER is failed), and whether a failed send lands a fault   *)
(* RESEND-WAIT! rests. A served or refused send's journal put may fail;    *)
(* its fault becomes the outcome, which REST-OUT? does not rest and        *)
(* TIER-OUT? does not send again (t => e = "failed").                      *)
Take(c) ==
    \/ /\ req[c] = "chunk"
       /\ \/ /\ req' = [req EXCEPT ![c] = ArmFor(c, rec[c])]
             /\ UNCHANGED <<rec, rest>>
          \/ /\ rest[c] = "reading"
             /\ rest' = [rest EXCEPT ![c] = "dropping"]
             /\ CancelMark(c)
             /\ req' = [req EXCEPT ![c] = ArmFor(c, rec'[c])]
       /\ UNCHANGED <<lc, transport, held, tierOut, resend, ecan>>
    \/ /\ req[c] \in {"response", "failed"}
       /\ rec' = [rec EXCEPT ![c] = "none"]
       /\ req' = [req EXCEPT ![c] = "none"]
       /\ transport' = [transport EXCEPT ![c] = FALSE]
       /\ lc' = [lc EXCEPT ![c] = "landed"]
       /\ ecan' = [ecan EXCEPT ![c] = cancelled[c]]
       /\ \E t \in IF cancelled[c] THEN {FALSE} ELSE BOOLEAN,
             e \in IF cancelled[c] THEN {"cut", "cut-failed"}
                   ELSE {"served", "refused", "failed"} :
          \E o \in IF e = "failed" /\ CanRest(c) THEN BOOLEAN ELSE {FALSE} :
             /\ t => e = "failed"
             /\ ~(t /\ o)
             /\ tierOut' = [tierOut EXCEPT ![c] = t]
             /\ resend' = [resend EXCEPT ![c].out = o]
       /\ UNCHANGED <<rest, held>>
    \/ TakeFault(c)

(* CANCEL-LOCKED under the gate (model.f): a started call is cut. A landed *)
(* call is marked when its send, not yet sensed, is marked already         *)
(* (UNSENSED?: sending, or resuming for a resend landed at once) or is to  *)
(* be sent again (RESEND-NEXT?), or when its outcome is E-CANCELLED; any   *)
(* other has its mark cleared. A mark is kept until the send is sensed, so *)
(* the call lands E-CANCELLED. The mark is CANCEL's answer.                *)
CancelBody(c) ==
    IF lc[c] = "started"
    THEN /\ cancelled' = [cancelled EXCEPT ![c] = TRUE]
         /\ IF transport[c]
            THEN /\ transport' = [transport EXCEPT ![c] = FALSE]
                 /\ CancelMark(c)
            ELSE UNCHANGED <<transport, rec>>
    ELSE /\ cancelled' = [cancelled EXCEPT ![c] =
                /\ lc[c] = "landed"
                /\ \/ ecan[c]
                   \/ /\ resend[c].step \in {"sending", "resuming"}
                      /\ cancelled[c]
                   \/ /\ resend[c].step = "sending"
                      /\ TierClass(c)
                   \/ RestClass(c)]
         /\ UNCHANGED <<transport, rec>>

(* CANCEL's answer: the mark CANCEL-LOCKED left. *)
Answer(c) == said' = [said EXCEPT ![c] = IF cancelled'[c] THEN "yes" ELSE "no"]

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

(* TASK:HALT-DUE?, HALT-UNWIND's test: a halt whose unwind has not begun  *)
(* (TCB-HALT-PENDING? reads TCB.STOP), outside every DEFER-ENTER.         *)
Pending == halted /\ ~unwound /\ ~deferred

(* The end of SERVE's RECEIVE-STARTED (model.f) after DEFER-LEAVE,         *)
(* ACCOUNT, TIER-STEP and RESEND-STEP: 0 TASK:HALT-UNWIND starts the       *)
(* unwind on a pending halt.                                               *)
ServeEnd ==
    IF Pending
    THEN /\ unwound' = TRUE
         /\ opc' = Unwind
    ELSE /\ opc' = <<"free">>
         /\ UNCHANGED unwound

(* How START, SERVE or AWAIT goes on once the call's machines have moved,  *)
(* the call now l: START returns; SERVE's arm ends (ServeEnd); AWAIT's     *)
(* starts the unwind on a pending halt, else receives a started call again *)
(* or makes a landed one idle and returns.                                 *)
Tail(c, f, l) ==
    IF f = "await" /\ ~Pending /\ l = "landed"
    THEN /\ lc' = [lc EXCEPT ![c] = "idle"]
         /\ opc' = Back
         /\ UNCHANGED unwound
    ELSE /\ lc' = [lc EXCEPT ![c] = l]
         /\ IF f = "start"
            THEN /\ opc' = <<"free">>
                 /\ UNCHANGED unwound
            ELSE IF f = "serve" \/ Pending
                 THEN ServeEnd
                 ELSE /\ opc' = <<"await", c>>
                      /\ UNCHANGED unwound

(* NEXT-TAKE's TASK:PAUSE after ABANDON runs inside RECEIVE-STARTED's      *)
(* DEFER-ENTER, so it only yields: the take answers failed CURLE_ABORTED,  *)
(* TAKE-WAIT or DISCARD-WAIT answers done and the receive lands the call;  *)
(* SERVE goes on to ACCOUNT, AWAIT to its DEFER-LEAVE (AwaitStep). Unless  *)
(* cut, or a chat whose stream had ended, it lands failed (E-TRANSPORT),   *)
(* which REST-OUT? may rest.                                               *)
PauseAfter(c, f) ==
    /\ lc' = [lc EXCEPT ![c] = "landed"]
    /\ ecan' = [ecan EXCEPT ![c] = cancelled[c]]
    /\ transport' = [transport EXCEPT ![c] = FALSE]
    /\ \E o \in IF ~cancelled[c] /\ CanRest(c) THEN BOOLEAN ELSE {FALSE} :
          resend' = [resend EXCEPT ![c].out = o]
    /\ opc' = IF f = "serve" THEN <<"account", c, "serve", "receive">>
               ELSE <<"await", c>>
    /\ UNCHANGED unwound

(* NEXT-TAKE's ABANDON on a pending halt, for SERVE or AWAIT (f), as       *)
(* CLEANUP's in FaultAs: a DONE record's call lands at once                *)
(* (PauseAfter), any other's after the owner's wait (AbandonStep).         *)
HaltAbandon(c, f) ==
    IF rec[c] = "done"
    THEN /\ rec' = [rec EXCEPT ![c] = "none"]
         /\ req' = [req EXCEPT ![c] = "none"]
         /\ held' = [held EXCEPT ![c] = FALSE]
         /\ PauseAfter(c, f)
    ELSE /\ rec' = [rec EXCEPT ![c] = "leaving"]
         /\ opc' = <<"abandon", c, f>>
         /\ UNCHANGED <<lc, transport, unwound, req, held, ecan, resend>>

(* CANCEL-RECORD of a send a stop or CANCEL kept from being sent again,    *)
(* its outcome E-CANCELLED already, stored by the gate hold that decided   *)
(* so: ACCOUNT runs the cancelled record of a dispatched request, the      *)
(* resend machine keeping its state until it returns. With none dispatched *)
(* ACCOUNT records nothing and does not wait, and the machine moves to     *)
(* done.                                                                   *)
CancelLand(c, f) ==
    /\ IF sent[c]
       THEN /\ opc' = <<"account", c, f, "cancel">>
            /\ UNCHANGED <<lc, unwound, resend>>
       ELSE /\ resend' = Settled(c)
            /\ Tail(c, f, "landed")

-----------------------------------------------------------------------------
(* The owner *)

Free == opc = <<"free">> /\ ~halted

(* START: START-LEDGER sets the tier and resend machines to preferred and  *)
(* sending and clears the ledger, then BEGIN-CALL under the gate (a        *)
(* stopped call is E-CANCELLED) and SEND-BEGUN.                            *)
Start(c) ==
    /\ Free
    /\ lc[c] = "idle"
    /\ starts[c] < MaxStarts
    /\ ~stopped[c]
    /\ lc' = [lc EXCEPT ![c] = "started"]
    /\ cancelled' = [cancelled EXCEPT ![c] = FALSE]
    /\ starts' = [starts EXCEPT ![c] = @ + 1]
    /\ tier' = [tier EXCEPT ![c] = "preferred"]
    /\ tierOut' = [tierOut EXCEPT ![c] = FALSE]
    /\ resend' = [resend EXCEPT ![c] = Sending]
    /\ ecan' = [ecan EXCEPT ![c] = FALSE]
    /\ said' = [said EXCEPT ![c] = "none"]
    /\ crecs' = [crecs EXCEPT ![c] = 0]
    /\ sent' = [sent EXCEPT ![c] = FALSE]
    /\ hcut' = [hcut EXCEPT ![c] = FALSE]
    /\ opc' = <<"dispatch", c, "start", "new">>
    /\ UNCHANGED <<rest, stopped, transport, span, hints, halted, deferred,
                   unwound, rec, req, left, held, ends, wroteFreed, wokeEnded,
                   rule, due>>

(* START-LOCKED's claim of a transfer, which DISPATCHED marks sent: its    *)
(* reply's end and length are chosen here.                                 *)
Claim(c) ==
    /\ rec' = [rec EXCEPT ![c] = "claimed"]
    /\ transport' = [transport EXCEPT ![c] = TRUE]
    /\ req' = [req EXCEPT ![c] = "waiting"]
    /\ rest' = [rest EXCEPT ![c] = "reading"]
    /\ held' = [held EXCEPT ![c] = FALSE]
    /\ sent' = [sent EXCEPT ![c] = TRUE]
    /\ \E e \in {"ok", "fail"}, n \in 0..MaxChunks:
          /\ ends' = [ends EXCEPT ![c] = e]
          /\ left' = [left EXCEPT ![c] = n]

(* SEND-BEGUN's SEND-MACHINE, to END-RUN under the gate. A cancel since    *)
(* the send began lands the call E-CANCELLED with no transfer: START-      *)
(* LOCKED's E-CANCELLED, or a failure the send met before it (cut-unsent), *)
(* whose line it keeps; nothing was dispatched, so ACCOUNT records nothing *)
(* and SENT-SENSE settles it. For a send SEND-AGAIN began, END-RUN lands   *)
(* it here, never out, since a cut ending does not rest (ENDING-RESTS?),   *)
(* and LAND-CUT stores E-CANCELLED in a later step (LandCut), the resend   *)
(* machine still resuming. Else a transfer is claimed; or the send lands   *)
(* at once with nothing dispatched, its ending chosen here: a journal hit  *)
(* (replayed or replay-failed), or unsent after a failed journal lookup or *)
(* a dispatch fault; key, whether its key read failed (KEY-FAILED?). It is *)
(* out only when unsent after its key read failed in RESENDS?'s class      *)
(* (REST-OUT?: a SECRET code rests only from the key read). A rest's move  *)
(* to sending is stored once SEND-AGAIN returns: at once with a transfer   *)
(* out, else at resent.                                                    *)
Dispatch(c) ==
    \E f \in Froms, k \in Kinds :
        /\ opc = <<"dispatch", c, f, k>>
        /\ IF cancelled[c] /\ k = "rest"
           THEN /\ lc' = [lc EXCEPT ![c] = "landed"]
                /\ resend' = [resend EXCEPT ![c].out = FALSE]
                /\ opc' = <<"landcut", c, f>>
                /\ UNCHANGED <<rest, transport, rec, req, left, held, ends,
                               sent, ecan, unwound>>
           ELSE IF cancelled[c]
           THEN /\ ecan' = [ecan EXCEPT ![c] = TRUE]
                /\ resend' = Settled(c)
                /\ Tail(c, f, "landed")
                /\ UNCHANGED <<rest, transport, rec, req, left, held, ends,
                               sent>>
           ELSE \/ /\ Claim(c)
                   /\ resend' = [resend EXCEPT ![c].step = "sending",
                                                ![c].out = FALSE]
                   /\ Tail(c, f, "started")
                   /\ UNCHANGED ecan
                \/ \E e \in {"replayed", "replay-failed", "unsent"},
                      key \in BOOLEAN :
                   \E o \in IF e = "unsent" /\ key /\ CanRest(c)
                            THEN BOOLEAN ELSE {FALSE} :
                      /\ lc' = [lc EXCEPT ![c] = "landed"]
                      /\ resend' = [resend EXCEPT ![c].out = o]
                      /\ opc' = IF k = "rest" THEN <<"resent", c, f>>
                                ELSE <<"sense", c, f>>
                      /\ UNCHANGED <<rest, transport, rec, req, left, held,
                                     ends, sent, ecan, unwound>>
        /\ UNCHANGED <<cancelled, stopped, span, starts, hints, halted,
                       deferred, wroteFreed, wokeEnded, rule, tier, tierOut,
                       due, said, crecs, hcut>>

(* LAND-CUT of a send SEND-AGAIN began, out of the gate, then SEND-AGAIN's *)
(* return, RESEND-MOVE and SENT-SENSE, which settles a send E-CANCELLED    *)
(* already.                                                                *)
LandCut(c) ==
    \E f \in Froms :
        /\ opc = <<"landcut", c, f>>
        /\ ecan' = [ecan EXCEPT ![c] = TRUE]
        /\ resend' = Settled(c)
        /\ Tail(c, f, "landed")
        /\ UNCHANGED <<rest, cancelled, stopped, transport, span, starts,
                       hints, halted, deferred, rec, req, left, held, ends,
                       wroteFreed, wokeEnded, rule, tier, tierOut, due, said,
                       crecs, sent, hcut>>

(* SEND-AGAIN has returned, or the begin faulted: RESEND-MOVE stores       *)
(* sending.                                                                *)
Resent(c) ==
    \E f \in Froms :
        /\ opc = <<"resent", c, f>>
        /\ resend' = [resend EXCEPT ![c].step = "sending"]
        /\ opc' = <<"sense", c, f>>
        /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                       hints, halted, deferred, unwound, rec, req, left, held,
                       ends, wroteFreed, wokeEnded, rule, tier, tierOut, due,
                       seen>>

(* RESEND-STEP's SENT-SENSE of a send that landed at once, under the gate: *)
(* a send a CANCEL marked (CANCEL-KEPT?) lands E-CANCELLED (LAND-CANCELLED,*)
(* CANCEL-RECORD); one in REST-OUT?'s class rests (BEGIN-REST, Publish);   *)
(* any other is settled, its line written (REPORT-SENT).                   *)
Sense(c) ==
    \E f \in Froms :
        /\ opc = <<"sense", c, f>>
        /\ IF cancelled[c] /\ ~ecan[c]
           THEN /\ ecan' = [ecan EXCEPT ![c] = TRUE]
                /\ CancelLand(c, f)
           ELSE IF resend[c].out
                THEN /\ opc' = <<"publish", c, f>>
                     /\ UNCHANGED <<lc, unwound, resend, ecan>>
                ELSE /\ resend' = Settled(c)
                     /\ Tail(c, f, "landed")
                     /\ UNCHANGED ecan
        /\ UNCHANGED <<rest, cancelled, stopped, transport, span, starts,
                       hints, halted, deferred, rec, req, left, held, ends,
                       wroteFreed, wokeEnded, rule, tier, tierOut, due, said,
                       crecs, sent, hcut>>

(* BEGIN-REST under the gate: the line naming the wait, then the call      *)
(* started again with no transfer and its due time set; the move to begun  *)
(* is stored once it returns.                                              *)
Publish(c) ==
    \E f \in Froms :
        /\ opc = <<"publish", c, f>>
        /\ lc' = [lc EXCEPT ![c] = "started"]
        /\ due' = [due EXCEPT ![c] = TRUE]
        /\ resend' = Rested(c)
        /\ opc' = <<"begun", c, f>>
        /\ UNCHANGED <<rest, cancelled, stopped, transport, span, starts,
                       hints, halted, deferred, unwound, rec, req, left, held,
                       ends, wroteFreed, wokeEnded, rule, tier, tierOut, seen>>

(* REST-SENSE of a rest just begun, in the same RESEND-STEP: a stop or     *)
(* CANCEL since the send was sensed lands the call (LAND-CANCELLED:        *)
(* REST-END-LOCKED and the outcome E-CANCELLED under the gate, then Cut);  *)
(* else the call rests, early or due, until the next SERVE or AWAIT step.  *)
BegunSense(c) ==
    \E f \in Froms :
        /\ opc = <<"begun", c, f>>
        /\ IF cancelled[c] \/ stopped[c]
           THEN /\ lc' = [lc EXCEPT ![c] = "landed"]
                /\ due' = [due EXCEPT ![c] = FALSE]
                /\ ecan' = [ecan EXCEPT ![c] = TRUE]
                /\ opc' = <<"cut", c, f>>
                /\ UNCHANGED <<resend, unwound>>
           ELSE /\ resend' = [resend EXCEPT ![c].step = "resting"]
                /\ Tail(c, f, "started")
                /\ UNCHANGED <<due, ecan>>
        /\ UNCHANGED <<rest, cancelled, stopped, transport, span, starts,
                       hints, halted, deferred, rec, req, left, held, ends,
                       wroteFreed, wokeEnded, rule, tier, tierOut, said, crecs,
                       sent, hcut>>

(* CANCEL-RECORD after REST-END-LOCKED, out of the gate: LAND-CANCELLED's  *)
(* of a rest, or the resuming row's when RESEND's RESUME-LOCKED refused    *)
(* the begin. The hold that ended the rest stored the outcome.             *)
Cut(c) ==
    \E f \in Froms :
        /\ opc = <<"cut", c, f>>
        /\ CancelLand(c, f)
        /\ UNCHANGED <<rest, cancelled, stopped, transport, span, starts,
                       hints, halted, deferred, rec, req, left, held, ends,
                       wroteFreed, wokeEnded, rule, tier, tierOut, due, ecan,
                       said, crecs, sent, hcut>>

(* SERVE's started arm (RECEIVE-STARTED) as one step: the receive only     *)
(* polls, deferred; a take that throws may wait in CLEANUP's ABANDON; then *)
(* DEFER-LEAVE, and a landed call's ACCOUNT, TIER-STEP and RESEND-STEP     *)
(* before HALT-UNWIND. A halted owner's poll of an answered read may throw *)
(* before the halt check (FaultAs), else gives the record up before        *)
(* NEXT-TAKE takes it, waiting in ABANDON while the loop holds it          *)
(* (HaltAbandon), and the call lands; its poll of a read with nothing      *)
(* leaves the call started and the owner unwinding.                        *)
(* REQUIRE-STARTER passes: the one owner starts every call.                *)
(* OWNED-INDEX's refusal of a left owner's record never fires: no TCB is   *)
(* reused.                                                                 *)
Serve(c) ==
    /\ opc = <<"free">>
    /\ lc[c] = "started"
    /\ \/ /\ \/ /\ req[c] \in Answered
                /\ IF halted /\ ~unwound
                      THEN FaultAs(c, <<req[c], rec[c]>>)
                      ELSE Take(c)
             \/ /\ halted
                /\ req[c] = "waiting"
                /\ UNCHANGED <<lc, rest, transport, rec, req, held, tierOut,
                               resend, ecan>>
          /\ IF rec'[c] = "leaving"
             THEN /\ opc' = <<"cleanup", c, "serve">>
                  /\ UNCHANGED unwound
             ELSE IF lc'[c] = "landed"
                  THEN /\ opc' = <<"account", c, "serve", "receive">>
                       /\ UNCHANGED unwound
                  ELSE ServeEnd
       \/ /\ halted /\ ~unwound
          /\ req[c] \in Answered
          /\ HaltAbandon(c, "serve")
          /\ UNCHANGED <<rest, tierOut>>
    /\ UNCHANGED <<cancelled, stopped, span, starts, hints, halted,
                   deferred, left, ends, wroteFreed, wokeEnded, rule, tier,
                   due, said, crecs, sent, hcut>>

(* The caller rule's park, straight after a round in which every SERVE     *)
(* answered false: no started call's read is answered, and no resting one  *)
(* is stopped or cancelled, which that round's SERVE would have landed. A  *)
(* stop or CANCEL after the round posts the park (RestWake), which then    *)
(* returns at once, so parking only before it loses no ending.             *)
RestWaits(c) ==
    due[c] => ~cancelled[c] /\ ~stopped[c]

WaitAny ==
    /\ Free
    /\ \E c \in Calls : lc[c] = "started"
    /\ \A c \in Calls :
          lc[c] = "started" => req[c] \notin Answered /\ RestWaits(c)
    /\ opc' = <<"parked">>
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   hints, halted, deferred, unwound, rec, req, left, held,
                   ends, wroteFreed, wokeEnded, rule, tier, tierOut, resend,
                   due, seen>>

(* TASK:WAIT returns by taking one hint, whoever posted it. With a call    *)
(* resting, the caller rule parks no longer than its RESEND-DUE: a timed   *)
(* park (TASK:WAIT-MS) may also return with no hint.                       *)
Resume ==
    /\ opc = <<"parked">>
    /\ \/ /\ hints > 0
          /\ hints' = hints - 1
       \/ /\ \E c \in Calls : due[c]
          /\ UNCHANGED hints
    /\ opc' = <<"free">>
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   halted, deferred, unwound, rec, req, left, held, ends,
                   wroteFreed, wokeEnded, rule, tier, tierOut, resend, due,
                   seen>>

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
                   wroteFreed, wokeEnded, rule, tier, tierOut, resend, due,
                   seen>>

(* AWAIT's started arm, deferred, as NEXT-TAKE's loop runs it: a halt      *)
(* whose unwind has not begun (TASK:HALT-PENDING?) gives the record up     *)
(* (HaltAbandon) before an answered read is taken, though the receive may  *)
(* throw before that check (FaultAs); else an answered read is taken; else *)
(* TASK:WAIT takes a hint, the halt's post among them. In the unwind it    *)
(* waits as an unhalted owner does. Once the call lands, DEFER-LEAVE, then *)
(* ACCOUNT, TIER-STEP and RESEND-STEP (PutDone).                           *)
AwaitStep(c) ==
    /\ opc = <<"await", c>>
    /\ \/ /\ lc[c] = "landed"
          /\ opc' = <<"account", c, "await", "receive">>
          /\ UNCHANGED <<lc, rest, transport, hints, unwound, rec, req,
                         held, tierOut, resend, ecan>>
       \/ /\ lc[c] = "started"
          /\ req[c] \in Answered
          /\ IF halted /\ ~unwound
                THEN FaultAs(c, <<req[c], rec[c]>>)
                ELSE Take(c)
          /\ opc' = IF rec'[c] = "leaving" THEN <<"cleanup", c, "await">> ELSE opc
          /\ UNCHANGED <<hints, unwound>>
       \/ /\ lc[c] = "started"
          /\ req[c] = "waiting"
          /\ hints > 0
          /\ hints' = hints - 1
          /\ UNCHANGED <<lc, rest, transport, opc, unwound, rec, req, held,
                         tierOut, resend, ecan>>
       \/ /\ lc[c] = "started"
          /\ halted /\ ~unwound
          /\ req[c] \in {"waiting"} \cup Answered
          /\ HaltAbandon(c, "await")
          /\ UNCHANGED <<rest, hints, tierOut>>
    /\ UNCHANGED <<cancelled, stopped, span, starts, halted, deferred, left,
                   ends, wroteFreed, wokeEnded, rule, tier, due, said, crecs,
                   sent, hcut>>

(* ABANDON's wait: the span is the owner's again only once the loop has   *)
(* let go (LEFT); the owner frees the record itself.                       *)
AbandonStep(c) ==
    \E f \in Takers :
        /\ opc = <<"abandon", c, f>>
        /\ \/ /\ rec[c] = "left"
              /\ rec' = [rec EXCEPT ![c] = "none"]
              /\ req' = [req EXCEPT ![c] = "none"]
              /\ held' = [held EXCEPT ![c] = FALSE]
              /\ PauseAfter(c, f)
              /\ UNCHANGED hints
           \/ /\ rec[c] /= "left"
              /\ hints > 0
              /\ hints' = hints - 1
              /\ UNCHANGED <<lc, transport, opc, unwound, rec, req, held,
                             ecan, resend>>
        /\ UNCHANGED <<rest, cancelled, stopped, span, starts, halted,
                       deferred, left, ends, wroteFreed, wokeEnded, rule,
                       tier, tierOut, due, said, crecs, sent, hcut>>

(* CLEANUP's ABANDON in LAND-FAULT's SETTLE-SENSE: the same wait; once the *)
(* owner has freed the record, END-RUN lands the call, then ACCOUNT runs.  *)
CleanupStep(c) ==
    \E f \in Takers :
        /\ opc = <<"cleanup", c, f>>
        /\ \/ /\ rec[c] = "left"
              /\ rec' = [rec EXCEPT ![c] = "none"]
              /\ req' = [req EXCEPT ![c] = "none"]
              /\ held' = [held EXCEPT ![c] = FALSE]
              /\ lc' = [lc EXCEPT ![c] = "landed"]
              /\ ecan' = [ecan EXCEPT ![c] = cancelled[c]]
              /\ opc' = <<"account", c, f, "receive">>
              /\ UNCHANGED <<hints, unwound>>
           \/ /\ rec[c] /= "left"
              /\ hints > 0
              /\ hints' = hints - 1
              /\ UNCHANGED <<lc, opc, unwound, rec, req, held, ecan>>
        /\ UNCHANGED <<rest, cancelled, stopped, transport, span, starts,
                       halted, deferred, left, ends, wroteFreed, wokeEnded,
                       rule, tier, tierOut, resend, due, said, crecs, sent,
                       hcut>>

(* ACCOUNT (model.f), run after RECEIVE-STARTED's DEFER-LEAVE or by        *)
(* CANCEL-RECORD: the recorder and journal put, which may wait, end; a     *)
(* cancelled record is one of an E-CANCELLED call whose request was        *)
(* dispatched. Then the machine whose action ran it moves on.              *)
(* After CANCEL-RECORD the resend machine moves to done.                   *)
(* After a receive, the recorder may fail, which takes the send out of     *)
(* TIER-OUT?'s and REST-OUT?'s class, then TIER-STEP: from preferred, a    *)
(* send that ended E-TIER, with no halt of the owner due there             *)
(* (TIER-SENSE's TASK:HALT-DUE?, Pending), resumes: TIER-STANDARD begins   *)
(* the call again under the gate (RESEND-LOCKED, with BEGIN-RESET's        *)
(* resets, its cancelled flag cleared), and on begun SEND-STANDARD         *)
(* dispatches it                                                           *)
(* (Dispatch) and the machine is standard; MaxStarts does not count it. A  *)
(* stop or CANCEL since the send landed is refused, RESEND-LOCKED storing  *)
(* E-CANCELLED as the outcome, and CANCEL-RECORD lands the call; a fault   *)
(* beginning it is its outcome, and RESEND-STEP's SENT-SENSE settles it.   *)
(* Any other send, one halted included, steps the tier machine to done,    *)
(* then SENT-SENSE reads the gate: a                                       *)
(* send a CANCEL marked lands E-CANCELLED (LAND-CANCELLED, CANCEL-RECORD), *)
(* even when the recorder's fault took it out of the class; one in         *)
(* REST-OUT?'s class rests (Publish), keeping the tier its send went out   *)
(* at (SEND-AGAIN restores TIER-STEP's move from RECEIPT-SENT); any other  *)
(* is settled. A halted send's halt is served as the owner goes on (Tail). *)
PutDone(c) ==
    \E f \in Takers, k \in {"receive", "cancel"} :
        /\ opc = <<"account", c, f, k>>
        /\ crecs' = [crecs EXCEPT ![c] =
                         @ + (IF ecan[c] /\ sent[c] THEN 1 ELSE 0)]
        /\ IF k = "cancel"
           THEN /\ resend' = Settled(c)
                /\ Tail(c, f, "landed")
                /\ UNCHANGED <<cancelled, tier, tierOut, ecan, sent>>
           ELSE \E fault \in IF tierOut[c] \/ resend[c].out
                             THEN BOOLEAN ELSE {FALSE} :
                IF tier[c] = "preferred" /\ tierOut[c] /\ ~fault /\ ~Pending
                THEN /\ tierOut' = [tierOut EXCEPT ![c] = FALSE]
                     /\ IF cancelled[c] \/ stopped[c]
                        THEN /\ tier' = [tier EXCEPT ![c] = "done"]
                             /\ ecan' = [ecan EXCEPT ![c] = TRUE]
                             /\ CancelLand(c, f)
                             /\ UNCHANGED <<cancelled, sent>>
                        ELSE \/ /\ tier' = [tier EXCEPT ![c] = "standard"]
                                /\ lc' = [lc EXCEPT ![c] = "started"]
                                /\ cancelled' = [cancelled EXCEPT ![c] = FALSE]
                                /\ sent' = [sent EXCEPT ![c] = FALSE]
                                /\ opc' = <<"dispatch", c, f, "tier">>
                                /\ UNCHANGED <<unwound, resend, ecan>>
                             \/ /\ tier' = [tier EXCEPT ![c] = "done"]
                                /\ resend' = Settled(c)
                                /\ Tail(c, f, "landed")
                                /\ UNCHANGED <<cancelled, sent, ecan>>
                ELSE /\ tierOut' = [tierOut EXCEPT ![c] = FALSE]
                     /\ UNCHANGED <<cancelled, sent>>
                     /\ IF cancelled[c] /\ ~ecan[c]
                        THEN /\ tier' = [tier EXCEPT ![c] = "done"]
                             /\ ecan' = [ecan EXCEPT ![c] = TRUE]
                             /\ CancelLand(c, f)
                        ELSE IF resend[c].out /\ ~fault
                        THEN /\ opc' = <<"publish", c, f>>
                             /\ UNCHANGED <<tier, lc, unwound, resend, ecan>>
                        ELSE /\ tier' = [tier EXCEPT ![c] = "done"]
                             /\ resend' = Settled(c)
                             /\ Tail(c, f, "landed")
                             /\ UNCHANGED ecan
        /\ UNCHANGED <<rest, stopped, transport, span, starts, hints, halted,
                       deferred, rec, req, left, held, ends, wroteFreed,
                       wokeEnded, rule, due, said, hcut>>

(* A halt served while the recorder or put waits: the task unwinds there,  *)
(* and LANDING-END leaves the call landed E-CANCELLED (LAND-CUT); the      *)
(* record may or may not have been written. The machine whose action ran   *)
(* ACCOUNT keeps its state. ACCOUNT waits only on a dispatched request's.  *)
PutCut(c) ==
    \E f \in Takers, k \in {"receive", "cancel"} :
        /\ opc = <<"account", c, f, k>>
        /\ Pending
        /\ sent[c]
        /\ ecan' = [ecan EXCEPT ![c] = TRUE]
        /\ hcut' = [hcut EXCEPT ![c] = TRUE]
        /\ unwound' = TRUE
        /\ opc' = Unwind
        /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                       hints, halted, deferred, rec, req, left, held, ends,
                       wroteFreed, wokeEnded, rule, tier, tierOut, resend, due,
                       said, crecs, sent>>

(* RESEND-STEP of a resting call (REST-SENSE), in SERVE or AWAIT, which    *)
(* receive nothing for it. A stop or CANCEL lands it (LAND-CANCELLED:      *)
(* REST-END-LOCKED and the outcome E-CANCELLED under the gate, then Cut).  *)
(* An early one stays: SERVE's arm ends; AWAIT's starts the unwind on a    *)
(* pending halt, leaving the call resting, else parks in REST-PARK, which  *)
(* reads the stop first and returns on a hint or at the due time: a hint   *)
(* taken here, or the next RestAwait. A due one's SERVE-HALT serves a      *)
(* pending halt first, leaving the call resting; else the machine resumes  *)
(* and RESEND begins the resend under the gate: RESUME-LOCKED ends the     *)
(* rest, and a stop or CANCEL since REST-SENSE is refused, E-CANCELLED     *)
(* stored as the outcome there (Cut, the machine resuming), one no other   *)
(* task tells from a stop or CANCEL REST-SENSE read; a fault beginning it  *)
(* lands the call, the fault its outcome, outside REST-OUT?'s class        *)
(* (Resent); else SEND-AGAIN dispatches it (Dispatch). The machine stays   *)
(* resuming until the row's action returns.                                *)
RestStep(c, f) ==
    /\ lc[c] = "started"
    /\ due[c]
    /\ resend[c].step = "resting"
    /\ \/ /\ cancelled[c] \/ stopped[c]
          /\ lc' = [lc EXCEPT ![c] = "landed"]
          /\ due' = [due EXCEPT ![c] = FALSE]
          /\ ecan' = [ecan EXCEPT ![c] = TRUE]
          /\ \E s \in IF Pending THEN {"resting"} ELSE {"resting", "resuming"} :
                resend' = [resend EXCEPT ![c].step = s]
          /\ opc' = <<"cut", c, f>>
          /\ UNCHANGED <<unwound, hints, sent>>
       \/ /\ ~cancelled[c] /\ ~stopped[c]
          /\ IF f = "serve"
             THEN /\ ServeEnd
                  /\ UNCHANGED hints
             ELSE IF Pending
                  THEN /\ unwound' = TRUE
                       /\ opc' = Unwind
                       /\ UNCHANGED hints
                  ELSE /\ hints > 0
                       /\ hints' = hints - 1
                       /\ UNCHANGED <<opc, unwound>>
          /\ UNCHANGED <<lc, due, sent, ecan, resend>>
       \/ /\ ~cancelled[c] /\ ~stopped[c]
          /\ IF Pending
             THEN /\ unwound' = TRUE
                  /\ opc' = Unwind
                  /\ UNCHANGED <<lc, due, sent, resend>>
             ELSE /\ due' = [due EXCEPT ![c] = FALSE]
                  /\ UNCHANGED unwound
                  /\ \/ /\ resend' = [resend EXCEPT ![c].step = "resuming"]
                        /\ sent' = [sent EXCEPT ![c] = FALSE]
                        /\ opc' = <<"dispatch", c, f, "rest">>
                        /\ UNCHANGED lc
                     \/ /\ resend' = [resend EXCEPT ![c].step = "resuming",
                                                  ![c].out = FALSE]
                        /\ lc' = [lc EXCEPT ![c] = "landed"]
                        /\ opc' = <<"resent", c, f>>
                        /\ UNCHANGED sent
          /\ UNCHANGED <<hints, ecan>>
    /\ UNCHANGED <<rest, cancelled, stopped, transport, span, starts, halted,
                   deferred, rec, req, left, held, ends, wroteFreed,
                   wokeEnded, rule, tier, tierOut, said, crecs, hcut>>

RestServe(c) == opc = <<"free">> /\ RestStep(c, "serve")

RestAwait(c) == opc = <<"await", c>> /\ RestStep(c, "await")

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
                   wroteFreed, wokeEnded, rule, tier, tierOut, resend, due,
                   seen>>

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
                   wroteFreed, wokeEnded, rule, tier, tierOut, resend, due,
                   seen>>

(* The serve loop finds the halt: DEFER-ENTER, then cancel and await      *)
(* every call.                                                             *)
HaltEnter ==
    /\ opc = <<"free">>
    /\ halted
    /\ deferred' = TRUE
    /\ opc' = <<"halting">>
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   hints, halted, unwound, rec, req, left, held, ends,
                   wroteFreed, wokeEnded, rule, tier, tierOut, resend, due,
                   seen>>

(* One step of the owner's cleanup of its calls, in halt handling or in   *)
(* the unwind: cancel a started call, await a cancelled one, await a       *)
(* landed one.                                                             *)
Settle(c) ==
    \/ /\ lc[c] = "started"
       /\ ~cancelled[c] \/ transport[c]
       /\ CancelBody(c)
       /\ Answer(c)
       /\ UNCHANGED <<lc, opc>>
    \/ /\ lc[c] = "started"
       /\ cancelled[c] /\ ~transport[c]
       /\ opc' = <<"await", c>>
       /\ UNCHANGED <<lc, cancelled, transport, rec, said>>
    \/ /\ lc[c] = "landed"
       /\ lc' = [lc EXCEPT ![c] = "idle"]
       /\ UNCHANGED <<opc, cancelled, transport, rec, said>>

Unsettled == \E c \in Calls : lc[c] \in {"started", "landed"}

HaltStep ==
    /\ opc = <<"halting">>
    /\ \/ /\ \E c \in Calls : Settle(c)
          /\ UNCHANGED <<deferred, unwound>>
       \/ /\ ~Unsettled
          /\ deferred' = FALSE                \* DEFER-LEAVE
          /\ unwound' = TRUE                  \* PAUSE starts the unwind
          /\ opc' = Unwind
          /\ UNCHANGED <<lc, cancelled, transport, rec, said>>
    /\ UNCHANGED <<rest, stopped, span, starts, hints, halted, req, left, held,
                   ends, wroteFreed, wokeEnded, rule, tier, tierOut, resend,
                   due, ecan, crecs, sent, hcut>>

(* The unwind's cleanups of an owner that does not follow the rule (their *)
(* PAUSE only yields): settle every call, close every call, then          *)
(* OWNER-END.                                                              *)
UnwindStep ==
    /\ opc = <<"unwinding">>
    /\ \/ /\ \E c \in Calls : Settle(c)
          /\ UNCHANGED span
       \/ /\ ~Unsettled
          /\ \E c \in Calls : CloseBody(c)
          /\ UNCHANGED <<opc, cancelled, transport, rec, said>>
       \/ /\ \A c \in Calls : lc[c] = "closed"
          /\ OwnerEnd
          /\ UNCHANGED <<lc, cancelled, transport, span, said>>
    /\ UNCHANGED <<rest, stopped, starts, hints, halted, deferred, unwound,
                   req, left, held, ends, wroteFreed, wokeEnded, rule, tier,
                   tierOut, resend, due, ecan, crecs, sent, hcut>>

(* The unwind of an owner that follows the rule runs its finally: for each *)
(* call still started, CANCEL (CANCEL-LOCKED), then AWAIT, catching        *)
(* AWAIT's fault (E-CANCELLED for a call it just cancelled), whose started *)
(* arm (AwaitStep) runs inside the unwind; then OWNER-END.                 *)
FinallyStep ==
    /\ opc = <<"finally">>
    /\ \/ \E c \in Calls :
             /\ lc[c] = "started"
             /\ CancelBody(c)
             /\ Answer(c)
             /\ opc' = <<"await", c>>
       \/ /\ \A c \in Calls : lc[c] /= "started"
          /\ OwnerEnd
          /\ UNCHANGED <<cancelled, transport, said>>
    /\ UNCHANGED <<lc, rest, stopped, span, starts, hints, halted, deferred,
                   unwound, req, left, held, ends, wroteFreed, wokeEnded,
                   rule, tier, tierOut, resend, due, ecan, crecs, sent, hcut>>

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
                   wroteFreed, wokeEnded, rule, tier, tierOut, resend, due,
                   seen>>

(* WAKE-STARTER: a CANCEL or stop of a resting call (its RESEND-DUE read  *)
(* under the gate) posts the owner's park; an ended owner's TASK:WAKE is   *)
(* E-TASK-STATE, which it drops. Only the first CANCEL or stop of a rest  *)
(* posts here, and the owner's own CANCEL in its cleanups does not: a      *)
(* later post is a spare hint that only ends a park early, and leaving it  *)
(* out keeps the hint count bounded.                                       *)
RestWake(c) ==
    IF due[c] /\ ~cancelled[c] /\ opc /= <<"ended">>
    THEN hints' = hints + 1
    ELSE UNCHANGED hints

(* CANCEL: E-STATE on a closed call (its arena is gone). *)
Cancel(c) ==
    /\ lc[c] /= "closed"
    /\ CancelBody(c)
    /\ Answer(c)
    /\ RestWake(c)
    /\ UNCHANGED <<lc, rest, stopped, span, starts, opc, halted,
                   deferred, unwound, req, left, held, ends, wroteFreed,
                   wokeEnded, rule, tier, tierOut, resend, due, ecan, crecs,
                   sent, hcut>>

Stop(c) ==
    /\ lc[c] /= "closed"
    /\ stopped' = [stopped EXCEPT ![c] = TRUE]
    /\ CancelBody(c)
    /\ RestWake(c)
    /\ UNCHANGED <<lc, rest, span, starts, opc, halted, deferred,
                   unwound, req, left, held, ends, wroteFreed, wokeEnded,
                   rule, tier, tierOut, resend, due, seen>>

-----------------------------------------------------------------------------
(* The CURL loop: SERVICE-ONE and the finish of a transfer *)

Add(c) ==
    /\ rec[c] = "claimed"
    /\ rec' = [rec EXCEPT ![c] = "running"]
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   opc, hints, halted, deferred, unwound, req, left, held,
                   ends, wroteFreed, wokeEnded, rule, tier, tierOut, resend,
                   due, seen>>

Produce(c) ==
    /\ rec[c] = "running"
    /\ left[c] > 0
    /\ ~held[c]
    /\ held' = [held EXCEPT ![c] = TRUE]
    /\ left' = [left EXCEPT ![c] = @ - 1]
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   opc, hints, halted, deferred, unwound, rec, req, ends,
                   wroteFreed, wokeEnded, rule, tier, tierOut, resend, due,
                   seen>>

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
                   opc, halted, deferred, unwound, rec, left, ends, rule, tier,
                   tierOut, resend, due, seen>>

(* STREAM-FINISH, then SETTLE: a streamed record is not woken here. *)
Finish(c) ==
    /\ rec[c] = "running"
    /\ (left[c] = 0 /\ ~held[c]) \/ ends[c] = "fail"
    /\ rec' = [rec EXCEPT ![c] = "done"]
    /\ UNCHANGED <<lc, rest, cancelled, stopped, transport, span, starts,
                   opc, hints, halted, deferred, unwound, req, left, held,
                   ends, wroteFreed, wokeEnded, rule, tier, tierOut, resend,
                   due, seen>>

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
                   opc, halted, deferred, unwound, left, wroteFreed, rule,
                   tier, tierOut, resend, due, seen>>

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
                   rule, tier, tierOut, resend, due, seen>>

-----------------------------------------------------------------------------

(* The owner has ended for good: TLC reads no deadlock here. *)
Ended ==
    /\ opc = <<"ended">>
    /\ UNCHANGED vars

OwnerStep ==
    \/ \E c \in Calls :
          \/ Start(c) \/ Dispatch(c) \/ Resent(c) \/ Sense(c) \/ Publish(c)
          \/ LandCut(c) \/ BegunSense(c) \/ Cut(c) \/ Serve(c) \/ Await(c)
          \/ AwaitStep(c)
          \/ AbandonStep(c) \/ CleanupStep(c) \/ PutDone(c) \/ PutCut(c)
          \/ Close(c) \/ RestServe(c) \/ RestAwait(c)
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
          /\ WF_vars(Start(c)) /\ WF_vars(Dispatch(c)) /\ WF_vars(Resent(c))
          /\ WF_vars(LandCut(c)) /\ WF_vars(Sense(c)) /\ WF_vars(Publish(c))
          /\ WF_vars(BegunSense(c)) /\ WF_vars(Cut(c)) /\ WF_vars(Serve(c))
          /\ WF_vars(Await(c)) /\ WF_vars(AwaitStep(c))
          /\ WF_vars(AbandonStep(c)) /\ WF_vars(CleanupStep(c))
          /\ WF_vars(PutDone(c) /\ ~Pending) /\ WF_vars(PutCut(c))
          /\ WF_vars(Close(c))
          /\ WF_vars(Add(c)) /\ WF_vars(Produce(c)) /\ WF_vars(Deliver(c))
          /\ WF_vars(Finish(c)) /\ WF_vars(CancelRun(c))
          /\ WF_vars(LeaveRun(c))
          /\ WF_vars(RestServe(c)) /\ WF_vars(RestAwait(c))
    /\ WF_vars(WaitAny) /\ WF_vars(Resume) /\ WF_vars(HaltEnter)
    /\ WF_vars(HaltStep) /\ WF_vars(UnwindStep) /\ WF_vars(FinallyStep)

Spec == Init /\ [][Next]_vars /\ Fairness

-----------------------------------------------------------------------------
(* Properties *)

SpanSafe == ~wroteFreed

Unwinding == unwound \/ opc = <<"ended">>

OneRequest ==
    \A c \in Calls :
        (/\ lc[c] = "started" /\ ~Unwinding /\ ~due[c]
         /\ opc \notin {"dispatch"} \X {c} \X Froms \X Kinds
         /\ opc \notin {"cleanup"} \X {c} \X Takers)
            => /\ rec[c] /= "none"
               /\ req[c] \in {"waiting"} \cup Answered

NoWakeAfterEnd == ~wokeEnded

(* A resting call is started with no transfer, record or read, its resend *)
(* machine begun or resting. The machine holds begun, resting or resuming *)
(* with the rest ended only inside the action that ends it, or after a    *)
(* halt cut that action's ACCOUNT.                                         *)
RestIdle ==
    \A c \in Calls :
        /\ due[c]
               => /\ lc[c] = "started"
                  /\ ~transport[c]
                  /\ rec[c] = "none"
                  /\ req[c] = "none"
                  /\ resend[c].step \in {"begun", "resting"}
        /\ (resend[c].step \in {"begun", "resting", "resuming"} /\ ~due[c])
               => Acting(c) \/ Unwinding
        /\ resend[c].step = "begun" => Acting(c) \/ Unwinding

(* A closed call has no transfer out and no record in use. *)
ClosedFreed ==
    \A c \in Calls : lc[c] = "closed" => ~transport[c] /\ rec[c] = "none"

(* An owner that follows the rule ends with no call started. *)
RuleLands ==
    (rule /\ opc = <<"ended">>) => \A c \in Calls : lc[c] /= "started"

(* What AWAIT returns or throws, as it makes the call idle: a CANCEL that  *)
(* answered true ends the call E-CANCELLED; an E-CANCELLED call had its    *)
(* last CANCEL answer true, was stopped with no CANCEL answered, or had a  *)
(* halt cut its recorder; and unless a halt cut it, an E-CANCELLED call    *)
(* whose request was dispatched has exactly one cancelled record, any      *)
(* other none.                                                             *)
Verdict(c) ==
    /\ said[c] = "yes" => ecan[c]
    /\ ecan[c] =>
          said[c] = "yes" \/ (said[c] = "none" /\ stopped[c]) \/ hcut[c]
    /\ ~hcut[c] => crecs[c] = IF ecan[c] /\ sent[c] THEN 1 ELSE 0
    /\ crecs[c] <= 1

CancelEnds ==
    [][\A c \in Calls :
          (lc[c] /= "idle" /\ lc'[c] = "idle") => Verdict(c)']_vars

(* The owner begins a send of c: START's, the tier machine's or a rest's.  *)
Sends(c) == opc \in {"dispatch"} \X {c} \X Froms \X Kinds

(* Once a CANCEL has run since the call's START (said is no longer none),  *)
(* the owner begins no send of it; a later START begins a new call.        *)
NoSendAfterCancel ==
    [][\A c \in Calls :
          (said[c] /= "none" /\ said'[c] /= "none" /\ ~Sends(c))
              => ~Sends(c)']_vars

(* A CANCEL that answered true, or a cut of a started call, ends the call  *)
(* E-CANCELLED unless its owner ends first.                                *)
CancelLands ==
    \A c \in Calls :
        (said[c] = "yes" \/ (lc[c] = "started" /\ cancelled[c]))
            ~> (ecan[c] \/ opc = <<"ended">>)

(* A stop or CANCEL during a rest lands the call E-CANCELLED, unless its   *)
(* owner ends first.                                                       *)
RestStops ==
    \A c \in Calls :
        (due[c] /\ (cancelled[c] \/ stopped[c]))
            ~> (ecan[c] \/ opc = <<"ended">>)

(* No send the tier machine or a rest starts begins while a halt of the    *)
(* owner is due (Pending): SERVE-HALT serves it before a rest's resend,    *)
(* and the tier machine, finding it due, sends no more.                    *)
NoHaltedSend ==
    [][\A c \in Calls :
          (~Sends(c) /\ Sends(c)' /\ opc'[4] \in {"tier", "rest"})
              => ~Pending]_vars

RecordsFreed == (opc = <<"ended">>) ~> (\A c \in Calls : rec[c] = "none")

OwnerEnds == halted ~> (opc = <<"ended">>)
=============================================================================
