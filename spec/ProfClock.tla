----------------------------- MODULE ProfClock -----------------------------
(* The profiler clock's handshake (src/habu/prof.f): the SIGALRM handler   *)
(* EMIT-PROF against the public control words prof-on, prof-off,           *)
(* prof-reset and prof-report (BPROF-ON, BPROF-OFF, BPROF-RESET and        *)
(* C-PROF-REPORT-CALL, over C-PROF-CLOCK-HOLD and C-PROF-CLOCK-RESUME).    *)
(* Two threads, main and a task, each run any control word at any time,    *)
(* and either takes a SIGALRM at any step of its own code. A handler masks *)
(* SIGALRM and runs to completion before the code it interrupted goes on,  *)
(* while the other thread's steps interleave with it.                      *)
(*                                                                         *)
(* Each step below is one atomic step of an interleaving. PROF-ARMED's     *)
(* stores are ARMv8 STLR and its loads LDAR; PROF-IN's +1 and -1 are       *)
(* atomic adds with acquire and release (LDADDAL) and the controller's     *)
(* read of it an LDAR. PROF-CTL is a ticket lock: the claim's              *)
(* fetch-and-add on CtlNext and its spin's loads of CtlServe are acquire,  *)
(* the release's store of CtlServe + 1 an STLR. ARMv8 orders a             *)
(* load-acquire after every earlier store-release of the same thread (the  *)
(* Arm ARM, B2.3, "Load-Acquire, Store-Release": RCsc), so the handler's   *)
(* add to PROF-IN then LDAR PROF-ARMED and the controller's STLR           *)
(* PROF-ARMED then LDAR PROF-IN are not reordered, and these accesses are  *)
(* sequentially consistent with each other. ARN-OWNER and ARN-OWNER-TID    *)
(* are plain aligned 64-bit accesses, single-copy atomic, written before   *)
(* the STLR of ARMED-RUNS that a handler's LDAR pairs with: one atomic     *)
(* read or write each is sound.                                            *)
EXTENDS Naturals, FiniteSets

(* The protocol is Wait = TRUE, OwnerFirst = FALSE. Two mutants must each  *)
(* break a property: Wait = FALSE drops the controller's wait on PROF-IN;  *)
(* OwnerFirst = TRUE tests the owner before counting the handler in, and   *)
(* counts only the owner's.                                                *)
(* CtlNext and CtlServe are naturals in the code; here they count modulo   *)
(* NTickets. A ticket lock compares them for equality only, and at most    *)
(* one ticket per thread is outstanding, so any NTickets above the number  *)
(* of threads orders the claims as the naturals would.                     *)
CONSTANTS Wait, OwnerFirst, NTickets

ASSUME NTickets > 2

Tickets == 0..(NTickets - 1)

Threads == {"main", "task"}
Words == {"on", "off", "reset", "report"}
None == "none"

(* armed: PROF-ARMED; flag: PROF-IN, the handlers inside; next and serve:  *)
(* PROF-CTL's CtlNext and CtlServe; tk: each thread's ticket; owner:       *)
(* ARN-OWNER, the thread the handler's owner test compares with; tid:      *)
(* ARN-OWNER-TID, the thread a forward signals, cleared by prof-off's      *)
(* C-PROF-OWNER-RELEASE.                                                   *)
(* pc and word: each thread's place in its control word; held: the word    *)
(* held a running clock; hpc: its handler's place; pend: a SIGALRM pending *)
(* on it; live: the task has not ended.                                    *)
VARIABLES armed, flag, next, serve, tk, owner, tid, pc, word, held, hpc,
          pend, live

vars == <<armed, flag, next, serve, tk, owner, tid, pc, word, held, hpc,
          pend, live>>

lock == <<next, serve, tk>>

Places == {"idle", "claim", "spin", "leave", "wait", "work", "arm", "release",
           "ended"}
HPlaces == {"none", "entry", "test", "fwd", "in", "load", "work", "out"}

TypeOK ==
    /\ armed \in {"Off", "Runs", "Held"}
    /\ flag \in 0..Cardinality(Threads)
    /\ next \in Tickets
    /\ serve \in Tickets
    /\ tk \in [Threads -> Tickets]
    /\ owner \in Threads \cup {None}
    /\ tid \in Threads \cup {None}
    /\ pc \in [Threads -> Places]
    /\ word \in [Threads -> Words \cup {None}]
    /\ held \in [Threads -> BOOLEAN]
    /\ hpc \in [Threads -> HPlaces]
    /\ pend \in [Threads -> BOOLEAN]
    /\ live \in [Threads -> BOOLEAN]

Init ==
    /\ armed = "Off"
    /\ flag = 0
    /\ next = 0
    /\ serve = 0
    /\ tk = [t \in Threads |-> 0]
    /\ owner = None
    /\ tid = None
    /\ pc = [t \in Threads |-> "idle"]
    /\ word = [t \in Threads |-> None]
    /\ held = [t \in Threads |-> FALSE]
    /\ hpc = [t \in Threads |-> "none"]
    /\ pend = [t \in Threads |-> FALSE]
    /\ live = [t \in Threads |-> TRUE]

-----------------------------------------------------------------------------
(* The kernel *)

(* setitimer's SIGALRM, or a late one after the timer stopped, on any live *)
(* thread. Standard signals do not queue: one pending at most.             *)
Post(t) ==
    /\ live[t] /\ ~pend[t]
    /\ pend' = [pend EXCEPT ![t] = TRUE]
    /\ UNCHANGED <<armed, flag, lock, owner, tid, pc, word, held, hpc, live>>

(* Delivery: SIGALRM is masked while its handler runs. *)
Take(t) ==
    /\ live[t] /\ pend[t] /\ hpc[t] = "none"
    /\ pend' = [pend EXCEPT ![t] = FALSE]
    /\ hpc' = [hpc EXCEPT ![t] = "entry"]
    /\ UNCHANGED <<armed, flag, lock, owner, tid, pc, word, held, live>>

-----------------------------------------------------------------------------
(* The handler, EMIT-PROF *)

HGo(t, p) == hpc' = [hpc EXCEPT ![t] = p]

HKeep ==
    UNCHANGED <<armed, flag, lock, owner, tid, pc, word, held, pend, live>>

(* LDAR PROF-ARMED: not ARMED-RUNS returns, nothing touched. *)
Entry(t) ==
    /\ hpc[t] = "entry"
    /\ HGo(t, IF armed /= "Runs" THEN "none"
              ELSE IF OwnerFirst THEN "test" ELSE "in")
    /\ HKeep

(* PROF-IN + 1, on every thread. *)
In(t) ==
    /\ hpc[t] = "in"
    /\ flag' = flag + 1
    /\ HGo(t, "load")
    /\ UNCHANGED <<armed, lock, owner, tid, pc, word, held, pend, live>>

(* LDAR PROF-ARMED again: not ARMED-RUNS leaves at once. *)
Load(t) ==
    /\ hpc[t] = "load"
    /\ HGo(t, IF armed /= "Runs" THEN "out"
              ELSE IF OwnerFirst THEN "work" ELSE "test")
    /\ HKeep

(* The owner test: ARN-OWNER against this thread. *)
Test(t) ==
    /\ hpc[t] = "test"
    /\ HGo(t, IF owner /= t THEN "fwd"
              ELSE IF OwnerFirst THEN "in" ELSE "work")
    /\ HKeep

(* C-PROF-FORWARD, still counted: SIGALRM to ARN-OWNER-TID, dropped when   *)
(* it names this thread or none, lost when that thread has ended.          *)
Fwd(t) ==
    /\ hpc[t] = "fwd"
    /\ HGo(t, IF OwnerFirst THEN "none" ELSE "out")
    /\ pend' = IF tid \in Threads /\ tid /= t /\ live[tid]
               THEN [pend EXCEPT ![tid] = TRUE] ELSE pend
    /\ UNCHANGED <<armed, flag, lock, owner, tid, pc, word, held, live>>

(* The sample: counters, ARN-LAST, the index. *)
HWork(t) ==
    /\ hpc[t] = "work"
    /\ HGo(t, "out")
    /\ HKeep

(* PROF-IN - 1, then sigreturn. *)
Out(t) ==
    /\ hpc[t] = "out"
    /\ flag' = flag - 1
    /\ HGo(t, "none")
    /\ UNCHANGED <<armed, lock, owner, tid, pc, word, held, pend, live>>

Handler(t) ==
    Take(t) \/ Entry(t) \/ Test(t) \/ Fwd(t) \/ In(t) \/ Load(t)
    \/ HWork(t) \/ Out(t)

-----------------------------------------------------------------------------
(* A control word. A thread's own code steps only while no handler runs on *)
(* it.                                                                     *)

Free(t) == live[t] /\ hpc[t] = "none"

Go(t, p) == pc' = [pc EXCEPT ![t] = p]

CKeep == UNCHANGED <<armed, flag, lock, owner, tid, word, held, hpc, pend,
                     live>>

Begin(t, w) ==
    /\ Free(t) /\ pc[t] = "idle"
    /\ Go(t, "claim")
    /\ word' = [word EXCEPT ![t] = w]
    /\ UNCHANGED <<armed, flag, lock, owner, tid, held, hpc, pend, live>>

(* The claim: fetch-and-add on CtlNext gives this thread its ticket. *)
Claim(t) ==
    /\ Free(t) /\ pc[t] = "claim"
    /\ tk' = [tk EXCEPT ![t] = next]
    /\ next' = (next + 1) % NTickets
    /\ Go(t, "spin")
    /\ UNCHANGED <<armed, flag, serve, owner, tid, word, held, hpc, pend,
                   live>>

(* Spin until CtlServe equals the ticket. *)
Spin(t) ==
    /\ Free(t) /\ pc[t] = "spin" /\ serve = tk[t]
    /\ Go(t, "leave")
    /\ CKeep

(* STLR of the new state: prof-on and prof-off store ARMED-OFF (0);        *)
(* C-PROF-CLOCK-HOLD stores ARMED-HELD over a running clock and leaves any *)
(* other alone. Only a controller writes PROF-ARMED, so its read here and  *)
(* the store are one step. Only a clock that was running is waited for.    *)
Leave(t) ==
    /\ Free(t) /\ pc[t] = "leave"
    /\ LET hold == word[t] \in {"reset", "report"}
           left == armed = "Runs"
       IN /\ armed' = IF hold THEN (IF left THEN "Held" ELSE armed)
                      ELSE "Off"
          /\ held' = [held EXCEPT ![t] = hold /\ left]
          /\ Go(t, IF left /\ Wait THEN "wait" ELSE "work")
    /\ UNCHANGED <<flag, lock, owner, tid, word, hpc, pend, live>>

(* Spin until LDAR PROF-IN reads 0: no handler inside. *)
WaitIn(t) ==
    /\ Free(t) /\ pc[t] = "wait" /\ flag = 0
    /\ Go(t, "work")
    /\ CKeep

(* The word's work: prof-on's C-PROF-OWNER sets ARN-OWNER and              *)
(* ARN-OWNER-TID to this thread, prof-off's C-PROF-OWNER-RELEASE clears    *)
(* ARN-OWNER-TID; the clears, the index and the report touch nothing       *)
(* modelled. prof-on arms; prof-reset and prof-report resume only a clock  *)
(* they held.                                                              *)
CWork(t) ==
    /\ Free(t) /\ pc[t] = "work"
    /\ owner' = IF word[t] = "on" THEN t ELSE owner
    /\ tid' = CASE word[t] = "on" -> t
                [] word[t] = "off" -> None
                [] OTHER -> tid
    /\ Go(t, IF word[t] = "on" \/ held[t] THEN "arm" ELSE "release")
    /\ UNCHANGED <<armed, flag, lock, word, held, hpc, pend, live>>

(* C-PROF-ARM: STLR ARMED-RUNS, after every write the handler reads. *)
Arm(t) ==
    /\ Free(t) /\ pc[t] = "arm"
    /\ armed' = "Runs"
    /\ Go(t, "release")
    /\ UNCHANGED <<flag, lock, owner, tid, word, held, hpc, pend, live>>

(* The release: CtlServe + 1, by the holder alone. *)
Release(t) ==
    /\ Free(t) /\ pc[t] = "release"
    /\ serve' = (serve + 1) % NTickets
    /\ Go(t, "idle")
    /\ word' = [word EXCEPT ![t] = None]
    /\ held' = [held EXCEPT ![t] = FALSE]
    /\ UNCHANGED <<armed, flag, next, tk, owner, tid, hpc, pend, live>>

(* The task ends between words; its pending signal goes with it. ARN-OWNER *)
(* and ARN-OWNER-TID may still name it.                                    *)
End(t) ==
    /\ t = "task" /\ Free(t) /\ pc[t] = "idle"
    /\ live' = [live EXCEPT ![t] = FALSE]
    /\ pend' = [pend EXCEPT ![t] = FALSE]
    /\ Go(t, "ended")
    /\ UNCHANGED <<armed, flag, lock, owner, tid, word, held, hpc>>

(* A started word's steps: the ones fairness drives. *)
Code(t) ==
    Claim(t) \/ Spin(t) \/ Leave(t) \/ WaitIn(t) \/ CWork(t) \/ Arm(t)
    \/ Release(t)

-----------------------------------------------------------------------------

Next ==
    \E t \in Threads :
        \/ Post(t) \/ Handler(t) \/ Code(t) \/ End(t)
        \/ \E w \in Words : Begin(t, w)

(* Weak fairness of each handler step and of delivery (Take). A thread's   *)
(* own code is strongly fair: SIGALRM may arrive again as each handler     *)
(* returns, so its code is enabled only between handlers, and the clock's  *)
(* interval, far longer than a handler, lets it run there. Starting a      *)
(* word, a signal's arrival and the task's end are not fair.               *)
Fairness ==
    \A t \in Threads : WF_vars(Handler(t)) /\ SF_vars(Code(t))

Spec == Init /\ [][Next]_vars /\ Fairness

-----------------------------------------------------------------------------
(* Properties *)

HandlerExcludesControl ==
    ~((\E t \in Threads : hpc[t] = "work")
      /\ (\E u \in Threads : pc[u] = "work"))

ControlExcludesControl == Cardinality({t \in Threads : pc[t] = "work"}) <= 1

OnlyOwnerCounts == \A t \in Threads : hpc[t] = "work" => owner = t

(* A forward reads ARN-OWNER-TID only while neither prof-on nor prof-off   *)
(* is changing it.                                                         *)
ForwardHandleStable ==
    \A t \in Threads :
        hpc[t] = "fwd"
            => \A u \in Threads :
                   ~(pc[u] = "work" /\ word[u] \in {"on", "off"})

(* Every started word ends; a controller on the owner thread is never left *)
(* spinning on its own handler.                                            *)
ControlEnds ==
    \A t \in Threads : (pc[t] \notin {"idle", "ended"}) ~> (pc[t] = "idle")

(* A delivery pending on the live owner of a running clock runs a handler  *)
(* there.                                                                  *)
ForwardReaches ==
    \A t \in Threads :
        (pend[t] /\ live[t] /\ owner = t /\ armed = "Runs")
            ~> (hpc[t] = "entry" \/ ~live[t])
=============================================================================
