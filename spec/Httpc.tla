------------------------------- MODULE Httpc -------------------------------
(* One HTTPC conn (src/lib/net/httpc.f) as its LIFECYCLE table runs it,    *)
(* with its caller and its peer. Each action named after a line of the     *)
(* table is that line: its sense's answer, its action, and on reaching a   *)
(* FINAL row that row's REST, which sets what the record holds (held). The *)
(* caller SENDs from any rest, a request marked malformed or oversize or   *)
(* not; NEXT resumes `sent` as `heading` and `partway` as `reading`; CLOSE *)
(* hangs up at any rest. The peer may close the connection, or send bytes  *)
(* nobody asked for, at any time. Every wait ends, with bytes or at a      *)
(* deadline, so every sense that reads or writes may answer `failed`; a    *)
(* wait with no deadline on a silent peer lasts as long, which the model   *)
(* leaves out. A probe may answer `stale` and a greeting `failed` at any   *)
(* time; a probe answers `quiet` only with one socket the peer has neither *)
(* closed nor written to. A read may take bytes from the room, which can   *)
(* hold the rest of an answer after a CLOSE, so the model does not tie a   *)
(* head or a piece to a socket. A body by the close ends only once the     *)
(* peer has closed. Bounds: Addrs addresses a name resolves to, Pieces     *)
(* pieces a body has, Interims interim heads before an answer's head.      *)
EXTENDS Naturals

Addrs == 2
Pieces == 2
Interims == 2

Rests == {"sent", "partway", "idle", "closed", "down"}
Runs == {"starting", "probing", "resolving", "connecting", "greeting",
         "sending", "heading", "reading", "settling"}

(* at: the machine's state. held: what the record holds, which only a REST *)
(* and CLOSE set. socks: the sockets the record holds, so one dialed while *)
(* another is held counts two. fresh: this SEND's run dialed the socket.   *)
(* probed: this SEND's run probed the held socket quiet. answer: what the  *)
(* current connection carries: "none" before its first request, "owed"     *)
(* from a write until its answer is read to its end, then "whole" for an   *)
(* answer framed by status, length or chunks, "unframed" for one read to   *)
(* the close. fin, stray: the peer has closed, has sent bytes past an      *)
(* answer. wrote: writes of this SEND's request. marked: REQUEST or        *)
(* HEADER+ marked this request. framed, persist, left: the answer head's   *)
(* framing, whether it allows reuse (HTTP/1.1, no `Connection: close`),    *)
(* and the body's pieces not read yet. interims: interim heads read since  *)
(* SEND. addrs: addresses left to dial.                                    *)
VARIABLES at, held, socks, fresh, probed, answer, fin, stray, wrote,
          marked, framed, persist, left, interims, addrs
vars == <<at, held, socks, fresh, probed, answer, fin, stray, wrote,
          marked, framed, persist, left, interims, addrs>>

Init ==
    /\ at = "down"
    /\ held = "none"
    /\ socks = 0
    /\ fresh = FALSE
    /\ probed = FALSE
    /\ answer = "none"
    /\ fin = FALSE
    /\ stray = FALSE
    /\ wrote = 0
    /\ marked = FALSE
    /\ framed = FALSE
    /\ persist = FALSE
    /\ left = 0
    /\ interims = 0
    /\ addrs = 0

(* A step from s to t; reaching a FINAL row runs its REST.                 *)
Go(s, t) ==
    /\ at = s
    /\ at' = t
    /\ held' = CASE t \in {"sent", "partway"} -> "busy"
                 [] t = "idle" -> "idle"
                 [] t \in {"closed", "down"} -> "none"
                 [] OTHER -> held

(* HANG-UP closes the socket the record names, if any.                     *)
HangUp == socks' = IF socks > 0 THEN socks - 1 ELSE 0

(* ---- The caller                                                         *)

Send ==
    /\ at \in Rests
    /\ at' = "starting"
    /\ marked' \in BOOLEAN
    /\ wrote' = 0
    /\ fresh' = FALSE
    /\ probed' = FALSE
    /\ interims' = 0
    /\ UNCHANGED <<held, socks, answer, fin, stray, framed, persist, left,
                   addrs>>

Resume ==
    /\ at \in {"sent", "partway"}
    /\ at' = IF at = "sent" THEN "heading" ELSE "reading"
    /\ UNCHANGED <<held, socks, fresh, probed, answer, fin, stray, wrote,
                   marked, framed, persist, left, interims, addrs>>

Close ==
    /\ at \in Rests
    /\ HangUp
    /\ held' = "none"
    /\ UNCHANGED <<at, fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims, addrs>>

(* ---- The peer                                                           *)

PeerCloses ==
    /\ socks > 0
    /\ ~fin
    /\ fin' = TRUE
    /\ UNCHANGED <<at, held, socks, fresh, probed, answer, stray, wrote,
                   marked, framed, persist, left, interims, addrs>>

PeerWrites ==
    /\ socks > 0
    /\ ~stray
    /\ stray' = TRUE
    /\ UNCHANGED <<at, held, socks, fresh, probed, answer, fin, wrote,
                   marked, framed, persist, left, interims, addrs>>

(* ---- The lines: starting, INTAKE                                        *)

Refused ==
    /\ Go("starting", "down")
    /\ marked
    /\ HangUp
    /\ UNCHANGED <<fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims, addrs>>

Unheld ==
    /\ Go("starting", "resolving")
    /\ ~marked
    /\ held = "none"
    /\ UNCHANGED <<socks, fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims, addrs>>

Kept ==
    /\ Go("starting", "probing")
    /\ ~marked
    /\ held = "idle"
    /\ UNCHANGED <<socks, fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims, addrs>>

Busy ==
    /\ Go("starting", "resolving")
    /\ ~marked
    /\ held = "busy"
    /\ HangUp
    /\ UNCHANGED <<fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims, addrs>>

(* ---- probing, PROBE                                                     *)

Quiet ==
    /\ Go("probing", "sending")
    /\ socks = 1
    /\ ~fin
    /\ ~stray
    /\ probed' = TRUE
    /\ UNCHANGED <<socks, fresh, answer, fin, stray, wrote, marked, framed,
                   persist, left, interims, addrs>>

Stale ==
    /\ Go("probing", "resolving")
    /\ HangUp
    /\ UNCHANGED <<fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims, addrs>>

(* ---- resolving, RESOLVE                                                 *)

Found ==
    /\ Go("resolving", "connecting")
    /\ addrs' \in 1..Addrs
    /\ UNCHANGED <<socks, fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims>>

Unresolved ==
    /\ Go("resolving", "down")
    /\ UNCHANGED <<socks, fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims, addrs>>

(* ---- connecting, DIAL: one address each; a failed attempt closes its    *)
(* own socket.                                                             *)

Dialed ==
    /\ Go("connecting", "greeting")
    /\ addrs' = addrs - 1
    /\ socks' = socks + 1
    /\ fresh' = TRUE
    /\ answer' = "none"
    /\ fin' = FALSE
    /\ stray' = FALSE
    /\ UNCHANGED <<probed, wrote, marked, framed, persist, left, interims>>

Redial ==
    /\ Go("connecting", "connecting")
    /\ addrs > 1
    /\ addrs' = addrs - 1
    /\ UNCHANGED <<socks, fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims>>

Undialed ==
    /\ Go("connecting", "down")
    /\ addrs = 1
    /\ addrs' = 0
    /\ UNCHANGED <<socks, fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims>>

(* ---- greeting, GREET                                                    *)

Greeted ==
    /\ Go("greeting", "sending")
    /\ UNCHANGED <<socks, fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims, addrs>>

Ungreeted ==
    /\ Go("greeting", "down")
    /\ HangUp
    /\ UNCHANGED <<fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims, addrs>>

(* ---- sending, DELIVER: a failed write may have sent part of the         *)
(* request.                                                                *)

Written ==
    /\ Go("sending", "sent")
    /\ wrote' = wrote + 1
    /\ answer' = "owed"
    /\ UNCHANGED <<socks, fresh, probed, fin, stray, marked, framed,
                   persist, left, interims, addrs>>

Unwritten ==
    /\ Go("sending", "down")
    /\ wrote' = wrote + 1
    /\ answer' = "owed"
    /\ HangUp
    /\ UNCHANGED <<fresh, probed, fin, stray, marked, framed, persist, left,
                   interims, addrs>>

(* ---- heading, READ-HEAD; FRAME on a head                                *)

Interim ==
    /\ Go("heading", "heading")
    /\ interims < Interims
    /\ interims' = interims + 1
    /\ UNCHANGED <<socks, fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, addrs>>

Headed ==
    /\ Go("heading", "reading")
    /\ framed' \in BOOLEAN
    /\ persist' \in BOOLEAN
    /\ left' \in 0..Pieces
    /\ UNCHANGED <<socks, fresh, probed, answer, fin, stray, wrote, marked,
                   interims, addrs>>

Unheaded ==
    /\ Go("heading", "down")
    /\ HangUp
    /\ UNCHANGED <<fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims, addrs>>

(* ---- reading, READ-PIECE                                                *)

Piece ==
    /\ Go("reading", "partway")
    /\ left > 0
    /\ left' = left - 1
    /\ UNCHANGED <<socks, fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, interims, addrs>>

Ended ==
    /\ Go("reading", "settling")
    /\ IF framed THEN left = 0 ELSE fin
    /\ answer' = IF framed THEN "whole" ELSE "unframed"
    /\ UNCHANGED <<socks, fresh, probed, fin, stray, wrote, marked, framed,
                   persist, left, interims, addrs>>

Unread ==
    /\ Go("reading", "down")
    /\ HangUp
    /\ UNCHANGED <<fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims, addrs>>

(* ---- settling, REUSE: bytes past the answer shut the connection when    *)
(* they reached the room, and wait for the next probe when they did not.   *)

Keep ==
    /\ Go("settling", "idle")
    /\ framed
    /\ persist
    /\ UNCHANGED <<socks, fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims, addrs>>

Shut ==
    /\ Go("settling", "closed")
    /\ ~framed \/ ~persist \/ stray
    /\ HangUp
    /\ UNCHANGED <<fresh, probed, answer, fin, stray, wrote, marked,
                   framed, persist, left, interims, addrs>>

Run ==
    \/ Refused \/ Unheld \/ Kept \/ Busy
    \/ Quiet \/ Stale
    \/ Found \/ Unresolved
    \/ Dialed \/ Redial \/ Undialed
    \/ Greeted \/ Ungreeted
    \/ Written \/ Unwritten
    \/ Interim \/ Headed \/ Unheaded
    \/ Piece \/ Ended \/ Unread
    \/ Keep \/ Shut

Next == Run \/ Send \/ Resume \/ Close \/ PeerCloses \/ PeerWrites

(* The code runs: a run's next line is weakly fair. The caller and the     *)
(* peer are not.                                                           *)
Spec == Init /\ [][Next]_vars /\ WF_vars(Run)

TypeOK ==
    /\ at \in Runs \cup Rests
    /\ held \in {"none", "idle", "busy"}
    /\ socks \in 0..2
    /\ fresh \in BOOLEAN
    /\ probed \in BOOLEAN
    /\ answer \in {"none", "owed", "whole", "unframed"}
    /\ fin \in BOOLEAN
    /\ stray \in BOOLEAN
    /\ wrote \in 0..2
    /\ marked \in BOOLEAN
    /\ framed \in BOOLEAN
    /\ persist \in BOOLEAN
    /\ left \in 0..Pieces
    /\ interims \in 0..Interims
    /\ addrs \in 0..Addrs

(* A request is written at most once per SEND.                             *)
WriteOnce == wrote <= 1

(* Every rest holds zero or one socket; `closed` and `down` hold none.     *)
Sockets ==
    /\ at \in Rests => socks <= 1
    /\ at \in {"closed", "down"} => socks = 0

(* A write goes to the one socket held, dialed by this run or probed quiet *)
(* by it, which carries no unread answer.                                  *)
QuietWrites ==
    [][wrote' > wrote =>
        /\ socks = 1
        /\ fresh \/ probed
        /\ answer # "owed"]_vars

(* A connection is written to again only after a whole framed answer.      *)
WholeReuse == [][wrote' > wrote /\ ~fresh => answer = "whole"]_vars

(* Every run SEND or NEXT starts reaches a FINAL row.                      *)
Ends == \A s \in {"starting", "heading", "reading"} : at = s ~> at \in Rests
=============================================================================
