---- MODULE HttpcFollows ----
(* Follows: every step of Httpc that moves the machine, or runs a line, is *)
(* one line of LIFECYCLE (HttpcTable, which a test keeps equal to the      *)
(* code's table) and every line's step moves as the line says. SEND and    *)
(* NEXT start a run from a rest; CLOSE, and a throw out of a run, rest     *)
(* the machine at `down`. No line does any of these.                       *)
EXTENDS Httpc
T == INSTANCE HttpcTable
Walks(l) ==
    CASE l = <<"starting", "marked", "down">> -> Refused
      [] l = <<"starting", "none", "resolving">> -> Unheld
      [] l = <<"starting", "idle", "probing">> -> Kept
      [] l = <<"starting", "busy", "resolving">> -> Busy
      [] l = <<"probing", "quiet", "sending">> -> Quiet
      [] l = <<"probing", "stale", "resolving">> -> Stale
      [] l = <<"resolving", "found", "connecting">> -> Found
      [] l = <<"resolving", "failed", "down">> -> Unresolved
      [] l = <<"connecting", "up", "greeting">> -> Dialed
      [] l = <<"connecting", "next", "connecting">> -> Redial
      [] l = <<"connecting", "failed", "down">> -> Undialed
      [] l = <<"greeting", "up", "sending">> -> Greeted
      [] l = <<"greeting", "failed", "down">> -> Ungreeted
      [] l = <<"sending", "sent", "sent">> -> Written
      [] l = <<"sending", "failed", "down">> -> Unwritten
      [] l = <<"heading", "interim", "heading">> -> Interim
      [] l = <<"heading", "head", "reading">> -> Headed
      [] l = <<"heading", "failed", "down">> -> Unheaded
      [] l = <<"reading", "some", "partway">> -> Piece
      [] l = <<"reading", "end", "settling">> -> Ended
      [] l = <<"reading", "failed", "down">> -> Unread
      [] l = <<"settling", "keep", "idle">> -> Keep
      [] l = <<"settling", "shut", "closed">> -> Shut
Line(l) == at = l[1] /\ at' = l[3]
Follows ==
    [][ /\ (at' # at \/ Run) => \/ \E l \in T!Lines : Line(l) /\ Walks(l)
                                \/ Send \/ Resume \/ Close \/ Throw
        /\ \A l \in T!Lines : Walks(l) => Line(l) ]_vars
====
