\ native-window-capture.f - the capture seam of test/native-window-owner.f's
\ tier-1 window, loaded last, after every fixture compiled against the fresh
\ checker and the adapter and family fixtures asserted the by-name regime.
\
\ One adapter capture ends that regime, and their captured halves run against
\ the owner-record dispatch it selects, before any checker preparation, as each
\ did alone. Then the checker's own capture preparation: tape-detach takes the
\ first one with its observer installed, and payload validation the later ones.
\
\ Each check runs inside its own package, as it did at its fixture's load: the
\ checker resolves some names it is asked about in the current search order
\ (NFAM:CON-FAM of the family `payload`, a scan that calls PAYLOAD-NUMERIC),
\ and only their own package finds them.

CHECKER-OWNER:CAPTURE-PREPARE

package OWNER-ADAPTER-CHECK
CAPTURED
;package

package OWNER-FAMILY-CHECK
CAPTURED
;package

package CHECKER-TAPE
RUN
;package

package OWNER-PAYLOAD-CHECK
RUN
;package
