---
title: Land a send halted in its journal lookup without a thrown fault
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T13:58:50.048296+02:00"
---

Problem (spec worker for dot 5da03241, carl-mcall 7badacd0; line numbers at 4255709c): src/lib/net/model.f settles a call halted in LOOK-UP (3746) through a thrown fault: after the finally's CANCEL (3979), AWAIT (4063) -> RECEIVE (3897) -> TAKE-WAIT (2917) -> CALL-HANDLE (2055-2058) throws E-STATE (the call has no handle), RECEIVE-CAUGHT (3960) catches it, LAND-FAULT (3913) settles, and LAND-CUT (3670) overwrites the outcome with E-CANCELLED; without CANCEL the outcome would be E-STATE. A started call with no handle is a state of the call, not a fault. Acceptance: a send halted in its journal lookup lands cancelled with no E-STATE thrown or caught on the way, and one halted with no cancel lands with a stated outcome, not E-STATE; spec/ModelCall.tla's action for that window matches the code. Files: src/lib/net/model.f, spec/ModelCall.tla, the model call test. Verify: a test that halts a send inside JOURNAL-GET? and reads the outcome; TLC on spark. Depends: habu-modelcall-tla-omits-5da03241 (carl-mcall 7badacd0). Ownership: model.f AWAIT/RECEIVE path for a handle-less call. Claim: agent=carl workspace=.jj-ws.noindex/carl-mcall
