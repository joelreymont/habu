---
title: "ModelCall.tla omits a halt during a send's journal lookup"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T13:20:57.049619+02:00"
---

Problem (phil, 2026-11-11, found by a review of phil's flex lane; present on master 4255709c): src/lib/net/model.f START -> BEGIN-SEND (3933) runs BEGIN-CALL (2653: the call is now started), then SEND-BEGUN, whose send machine runs LOOK-UP (3743: JOURNAL-GET?, a consumer callback that may wait) before DISPATCH (3780) creates the transfer. A halt served while LOOK-UP waits unwinds a started call with no handle and no CURL record; the caller's finally then CANCELs and AWAITs, and the missing handle's fault is caught and settled there. spec/ModelCall.tla goes from a started call straight to 'cancelled -> landed, no transfer' or 'transfer claimed': the lookup, its wait, a halt inside it, its replay and fault outcomes, and the cleanup of a started call with no transfer are not modelled, so TLC says nothing about that state. Acceptance: the spec models that window and TLC passes on spark, or TLC shows a violation whose code cause becomes its own fix. Files: spec/ModelCall.tla, spec/ModelCall.cfg, spec/README.md; read src/lib/net/model.f. Verify: ~/.local/bin/tlc on spark per spec/README.md. Workspace: .jj-ws.noindex/carl-mcall. Ownership: carl.
