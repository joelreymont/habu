---
title: Refuse START on a call already started
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T14:44:54.283668+02:00"
---

Problem (review of carl-mcall 91299ad4, by reading): a consumer's journal get that SERVEs or AWAITs its own call re-enters through JOURNAL-GET? (src/lib/net/model.f 3125) to CALL-HANDLE's none arm (2057) and gets E-STATE; if the get catches it and answers a miss, DISPATCH starts a transfer on the call, because START-LOCKED does not check the call's state. Acceptance: START-LOCKED refuses a call not in the state START expects; a test where the journal get re-enters its own call ends with one landed call and no transfer. Files: src/lib/net/model.f, src/lib/net/model-test.f, spec/ModelCall.tla if the state machine changes. Verify: model-test.f rc 0 with the new case failing on the parent. Depends: carl-mcall lands. Ownership: model.f START-LOCKED. Claim: unassigned.
