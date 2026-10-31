---
title: "Check the definition cache's record writers"
status: closed
priority: 2
issue-type: task
created-at: "2026-10-31T13:08:03.796615+02:00"
closed-at: "2026-10-31T14:16:13.315233+02:00"
close-reason: "Landed 89295fc4 (the 11 REC-* writers are : definitions) and db38be25 (a cached install first checks that the engine has rep-mark, which REC-REP needs). Full suite 510/510 and x86-64 cross-build rc 0 on the batch stack."
---

Problem: tools/native-unit-object.f's 11 REC-* install writers are TRUSTED: but check as : (census tc3); the block header says the rest are TRUSTED: rows. Acceptance: all 11 are :, the header says every writer is checked through its cast, check.f on the file accepts all but SLOT-*, rows pass. Files: tools/native-unit-object.f. Verify: rows native-file-types, native-file-state. Depends: none. Ownership: that file. Claim: agent=jerry worker workspace=.jj-ws.noindex/jerry-tc6
