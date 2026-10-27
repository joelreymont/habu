---
title: Record every loader action as an edge
status: open
priority: 1
issue-type: task
created-at: "2026-10-26T13:52:38.596489+02:00"
---

Joel 2026-10-26: every require/required/include at a loader cut writes one EV-LOAD row and a warm load performs the edge; no skip records, no provided mode. Cache format 24 -> 25. Lane A1 records edges (design sections Row, Observer, NUNIT-OBJECT); lane A2 then deletes section 8 and the require counting. Design ~/.cache/tmp/carl-file/edge-design.md, brief brief-edge1.md. Major review (persisted format, recovery). Workspace: .jj-ws.noindex/carl-edge. Claim: unassigned.

A1 landed on master 9f905e31 (nlupwrsn "Record every loader action as an edge"). A2 remains: delete section 8 and the require counting (edge-design list: FO-SEG-REQ, OPEN-REQS, REQ-COUNT-XT, REQ-XT/REQ-NOW, FO-REQ-XT, SEGMENT reqs cells, REQ-ROW/REQS-OUT, REQUIRES-CK, INSTALL-REQUIRES, SEG-BASES rbase, FILE-COUNTS reqs and `requires=` in native-definition-cache.f, SOURCE-UNIT:PROVIDED-RESOLVED, frame e2e `requires=` asserts); all still present at 9f905e31.
