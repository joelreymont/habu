---
title: Record the quotation window row for catch only
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T18:21:34.115632+01:00"
closed-at: "2026-11-13T19:01:19.118394+01:00"
---

Problem: src/core/checker.f CWIN-COMMIT records the quotation window row with CWIN-KIND 0 (catch), 1 (execute) or -1 (finally), and only catch's is read (EFFECT-CATCH-CELLS); NDICT:MEM-CELLS (src/compiler/native/dict.f) has no caller, and WF-W-AT is called only by it and a test. Acceptance: execute's and finally's rows are not written, CWIN-KIND goes, MEM-CELLS and WF-W-AT are deleted; catch sites compile as before. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-fu7.
