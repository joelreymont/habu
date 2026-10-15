---
title: Render diagnostic envelopes through ROW-JSON
status: closed
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:30.956126+03:00"
closed-at: "2026-10-15T18:53:56.868407+03:00"
close-reason: "Landed: JSTART/JHEAD/JTAIL carry every envelope's shared fields in src/core/render.f; JSON bytes identical (66-file before/after capture), full suite 665/665."
---

Problem: src/core/render.f writes the JSON diagnostic envelope out by hand 12 times (:1157 to :1847), while ROW-JSON (:1844-1855) is its generic form. About -120 lines. Acceptance: all 12 envelopes go through ROW-JSON or one shared envelope word; JSON output is byte-identical under the existing diagnostic tests. Files: src/core/render.f. Verify: rebuild bin/hb if render.f is in the engine prefix; diagnostic JSON tests (test/gate-diagnostics-lib.f rows). Depends: none. Ownership: src/core/render.f. Claim: unassigned.
