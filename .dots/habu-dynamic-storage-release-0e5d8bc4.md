---
title: "DYNAMIC-STORAGE:RELEASE leaves a record registered when its unmap is refused"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T17:38:32.554188+02:00"
---

Problem: src/core/dynamic-storage.f RELEASE throws at :159 on a refused unmap before it takes the record off the registry (:100-112, :275-294). For a dictionary record that is harmless; a record in arena memory would stay named after the arena frees it. Acceptance: a refused unmap leaves the registry without the record, or the owner-columns design's RELEASE-RECORDS is the one word that releases arena-laid records and RELEASE's header says dictionary records only. Files: src/core/dynamic-storage.f. Verify: a test that refuses the unmap. Depends: habu-give-a-run-d938ab7f. Ownership: unassigned. Claim: unassigned.
