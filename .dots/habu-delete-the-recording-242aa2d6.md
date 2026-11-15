---
title: "Delete the recording unit's uncalled release"
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T17:46:19.794228+01:00"
---

Problem: src/core/checker.f REC-RELEASE (:14152) has no caller (t0-wasm.f and curl.f define their own), and EV-RELEASE (:14127), CWIN-RELEASE (:13859) and BWIN-RELEASE (:13713) are reached only through it; uncalled words are deleted. Acceptance: the four words are gone and no reference to them is left; the engine builds and converges. Depends: none. Claim: unassigned.
