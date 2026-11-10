---
title: Test a cross build and a crafted early retirement
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T20:52:56.464217+02:00"
closed-at: "2026-11-10T23:37:09.182537+02:00"
close-reason: "landed in 'Test a cross build'; suite 273 of 273 and the measure green on the landing tip (Maki, Zen, Tender warm equal to cold)"
---

Problem: The stack's two review fixes (a cross build's word has no emission; FACTS-CK bounds a retirement's record by its count) have no test. Acceptance: a --target wasm build case in test/file-objects.f; a crafted retirement edit in test/fixtures/file-objects/build.f that only the new check refuses. Files: those two. Verify: file-objects passes. Brief: .out/briefs/t3-cross-and-crafted.md. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-t3.
