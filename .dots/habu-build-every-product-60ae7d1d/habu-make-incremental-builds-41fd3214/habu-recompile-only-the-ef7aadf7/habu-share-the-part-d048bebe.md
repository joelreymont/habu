---
title: "Share the part-link cases' builds and show J2 linked"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-10T20:52:56.413204+02:00"
closed-at: "2026-11-10T23:37:09.157291+02:00"
close-reason: "landed in 'Share the part-link cases' builds'; suite 273 of 273 and the measure green on the landing tip (Maki, Zen, Tender warm equal to cold)"
---

Problem: REDO-CASE does not assert that redo-main.f's part links; MID-CASE, START-CASE and REDO-CASE start seven builds and three runs for three independent fixtures (feature review of the stack, findings 5 and 6). Acceptance: one word, three builds, at most two runs, every assertion kept, redo-main.f's object LINKED after build 2. Files: test/file-objects.f, test/fixtures/file-objects roots. Verify: file-objects passes; row time before and after. Brief: .out/briefs/t1-share-part-link-builds.md. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-t1.
