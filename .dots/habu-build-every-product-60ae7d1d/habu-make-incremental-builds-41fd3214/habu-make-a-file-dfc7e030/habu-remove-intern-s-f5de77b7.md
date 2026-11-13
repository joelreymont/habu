---
title: "Remove INTERN's source and prepare words and AOT-ARM:PAYLOAD-PERSISTENT"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-11T08:09:25.687385+02:00"
closed-at: "2026-11-11T08:28:23.558671+02:00"
close-reason: landed on master as 61f1c91f and installed; engine twice, suite 273 of 273, chain fixpoint at generation 2; Maki image and viewer, Zen and Tender warm = cold and tender.f loads on the candidate and on the installed hb
---

Problem: src/habu/intern.f keeps nine public words of the deleted per-definition build cache (SOURCE-BEGIN, SOURCE-END, SOURCE-COUNT, SOURCE@, SOURCE-CLOSE :311-325; PREPARE-BEGIN, PREPARE-INTERN, PREPARE-DATA-END, PREPARE-CLOSE :412-440) and src/habu/aot-arm.f:85 keeps PAYLOAD-PERSISTENT. No code in Habu, Zen, Tender, Maki or Etch calls one (rg -i --no-ignore, bare and qualified, 2026-11-11); ten probes (p1.f to p10.f) load rc 0 on the installed hb d63bea19, so a program can call them and change the hb doing its build. Acceptance: the ten words and the storage only they read are deleted; each probe is refused as undefined on the new engine; INTERN-SOURCE keeps its shape and answers -1. Files: src/habu/intern.f, src/habu/aot-arm.f, src/habu/t0.f (one comment). Verify: the brief's chain check and probes (.jj-ws.noindex/carl-layout/.out/briefs/remove-intern-source-prepare.md); suite and measure once at the landing. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-intern.
