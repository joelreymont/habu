---
title: Create GPT2-FIXTURE checkpoint provider
status: closed
priority: 2
issue-type: task
created-at: "2026-07-26T22:41:19.093654+02:00"
closed-at: "2026-08-02T16:43:02.266370+02:00"
close-reason: authoritative ancestor 5b0ebb070a5b8ef7c04e2d28772421f796b686c6 deleted the unused GPT2LOAD/GPT2TX/WSTORE/MODELPROV host architecture and suites; retaining the task would resurrect deleted architecture.
---

One package-owned provider for the pinned real-checkpoint facts: package GPT2-FIXTURE (test support under maki/infer) with PATH$, BYTES, CENSUS, CFG, PRESENT?, and VERIFY. Digest ownership INVERTED per codex correction (accepted on the merits of the seam, not the size claim - gpt2-reference-data.f is 196 lines, but identity belongs to the minimal fixture owner and correctness references consume it): the single SHA literal moves from GPT2-REFERENCE:VERIFY (gpt2-reference-data.f:165) into GPT2-FIXTURE as DIGEST$, and GPT2-REFERENCE:VERIFY-ARTIFACT delegates to GPT2-FIXTURE:VERIFY-PATH in the same commit - one digest authority, fixture owner minimal, no consumer loads reference vectors to reach artifact identity. No consumer migration in this leaf; local copies stay until their consumer leaf lands. Acceptance: provider suite green (present and absent paths); both diff lints; FILEMAP row.

Amended (codex preflight 1): the public surface is FROZEN exactly - PATH$ ( -- ptr u8 n ), DIGEST$ ( -- ptr u8 n ), BYTES ( -- n ), CENSUS ( -- n ), CFG ( -- MDLCFG:mcfg ), PRESENT? ( -- bool ), VERIFY-PATH ( ptr u8 n -- ), VERIFY ( -- ). Named missing/size/digest failure codes; no optional success path.
