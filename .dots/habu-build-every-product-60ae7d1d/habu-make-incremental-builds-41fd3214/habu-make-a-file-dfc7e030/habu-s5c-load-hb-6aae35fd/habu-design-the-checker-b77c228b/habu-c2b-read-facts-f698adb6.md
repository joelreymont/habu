---
title: "C2b: read a variable's kind and scope from facts text"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T20:16:59.556621+02:00"
closed-at: "2026-11-06T23:54:25.211406+02:00"
close-reason: "landed: chain fixpoint at gen 2 rc 0 (320.6 s real, 268.8 s user); full suite 293/293 rc 0 (88.4 s wall); review fold rebuilt 125.4 s, focused tests and review probes pass; probe all 11079 live rows same"
---

Problem: design r2 D2 (/tmp/jerry-big/design-facts-r2.md): facts text is `( EFFECT ) TAGS` with every family named with its package; three facts about an effect's variables do not survive the text today: a type variable's kind the body decided (TVK-RAW, TVK-NULL, TVK-DBASE, TVK-OFF, TVK-CELL, TVK-BASE-ONLY; 94 live words, and every val/addr record outside raw mode), the no-scope mark on a type or row variable (299), and ATOM-REND's fresh-atom ordinal (2; source signatures spell fresh-region-b as a name, docs/forth-card.md:191-192, so only facts text reads a trailing -<letter> as the ordinal). Tags `raw|null|dbase|offset|cell|base-only <var>` and `noscope <var|Row>`; a row carrying NSCOPE is always named. Acceptance: render.f REND-FACTS writes the text and the parser in facts mode (SIG-FACTS!) reads it; the fid2 probe turns every live row differing only by a kind, a no-scope mark or a fresh atom into same, val/addr rows stay same without raw mode; source signatures parse as today. Brief /tmp/jerry-big/brief-c2b1.md. Files: src/core/render.f, src/core/checker.f. Verify: the probe; bin/hb --load test/run.f -- engine checker-soundness diag-position; chain and full suite at landing (effect text changes what the AOT capture carries). Depends: C2a. Ownership: those files. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c2b1.
