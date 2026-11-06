---
title: "C2b2: read a word's control word from facts text"
status: open
priority: 2
issue-type: task
created-at: "2026-11-06T21:29:53.876111+02:00"
---

Problem: design r2 D2 (/tmp/jerry-big/design-facts-r2.md): after C2b1 the facts text carries an effect's variables but not the facts a caller's check reads from a symbol's control word (checker.f CTL-WORD-SYM: flags bits 0-18, intact masks 20-39 and 40-59, XFER-PACK; defer bit 16 from DFER-FIND-SYM as ASIG-GRAPH-COPY packs it). Acceptance: tags noret, throws D R (decimal masks), barrier, external, core-op, zero-true, zero-false, renders, intrinsic NAME, parses, creates, nominal, defer, names, generates; render.f REND-CTL writes them, SIG-TAGS reads them into SIG-CTL; a control word the tags cannot say sets RQM; the control constants move before the signature parser; a control probe over every symbol with a nonzero word finds every one read back equal. Brief: /tmp/jerry-big/brief-c2b2.md. Files: src/core/checker.f, src/core/render.f. Verify: the control probe; the effect probe unchanged; bin/hb --load test/run.f -- engine checker-soundness diag-position. Depends: C2b1. Ownership: those files. Claim: unassigned.
