---
title: Lift the tier-1 name cap to the tier-0 bound
status: open
priority: 2
issue-type: task
created-at: "2026-10-02T19:07:57.006800+02:00"
---

Problem (lane 316 r4-tapename, b8b31141): tier 1 refuses a definition name over 64 bytes (NAME-CAP, E-NCOMP-NAME-CAP) while tier 0 compiles names up to the 8000-byte body capture: the tiers disagree on which programs compile. The cap exists because each backend copies a function's name into a 128-byte buffer (FUN-NAME/NAMEBUF in select.f, select-x64.f, prune.f, loop.f, spill.f; elaborate.f QNAME builds '<name>;does', '[:' and an ordinal) and throws E-IR-SYM-RANGE past it. Acceptance: tier 1 compiles every name tier 0 does (the backends name a function by a span into the definition's own text, or a symbol id, not a fixed copy); the 64-byte cap and its docs bullet go; tier-0/tier-1 probes at 65, 1000 and 7900 bytes compile and run; rebuild, g1 == g2, two-gen. Files: src/compiler/native/{compiler,elaborate,select,select-x64,prune,loop,spill}.f, docs/forth.md, docs/forth-card.md.
