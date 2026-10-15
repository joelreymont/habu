---
title: Merge the DEBUG package files
status: open
priority: 2
issue-type: task
created-at: "2026-10-15T17:16:31.103941+03:00"
---

Problem: debug.f, debug-watch.f and stepper.f are one package split across three files. stepper.f:90 silently truncates at 1023 bytes. S-FALSE redefines prelude false. stage2.f:20-21 uses create, allot and TRUST where DYNAMIC-BUFFER exists. data-bands.f belongs in data-claims.f. Acceptance: one DEBUG file; stepper refuses or handles input over the limit instead of truncating, with a test; S-FALSE replaced by false; stage2 uses DYNAMIC-BUFFER; data-bands.f folded into data-claims.f. Files: src/habu/debug.f, debug-watch.f, stepper.f, stage2.f, data-bands.f, data-claims.f, their load lists. Verify: debug and stepper tests; rebuild bin/hb if these are in the prefix. Depends: carl-final-core chain landed on master (it edits these files); start from master after it lands. (data-claims.f is in it.) Ownership: listed files. Claim: unassigned.
