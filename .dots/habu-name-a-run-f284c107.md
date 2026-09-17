---
title: Name a run-time is or xt! in a stripped image
status: open
priority: 3
issue-type: task
created-at: "2026-09-17T18:24:32.556536+03:00"
---

Problem: xt! (and the is that routes through it) are not in src/habu/aot-closure.f AOT-UNSAFE?, so a program that binds a defer or stores a quotation at RUN time inside a stripped image reaches the address-cell registrar the image does not carry and the link dies with the unnamed 'PC-relative target removed or outside closure' (measured 2026-09-17 by the stripped-xt lane while pulling an initializer into the closure). Acceptance: AOT-UNSAFE? names the registrar path (xt!, is at run time) so the refusal is by name with the calling word, as the other unsafe words are refused; load-time bindings that the declared-cell relocation carries keep passing; a rejected fixture binds a defer at run time. Files: src/habu/aot-closure.f, test/compiler/aot-data-cell-refusals.f, docs/native-applications.md. Verify: the fixture; test/compiler/aot-xt-cells.f; test/run.f. Depends: habu-let-a-stripped-0a064bf5. Ownership: AOT linker. Claim: unassigned.
