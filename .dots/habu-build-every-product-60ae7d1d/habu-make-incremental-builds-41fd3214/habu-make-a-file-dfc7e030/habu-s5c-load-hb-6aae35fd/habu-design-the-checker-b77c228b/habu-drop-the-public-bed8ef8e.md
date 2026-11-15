---
title: Drop the public type fallback
status: closed
priority: 2
issue-type: task
created-at: "2026-11-06T15:44:52.374735+02:00"
closed-at: "2026-11-10T21:50:20.613598+02:00"
close-reason: "done: TFAM-FIND-PUBLIC is gone from type-family.f; c870085e \"Drop the removed type fallback from forth.md\"; the using-test.f refusal case was not checked"
---

Problem: docs/forth.md 'Rows, variables, and lexing': a bare type name is found as a bare word is (open package, globals, using imports); the unique-public fallback (src/core/type-family.f:722-746 TFAM-FIND-PUBLIC, reached from TFAM-RESOLVE :787) goes, and its users name the package or import it. Census: /tmp/jerry-tfam/suite-sites.tsv (65 suite sites, 3 in hb's own build). Acceptance: TFAM-FIND-PUBLIC and the checker's any-public type-mark coverage go; every site qualified or imported; the type suites' fallback cases become refusals; a load-path refusal case in test/using-test.f. Files: src/core/type-family.f, src/core/checker.f, src/core/render.f, docs/forth.md, the site files. Verify: native build, then the type suites and edited tests; full suite at landing. Depends: none. Ownership: workspace .jj-ws.noindex/jerry-tfb. Claim: unassigned

State (2026-11-06, jerry): the worker's change mprskyqt (2d2b099c, "WIP: Drop the public type fallback", on e8ad8003) has edits 1-4 and 6 and the qualifiable sites of edit 5; tfb-handoff.md is tracked in it and goes before landing. 60 census sites are definer calls (`TASK:SEMAPHORE X`, `AIO:GROUP X`, `QUEUE:QUEUE X`): the created word's `does>` effect, written bare inside the definer's package (src/lib/task.f:2074 `( -- sem )`, queue.f:245, aio.f:1453), is refused where the word is used: date-helpers exits 70 "bad stored signature '-- sem'". Qualifying the three definers' effects (as src/lib/span.f:128 does) passes date-helpers, bounded-queue, aio-uring, tasking-threads, type-decl, type-match, using-import, c2-mut-records and wasm-kernel-words, but leaves every other definer resolving its effect in the caller's scope; the lead chose the root cause. No worker runs; the WIP stays in .jj-ws.noindex/jerry-tfb. Waits for C2d (habu-c2d-write-declarations-f94670f2): once a definer's does> effect is stored as facts text with every type named with its package, a created word's effect no longer resolves in the caller's scope, and the three definers need no edit. The worker's first build timed out at 600 s (load 48) and it reran with HB_BUILD_TIMEOUT_MS=2400000, which docs/bootstrap.md "How hb builds hb" forbids; its PASS lines come from that build.
