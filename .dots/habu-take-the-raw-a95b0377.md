---
title: Take the raw poll primitive out of the global names
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T18:32:22.761542+02:00"
---

Problem: the engine primitive poll ( fds nfds ms -- n ) (src/habu/prims.f:614) is a global word, so PROCESS:POLL (Joel's name, 2026-11-11, dot habu-wait-on-a-2ef74445) shares its bare name with a global: docs/forth-card.md section 1 says a public whose bare tail a global owns is refused under using (E-USING-SHADOW-GLOBAL) and to rename the public. Inside package PROCESS a bare poll after POLL's definition is POLL, so POLL-IN now calls the private POLL-ONCE, the one word that calls the primitive. Cause: a raw system call, which takes an unchecked pollfd address (test/seal.f:122-125 and test/protection-span.f:69 pin that it traps), owns a plain global name a library word needs. Acceptance: the primitive is reachable only by PROCESS (a private section, per docs/design.md "Machinery being replaced": engine-only marks become private sections) or carries a name no public word wants; using PROCESS then imports POLL bare; the bare callers follow (src/lib/process.f POLL-ONCE; test/gate-signal-row.f:42, test/gate-pool-orphan-test.f:27, test/gate-signal-test.f:74, test/gate-pool.f:1235, :1304, test/seal.f:125, test/protection-span.f:69). Files: src/habu/prims.f and the engine bodies that name the row, src/lib/process.f, the tests named. Verify: engine both generations, suite, a using PROCESS program calling bare POLL. Depends: habu-wait-on-a-2ef74445 landing; the privacy strip (private sections for engine words). No file imports PROCESS with using today, so no consumer meets it. Ownership: carl. Claim: unassigned.
