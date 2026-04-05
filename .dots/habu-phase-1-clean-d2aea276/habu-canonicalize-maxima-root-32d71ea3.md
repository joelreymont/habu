---
title: Canonicalize Maxima root contract
status: active
priority: 1
issue-type: task
created-at: "\"2026-04-05T11:37:31.427403+02:00\""
---

Problem: PLAN.md 1.1 still needs a worker-ready leaf covering the single authoritative Maxima manifest, trusted-root contract, upstream tree fingerprint, no-ambient-root policy, and race-free authoritative load checks. Acceptance: authoritative Maxima loads/bench/tools accept only declared trusted roots, reject ambient /tmp and ~/.maxima execution paths, and record manifest/tree identity in the canonical loader state. Files: PLAN.md:262-308, lib/maxima-loader.lisp, lib/maxima-manifest.lisp, tools/maxima-rtest.lisp, bench/maxima_workload.zig, src/interp/repl.zig. Verify: canonical trusted-root smoke accepts declared ../maxima and rejects ambient roots with explicit failure.
