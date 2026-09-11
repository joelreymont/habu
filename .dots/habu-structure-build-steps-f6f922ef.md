---
title: Structure build steps
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:38:01.356364+02:00"
---

Evidence: lib/build.f:12-23 defines an 11-cell build-step record through raw numeric offsets; lines 45-75 expose generic field and pointer/length helpers accepting any offset. BUILD-STEP-CLEAR at 77-83 manually resets five spans and overloads rc=-1 as not-run; named accessors at 85-123 wrap the unsafe offsets, and BUILD-STEP-VALIDATE/RUN at 214-224 consume them. Command, argv, temporary path, artifact path, and name are indistinguishable pointer/length pairs, so any field or pointer/length swap type-checks; partial reuse can leave stale result state. Replace the positional layout with a checked STRUCTURE build-step containing named spans and a payload ENUM step-state pending|completed(rc), adding a distinct failed payload only if the retained runtime contract distinguishes it. Remove generic public offsets/setters and make construction/transition transactional and exhaustive. Preserve exact BUILD-STEP-RUN command, artifact, cleanup, and return-code behavior. Prove compile-negative cross-field writes, construction/clear yields only pending, illegal transitions reject, invalid command/path leaves the step unchanged, exact run behavior, malformed span/capacity canaries, and existing build/build-cache/fixpoint suites. Measure accessor/source definitions, JIT/DATA bytes, record size, and run overhead before and after.
