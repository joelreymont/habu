---
title: "Seal: syscall full-range overlap guards"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T08:30:09.220031+02:00"
---

Current-state correction on master 8f3ce0e9: the original native range-checking gap is already fixed. `GUARD-SPAN` rejects zero-excluding half-open overlap and address+length wrap, and native BREAD, BREADLINK, BSTAT64/BLSTAT64, BGETDIRENTRIES64, BPOLL, fixed BMMAP, and `GUARD-IOCTL` use known extents or fail closed on an unknowable ioctl request. The remaining parity gap is bootstrap `BIOCTL`, which still point-checks x2 with `PROT-GUARD`; existing child forges start inside a band and do not prove start-below straddling or wraparound behavior. Fix: mirror the native request-direction/extent model in `bootstrap/cg/forth.fs` without duplicating policy, and add hostile child fixtures for start-below straddles, exact adjacency, zero length, end wrap, unknown ioctl direction/size, encoded read extents, and fixed MAP_FIXED/read/stat/getdirentries spans. Acceptance: every kernel-written native and recovery syscall extent reaches one range guard before the syscall; unknown write extents reject; straddling and wrap mutations exit E-SEAL-VIOLATION before bytes change; adjacent/zero/read-only requests retain current behavior; native/recovery guard policy and protected-band census cannot drift. Files: bootstrap/cg/forth.fs, focused seal fixtures and parity/census owner only; touch src/habu/habu1.f only if a single shared checked schema can replace duplicated policy. Verify: seal and seal-absence suites, bootstrap check, native fixpoint, typed-local diff lint, host/filemap/dot lints, full native gate. Serialize with active span-guard ownership; do not reimplement the landed native `GUARD-SPAN` sinks.
