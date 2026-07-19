---
title: Package build driver I/O
status: open
priority: 1
issue-type: task
created-at: "2026-07-19T21:28:20.416451+02:00"
---

src/habu/driver-io.f:3-152 exposes 35 DRV-* words globally. It is appended to build/stdin images and called by stage2, AOT, snapshot, and maker paths, yet most status rendering, buffer/cursor, size-map, reload-retirement, and failure helpers are implementation details. Wrap it in package DRIVER-IO; export only the actual emit-image, success/failure, size-map, and reload-retirement entry points used across build boundaries; keep scratch buffers, low-level writes, formatting, and state private. Update all generated/direct callers without compatibility globals and ensure package qualification survives the appended-source/AOT/snapshot paths. Preserve stdout/stderr, exit codes, engine bytes, size-map rows, reload behavior, and build ordering exactly. Add old-global/private rejects, qualified public positives, and byte-golden success/failure/size-report fixtures on every command path. Measure dictionary-name bytes, JIT/DATA/CODELEN, build image size, and driver latency before/after. Verify stdin/metabuild/maker/AOT/snapshot/fixpoint and both target gates, package/host/filemap/dot lints, and full native gate. Parent: habu-pkg-native-build-f598557c.
