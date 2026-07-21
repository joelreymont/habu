---
title: Own AOT report test paths and buffers
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:03:14.954327+02:00"
---

tools/aot-call-report-test.f copies caller paths into a fixed 128-byte buffer without a preceding length proof and uses shared fixed temporary names and one fixed data buffer. Replace these fixture globals with one package-owned test context created under a validated private HB_TMP root. Size or allocate path and data spans from checked lengths, preflight all arithmetic, create files exclusively, and clean the context on every success or throw. Keep production AOT report behavior unchanged. Add path lengths around every boundary, large report data, embedded hostile path bytes allowed by the filesystem contract, concurrent test processes, preexisting file and symlink attacks, injected write/read/AOT failures, cleanup failure, and canaries. Every reject must occur before copy or file mutation. Files: AOT call report test support and focused test registration only; do not add host logic. Verify AOT positive/negative/report/tool-boundary suites, parallel native gate, typed-local/package/host/filemap/dot lints, and full native gate.
