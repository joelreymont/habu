---
title: Remove local buffer shadowing from the host I/O fixture
status: open
priority: 2
issue-type: task
created-at: "2026-09-14T04:24:40.095603+03:00"
---

The current M host-io-image regression stops in the first SERIAL worker with E-SERIAL-OPERAND (-9110). Its RUN input local path shadows the global PATH buffer under the repaired case-insensitive local-first lookup, so path PATH size BYTE-COPY copies the input onto itself. Instrumented worker sees ten zero bytes in PATH with correct length 10. Rename the local to text, preserve all I/O and capture assertions, and rerun three concurrent UDP/serial generations. Original and guarded evidence: /tmp/cedar-M-host-io/. Cedar owns the fixture correction; this is not a new compiler defect.

Independent Astra source review approves the local rename and the executable fixture selecting tier1 before subject dependencies. M now executes both concurrent I/O rounds and cleanup/reuse (`io=ok` twice), then exposes the separate unknown task-entry code gap in 4b3d5801. That refusal persists with explicit tier1; no full image pass is claimed. Exact evidence: `/tmp/cedar-M-host-io/{unshadowed,native-subject}.{json,log}`.
