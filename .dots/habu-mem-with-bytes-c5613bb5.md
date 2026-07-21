---
title: "MEM:WITH-BYTES quotation-scoped mapped memory"
status: open
priority: 1
issue-type: task
created-at: "2026-07-21T06:38:18.095898+02:00"
blocks:
  - habu-release-vector-resize-95575b87
---

Joel-directed 2026-07-21: a with-open-file-style scoped-resource construct for mapped memory, in the QUOTATION-SCOPED form (settled by Joel over the MMAP...;MMAP parsing-pair form - quotations are what the checker types well and they nest for the real two-buffer cases like vector grow and layout-buffer copy-on-grow). Shape: MEM:WITH-BYTES ( CAD-NUM:alloc-byte-len [ ptr u8 len -- ... ] -- ... ) allocates, runs the body quotation, and releases via MEM:RELEASE-BYTES (541b691f) on BOTH normal return and throw, preserving the primary error - the lib/ptx/cuda-scope.f ledger/frame pattern (consume-on-release, reverse order for nested frames) generalized to host memory, either by extracting the frame machinery into a shared lib module cuda-scope also consumes (preferred if the extraction is honest) or by a focused twin with the same proven discipline. Also decide whether bounded-lifetime consumers (build tools, lints, tests among the 42 ALLOC-BYTES users) migrate in this dot or in follow-up waves (default: mechanism + 2-3 exemplar migrations here, waves dotted separately). Red-first: throw-mid-body leaks on the unfixed base (no release), scoped version releases exactly once; repeated/double release structurally impossible. Blocked on habu-release-vector-resize-95575b87 (same lib/memory.f seam in flight). Long-term note: linear owner types eventually subsume this, per the cuda-scope landing record.
