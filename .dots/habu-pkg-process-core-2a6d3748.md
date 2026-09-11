---
title: Package process core
status: open
priority: 2
issue-type: task
created-at: "2026-07-21T22:37:50.378913+02:00"
blocks:
  - habu-libs-migrate-process-6bfe40be
---

Invariant: process lifecycle, polling, capture, cleanup state, and implementation helpers belong to one real package; callers see only a small checked process API. The current core exposes PROC-prefixed constants and helpers, RUN-CAPTURE, polling tables, capture buffers, and cleanup machinery globally. Separate trace and captured-result record owners exist, but no package owns the process engine itself, so prefixes provide naming without privacy.

Create PROCESS as the core owner, use short private tails, export only the lifecycle, wait, capture, and result operations actually consumed outside it, and remove compatibility globals. Keep PROCESS-TRACE and PCAP as deliberate adjacent record owners unless a measured design shows they are implementation details. Migrate every process consumer and test, including the native recovery and bootstrap seed load paths. Coordinate the OUTCOME declaration change with habu-libs-migrate-process-6bfe40be so package spelling and type migration happen once.

Preserve fork and spawn behavior, file-descriptor ownership, timeout and signal semantics, stdin and pipe handling, exact result and trace values, cleanup ordering, error propagation, platform selection, and allocation bounds. Prove old globals and private access reject, every public operation certifies, all process, timeout, pipe, stdin, capture, cleanup, standard-library, bootstrap, recovery, fixpoint, package, host, and full native gates pass. Measure definitions, dictionary-name bytes, JIT, DATA, CODELEN, and process load and focused-run time before and after; require a smaller public and loaded surface with no unexplained growth.
