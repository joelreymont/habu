---
title: Shorten CHECK session internals
status: open
priority: 1
issue-type: task
created-at: "2026-07-23T02:12:28.913683+02:00"
blocks:
  - habu-cut-over-check-ac1b7cdf
---

Why: after the atomic session cutover, the session, capture, source-selection, and materialization concern still exposes legacy CHK-prefixed private names and the checker hook lacks one canonical HOOK-SITES identity. Owner: package CHECK. Files: tools/check-core.f, tools/check-test-lib.f, tools/hook-sites.f. Preserve the public CHECK session API and all effects from the cutover. Rename CHK-CHECK-HOOK to private HOOK and add its exact installed path, token, and check-hook kind to HOOK-SITES. Rename the old private materialized-path getter to MAT-PATH$, the selected diagnostic label getter to LABEL$, and remove CHK from every other private definition and storage slot whose sole role is session configuration, copied selection, capture buffers, temporary paths, source materialization, and list materialization. Delete obsolete RESET-CFG, ADD-POS, eager source materializers, and DIRECT-RUN after their callers use the session operations. Acceptance: zero executable CHK-prefixed name remains in this concern; no new public word or mutable state; HOOK-SITES plus the current checked-boundary fixtures prove the hook identity; caller-owned selection buffers can change without affecting a pending run; empty source, no selection, file, list, stdin, rerun, capture lifetime, failure cleanup, and capacity diagnostics remain byte-exact through the production CHECK entry points. Forbidden: aliases, forwarding globals, borrowed spans, inferred modes, sentinel lengths, duplicated hook rows, or behavior changes. Pre-change proof: the package ownership gate rejects a representative renamed private definition until it is inside CHECK, and the current checked-boundary wrong-path fixture accepts the stale name-only authorization. Verify through tools/check-test.f, checked-boundary lint and fixtures, gate diagnostics, exact diff ownership/type, and host-lint.
