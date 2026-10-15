---
title: Bound default ZIP archive storage
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-10-15T04:02:32.884178+03:00\\\"\""
closed-at: "2026-10-15T07:28:15.695939+03:00"
close-reason: Default OPEN/EDIT and FILE aliases now share the existing bounded private snapshot/file-output implementation; obsolete whole-archive input/output allocation paths are removed. SOURCE$ remains a lazy immutable borrow with documented close/commit lifetime, and failure/ZIP64/byte-preservation contracts pass. Full ZIP source suite including concurrent Tasks, lifecycle, offsets, source/native/REPL ZIP-XML and source/native unusable-TMPDIR refusal pass on qualified public e6208777 engine d3791473/map81f13346. Independent Astra review, focused GETENV finding closure and lead review are clear. The source-only prefix is qualified with identical baseline engine/map/XML; whole-workbook memory acceptance remains under the Kaizen memory task.
---

Canonical native Office corruption verification still retains full archive snapshots and allocates full commit output through default OPEN/EDIT. Standardize defaults and FILE aliases on the existing owner-private snapshot/file-output implementation, remove obsolete memory backend branches, preserve immutable SOURCE$ and all failure/retry/close/ZIP64/byte-preservation contracts. Document writable TMPDIR/disk requirement with clean refusal and no memory fallback. Prepare existing framing/offset coverage first; run affected source/native public ZIP/XML and Office consumers, independent Astra review, and qualify an immutable release from the matching qualified public engine/source tree plus only the reviewed ZIP delta before selector promotion and unchanged writer measurement.

Release dependency habu-make-getenv-invocation-afdf8e70: default snapshot opens exposed the baked GETENV/ENV=? shared-cursor race between Tasks. A two-Task environment-only reproducer returns unrelated or empty TMPDIR bytes; ZIP's existing concurrent consumer consequently refuses valid opens. The invocation-owned source repair a510bffc is independently Astra-reviewed; Carl owns matching core build and public qualification. Do not ship a ZIP lock, environment cache, shadow lookup or private engine. Independent ZIP review found no additional material issue; source/native/REPL XML consumers and bounded-output/offset/refusal checks pass. Requalify against the repaired public release before landing and selecting the ZIP change, with old/new ZIP measured on that same engine.
