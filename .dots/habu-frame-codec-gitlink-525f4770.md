---
title: "Frame codec: gitlink/submodule form"
status: open
priority: 2
issue-type: task
created-at: "2026-07-19T22:45:55.230696+02:00"
---

The M5 framed diff codec (tools/lint/diff-frame.f) supports 5 forms: text/mode/empty/pure/binary. The gitlink (submodule) form is deferred: the shared parser tools/lint/diff.f discards a new file's mode (NEW-FILE-LINE does MODE-OF drop) and requires an empty-blob index for meta-only adds, so a submodule section (mode 160000, commit-hash index) cannot be distinguished from an empty add and is not accepted. Adding gitlink needs the shared parser to (a) accept gitlink new/delete/modify sections with commit-hash indexes and (b) expose the file mode so DIFF-FRAME:VALIDATE-SECTION can classify a gitlink form. Then add DIFF-FRAME:form gitlink + its byte mapping + tests. The recovered diff-frame-test.f gitlink case used mode 040000 (a tree mode, not a real submodule mode 160000) and is not a correctness reference.
