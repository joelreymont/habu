---
title: Restore compile observers for FILE capture
status: active
priority: 2
issue-type: task
created-at: "2026-10-15T07:41:45.782967+03:00"
---

Private native build from6b509943 fails -8597 while packing src/compiler/native/feed.f. Same-process diagnostics prove SLOTS-PROFILE row810 contains the exact retained host COMPILE-SEEN XT installed by BIND-TARGET-COMPILE into the fresh target NFEED FILE-COMPILE-OBSERVE deferred cell. That build observer is process-local tool state and cannot be a persistent target FILE code reference. Establish the existing NFEED compile-observer default at the responsible capture boundary, preserve/restablish live observation for subsequent source and warm replay, and cleanly restore on failure. Use existing FILE-COMPILE-DEFAULT/USE owner operations where they express this; no scanner exception, untracked callback registry, blanket neutralization or serialized host XT. Existing real native-build-edit E2E must build the engine with FILE units then demonstrate correct actual warm/root/leaf hits and artifacts; do not add a duplicate isolated test. Carl independent_file_core owns the fix with main native candidate integration and independent Astra review; source-complete149 private donor is available.
