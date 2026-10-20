---
title: Restore exact record links after capture
status: active
priority: 1
issue-type: task
created-at: "2026-10-16T13:45:48.589293+03:00"
---

Ordinary seeded startup reissues dictionary occurrence tokens while retained checker record links keep old tokens, so four deferred-layout declarations lose their exact contract and FILE import fails. Simply enabling index-based RECORD-LINK-RESTORE is unsafe: capture omits retired and stripped dictionary records and shifts ordinals without remapping the saved checker links. Preserve exact declaration ownership at the capture/startup boundary for all supported existing seed and snapshot paths, including compacted dictionaries. No spelling lookup or producer fallback. Extend existing real export/import and redefinition/retirement behavior coverage before implementation; matching engine, independent Astra review, full native suite and isolated convergence are required.
