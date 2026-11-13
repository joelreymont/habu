---
title: Delete the uncalled native unit import path
status: active
priority: 2
issue-type: task
created-at: "2026-11-13T10:00:09.668210+02:00"
---

Problem: NPUB:IMPORT-FILE-UNIT (src/compiler/native/publish.f:380) has no caller, so IMPORT-UNIT, PUBLISH-UNIT and the native-unit-publish primitive (src/habu/prims.f:644-647, habu1.f NPUBWIN:BNATIVEUNITPUBLISH, kernel-x64.f NATIVE-UNIT-PUBLISH,) are dead, and comments at src/habu/xref.f:301-303, habu1.f:4629-4630 and kernel-x64.f:445-446, :3577-3578 describe the path as live. Acceptance: the path and every word only it reached are gone; no comment describes it. Depends: none. Claim: agent=carl workspace=.jj-ws.noindex/carl-npub.
