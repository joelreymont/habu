---
title: "Name a build's stage when it ends there"
status: active
priority: 2
issue-type: task
created-at: "2026-11-15T22:01:04.535390+02:00"
---

Problem: tools/hb-build-lib.f:384-411 HBB-LINK-FAILED and HBB-EXITED name the build's stage from HB-BUILD-CORE:ROW$, the last top-level file set (tools/hb-build-core.f:117, :133), cleared before each LINKER:WITH (:442, :468, :478, :488): a refusal during a link's open (src/core/check-hook.f ROWS-UNSAID from the linker's PRIMS-IN) says 'the build ended the process before its first file' with no build named, a throw there prints an empty file name, and an end after the last file's load names that file. Acceptance: each message names the build and whether it was opening, loading a named file, or outside its files. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/habu-build-stage.
