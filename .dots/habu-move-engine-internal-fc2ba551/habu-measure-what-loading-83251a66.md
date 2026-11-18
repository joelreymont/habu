---
title: Measure what loading ten library files before the seal costs a program
status: closed
priority: 2
issue-type: task
created-at: "2026-11-18T16:56:22.483996+01:00"
closed-at: "2026-11-18T17:10:25.555976+01:00"
---

Problem: the DATA-cell design (3499c957, 159567d9) loads fmt.f, the FS files, at-exit.f, fork-child.f, codegen.f, ffi-callback.f and genio-core.f before the seal in every program build; its cost to a program's size and build time was not measured. Acceptance: size and cold and warm build times of a minimal program and of one that uses fs.f and fmt.f, with and without them. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-dm.
