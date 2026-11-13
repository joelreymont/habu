---
title: Bind free and realpath at load, not per call
status: closed
priority: 2
issue-type: task
created-at: "2026-11-13T10:12:00.911193+02:00"
closed-at: "2026-11-13T12:13:12.164957+02:00"
---

Problem: the arm64 REALPATH primitive (src/habu/habu1.f) looks up free and realpath by dlsym on every one of 5,746 calls in a warm engine build; the macOS fork body looks up fork the same way. Acceptance: REALPATH and the macOS fork body load their functions from the slots the writers fill (dot habu-write-the-engine-b5cd5220) and make no dlsym call; macOS and Linux arm64 engines built through a stage of habu-write-the-engine-b5cd5220 run. Depends: none. Claim: agent=worker workspace=.jj-ws.noindex/jerry-imports
