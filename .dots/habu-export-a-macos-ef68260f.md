---
title: Run an exported macOS engine on a Mac
status: open
priority: 2
issue-type: task
created-at: "2026-11-25T18:10:26.238750+02:00"
---

Problem: a macos-aarch64 engine built on Linux (`bin/hb --load tools/hb-build.f -- --engine --target macos-aarch64 -o OUT`, rc 0) has not run on a Mac since the sessions left it. Acceptance: on a Mac the exported engine rebuilds itself byte for byte (before signing) and passes test/run.f. Depends: a reachable Mac. Claim: unassigned.
