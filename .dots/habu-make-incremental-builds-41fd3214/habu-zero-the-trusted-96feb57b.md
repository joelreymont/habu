---
title: Zero the trusted cell in snapshots
status: active
priority: 2
issue-type: task
created-at: "2026-10-24T06:24:14.845679+03:00"
---

TRUSTED-CELL (src/habu/layout.f:691) is cleared beside DOESB and CLAUSE at every def-open/close/abort and at ARM startup (habu2.f:9199). It is not in snap-lib.f SND-ZERO-LIVE (:258-320), and x64 SNAP-START (boot-x64.f:880-914) resets nothing. This is the exposure Astra showed for CLAUSE-CELL (does lane P2). No failure has been demonstrated yet: first probe a snapshot taken with TRUSTED set. Found by the does re-review. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-trz (brief carl-dots/.out/carl-file/brief-trz.md).
