---
title: Zero the trusted cell in snapshots
status: closed
priority: 2
issue-type: task
created-at: "2026-10-24T06:24:14.845679+03:00"
closed-at: "2026-10-31T10:03:19.150003+02:00"
close-reason: "No failure on arm64 (probe, carl-trz, installed hb). A TRUSTED: compile cannot reach a capture with the cell set: APP-IMAGE:SAVE inside one refuses rc 74 'snap: active compiler state at capture' (snap-lib.f VERIFY-QUIESCENT, PEND-CELL set). A direct store does reach the image (byte at DATA-OFF + 8 + TRUSTED-CELL reads 1 vs 0), but first use prints 0 for both: boot runs EM-SNAPSHOT-RESTORE then EM-STARTUP-RUNTIME-STATE, which zeroes TRUSTED-CELL (habu2.f:8825) before anything reads it. Open/close/abort clear the cell and capture refuses an open definition, so at any legitimate capture it is already 0; zeroing it in SND-ZERO-LIVE would only canonicalise an image no supported path writes. x86-64 not probed."
---

TRUSTED-CELL (src/habu/layout.f:691) is cleared beside DOESB and CLAUSE at every def-open/close/abort and at ARM startup (habu2.f:9199). It is not in snap-lib.f SND-ZERO-LIVE (:258-320), and x64 SNAP-START (boot-x64.f:880-914) resets nothing. This is the exposure Astra showed for CLAUSE-CELL (does lane P2). No failure has been demonstrated yet: first probe a snapshot taken with TRUSTED set. Found by the does re-review. Claim: agent=carl worker workspace=.jj-ws.noindex/carl-trz (brief carl-dots/.out/carl-file/brief-trz.md).
