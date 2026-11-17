---
title: Shared BUF wipe, curl loop and closed JSON reader
status: open
priority: 2
issue-type: task
created-at: "2026-11-17T15:34:08.775178+01:00"
---

Goal: one shared word each for a BUF's whole-capacity wipe, running a step under AIO's and CURL's loops (CURL:LOOP-START logging and throwing its own failures), and reading a closed JSON object with line:column rejections, replacing the copies in sapd, Tender, Zen and Habu (src/lib/net/{mcp,bearer,model}.f, model-test.f:131-134, whose cleanup skips AIO:STOP when LOOP-STOP throws). Acceptance: the copies are gone and the byte-buffer, curl and json-read suites pass. Depends: Joel's names. Claim: unassigned.
