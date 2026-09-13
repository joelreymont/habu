---
title: Release process argument and environment caches before image capture
status: active
priority: 1
issue-type: task
created-at: "2026-09-13T21:51:00.321389+03:00"
---

The argv and environment builders cached OS mappings in ordinary DATA pointer cells. Their RESET words cleared counts and offsets only, so APP-IMAGE retained the original heap addresses. On maki-pin source10d66991 (bin/hb SHA b3ab5b3b2eff14fa3535df2a103cc4b0703423d381741f51d0d953f68c757495), this public sequence succeeded before capture and the restored PROCESS-WARM faulted at pc0x40e3a8, rc134:

```forth
require src/habu/app-image.f
: PROCESS-WARM ( -- ) PROC-ARGV-RESET s" safe" >LEN PROC-ARGV+ ;
PROCESS-WARM
s" <private-directory>/warm" APP-IMAGE:SAVE
```

Independent PROC-ENV+ and PROC-CMD command controls identified the same defect. Maki's retained warm REFEREE image faults at the same byte-store instruction through its old mapping. A separate BUF/JR disposal and reinitialization control passes; no referee-hook rewrite is justified. The process source was unchanged through d7184652.

Implemented in lib/process-argv.f and lib/process-env.f: each owner registers cleanup before its first allocation, so registration failure cannot strand an untracked mapping. Capture releases all five mappings, clears cache addresses, counts, offsets, argv pointer rows and environment defaults, then clears registration state for later use. Argument-free PREPARE also registers its pointer rows. Ordinary RESET semantics and public process APIs are preserved. docs/stdlib.md states the process-state lifetime.

Validation: the changed source on the actual Maki pin passes the ordinary argv/env/command suites. The registered test/process-image.f passes through a cold capture, two warm recaptures and final restored use, outside the source checkout, with a controlled environment and a real shell command. Its subject checks all five cache addresses, counts, offsets, default lookup and every argv pointer row, including argument-free preparation and repeated cleanup. The same actual-source subject passes with the changed library bodies compiled at both tiers. Existing failing controls already establish the bug; no further fault runs are needed.

Outstanding acceptance: independent Astra review, integration and the full suite on a coherent rebuilt current Habu engine, then Maki's warm REFEREE recapture lane. Current d718 private B1 cannot load its app-image.f (snd-canon-origin is refused at SND-XT-CELL!, expected n n / actual a ptr b); no current-B1 image pass is claimed. Allocation failure is covered by registration-before-allocation ordering, not injected. Related contract dot: habu-state-what-survives-a10d0aa1. Owner: Cedar; bounded implementation in .jj-ws/cedar-process-lifecycle.
