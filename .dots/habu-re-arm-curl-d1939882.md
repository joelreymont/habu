---
title: Re-arm curl image cleanup after each capture
status: active
priority: 2
issue-type: task
created-at: "2026-09-20T02:02:45.053381+03:00"
---

lib/net/curl.f FORGET-GLOBAL clears GLOBAL-DONE but not GLOBAL-REGISTERED; IMAGE-LIFECYCLE:PREPARE consumes the hook, so subsequent GLOBAL-READY skips cleanup registration and later images can retain initialized state from another process. Source finding from FFI lifecycle reduction (9c283543). Claim: alder, .jj-ws/alder-lifecycle-followups; Hazel says fix after the current stripped-lifecycle/FFI/deadline stack. Measure repeated initialization/prepare cycles, reset the registration at its owner, and prove restored images initialize correctly. Keep scope to this lifecycle defect and its fixture.

Claim moved to .jj-ws/alder-curl-recapture, based on the reviewed lifecycle/FFI/
deadline stack 27219507. A public INIT/prepare/INIT/prepare probe gives hook counts
2,1,1,1 on its private gen3 (the retained hook is FFI's), proving curl does not
rearm its cleanup. Probe: /tmp/alder-capture-persistent/curl-hook-probe.f.

FORGET-GLOBAL now clears GLOBAL-REGISTERED together with GLOBAL-DONE. The
regression performs two INIT/CLEANUP/PREPARE cycles before starting any server
task: each INIT must add one hook, and each PREPARE must consume it. Baseline
fails assertion 3 (expected 2 hooks, got 1); fixed complete curl-http registry
row passes, including the existing loopback HTTP cases. This pins preparation
and reinitialization; no new saved/restored-image test or external HTTPS call.

Private parent gen3 host 31be3fb0, isolated tree and HOME/HB_TMP under
/tmp/alder-curl-recapture; before.log and after.log record the regression.
Astra review clear after adding the fixture's explicit lifecycle dependency.
No engine change or full gate; Hazel chains after the lifecycle/FFI prerequisite.
