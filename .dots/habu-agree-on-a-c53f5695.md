---
title: Agree on a named stream input
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T00:04:02.325515+01:00"
---

Problem: bin/hb --load /dev/null loads and exits 0, and bin/hb FIFO blocks until killed (the loader opens it without O_NONBLOCK), while tools/check.f rejects each with E-UNREADABLE-SOURCE 'is not a regular file', rc 74 (tools/check-core.f CHK-NO-INPUT). A Unix socket file as a source fails its open ENXIO and gets 'cannot open (os error 6)' with the help 'check that the file exists and is readable' (src/core/include.f OPEN-ERRNO$). tools/check.f --all-errors on a file that requires a FIFO hangs: the walk keeps its stop and the composition's reader opens the FIFO, which blocks before src/habu/verify-source.f REFUSE-SOURCE-READ can reject it. Acceptance: the load and check.f give a named or required input that is no regular file (FIFO, device, socket) the same outcome and record. Depends: habu-campaign-c4-diagnostics-3b6de147. Claim: unassigned.
