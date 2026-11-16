---
title: "Keep a check run's records past its stderr cap or deadline"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T16:26:06.277776+01:00"
---

Problem: tools/check-core.f captures the run's stderr into CHK-ERR-CAP (128 KiB) to replay it; a run that writes past it is killed with E-PROC-TRUNCATED (CHK-RUN-TOO-BIG), and a run still going at its deadline is killed with E-PROC-TIMEOUT (CHK-RUN-LATE); either way none of the records it wrote is replayed, though --load prints them. habu-pass-a-check-e6b3fc97 lifts only the stdout cap. Acceptance: a checked program that writes 1 MB of records to stderr, and one that writes a record then passes its deadline, show under tools/check.f every record the run wrote, then the stop. Depends: habu-keep-a-program-2265ff23. Claim: unassigned.
