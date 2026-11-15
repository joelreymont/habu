---
title: Keep the low-speed test clear of libcurl rounding
status: open
priority: 2
issue-type: task
created-at: "2026-11-15T18:14:01.606516+02:00"
---

Problem: src/lib/net/curl-test.f DRIBBLE-LOW-SPEED (:1725) runs a 1 s low-speed window (LOW-SECONDS :105). libcurl 8.23.0 times that window in ms rounded toward zero (lib/progress.c:141-150, lib/curlx/timeval.c:194) while its first once-a-second speed record (lib/progress.c:488) does not exist yet, so a check at 999.x ms sees speed 0 and ends a healthy transfer with code 28. The curl CLI shows it: one byte every 0.3 ms with --speed-limit 10 --speed-time 1 fails 6 of 6 at about 1005 ms; --speed-time 2 passes. Acceptance: LOW-SECONDS is 2 and the dribble outlasts it (DRIBBLE-GAP-MS raised so the body takes over 2 s, DRIBBLE-CEILING still holding); :105's comment no longer calls 1 s a window libcurl measures; the libcurl defect is reported upstream with the CLI reproducer. Depends: none. Claim: unassigned.
