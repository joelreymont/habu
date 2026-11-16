---
title: "Give a --build entry the loader's record"
status: open
priority: 2
issue-type: task
created-at: "2026-11-16T05:23:25.598738+01:00"
---

Problem: src/habu/main.f STREAM reads the first --build file itself through FILE (main.f:140-142), so a missing, unreadable, empty-named or over-long path prints the plain line 'hb: cannot open PATH' and exits 74, while every other command-line entry gets the loader's Rust-style record (hb P, hb --load P, later --build files). It also bypasses the stream and search checks the loader applies: `hb --build FIFO` blocks forever, `hb --build /dev/null` exits 0, and a socket prints the plain line. Acceptance: hb --build P for a missing, unreadable, unsearchable, stream or over-long P prints the same record and exit status as hb --load P. Depends: ed1db0a7, c550f89a (their entry records). Claim: unassigned.
