---
title: Fix readlist base>10 parsing and add readbase probes
status: closed
priority: 1
issue-type: task
created-at: "\"\\\"2026-03-07T19:20:07.518447+01:00\\\"\""
closed-at: "2026-03-07T20:01:04.652144+01:00"
close-reason: done (rewrote lib/maxima-post-load.lisp readlist to trust parse-integer consumed position under the active base instead of hard-coding B/D/E/F exponent exclusions; validated with direct ./zig-out/bin/habu probe showing 0D4/0E0/0F1/0B8 parse to 212/224/241/184 at *read-base*=16)
---

lib/maxima-post-load.lisp:1-28; lib/stdlib.habu:4201-4325; ../maxima/tests/test_readbase_maxima.mac; ../maxima/tests/test_readbase_lisp.lisp. Root cause: readlist treats valid hex digits b/d/e/f as exponent markers and silently misreads numbers. Fix: make base>10 integer parsing respect valid digits and add focused readbase validation. Why: readbase-related results are currently unreliable.
