---
title: "Help an engine-made reserve's E-MAP by its owner"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-16T13:46:13.043151+01:00"
closed-at: "2026-11-18T07:35:06.228814+01:00"
---

Problem: src/core/dynamic-storage.f MAP-REJECT (:68-76) helps every refused reserve with 'reserve fewer elements, or release storage the program no longer needs', also for reserves the engine makes itself: the stdin stream's TEXT (src/habu/main.f STREAM/STEP) and the interpreter's LOCAL-NAMES. Under ulimit -v, a 300 MB program piped to hb prints that help though the program reserved nothing. A refusal inside a compile loses its record: src/habu/t0.f COMPILE and COMPILE-FIXED catch the throw and src/compiler/native/definition.f SETTLE-UNIT throws it again from another unit, so it prints E-THROW for code 7138. Acceptance: every E-MAP record's help holds whoever reserved (raise the address-space limit, or reserve less), with no record of what the engine held (one help, no per-site machinery); a refused reserve during a compile prints its E-MAP record; a test fails first. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-maphelp.
