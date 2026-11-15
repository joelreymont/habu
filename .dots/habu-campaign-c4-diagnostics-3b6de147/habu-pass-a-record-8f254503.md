---
title: "Pass a record exactly at the engine's rethrows"
status: closed
priority: 2
issue-type: task
created-at: "2026-11-15T12:34:21.127647+01:00"
closed-at: "2026-11-16T04:07:35.219535+01:00"
---

Problem: REJECTION:CARRIED? (rejection.f) compares the stamp with the throw count only, so a re-PASS of a code with no kept record (include.f LOAD 2288, INCLUDED-AS 2357, REQUIRE 2380; interpreter.f EVAL-FRAMED-RANGE end) makes a record-less pass carried and the root prints nothing; and a re-PASS revives a kept record of an earlier failure of the same code, because it stamps whatever is kept. Acceptance: carried means a record is kept and its throw is the newest; each engine rethrow passes the record on only if it was carried when its catch landed (LOAD-RESULT include.f:2270 and EVAL-FRAMED-RANGE note it then), else throws plainly. Depends: none. Claim: agent=jerry workspace=.jj-ws.noindex/jerry-c4m2 (in the integration stack, closes at landing).
