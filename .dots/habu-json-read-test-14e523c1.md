---
title: "json-read-test's release check fails under CPU load"
status: active
priority: 2
issue-type: task
created-at: "2026-11-11T08:58:57.294110+02:00"
---

Problem: src/lib/json-read-test.f JRT-RELEASES (:939) frees two 4096-byte probe mappings, runs a body, maps two probes again and asserts they land at the old addresses ('relies on the kernel's first-fit reuse of a freed mapping', as src/lib/byte-buffer-test.f BUFT-DISPOSE-RELEASES does). The tenth case, 'VALUE= past 64 levels releases every record's stack, a throw included' (JRT-DEEP-VALUE-EQ :1053), fails when the Mac is loaded: asserts 101693 and 101682, 13 of 25 runs of bin/hb --load test/run.f -- tail-pure-fixtures on the cross-OS candidate and at least 11 of 25 on the installed hb (61f1c91f's tree) with 24 busy loops (yes > /dev/null) on 12 cores, 0 of 30 on a quiet machine; it turned the landing suite red once (carl-m3/r17/suite.log) while other work ran. The other nine cases never failed. Not known: what maps or holds memory during that body only under load. Acceptance: the cause is named (what takes the hole, with path:line or the kernel behaviour shown by a reduced program); either the leak or late release is fixed, or the test checks release by a measure that does not depend on address reuse; 0 failures in 50 loaded runs. Files: src/lib/json-read-test.f, src/lib/json.f (expected). Verify: the loaded loop. Depends: none. Ownership: carl. Claim: agent=carl workspace=.jj-ws.noindex/carl-json (root-cause hunt, brief carl-layout/.out/briefs/hunt-json-release-under-load.md; the fix is a second brief).


Cause (hunt, 2026-11-11): the release check never fails. The red asserts 101682 and 101693 are JRT-TEST-VALUE-EQ-DEPTH-TIME and JRT-TEST-VALUE-EQ-MEMBERS-TIME (src/lib/json-read-test.f), which set no label and so reported under the VALUE= case's. JRT-EQ-LEASTS timed them with TIME:MONO-NS, wall time that includes descheduled time; under 24 busy loops the 8000 timing passed 3 times the 4000 one in 10 of 40 runs, and "t8 >= 3*t4" predicted all 40 outcomes. With TIME:THREAD-CPU-NS the same load gave 0 red of 40, worst ratio 2.10 (scratch ~/.cache/tmp/carl-json2). Fix: commit "Time the JSON equality bounds on thread CPU time" in carl-json (thread CPU clock, a label on each bound); quiet run rc 0. Left before closing: the loaded loop on the committed file, run when no worker is testing.
