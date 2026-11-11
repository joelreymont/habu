---
title: "json-read-test's release check fails under CPU load"
status: open
priority: 2
issue-type: task
created-at: "2026-11-11T08:58:57.294110+02:00"
---

Problem: src/lib/json-read-test.f JRT-RELEASES (:939) frees two 4096-byte probe mappings, runs a body, maps two probes again and asserts they land at the old addresses ('relies on the kernel's first-fit reuse of a freed mapping', as src/lib/byte-buffer-test.f BUFT-DISPOSE-RELEASES does). The tenth case, 'VALUE= past 64 levels releases every record's stack, a throw included' (JRT-DEEP-VALUE-EQ :1053), fails when the Mac is loaded: asserts 101693 and 101682, 13 of 25 runs of bin/hb --load test/run.f -- tail-pure-fixtures on the cross-OS candidate and at least 11 of 25 on the installed hb (61f1c91f's tree) with 24 busy loops (yes > /dev/null) on 12 cores, 0 of 30 on a quiet machine; it turned the landing suite red once (carl-m3/r17/suite.log) while other work ran. The other nine cases never failed. Not known: what maps or holds memory during that body only under load. Acceptance: the cause is named (what takes the hole, with path:line or the kernel behaviour shown by a reduced program); either the leak or late release is fixed, or the test checks release by a measure that does not depend on address reuse; 0 failures in 50 loaded runs. Files: src/lib/json-read-test.f, src/lib/json.f (expected). Verify: the loaded loop. Depends: none. Ownership: carl. Claim: unassigned.
