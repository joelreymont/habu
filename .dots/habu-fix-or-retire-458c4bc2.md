---
title: Fix or retire test/aot-native-does.f
status: open
priority: 3
issue-type: task
created-at: "2026-10-24T02:34:21.830319+03:00"
---

test/aot-native-does.f is in no gate suite and fails on master: `bin/hb --load test/aot-native-does.f -- <out>.aot` gives rc 70 E-UNDEFINED: NSHADOW:SOURCE-FUNCTION (master), rc 67 'ncomp: cannot compile MAKER' with e122d857's engine. Logs: ~/.cache/tmp/jerry-nocheck.ohyX/aot-native-does*.log. Decide whether its coverage is already held by test/aot-shadow-capture.f (registered); if so delete it, else repair it and register a SUITE row.
