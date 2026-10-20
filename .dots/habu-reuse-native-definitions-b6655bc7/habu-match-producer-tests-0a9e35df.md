---
title: Match producer tests to definition reuse
status: closed
priority: 1
issue-type: task
created-at: "2026-10-19T17:16:42.097949+03:00"
closed-at: "2026-10-19T17:24:50.491328+03:00"
close-reason: "Changed only two obsolete whole-file compile expectations. Independent Astra review traced binding/relocation and value/executed invalidation and cleared it. Existing E2E passes exit0: builds1/0/0,1/0; outputs41/41/41,41/999 and cleanup unchanged. No producer identity/freshness changes."
---

Assertions 10 and 19 in hb-build-prefix-producer-test required caller recompilation after an unrelated preloaded PAD insertion or a called VALUE body edit. Unchanged b6ced8e7 with the supplied host pair (hb SHA-256 3868075185f6eef363f3e938fbe28312e3e1c6a54cd0e91c0c2d6582c53d85df, map 55ee51e1af4295dd1468f1dac6c7742aa4eb74b5dbc0a315a823fce59f63cbf2) failed only these count assertions; every build and runnable output passed.

Changed only those expectations to compiled 0 and explained definition reuse. MAIN binds an ordinary colon call: NUNIT-DEP:OBSERVE-BOUND records its binding contract, NUNIT-OBJECT:DEF-PREP-OPEN checks live dependencies, and DEF-PREP-CALL resolves REF>CODE against the current declaration and patches the call. Fixed values and executed compiler dependencies retain separate VALUE and EXECUTED checks. Nested input acquisition, the cold/warm checks, edit-after-load behavior, runnable outputs and cleanup remain unchanged.

The existing E2E passed with that same pair from the assigned workspace: exit 0, test: ok and hb-build-prefix-producer-test: ok; 84.17 s wall, 62.20 s user CPU, 1.64 s system CPU. The five build expectations are compiled 1/0/0 and 1/0; runnable outputs remain 41/41/41 and 41/999. Both fixture cleanup assertions passed. Run used explicit HABU_UNDER_TEST and an absolute test load from this workspace, EOF stdin, caffeinate -is, and private external HB_TMP/cache under /Users/joel/.cache/tmp/habu-producer-reuse.Pk993H; prefix.stdout and prefix.stderr hold the result and timing.

Implementation complete; keep this dot active pending independent Astra review and integration. Do not alter producer identity or relax source freshness. Root owns review, integration and workspace retirement.
