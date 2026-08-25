---
title: "Tier-parity lint direction: decide the boundary"
status: open
priority: 2
issue-type: task
created-at: "2026-08-15T18:39:09.533638+02:00"
---

Split from d9ca528d (the disk-closure rule landed there; the seven fork-only registrations landed by hand). Open question: should schedule-lint enforce TIER PARITY (a test source in the fork list must also have a SUITE row and vice versa, unless documented)? Measured collateral (lint-sweep lane 2026-08-15): tree-wide Rule B reds 9 more fork-only files (gate-size-attribution-test, match-factor-pin, require-cap-test, tools/ddc-scheduled/-verify/event-closure/include-events/size-report/source-discovery tests) + 59 undocumented cases-only files, mostly lib/ptx and tools/ptx device tests that CANNOT run on the resident host tier - so most tree-wide findings are structurally justified, which makes an undocumented-exception rule wrong as stated. A scope of test/compiler/ alone lands on exactly the seven but rests on 'happens to give seven' - a value heuristic. Decide the structural boundary first (e.g. device-tier tests identified by their own property, not directory), or refute the direction. Existing pragma convention: '\ <lint>-lint: allow-<thing>' via LINT-CONTAINS?. Depends: d9ca528d landing.
