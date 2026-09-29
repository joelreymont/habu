---
title: Run stdin gate rows without draining the pool
status: closed
priority: 2
issue-type: task
created-at: "2026-09-29T13:58:20.594593+02:00"
closed-at: "2026-09-29T14:58:45.933861+02:00"
close-reason: Landed on master in qyqrlxww; cold-cache native gate passed 494/494 (final-gate-cold.log) with stdin rows running beside the pool.
---

Problem: ITEM-RUN-STDIN (lib/test/suite.f:302-307) calls DRAIN before and after every SUITE-STDIN row. The drains date from the framework's extraction; the gate now runs stdin rows as ordinary pool slots fed by one atomic pipe write (SUITE-HB-RUN-STDIN, test/gate-stdlib-lib.f:101-103; GT-POOL-START-STDIN, test/gate-pool.f:916-937), and it installs GT-POOL-DRAIN-SOFT as DRAIN (gate-stdlib-lib.f:135), so each stdin row is a full pool barrier. In native-gate.log the 0.1 s source-stdlib-stdin row (test/gate-stdlib-cases.f:1100) waited for native-window-owner (219 s, log lines 424-425) and held back the four hb-build rows registered after it; a schedule model that reproduces the pool phase to 1 s gives 531 s without the barrier against 689 s with it. Acceptance: ITEM-RUN-STDIN runs its item without draining; sequential items, group headers and the end of RUN still drain; the DRAIN-N expectation in lib/test/suite-test.f:142 states the remaining count; source-stdlib-stdin passes at its current registry position. Files: lib/test/suite.f, lib/test/suite-test.f. Verify: bin/hb --load lib/test/suite-test.f; the gate run shared with the sibling dots. Depends: none. Ownership: lib/test/suite.f, lib/test/suite-test.f. Claim: agent=kestrel workspace=.jj-ws/stdin-drain.
