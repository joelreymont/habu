---
title: Seal set-check behind the friend latch
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T02:22:45.564826+02:00"
---

From docs/design-tfam-2b-i.md out-of-scope flag: set-check installs an arbitrary xt as the checker hook (writes HOOK-CELL via its own STR, not the guarded ! sink) - a checker-bypass + execution sink that stays user-callable after 2b-i lands. Once the friend latch exists, gate set-check (and TRUSTED: which uses it) so user source cannot silently swap the checker hook post-seal; engine/friend path and the audited bootstrap launcher keep working. Fixtures: user set-check rejects post-seal; test files that legitimately use set-check (test/engine-suite.f:1191+, test/prop-test-core.f:22+, test/gate-aot-negative-lib.f:8) run through a test-mode friend path or migrate. Depends: 2b-i.
