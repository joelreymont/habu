---
title: Spawn-level fault injection for boot-path coverage
status: open
priority: 2
issue-type: task
created-at: "2026-07-16T21:25:48.679997+02:00"
---

Residual from habu-diagnose-fixed-va-ed649528 (landed): the fixed-VA mmap diagnostic's runtime forced-failure proof was gathered manually via a compare-flip on a stage binary - not repeatable in-gate. No reliable cross-platform force exists from checked Habu today: no setrlimit primitive; macOS does not honor ulimit -v/RLIMIT_AS; MAP_FIXED replaces mappings so pre-mapping cannot force ENOMEM. Work: a typed spawn-harness fault-injection capability - candidates: (a) a setrlimit primitive (native + stage0 mirror + checker effect row) applied in the child between fork and exec where the platform honors it (Linux RLIMIT_AS works; macOS needs an alternative), (b) a build-time fault-arm flag that compiles a stage binary with an inverted success compare under an explicit test-only knob (the manual proof mechanized, install-guarded so bin/hb can never bake it), or (c) posix_spawn attr resource limits where supported. Acceptance: an in-gate test forces BOTH fixed-VA mmap failures and asserts exit 78 + the exact 33-byte fd-2 diagnostic; the mechanism cannot leak into production binaries (prove: fixpoint byte-identical with the knob absent); works or documents per-platform honestly. Files: lib/process*, src/habu/habu2.f + mirrors if (a), tools/build-fixpoint.f if (b), test coverage. Ownership: test-harness fault injection.
