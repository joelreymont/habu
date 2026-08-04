---
title: Retire 0 set-check sites
status: open
priority: 2
issue-type: task
created-at: "2026-07-01T22:54:40.854598+02:00"
blocks:
  - habu-seal-set-check-b3676b33
---

Invariant: no production caller can disable checking through an unowned
`0 set-check` span. Retire a site when the checker can express it; otherwise
the source itself names the exact unexpressible operation, retirement owner,
and focused production-path test.

Exact live executable-source census on this contract base: 21 sites, excluding
comments and inert string fixtures but including production generators:

- `src/core/internal-mark.f:48`
- `src/habu/build.f:14`, `src/habu/maker.f:9`, and
  `src/habu/hide.f:30` (`BFR-CHECK-OFF`)
- `tools/codegen-role.f:334` (`CGR-EVALUATE-UNCHECKED`)
- `tools/hb-build-lib.f:716` (`HBB-RESET-RUNTIME-SOURCE`)
- `tools/check-core.f:1114` (`CHK-BUILD-PREFIX`)
- `maki/eval/device.f:123` (`GRADE-WRITE-UNCHECKED-DRIVER`)
- `test/effect-read-api-test.f:45`, `test/rigid-region-suite.f:24`,
  `test/prim-link-test.f:42`, and `test/compile-preflight-recovery.f:17`
- `test/engine-suite.f:341,701,739,786,1028,1245,1809`
- `test/prop-test-core.f:223,336`

Result: probe every site against the current checker, replace each expressible
span with checked code, and keep only genuine seed/bootstrap,
production-generation, or test-metaprogramming boundaries. A literal fixture
containing `0 set-check` is not a live site. Do not add a global ledger,
inventory, count ratchet, or lint; the authoritative census is the executable
source.

Acceptance: a fresh structural source census returns exactly these 21 live
sites and distinguishes the three production generators from inert string
fixtures; every retained site has source-local rationale, this or another live
retirement owner, and a focused production test; deleting its check-off
operation or replacing it with a checked path proves whether it is still
load-bearing. No unowned ordinary load, stdin, REPL, or generated source path
can disable checking. Run the focused site tests, top-row/hook tests,
bootstrap, fixpoint, package and typed-local gates, then the full native gate.
