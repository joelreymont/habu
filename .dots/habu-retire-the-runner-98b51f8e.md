---
title: "Retire the runner's aot-mode prepend"
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T07:17:11.186523+03:00"
---

Problem (measured by the native-tail lane, e42b6695): test/gate-stdlib-lib.f SUITE-AOT? prepends test/compiler/aot-mode.f ('1 set-tier') to every 'test/compiler/native-*.f' row (77 in test/gate-stdlib-cases.f) and to test/compiler/codegen-tail-probe.f, so a suite whose assertions are tier-1 facts is green under test/run.f and red alone without saying why: test/compiler/native-tail.f was (fixed: it selects the tier itself), test/compiler/codegen-tail-probe.f still is (standalone '15' / 'test: failures', rc 1, /tmp/hazel-native-tail/probe-alone.log), and the other rows are unmeasured. The tier is a property of the code under test, not of the runner. Acceptance: every row the adapter covers is run standalone under env -i with a scratch HOME; each one that is red at tier 0 selects '1 set-tier' before its requires with a one-line reason (precedent: test/aot-seeded-address-sites.f:2, test/native-fixture-write.f:6, native-tail.f); SUITE-AOT?, SUITE-ARG+'s prepend and test/compiler/aot-mode.f are removed; docs/gate.md's 'How a suite runs' list states the rule (a suite that depends on the tier selects it itself); test/run.f green. Files: test/compiler/native-*.f as measured, test/compiler/codegen-tail-probe.f, test/gate-stdlib-lib.f, test/compiler/aot-mode.f, docs/gate.md. Verify: each changed suite alone; test/run.f. Depends: none. Ownership: hazel (gate/test load path). Claim: unassigned.
