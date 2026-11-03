---
title: Point aot-startup-reach-lint at the startup emitter
status: open
priority: 2
issue-type: task
created-at: "2026-10-30T01:13:58.599194+02:00"
---

Problem: tools/aot-startup-reach-lint.f MAIN scans src/habu/aot-lib.f, now a 22-line target loader; the startup emitter lives in src/habu/aot-link-arm.f, so the lint checks none of it and its clean result means nothing. Acceptance: the lint scans the file that holds the emitter and reports a planted unreachable word. Files: tools/aot-startup-reach-lint.f. Verify: bin/hb --load tools/aot-startup-reach-lint.f on master, then with a planted defect. Depends: none. Ownership: tools/aot-startup-reach-lint.f. Claim: unassigned.

Note 2026-11-03: a scratch copy of the lint scanning src/habu/aot-link-arm.f (24 ADR, uses) in place of aot-lib.f also prints "clean", rc 0, so whether its label triples match the linker's ADR, forms is the open question the planted defect answers. No suite row holds the lint clean: test/lint-cli-standalone-load.f only loads each tools/*-lint.f and accepts an exit with findings. Report /Users/joel/Work/reviews/habu-infra-A.md M4 lists this lint and tools/aot-section-reach-lint.f as run by no gate.
