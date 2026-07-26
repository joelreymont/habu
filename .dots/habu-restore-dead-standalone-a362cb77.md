---
title: Restore dead standalone lint CLIs
status: active
priority: 2
issue-type: task
created-at: "2026-07-26T10:20:51.606392+02:00"
---

Why: tools/signature-lint.f, tools/reserved-name-lint.f, and tools/duplicate-definition-lint.f fail to load standalone with E-UNDEFINED: ARGV:LABEL? - lib/argv.f exports LABEL$ and REQUIRE-LABEL, and LABEL? does not exist. They pass inside test/run.f as unit tests through a different load path, so the gate never exercises the CLI entries and the standalone commands are silently dead (found 2026-07-26 by the MODELPROV lane). Behavior: root-cause the divergence (an argv surface rename not migrated in the CLI wrappers), migrate the three CLIs to the real lib/argv.f surface - no forwarding shim in lib/argv.f - and add a gate leg that loads each standalone lint CLI through its real bin/hb --load path so a dead CLI is a red finding, not a silent gap. Owner: the three CLI files' existing packages, plus the owning gate suite for the new leg. Dependencies: none. Acceptance: all three CLIs load and run their usage path rc=0 through bin/hb --load; the new gate leg goes red when a CLI entry word is undefined, proven once by mutation; hostile fixture: a CLI that loads but throws during argument parsing must still be distinguishable from a dead load.

Amended 2026-07-26: scope grown by one same-class instance - standalone test/gate-stdlib-lint-tools.f is red on master with E-UNDEFINED: LINT-LEX:COUNT (reproduced on a clean tree by the S6b1 lane); the same slice through test/run.f is green, so the gate never sees the dead standalone entry. Fix it in this lane with the same discipline: migrate to the real surface, and the new gate leg must load every standalone lint entry point (the three CLIs and this gate file) through its real bin/hb --load path so a dead entry is a red finding. Claim: agent=deadcli workspace=.jj-ws/habu-dead-cli
