---
title: Package the six snapshot relocation engine words
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-29T21:34:38.649724+02:00\""
---

Full context: the step-2 relocation WIP (commit e50fb3ec in .jj-ws/habu-relocate-snapshot-region-752042fe) introduces six new words in src/habu/habu2.f: LSNAPCALL, SNAPCALL-MSG-LEN, BL-OPCODE-HI, LSNAPRBC, EM-SNAPSHOT-REBASE-CALLS, EM-SNAPSHOT-REBASE-DATA-XT. The ENGINE-BODY-EDIT exemption from commit a943eb40 deliberately admits only body edits - new global definitions in habu2.f still fail the package gate (verified 2026-07-29 by probe: new global PROBE-GLOB rc=1). These six words need a real package owner (or placement in an already-packaged snapshot module) before the WIP can commit. Depends on the layout.f/snap-lib.f packaging dot only if the words move into snap-lib.f. Acceptance: package-diff-lint exit 0 on the exact step-2 diff; engine fixpoint rebuild; the 200-consecutive-clean-boot acceptance run from the parent relocation dot still applies to the campaign, not this dot.

Claim: agent=snapreloc workspace=.jj-ws/habu-relocate-snapshot-region-752042fe
