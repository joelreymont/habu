---
title: "Merged-engine NMIGRATE:DEFINE crashes"
status: active
priority: 2
issue-type: task
created-at: "\"2026-08-16T01:09:50.141217+02:00\""
---

Claim: agent=bake-chain-10 workspace=.jj-ws/habu-bake-chain

Found by bake-chain-9 (2026-08-16) past the wordlist fix: with the two sealed-WID gate refusals bypassed experimentally, the merged engine boots, runs ordinary programs (1 2 + . works), the chain's boot-run installer resolves and runs - but NMIGRATE:DEFINE SIGSEGVs (exit 134, pc 0x10052191c) where the same call on the source-loaded chain returns 0; without the chain's installer it throws 7134 E-PATH-RANGE. Needs a debugger session (docs/debugging.md; lldb: process launch --stop-at-entry THEN br set - the breakpoints-before-run lesson), not inference. Suspects to eliminate in order: a DATA literal the merge rebased into the wrong band; a code literal (csite) value; the DKEEP-HOOK defer state; a chain word whose body holds state the capture snapshotted mid-initialization. Blocks e98b03d4 items (3)-(6) and the boot milestone with 9d7d8e72's gate ruling. Build the tool, don't guess: extend the imgdump/record readers if the state is not visible.
