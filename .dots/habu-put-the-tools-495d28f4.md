---
title: Put the tools fixture makers under HB-TMP-MKDIR
status: open
priority: 3
issue-type: task
created-at: "2026-09-22T18:40:25.041191+03:00"
---

Problem: twelve tools fixtures still make their trees with TMPDIR-MKDIR (tools/hb-build-test.f:752, hb-cli-contracts-test.f:114, json-only-test-lib.f:62, hb-baseline-contracts-test.f:75, aot-lint-test-lib.f:97, json-file-test.f:38, check-all-errors-test.f:344, object-image-test.f:48, ddc-verify-test.f:42, engine-size-test.f:96, chain-run-test.f:29, lint/slab-test.f:40), which takes TMPDIR then /tmp and never HB_TMP, so under the pool (HB_TMP = the slot directory) they land in /tmp and outlive a killed child; 17 habu-hb-build-* trees sit in /tmp now. Acceptance: every maker resolves through HB-TMP-MKDIR (lib/fs-mutate.f:289); rg TMPDIR-MKDIR under tools/ test/ lib/ answers only the resolver; a full run under HB_TMP adds no /tmp entry with those prefixes. Files: the twelve tools files. Verify: rg; test/run.f under HB_TMP with an ls /tmp count before and after. Ownership: hazel. Claim: unassigned.
