---
title: Single edit point for trusted-word bookkeeping
status: closed
priority: 2
issue-type: task
created-at: "2026-07-18T23:21:11.280481+02:00"
closed-at: "2026-07-19T02:10:46.765108+02:00"
---

Every TRUSTED: word today needs two hand-maintained entries: a TRUSTED.md markdown table row AND a line in the trusted-inventory-classes block, and the lints check each separately - the mirror is the gratuitous part. Design: the class/dot annotation becomes a column of the one markdown table; the trusted-inventory ratchet derives its per-site facts by parsing that table (tools lint side) and compares against source reality (the actual TRUSTED:/TRUST sites found by scan). Adding a trusted word then means writing exactly one table row; drift between table and source stays a loud lint failure (that check is the valuable one and stays). Touches tools/trust-lint*, the inventory block format in TRUSTED.md, and the lint tests. Serialize behind habu-retire-deftype-onto-07227854 stage A (both edit TRUSTED.md).

RELATION MAPPED, OPTION 1 ADOPTED 2026-07-19 (orchestrator, from the feasibility lane's stop report; zero edits made). The table and the classes block are NOT mirrors: 723 table rows vs 914 class rows over 1059 repo-wide sites of 5 kinds; only 573 rows are genuinely duplicated; 330 class rows have no table row (test-only + non-word sites: set-check, hook installs, bare TRUST); 150 table rows are absorbed by 11 deliberate file-level fold rows protected by fold-baseline (contested-file merge protection - must survive). Full unification is either lossy or fails to delete the block, so it is rejected. ADOPTED (lossless): add Class and Owner columns to the markdown table, populated for exactly the 573 manifest-word rows; trusted-inventory resolves class/owner for those from the table by (file, Word); the residual machine block shrinks to what the table structurally cannot hold (11 fold rows + fold-baseline, 28 non-word sites with counts, 308 test-only sites) and the inventory still validates all 1059 source sites exactly as strictly. End state: adding or retiring a manifest trusted WORD is one table-row edit; the 341-line residual block carries only non-word/test/fold facts. Key files: tools/trust-lint-core.f, tools/trusted-inventory.f, tools/lint/lib.f TRUST-SITE?, TRUSTED.md (table 34-758, block 945-1861, fold rationale 902-918).
