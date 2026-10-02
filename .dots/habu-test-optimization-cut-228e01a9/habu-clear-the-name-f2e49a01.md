---
title: Clear the name-lookup scratch at capture
status: open
priority: 3
issue-type: task
created-at: "2026-10-03T22:54:13.062037+03:00"
---

Problem: HIDX-RESET (src/core/checker.f:6912 on 9084b558), which runs at capture, leaves HIDX-H, HIDX-I and HIDX-CUR and the last SYM-FIND's found symbol in DATA, so generation 1 bakes the host's last name lookup: lane 521 (ownrec, commit 6b64131c) measured g1 != g2 at aot/data-cell-values (cmp offset 2170328): g1 holds FNV-1a(CHECKER-REG, vis 1, SEAL) and symbol 19164, g2 FNV-1a(SEAL-CAPTURE) and symbol 164, because the release host and g1 asked NO-RETURN? differently. The chain converges at g2 (g2 == g3), but g1's bytes depend on the host's lookup order rather than on the source. Evidence: $HOME/.cache/tmp/kestrel-jerry-ownrec/HANDOFF.md. Fix: clear every name-lookup scratch cell at capture with the rest of HIDX-RESET's state. Acceptance: rebuild ownrec's tree from the 9084b558 release engine and from its own g1; g1 == g2 with .names; two-generation build rc 0. After: 7b3f85de.
