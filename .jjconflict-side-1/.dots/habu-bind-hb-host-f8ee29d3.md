---
title: Bind hb-host to the build stamp
status: open
priority: 2
issue-type: task
created-at: "2026-08-20T13:14:16.845518+02:00"
---

VERIFIED 2026-08-20 (review stop-ship 2, root-caused): the cast landing refreshed bin/hb in every workspace but not bin/hb-host - the stale host died E-UNDEFINED: CAST: on ir/id.f while the registered wrapper (test/host-run-lib.f:32 hardcodes bin/hb-host) redded. Resolved operationally (fresh host propagated from merge-gate). STRUCTURAL FIX owed: the host must carry the source/product build stamp and the wrapper must REFUSE a mismatched host by name before the long gate - manual binary refresh is the failure mode twice now (the seed-break class). install --force already builds both (build-fixpoint.f:1682); the stamp check is the missing gate.
