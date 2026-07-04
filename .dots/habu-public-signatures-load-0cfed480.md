---
title: "public-signatures: load-faithful package scope for nested unclosed packages"
status: open
priority: 2
issue-type: task
created-at: "2026-07-04T17:59:44.273407+02:00"
---

From TFAM 5 cache-keys work (habu-tfam-5-public-3a692040, landed): PS-PRESCAN-CLOSURE in tools/public-signatures-core.f uses BFS closure order, so for deep, transitively-nested UNCLOSED packages the residual-scope order is not perfectly load-faithful (correct for balanced packages and single-level unclosed, the tested cases; verified no-op on existing stdlib manifests). Fix when it matters: derive scope replay from the ordered event closure in true load order (the TFAM-5 redrive producer gives per-file ordered events; thread residual package state file-by-file in dep order instead of BFS). Add a fixture with a package opened in dep A, continued unclosed through dep B, closed in the entry file.
