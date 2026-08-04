---
title: search-wl walks the whole dictionary
status: open
priority: 3
issue-type: task
created-at: "2026-08-04T10:47:48.104640+02:00"
---

Full context: BSWL in src/habu/habu1.f is the search-wl primitive - the Habu-callable name lookup in one wordlist - and it scans every dictionary record from index 0 to NDICT with no early exit, keeping the LAST match. Measured: 8.0 us per call at ndict 11344 and 9.6 us at 13968, whether the name is present or absent, which is 0.59 ns per record scanned. It is not on the engine's compile path (LFIND is, and LFIND now probes the hash index - dot habu-compile-shaped-cost-4e74a181), so this is a user-facing and test-facing cost rather than a compile one: test/engine-suite.f alone calls it a dozen times to assert absence, and src/core/layout-buffer.f calls it per definition to reject duplicates. The same table answers it: the record's own wid is the key the index already stores, so a probe with the caller's wid finds the row or proves it absent, with the scan kept for the two cases LFIND keeps it for (no table, chain exhausted). One semantic point to settle first and pin with a fixture: the scan keeps the LAST match while a probe returns the one on the chain, and the two agree only because a wordlist rejects duplicate tails - assert that agreement before removing the scan. Depends: src/habu/habu1.f BSWL, C-HIDX-HASH; test/engine-suite.f. Ownership: search-wl primitive. Claim: unassigned.
