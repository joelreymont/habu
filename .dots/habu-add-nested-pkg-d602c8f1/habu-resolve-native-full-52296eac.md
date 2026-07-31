---
title: Resolve native full namespace paths
status: active
priority: 1
issue-type: task
created-at: "2026-07-31T06:34:59.946328+02:00"
---

Source dependency: exact reviewed E1 namespace-row candidate; this stop-the-world branch keeps E1 active until M17, so exact code ancestry, not an independently publishable intermediate engine, enforces the dependency. Owner: native dictionary engine. Implement one emitted last-separator scanner used by both lookup and definition. It returns unqualified, valid-qualified with the last colon index, or malformed; leading, trailing, and doubled separators reject before lookup or mutation. Implement one emitted full-prefix ensure walker that reuses E1 LNSFIND and package-row creation for every exact prefix. LFIND resolves the full prefix and accepts package or type rows for lookup. Qualified definition creates missing package prefixes, accepts an existing package row, and rejects an existing type row. Preserve E1 record shape and rollback; no parent link, side table, compatibility spelling, version, second scanner, ancestor lookup, using semantics, nested package blocks, or recovery edits. Write set: src/habu/habu1.f, src/habu/habu2.f, test/gate-dictionary-lib.f, and only necessary current TRUSTED.md rows. Pre-M17 proof is source census and hunk review only. M17 acceptance: deep lookup/definition at arbitrary supported depth, malformed paths publish nothing, package and type lookup work, defining into a type rejects, and shallow behavior remains exact. Claim: agent=e2a_native_impl workspace=.jj-ws/habu-resolve-native-full-52296eac.
