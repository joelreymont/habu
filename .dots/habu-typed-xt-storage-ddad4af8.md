---
title: Typed xt storage cells (xt<effect> as cell type)
status: active
priority: 2
issue-type: task
created-at: "\"2026-07-15T23:32:34.908796+02:00\""
---

Capability prerequisite for habu-checker-exec-of-5923c543 (soundness: stored-xt execute laundering), specced by the xtprov RCA 2026-07-15: add xt<effect> as an admissible storage-cell type. Extend CHECKER-STORAGE-INFO (src/core/checker.f ~2706, currently rejects quotation cell types) and TYPED-VARIABLE/TYPED-BUFFER (layout-buffer surface) to accept a closed quotation type; a TYPED-VARIABLE HK xt<( n -- n )> accessor yields a pointer whose @ recovers xt<E> (T-QUOT, NOT a per-occurrence-freshened raw var - needs a persistent monomorphic cell type) so HK @ execute fit-checks the row against E, mirroring how ptr-typed cells preserve pointee types. Acceptance: positive - declared xt<E> cell round-trips store/fetch/execute with fit-check (wrong-effect store rejects; wrong-row execute rejects); negative - plain variable @ execute stays modeled as today until the RSEXEC flip (step 3, owned by 5923c543); fixpoint x2; checker suites + xt-effect v1-v9 green; docs/effects.md updated. Files: src/core/checker.f (storage-info + cell typing), src/core/layout-buffer.f or the typed-definer owner, tests. COORDINATE: same checker.f regions as tfam's active sealed-packages lane - hold or coordinate regions before dispatch. Follow-up (separate work item inside 5923c543's sequencing): migrate ~36 engine hooks (tfam-resolve*/match-*-tok/checker-*/lbuf/tdecl/habu1 hooks) to defer/is or typed cells; fprim* stay TRUSTED machine-code boundaries. Ownership: checker type-system capability.

Claim: agent=xtcells workspace=.jj-ws/fable-xtcells
