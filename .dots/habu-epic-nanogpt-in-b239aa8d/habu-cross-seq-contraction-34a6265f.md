---
title: Cross-seq contraction checker reject
status: closed
priority: 2
issue-type: task
created-at: "\"\\\"2026-07-18T17:36:22.594997+02:00\\\"\""
closed-at: "2026-07-20T14:29:38.352443+02:00"
close-reason: "Landed 5f105ff0: BTC-5 soundness closer. The bad program (contracting the WHOLE B*T fold, redx<extprod<b,t>>) is proven a genuine LOAD-time process reject - a spawned child engine (fork+exec, CUDA-safe) loads it and dies exit 70 with the diagnostic naming the site; the legal direction (split the fold, contract inner #T only) loads AND runs real split arithmetic in the same fixture. Honest overlap statement: the checker RULE is BTC-7's (in-process candidate scores already cover the verdict); BTC-5's added value is the pinned process-level exit-70 regression the contract requires - not a runtime guard, not an in-process score. Closed by the checker rejecting the reduced bad program, exactly as the dot demanded"
---

Soundness closer for the (B,T,C) fold: minimal checked negative fixture proving a plain MATMUL over folded B*T rows fed where a within-sequence #T contraction is required is a load-time checker reject (exit 70), not a runtime error. Until the extent-role + factorization capabilities land this dot documents the gap (segment-op construction-only enforcement). MUST NOT be closed by a runtime guard - only by the checker rejecting the reduced bad program. Full contract: docs/batch-sequence-design.md section 5 BTC-5.

2026-07-20 SERIALIZED behind habu-extent-roles-b-df9d232f (spark): the reject fixture should be authored against the BTC-2 surface that lane builds; BTC-7 (the capability it needs) landed b192992e.

2026-07-20 serialization released (BTC-2 landed 2b6ad8f8 - the surface the fixture wants now exists; BTC-7 landed b192992e).
Claim: agent=crossseq workspace=.jj-ws/fable-crossseq machine=spark (TEST-ONLY lane: new fixture file + registration; maki/spec.f belongs to the rank0reg lane)
