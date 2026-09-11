---
title: Reduce repeated IR arena and context access cost in AOT builds
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-11T16:07:57.864391+03:00\""
---

Owner: Rowan arena lane .jj-ws/rowan-arena; ownership src/compiler/ir/arena.f and context.f, base41df9051. Ask before shared-reader edits. Prior direct O(1) registry-index fix is complete in habu-remove-linear-lookup-b8a6ab72; do not duplicate it or describe FIND-A/FIND-SLOT as linear64-slot scans.

Profile .jj-ws/rowan-context/tmp/measurements/tender-sampling.txt and tender-perdef.tsv: roughly49% sampled time in validated IR handle/arena accessors, another14.6% ambiguous nearby. FIND-SLOT12.4s, FIND-A9s, HANDLE@8.2s, ASTATE/AHANDLE/AOWNER17.2s, RESOLVE6.5s,NATIVE-SLOT6.1s on older173s calibrated compile. Dominant cost is nested accessor/validation chains, not a fresh registry scan. Session-reuse prototype offered only16.9s and was dropped in favor of this measured layer.

Acceptance: same fresh integrated before/after Tender source forced entirely through tier1, count NCOMP:COMPILE calls, retain stale-generation/context-retirement/capture negative tests and canonical identities. Report measured full-workload gain; no validator gating or benchmark result cache. User target is seconds for optimizing AOT; current full compile remains159s on a later root candidate.

Rowan13:15UTC clean sampling/counters: arena31.6%,context13.1%,CDIGEST slot7.6%, combined52.3%; 1.774billion handle resolutions,1.233billion cell reads. Single ANSWER-COUNT7.806-7.816s matches root7.817s baseline. First candidate only arena/context flattens16 nested read calls to3 with same handle/owner/state/bounds checks and order; cold/test/full before-after pending. No broad scoped-reader API or caller migration authorized yet; finish this measured candidate and report residual first. Sampling works without counter dispatch wrapper; separate profiler failure recorded elsewhere.
