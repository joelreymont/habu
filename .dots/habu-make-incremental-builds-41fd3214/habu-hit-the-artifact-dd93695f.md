---
title: Hit the artifact cache after reverting an input
status: open
priority: 1
issue-type: task
created-at: "2026-10-22T12:18:15.921687+03:00"
---

Problem: promise 'an unchanged whole build is a whole-image cache hit'. The AOT path reaches its artifact only through one record per context, hb-build-inputs-<ctx> (tools/hb-build-lib.f:1035-1046); when its rows mismatch, HBB-ARTIFACT-CACHE is 0 (:1125) and both lookups are skipped (:1175, :1280) although the matching artifact exists. Probe: app requiring lib.f, lib A->B->A reran the maker on the return to A with A's artifact in the cache. The engine path keeps the same single record engine-selection-<ctx> (:1386-1397, :1485-1491, :1601). Acceptance: the context record keeps every observed row-set (or one record per result key) and lookup checks each until one validates; E2E: A->B->A is an artifact hit on both paths. Files: tools/hb-build-lib.f. Depends: none. Claim: unassigned.
