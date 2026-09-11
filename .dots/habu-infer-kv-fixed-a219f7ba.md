---
title: "Infer KV: fixed block-table geometry"
status: closed
priority: 1
issue-type: task
created-at: "2026-07-22T09:38:16.909035+02:00"
closed-at: "2026-07-22T13:28:17.090569+02:00"
close-reason: Merged fe107b5d on green master; fresh destruction review accepted fixed geometry and exact KV/Maki gates passed.
---

Why this exists:
maki/infer/kv-cache.f stores one growable vector per sequence, so maximum-context capacity is neither fixed nor fully accounted at cache creation.

Required result:
derive block capacity from the checked maximum context and page-token configuration, reserve one fixed block-table slice per sequence inside the metadata mapping, and reject overflowing geometry before allocation.

Done when:
exact-capacity tables append to their last slot, one-slot-over rejects without mutation, dead slots contain no page identifiers, and metadata byte accounting is exact.

Expected touch points: maki/infer/kv-cache.f, maki/infer/kv-cache-test.f.
Smallest check: bin/hb --load maki/infer/kv-cache-test.f; typed-local diff lint.
Prerequisites: landed base allocator 1835f711.
Owned result: fixed table geometry and metadata partition only.
Claim: agent=kvfixed workspace=.jj-ws/habu-infer-kv-fixed-a219f7ba machine=spark.
