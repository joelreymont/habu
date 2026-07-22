---
title: "Infer KV: immutable device snapshot"
status: open
priority: 1
issue-type: task
created-at: "2026-07-22T09:38:16.958018+02:00"
blocks:
  - habu-infer-kv-fixed-a219f7ba
  - habu-add-immutable-lexical-28b79e06
---

Why this exists:
a device kernel must not read the mutable host ownership table directly.

Required result:
define a separately owned immutable snapshot containing generation-tagged sequence rows, lengths, and page identifiers in the exact device layout, built completely before publication.

Done when:
snapshot bytes are deterministic, stale generation and out-of-range rows reject before publication, host mutation after build cannot change a published snapshot, and snapshot disposal returns its mapping exactly once.

Expected touch points: new maki/infer/kv-snapshot.f, new maki/infer/kv-snapshot-test.f, maki/maki.f, FILEMAP.md.
Smallest check: bin/hb --load maki/infer/kv-snapshot-test.f; filemap lint.
Prerequisites: fixed block-table geometry and the common immutable-borrow capability.
Owned result: snapshot schema, builder, and lifetime only.
Claim: unassigned.
