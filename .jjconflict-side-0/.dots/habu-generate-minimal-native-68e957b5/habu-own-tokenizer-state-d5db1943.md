---
title: Own tokenizer state
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T22:43:26.103927+02:00"
---

Current master architecture, package, and resident-state defect in new maki/tokenizer.f: the module defines global E-TOK-* words before any package, then represents one logical vocabulary as five ambient MAKI-package globals under a TOK-* pseudo-namespace: byte-to-id, id-to-byte, seen scratch, size, and build cursor. The broad MAKI:TOK-* public surface provides no package boundary, the scratch array remains resident after construction, rebuild mutates live tables in place, and no value ties the inverse, forward table, size, or readiness together. Concurrent, nested, failed, or pre-build consumers can observe a mixed vocabulary. After unified type lowering lands, create package TOKENIZER containing its errors plus STRUCTURE vocabulary with bounded forward and inverse views, size, and payload-bearing ENUM state = uninitialized | ready(vocabulary). Build into caller-owned or one staged owner buffer, derive the inverse and size from the same staged value, validate bijection/completeness, then publish or return it once; keep construction scratch local/reusable and outside ready state. Make ENCODE, DECODE, ID, CHAR, and SIZE consume an explicit immutable vocabulary or the typed ready state; expose short qualified names and delete every global E-TOK-* and MAKI:TOK-* alias. Preserve ascending byte ordering and exact encoded corpus bytes. Compact the inverse representation too: TOK-INV currently spends 2,048 bytes of cells on ids bounded to 0..255, beside 256-byte seen and forward tables. Use byte storage plus explicit readiness/presence, or another measured bounded representation, without sentinel ambiguity. Prove old/private names reject, independent vocabularies coexist, nested/concurrent builds cannot cross-contaminate, failed rebuild leaves the prior value byte-identical, all 256-byte vocab and sparse vocab round trips, capacity/canaries, and before/after source definitions, dictionary names, JIT, DATA, CODELEN, resident table bytes, construction, and encode/decode throughput with a required resident-state reduction. Files: maki/tokenizer.f, data-loader callers, tests. Blocker: habu-lowering-hash-unified-586f7881. The immediate raw bounds are owned by habu-bound-tokenizer-api-111a9a88.

GROOMED 2026-08-04 (dot-groom). Path repoint, contract unchanged. Commit cec8db652 "Move
nanoGPT app into maki/examples/nanogpt" moved the subject file: maki/tokenizer.f is now
maki/examples/nanogpt/tokenizer.f, and its suite is maki/examples/nanogpt/tokenizer-test.f.
The data-loader caller moved with it (maki/examples/nanogpt/data-loader.f). The FILEMAP entry
in the Files list is void - FILEMAP.md was deleted by commit 85a9646fd "Delete FILEMAP and
census gates" - so the write set is the tokenizer module, its callers and its tests only.
