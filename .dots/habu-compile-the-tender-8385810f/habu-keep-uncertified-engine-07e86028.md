---
title: Keep uncertified engine helpers internal after native compilation
status: active
priority: 1
issue-type: task
created-at: "2026-09-14T00:49:30.490501+03:00"
---

Owner Cedar. G2 exposed U-TYPE, T-RES, CT-LIVE? and CHECKER-BOUND:CURSORS after native compilation: inferred call shapes were being treated as source authority, and internal-mark recognized only a JIT prologue. The original reproducer is /tmp/cedar-internal-records.f.

The repair uses positive EFFECT-EXTERNAL ($8) provenance in the existing per-symbol flags. Explicit declarations and enforced successful checks grant it; ABI-only scans and failed multi-error rows do not. The checker-owned ABI scope restores enforcement on throws. Export, owner transfer and graph import preserve the bit. Ordinary checking may use a PRIM declaration's own effect when a user row is ABI-only; its trusted-only policy still applies. EFFECT-QUERY remains an ABI query, while CHECKER-RESOLVES? requires a source-visible user effect and still excludes primitive-only rows. Prefix sealing uses DKIND plus the source-authorized minimum, with no instruction decoding. ABI-only rows publish no dictionary minimum.

No owner ABI, graph-record or dictionary-layout growth. The graph mask admits $1000F and rejects unknown bits. Old producer rows without provenance acquire no grant at import; an enforcing target therefore cannot assume early old-host declarations were public. Activation is staged: private producer A records provenance before consumer B enforces it.

Frozen production source:

- 42f05fd7, parent 6ebbd50f: producer publication, scoped scan and native dispatch. This transitional product deliberately retains the old consumer gate.
- 39649b46, parent 42f05fd7: enforcing checker, source query, DKIND seal and focused authority/graph tests.

Actual products under /tmp/cedar-family-stage-abi:

- hb-effect-authority-A, reader-product hosted build, rc 0 in 130.64 s; SHA256 9c8b3906cbb3dc4fc2b171e1ae55074472c297d83854cab6934190539abdb125.
- hb-effect-authority-B, A-hosted build of 39649b46, rc 0 in 129.15 s; SHA256 9cb7464598b9c8a3055830e8ddea905b1a8209a850a87d9018f4103c68ce82ca.
- hb-effect-authority-C, B-hosted repeated build of the same source, rc 0 in 127.29 s; byte-identical to B.

B and C pass the actual tier-1 checker-effect-authority fixture (judged/ABI/explicit rows, primitive pairing, thrown scope, failed check, rollback/regrowth, package shadow/export and real native pre-hook/redefinition publication), the complete original internal-word-gate with an added CHECKER-BOUND cursor negative, and prefix-mark-test. Logs are /tmp/cedar-effect-authority-{unit,internal-gate,prefix-mark}.log and the corresponding -C- names. The complete B aot-payload-graph suite passes, including unknown-bit rejection and exited native producer/reader followed by a fresh consumer that refuses the ABI-only row and executes asserted/checked public calls. The fresh test must trigger lazy intake with CHECK! before EFFECT-QUERY; the ABI query itself does not import rows. The metadata fixture also requires a private bin/hb link to its paired product. Log: /tmp/cedar-effect-authority-graph.log.

Root's independent production review approved all publication paths, CTL ordering, rollback caches, transfer/import flags, primitive pairing and sealing. Root's unchanged checker-scan-index suite passes B (81.828 s). Its type-export run found the former whole-flags-zero expectation now includes EFFECT-EXTERNAL; the focused repair asserts control flags and authority separately and retains raw-zero rollback checks. The repaired suite passes B, rc 0, /tmp/cedar-effect-authority-type-export.log. Root owns combined product composition and the full gate. No compile-floor claim belongs to this correctness repair.
