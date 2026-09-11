---
title: Type JSON reader state
status: open
priority: 1
issue-type: task
blocks:
  - habu-lowering-hash-unified-586f7881
created-at: "2026-07-19T21:07:21.694273+02:00"
---

Current master packages the reader as JR but leaves its semantic state raw. lib/json-read.f:85-126 encodes three closed domains as integers: 12 public token kinds (T-*), six parser states (ST-*), and two container kinds (JR-CTX-*). JR-KIND/JR-STATE are generic variables and JR-CTX is a byte array; TOKEN/NEXT publicly return n at 182-183 and 442-443, so every consumer compares raw constants and the checker accepts unrelated integers. JR-STEP at 427-443 injects JR-RETRY=99 into the token channel, using control flow as a magic out-of-domain value. State dispatch is a comparison fallthrough chain, token predicates duplicate equality chains, and invalid stored states/kinds remain representable. Declare package-owned token, parser-state, and container-kind ENUMs; store them with TYPED-VARIABLE/LAYOUT-BUFFER; make NEXT/TOKEN and accessors return the token enum; replace state/token chains with exhaustive MATCH. Replace JR-RETRY with a payload ENUM whose variants are retry and token(token-kind), or a loop structure that cannot expose retry as a token. Preserve zero allocation, one-pass cursor behavior, token order, exact RFC 8259 rejection, source spans, the qualified JR API, and all downstream parsing. Add checker negatives for token/state/container swaps and raw-token consumers, exhaustive mutation coverage for every state/token/container, malformed-state impossibility, plus CODELEN/JIT/DATA and parse-throughput before/after; require no unexplained growth or throughput loss. Files: lib/json-read.f, json-read-test.f, every direct tool/library consumer found by rg, docs if public API changes. Ownership: JSON semantic-domain typing/control result only; package ownership and standalone-load repair have landed.
