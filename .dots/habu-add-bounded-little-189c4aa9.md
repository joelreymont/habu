---
title: Add bounded little-endian u32 read on mappings
status: active
priority: 1
issue-type: task
created-at: "2026-07-28T13:45:26.362405+02:00"
blocks:
  - habu-seal-safet-wordlists-70973382
  - habu-publish-typed-byte-429962bb
---

Problem: SAFET has no checked way to read a little-endian 32-bit value while
proving that all four bytes lie inside an owned mapping.

Owner: package SAFET in `maki/infer/safetensors.f`.

Dependencies: `habu-seal-safet-wordlists-70973382` and
`habu-publish-typed-byte-429962bb` must land first.

Public interface:
`SAFET:U32-LE@? ( SAFET:mapping CAD-NUM:byte-off -- SAFET:mapping option<n> )`.

Contract: use the mapping record's carried byte length. Prove that typed
`off + 3` is representable and strictly contained before projecting a raw
address or reading any byte. A valid aligned or unaligned input returns the four
little-endian bits as `some(n)`. Negative, overflow, underflow, bad-alignment,
or misaligned numeric outcomes, and every out-of-range valid offset, return
`none`. Preserve mapping ownership on every outcome. Production and test code
both directly require CAD-NUM; production has one bounded import of it. The
single address advance uses public `CAD-NUM:BYTE+`; the reader defines no
projection of its own.

Acceptance: fixed byte constants independent of the reader prove aligned and
unaligned values from a real file-backed `SAFET:LOAD` mapping. A revoked-page
child proves that both the first straddling offset and `MAX-N` return `none`
without touching the revoked pages. Eager-read mutations at both invalid
offsets fail; each mutation performs a guaranteed byte read from the revoked
mapping in its refusal arm, so a wrapped or non-faulting address cannot
satisfy the proof. The SAFET test entry passes through the production load
path.

Forbidden: an extent or pointer accessor; F32 decode; a raw pointer API;
a private `SAFET:BOFF>N` or any other trusted projection; any TRUSTED.md
growth; PRODUCT, SUMTYPE, or another legacy type; a magic fixture length;
generated child source; double unmap; discarded cleanup results; aliases;
shims; unrelated model work.

Source order: this is the last of three serial source commits. The typed
`CAD-NUM:BYTE+` advance lands first, then the generic seal. One atomic commit
then adds the bounded implementation, its independent fixed-byte oracle, the
direct CAD-NUM dependencies, and the revoked-page no-touch proof so no partial
reader contract can land.

Verify: `bin/hb --load maki/infer/safetensors-test.f`.

Claim: agent=claude workspace=.jj-ws/habu-add-bounded-little-189c4aa9
