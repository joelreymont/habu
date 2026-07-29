---
title: Add generic bounded-copy result family
status: open
priority: 2
issue-type: task
created-at: "\"2026-07-22T15:51:34.318889+02:00\""
blocks:
  - habu-type-dsl-prove-93da83c4
  - habu-checker-sealed-destructure-d967fc03
---

Problem: JSON-WRITE, model-pack, normalized-config, and safetensors callers all
need one bounded-copy outcome that preserves a possibly linear owner on success
and refusal. Rejected commits 7a2572b6 and 7b62d6af used the internal parse-only
ENUM-DECL:ED-RUN entry, left package COPY reopenable, tested constructors
without a real copy, and enrolled no canonical gate.

Required result: after the unified declaration cutover, package COPY declares
the public full-form payload ENUM result<a> with exactly two variants:
copied(owner:a, length:len) and required(owner:a, length:len). COPY:TO consumes
an owner, source span, destination span, and destination capacity. It validates
the complete source length before writing. Sufficient capacity copies every
byte and returns copied with the same owner and exact length; insufficient
capacity writes no destination byte and returns required with the same owner
and exact required length. Its exact effect is
`COPY:TO ( a ptr u8 len ptr u8 len -- COPY:result<a> )`, ordered as owner,
source address, source length, destination address, destination capacity. The
package exposes no raw tag, constructor,
ED-RUN, SUMTYPE, PRODUCT, optional/absent family, bool, sentinel, compatibility
alias, or public mutation surface. Seal the generated constructor owner and
package so external source cannot reopen or publish into either namespace.

Acceptance: the declaration uses only the public unified ENUM surface. A
DEFLINEAR test owner passes through both COPY:TO outcomes exactly once; checked
negatives reject dropping or duplicating it. The production COPY:TO test covers
zero length, exact capacity, excess capacity, and one-byte-short refusal; a
sentinel-filled destination remains byte-identical on refusal. Reopening COPY,
publishing COPY:FORGE, and calling private/generated raw construction reject.
lib/adt/copy-test.f is enrolled in the canonical type-linear gate, and a
mutation that removes its suite entry fails suite coverage. Typed-local,
package, type-linear, suite-coverage, host, and file-map gates pass.

Files: lib/adt/copy.f, lib/adt/copy-test.f, the canonical type-linear suite
inventory, and FILEMAP.md. Smallest real check: the enrolled type-linear slice
running COPY:TO against a real destination buffer. Depends:
habu-type-dsl-prove-93da83c4 and habu-checker-sealed-destructure-d967fc03. Owned result:
generic copied/required outcome and
COPY:TO only; optional/absence policy remains consumer-owned. Claim: released;
commits 7a2572b6 and 7b62d6af and workspace .jj-ws/copy-result-impl are rejected
evidence only.
