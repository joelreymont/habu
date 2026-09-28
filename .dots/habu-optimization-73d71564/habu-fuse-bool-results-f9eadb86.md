---
title: Fuse boolean results into branches at tier 1
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:09:22.609973+03:00"
---

Current engine `24003f017a60` at source `e337d11fc098` already fuses a single-use
comparison into its branch: native select.f:1719 and baked CORE-STR= contain the
active path. The earlier claim that every boolean is materialized is obsolete.
REQUIRE-BOOT-OPEN? still emits two cmp/cset/neg groups for `@ 0= 0=` in a
48-byte body plus RET. REQUIRE-BOOT-LIMIT calls it, reloads its stack result and
branches; the call overhead is separately tracked by the small-colon dot.

For arbitrary n, `0= 0=` normalizes to 0/-1; it is not identity. Combine it to
one nonzero test. Eliminate normalization entirely only when canonical boolean
provenance is established structurally. Preserve escaping results, shared uses,
and existing comparison-to-branch fusion. Verify real native execution for zero,
canonical flags and noncanonical positive/negative inputs, with escaping and
branched consumers. Use before/after disassembly as a measurement artifact,
not a fixed opcode-count test. Record actual code/image delta, native byte
convergence and the full gate. Source and baked evidence:
`~/.cache/tmp/habu-generated-code-audit-20260928-01.md`.

Ownership: native selection; unclaimed. No implementation is asserted here.
