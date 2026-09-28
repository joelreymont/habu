---
title: Reduce repeated boolean normalization
status: open
priority: 2
issue-type: task
created-at: "2026-09-16T16:09:22.609973+03:00"
---

## Rejected implementation

Candidate `bcd52c01400a372a4e5e469945ee0137830474c5` correctly combines
single-use integer comparisons followed by `0=`, including shared values,
canonical masks, overlapping chains and branch/select consumers. Independent
review passed after correcting a consuming equality's trap-schema check.
Focused semantic tests, five-generation engine/names equality, signatures and
both actual Maki board smokes passed. The full 492-suite registry was not run.

The candidate adds 1,556 bytes of optimizer code and removes 1,680 emitted
bytes: only **124 net AOT code bytes saved**. Other serialized content grows
200 bytes, so engine payload grows **76 bytes** and the signed file remains
2,790,775 bytes. Maki code falls 660 bytes, exactly offset by DATA/other growth;
its file remains 20,553,248 bytes. The local REQUIRE-BOOT-OPEN? body shrinks
48 to 36 bytes, but that does not justify the aggregate tradeoff.

Rejected for landing; no candidate source or tests are integrated. Keep this
task open for a smaller implementation with a measured overall benefit.
Receipt: `~/.cache/tmp/habu-native-bool-completion-20260928-01.md`.
Source/test patch: `~/.cache/tmp/habu-native-bool-fix-20260928-01/rejected.patch`,
SHA-256 `7e789f46b075e7bc18b7619f42651030d55c1aeec0c732e4f655db8d90ae0669`.

## Requirement and evidence

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
