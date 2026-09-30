---
title: Count the ARM64 code-generation patterns
status: active
priority: 1
issue-type: task
created-at: "\"2026-09-30T10:58:28.225772+02:00\""
---

Campaign: ARM64 code-size fixes, design revision 3 (§2). Line references are at master 8c9b75af; re-verify before editing.

Problem: no tool counts the shapes the campaign's fixes target. tools/tier-census.f counts bytes, bl, frame traffic, moves and movk per word (tools/tier-census.f:1-60) and tools/engine-size.f tiles the file (tools/image-size-lib.f:913-971); neither says how many guarded divisions, constant shifts, mask chains, scaled indexes, call-crossing spills, terminal-only frames, no-return fallbacks, wide-store runs, signed maxima or DATA carriers per island a product contains. Every later slice's byte claim needs that multiplier.

Acceptance: tools/codegen-census.f, checked Habu, walks the aot/code-spans records of a tools/native-build.f product with src/arch/arm64/disasm.f and writes codegen-census.txt: first line the engine SHA-256 and source commit; then one `P <pattern> <sites> <bytes> <estimated-saving>` line per pattern (guarded division, terminal-only frame, mask chain, constant shift, scaled index, remainder, signed maximum, call-crossing spill, DATA carrier, no-return fallback, wide-store run); then the top forty records per pattern as `W <pattern> <name-or-offset> <sites>`. The DATA carrier row also reports distinct targets per record and per 1 MiB island. On the current product it reports nonzero totals for every pattern that exists today; on a non-image it refuses by name. Where its counts overlap the campaign baseline (~/.cache/tmp/heron-arm64/baseline/BASELINE.md) they agree, or the difference is explained. Artifact: the census report and the tier pair on the 13-file corpus with the engine SHA, totals quoted in the report.

Files: tools/codegen-census.f (new), tools/codegen-census-test.f (new; drives the tool on bin/hb and a non-image), test/gate-stdlib-cases.f (one SUITE row), docs/engine-size.md (one paragraph naming the tool).

Verify: tools/native-build.f product; census on it; tier pair (docs/compiler-measurements.md "Reproducing"); bin/hb --load tools/codegen-census-test.f; the new gate row. No engine text; no seed mirror.

Depends: none.

Ownership: the files above.

Claim: agent=heron workspace=.jj-ws/arm-census
